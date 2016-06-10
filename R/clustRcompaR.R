# Hard coded stopwords

standard_stopwords <- c("a", "an", "the", "to", "of", "and", "for", "by", "on", "is", "I", "all", "this", "with",
                        "it", "at", "from", "or", "you", "as", "your", "are", "be", "that", "not", "have", "was",
                        "we", "what", "which", "there", "they", "he", "she", "his", "hers", "had", "word", "our",
                        "you", "about", "that", "this", "but", "not", "what")
additional_stopwords <- c("lorum")
all_stopwords <- append(standard_stopwords, additional_stopwords)

#' Cluster wrapper function
#'
#'@param data The data frame comparing the text vector as the first column
#'  and any metadata in subsequent columns
#'@param n_clusters The number of clusters to be used for the clustering solution
#'@param minimum_term_frequency The minimum number of occurances for a term to be included
#'@param min_terms The minimum number of terms for a document to be included
#'@param num_terms Number of terms to display in clustering summary output
#'@details Performs the clustering half of the process, including assembling
#'  and cleaning the corpus, deviationalizing and clustering.
#'@export
cluster_text <- function(data, ..., n_clusters, minimum_term_frequency = 3, min_terms = 3, num_terms = 10){
  corpus <- assemble_corpus(data, ...)
  cleanDFM <- clean_dfm(corpus)
  devVects <- deviationalize(cleanDFM)
  # add parameter for clean DFM
  cluster(devVects$MAT, devVects$DEV_MAT, cleanDFM, num_terms)
}

clusteredData <- cluster_text(cutdata, data.grade, data.teacher, data.time) # what the eff is _YYYYY_
assemble_corpus(cutdata, data.grade, data.teacher, data.time)

#' First corpus building function
#'
#' @param dataset The data from which the corpus is drawn with documents in
#'  first column
#' @param ... The metadata columns following the text column
#' @details Puts together the corpus and dfm from the data frame provided
#' @export
assemble_corpus <- function(dataset, ...){
  corpus_frame <- dplyr::select(dataset, everything())
  text_vector <- as.character(corpus_frame[,1])
  dfm <- quanteda::dfm(text_vector, removeTwitter = T, stem = T, ignoredFeatures = all_stopwords)
  a_corp <- quanteda::corpus(text_vector)
  quanteda::metadoc(a_corp) <- corpus_frame[,2:ncol(corpus_frame)]
  results = list(Corpus = a_corp, DFM = dfm)
  invisible(results)
}

#' Cleans the DFM based on specified term minimums
#'
#' @param corpus A corpus opject as created by \code{assemble_corpus}.
#' @details Removes terms and documents that don't meet term and doc minimums
#' @export
clean_dfm <- function(corp){
  term_sums <- colSums(as.matrix(corp$DFM))
  term_logical <- term_sums >= minimum_term_frequency
  doc_sums <- rowSums(as.matrix(corp$DFM))
  doc_logical <- doc_sums >= min_terms
  cleaned_dfm <- corp$DFM[doc_logical, term_logical]
  cleaned_dfm
}

# calculates vector projections
vect_project <- function(a,b){
  project <- crossprod(a,b) * b
  project
}

# calculates deviation vectors
dev_vector <- function(vect_list){
  norm_vects <- lapply(vect_list, normalize.vector)
  sum_vect <- colSums(do.call(rbind, norm_vects))
  norm_sum <- normalize.vector(sum_vect)
  projects <- lapply(norm_vects, vect_project, norm_sum)
  difference <- mapply('-', norm_vects, projects)
  dev_vects <-  apply(difference, MARGIN = 2, FUN = normalize.vector)
  dev_vects
}

#' Deviationalizes term vectors using Sherin's (2013) technique
#'
#' @param cleaned_dfm A clean dfm object as created by \code{clean_dfm}
#' @details Turns term vectors into deviation vectors.  This turns the
#'  magnitude of each vector into a representation of its distance from
#'  the centroid, rather than its absolute direction.
#'  @export
deviationalize <- function(cleaned_dfm){
  cleaned_dfm_mat <- as.matrix(cleaned_dfm)
  cleaned_dfm_mat_t <- t(cleaned_dfm_mat)
  list_dfm_mat <- apply(cleaned_dfm_mat_t, 2, list)
  mat_vec <- lapply(list_dfm_mat, unlist)
  mat_dev <- dev_vector(mat_vec)
  results = list(MAT = cleaned_dfm_mat_t, DEV_MAT = mat_dev)
}

#' Clusters the vectors using 2-stage clustering algorithm
#'
#' @param mat The clean dfm as a matrix and transposed, from \code{deviationalize}
#' @param dev_mat The deviation matrix of the dfm, from \code{deviationalize}
#' @details Applies 2 stage clustering algorithm, using Ward's method for
#'  hierarchical agglomerative clustering to set the centers for the specified
#'  number of clusters.  K-means algorithm uses these centers as a starting
#'  point and fits its model.
#'  @export
cluster <- function(mat, dev_mat, cleanDFM, num_terms){
#   wss <- list()
#   for (i in 2:18){
#     wss[i] <- sum(kmeans(mat, centers = i)$betweenss) / kmeans(mat, centers = i)$totss
#   }
  dev_mat_t <- t(dev_mat)

  # What distance measure should we be using here? should be cosine similarity
  distance <- dist(dev_mat_t, method = "euclidean")
  mat_dev_t_clust <- hclust(distance)
  hclust_cut <- cutree(mat_dev_t_clust, n_clusters)

  clusters1 <- list()
  for (i in seq(n_clusters)){
    clusters1[[i]] <- dev_mat_t[hclust_cut == i,]
  }

  ordered_clusters1 <- list()
  cluster_freqs1 <- list()
  for (i in seq(length(clusters1))){
    ordered_clusters1[[i]] <- colSums(as.matrix(clusters1[[i]]) / nrow(clusters1[[i]]))
    cluster_freqs1[[i]] <- ordered_clusters1[[i]]
  }

  start <- data.frame(matrix(unlist(cluster_freqs1), nrow=length(cluster_freqs1[[1]]), byrow=T), stringsAsFactors=FALSE)
  start <- as.matrix(start)
  start <- t(start)
  kfit <- kmeans(dev_mat_t, start)
  ss_explained <- sum(kfit$betweenss) / kfit$totss
  cluster_words <- list()

  outputList <- list()
  for(i in 1:n_clusters){
    clusterDFM <- cleanDFM[kfit$cluster == i,]
    totalTermFreqs <- colSums(clusterDFM)
    sortedTermFreqs <- sort(totalTermFreqs, decreasing = T)
    outputTerms <- sortedTermFreqs[1:num_terms]
    outputList[[length(outputList) + 1]] <- names(outputTerms)
    names(outputList)[length(outputList)] <- paste("Cluster", i, "Terms")
    outputList[[length(outputList) + 1]] <- outputTerms / nrow(clusterDFM)
    names(outputList)[length(outputList)] <- paste("Cluster", i, "Term Frequencies")
  }
  clusterTerms <- as.data.frame(outputList)
  row.names(clusterTerms) <- NULL
  # new output including the kmeans output as well as the most frequent terms
  results = list(clusters = kfit, terms = clusterTerms)
}

# clusteredData <- cluster_text(cutdata, data.grade, data.teacher, data.time) # what the eff is _YYYYY_

process_cutdata <- function(){
  # this removes responses from cutdata that weren't clusters (due to length, etc.)
  # this creates a new cutdata, cutdata_ss

  # can we just use cutdata? it breaks somehow when we do, but had to add text name so we can find which are missing
  cutdata_new <- data.frame(data.textid = paste0("text", 1:nrow(data)), data$purpose, data$grade, data$teacher, data$time)
  # finds which are missing from cluster function output and removes them
  not_missing_cluster_assignment <- cutdata_new$data.textid %in% names(clusteredData$clusters$cluster)
  cutdata_ss <- cutdata_new[not_missing_cluster_assignment, ]
  cutdata_ss$cluster <- clusteredData$clusters$cluster # adds cluster assignment from cluster function to cutdata
}

#' Compare wrapper function
#'
#' @param compare_which A factor variable of the groups of interest for comparison.
#' @param which_clusters Clusters to be included in the comparison. Default is all clusters.
#' @param which_groups Levels of the grouping factor to be included in the comparison. Default is all levels.
#'
#' @details Function for comparing clustering solution between subgroups.  Output is contingency table for the specified groups and clusters.
#' @export
compare <- function(compare_which, which_clusters, which_groups){
  # Compare specified groups based on cluster frequencies
  # chi square differences for the groups
  df <- process_cutdata()
  comparison_table <- table(df$cluster, eval(parse(text = paste("df", compare_which, sep = "$"))))
  invisible(comparison_table[which_clusters, which_groups])
}

#' Compare plot function
#'
#' @param comparison_table The table output from the \code{compare} function
#'
#' @details Creates a plot visualizing group clustering differences across the groups and clusters specified in the \code{compare} function. Creates a ggplot 2 object, so default parameters can be overridden by adding layers to this object.
#' @export
compare_plot <- function(comparison_table){

  # chisq_p <- chisq.test(comparison_table)
  # asterisk <- as.vector(chisq_p$stdres > 1.96 | chisq_p$stdres < -1.96)
  # asterisk[asterisk == TRUE] <- "*"
  # asterisk[asterisk == FALSE] <- ""

  to_plot <- t(t(comparison_table)/colSums(comparison_table))
  to_plot <- as.data.frame(to_plot)
  to_plot
  # doc_plot <- cbind(doc_by_index, asterisk) # this is associated with the asterisk code above

  names(to_plot) <- c("Cluster", "Group", "Proportion")

  dodge = position_dodge(.9) # not sure this is needed

  # lots of defaults that could be changed or made more simple

  plot <- ggplot(to_plot, aes(x = Group, y = Proportion, color = Cluster, ymax = max(Proportion)),
                 scale_color_brewer(palette="Set3")) +
    # geom_bar(width = .825, position = dodge, stat = "identity") +
    geom_line(aes(group = Cluster), size = .75) +
    geom_point() +
    # geom_text(aes(label = ChiSq), position = dodge, vjust = .25) +
    theme_minimal() +
    # ylab("Proportion of Responses") +
    ylab("Proportion of Responses") +
    xlab("Group") +
    scale_color_brewer(palette="Set1") +
    scale_x_discrete(name="") +
    theme(text = element_text(size = 20)) +
    theme(legend.position = "bottom") +
    theme(legend.title=element_blank()) +
    theme(legend.direction = "vertical") +
    theme(legend.key.size = unit(1.66, "lines")) +
    theme(text=element_text(size = 14, family = "Avenir"))
  plot
}

#' Compare test function
#'
#' @param comparison_table The table output from the \code{compare} function
#'
#' @details Performs a chi-squared test across the groups and clusters specified in the \code{compare} function. Output gives omnibus test results and a table indicating significant individual chi-squared differences.
#' @export
compare_test <- function(comparison_table){
  chisq_p <- chisq.test(comparison_table)
  chisq_p$stdres[chisq_p$stdres > 1.96] <- "Sig. Greater"
  chisq_p$stdres[chisq_p$stdres < -1.96] <- "Sig. Lesser"
  chisq_p$stdres[chisq_p$stdres < 1.96 & chisq_p$stdres > -1.96] <- "Not Sig."
  results <- list(chisq_p, chisq_p$stdres)
  invisible(results)
}

compare_test(compare("data.teacher"))

out <- compare("data.teacher")
out
create_plot(out)

kfit <- clusteredData$clusters
cleanDFM <- clusteredData$dfm
num_terms <- 5

outputList <- list()
for(i in 1:n_clusters){
  clusterDFM <- cleanDFM[kfit$cluster == i,]
  totalTermFreqs <- colSums(clusterDFM)
  sortedTermFreqs <- sort(totalTermFreqs, decreasing = T)
  outputTerms <- sortedTermFreqs[1:num_terms]
  outputList[[length(outputList) + 1]] <- names(outputTerms)
  names(outputList)[length(outputList)] <- paste("Cluster", i, "Terms")
  outputList[[length(outputList) + 1]] <- outputTerms / nrow(clusterDFM)
  names(outputList)[length(outputList)] <- paste("Cluster", i, "Term Frequencies")
}
clusterTerms <- as.data.frame(outputList)
row.names(clusterTerms) <- NULL

dumbList <- list(c(1,2,3), c(4,5,6), c(7,8,9))
names(dumbList) <- c("first", "second", "third")

as.data.frame(dumbList)

str(clusteredData$cluster)
clusteredData$cluster == 1

##########################################

library(ppls)
library(quanteda)
setwd("/home/alex/Dropbox/clustRcompaR") # Alex
setwd("~/dropbox/research/clustRcompaR") # Josh
data <- read.csv("scip_data.csv", header = T)
# View(data)
setwd("/home/alex/Dropbox/R Alex Files/cluster-compare-text")

cutdata <- data.frame(data$purpose, data$grade, data$teacher, data$time)

corpus_frame <- dplyr::select(data, ...)
text_vector <- as.character(corpus_frame[,1])
dfm <- quanteda::dfm(text_vector, removeTwitter = T, stem = T, ignoredFeatures = all_stopwords)
a_corp <- quanteda::corpus(text_vector)
quanteda::metadoc(a_corp) <- corpus_frame[,2:ncol(corpus_frame)]
results = list(Corpus = a_corp, DFM = dfm)
invisible(results)

corpus <- assemble_corpus(cutdata, data$purpose, data$grade, data$teacher, data$time)
cleanDFM <- clean_dfm(corpus)
devVectors <- deviationalize(cleanDFM)
clustKfit <- cluster(devVectors$MAT, devVectors$DEV_MAT)
clustNames <- names(clustKfit$cluster[clustKfit$cluster == 1])

View(cleanDFM)
clust1rows <- row.names(cleanDFM) %in% clustNames
clust1 <- cleanDFM[clust1rows,]
sort(colSums(clust1), decreasing = T)[1:10] / nrow(clust1)

cleanDFMdf <- as.data.frame(cleanDFM)
cleanDFMdf$cluster <- clustKfit$cluster

clustered <- group_by(cleanDFMdf, cluster)

summarize(clustered, sort(colSums(), decreasing = T)[1:10] / nrow())
sort(colSums(clust1), decreasing = T)[1:10] / nrow(clust1)

tapply(cleanDFMdf, cleanDFMdf$cluster, FUN = function(x){colSums(x)})

