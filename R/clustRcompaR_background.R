process_cutdata <- function(data, corpus, min_terms){
    # this removes responses from cutdata that weren't clusters (due to length, etc.)
    doc_sums <- rowSums(as.matrix(corpus$DFM))
    doc_logical <- doc_sums >= min_terms
    process_data <- data[doc_logical,]
    results <- invisible(process_data)
    results
}

#' First corpus building function
#'
#' @param data The data from which the corpus is drawn with documents in
#'  first column
#' @param ... The metadata columns following the text column
#' @param stopwords Words to exclude from the clustering
#' @details Puts together the corpus and dfm from the data frame provided
#' @export
assemble_corpus <- function(data, stopwords, remove_twitter){
  data <- data.frame(data, stringsAsFactors = F)
  corpus_frame <- dplyr::select(data, dplyr::everything())
  text_vector <- as.character(corpus_frame[,1])
  dfm <- quanteda::dfm(text_vector, remove_twitter = remove_twitter, stem = TRUE, remove = stopwords, remove_punct = TRUE)
  a_corp <- quanteda::corpus(text_vector)
  quanteda::metadoc(a_corp) <- corpus_frame[,2:ncol(corpus_frame)]
  results = list(Corpus = a_corp, DFM = dfm)
  invisible(results)
}

#' Cleans the DFM based on specified term minimums
#'
#' @param corp A corpus opject as created by \code{assemble_corpus}.
#' @param minimum_term_frequency Minimum number of occurances for a term to be used
#' @param min_terms Minimum number of terms for document to be used
#' @details Removes terms and documents that don't meet term and doc minimums
#' @export
clean_dfm <- function(corp, minimum_term_frequency, min_terms){
  term_sums <- colSums(as.matrix(corp$DFM))
  term_logical <- term_sums >= minimum_term_frequency
  doc_sums <- rowSums(as.matrix(corp$DFM))
  doc_logical <- doc_sums >= min_terms
  cleaned_dfm <- corp$DFM[doc_logical, term_logical]
  cleaned_dfm
}

# calculates vector projections
vect_project <- function(a,b){
  project <- suppressWarnings(crossprod(a,b) * b)
  project
}

# calculates deviation vectors
dev_vector <- function(vect_list){
  norm_vects <- lapply(vect_list, ppls::normalize.vector)
  sum_vect <- colSums(do.call(rbind, norm_vects))
  norm_sum <- ppls::normalize.vector(sum_vect)
  projects <- lapply(norm_vects, vect_project, norm_sum)
  difference <- mapply('-', norm_vects, projects)
  dev_vects <-  apply(difference, MARGIN = 2, FUN = ppls::normalize.vector)
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
#' @param n_clusters number of desired clusters
#' @param cleanDFM DFM object from \code{clean_dfm} function
#' @param num_terms Minimum number of terms per document
#' @details Applies 2 stage clustering algorithm, using Ward's method for
#'  hierarchical agglomerative clustering to set the centers for the specified
#'  number of clusters.  K-means algorithm uses these centers as a starting
#'  point and fits its model.
#'  @export
cluster_text <- function(mat, dev_mat, n_clusters, cleanDFM, num_terms){
  #   wss <- list()
  #   for (i in 2:18){
  #     wss[i] <- sum(kmeans(mat, centers = i)$betweenss) / kmeans(mat, centers = i)$totss
  #   }
  dev_mat_t <- t(dev_mat)

  # What distance measure should we be using here? should be cosine similarity
  distance <- stats::dist(dev_mat_t, method = "euclidean")
  mat_dev_t_clust <- stats::hclust(distance)
  hclust_cut <- stats::cutree(mat_dev_t_clust, n_clusters)

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
  kfit <- stats::kmeans(dev_mat_t, start)
  ss_explained <- sum(kfit$betweenss) / kfit$totss
  cluster_words <- list()
  # Problem here in the current version
  outputList <- list()
  for(i in 1:n_clusters){
    clusterDFM <- cleanDFM[kfit$cluster == i,]
    totalTermFreqs <- quanteda::colSums(clusterDFM)
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
