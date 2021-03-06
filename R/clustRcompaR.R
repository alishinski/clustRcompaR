#' Cluster wrapper function
#'
#'@param data The data frame comparing the text vector as the first column
#'@param ... Additional columns of the data frame containing metadata cfor comparison
#'@param n_clusters The number of clusters to be used for the clustering solution
#'@param minimum_term_frequency The minimum number of occurences for a term to be included
#'@param min_terms The minimum number of terms for a document to be included
#'@param num_terms Number of terms to display in clustering summary output
#'@param stopwords Additional stopwords to exclude from clustering analysis
#'@param remove_twitter Whether to remove text associated with Twitter content, useful for when analyzing data from this source (defaults to FALSE)
#'@details Performs the clustering half of the process, including assembling
#'  and cleaning the corpus, deviationalizing and clustering.
#'@examples
#' library(clustRcompaR)
#' library(dplyr)
#' library(quanteda)
#'
#' d <- inaugural_addresses
#' d <- mutate(d, century = ifelse(Year < 1800, "17th",
#'                                 ifelse(Year >= 1800 & Year < 1900, "18th",
#'                                        ifelse(Year >= 1900 & Year < 2000, "19th", "20th"))))
#'
#' three_clusters <- cluster(d, century, n_clusters = 3)
#' extract_terms(three_clusters)
#'
#' three_clusters_comparison <- compare(three_clusters, "century")
#' compare_plot(three_clusters_comparison)
#'@export

cluster <- function(data, ..., n_clusters, minimum_term_frequency = 3, min_terms = 3, num_terms = 10, stopwords = NULL, remove_twitter = FALSE){
  standard_stopwords <- c("a", "an", "the", "to", "of", "and", "for", "by", "on", "is", "I", "all", "this", "with",
                          "it", "at", "from", "or", "you", "as", "your", "are", "be", "that", "not", "have", "was",
                          "we", "what", "which", "there", "they", "he", "she", "his", "hers", "had", "word", "our",
                          "you", "about", "that", "this", "but", "not", "what")
  additional_stopwords <- stopwords
  if(!is.null(additional_stopwords)){
  all_stopwords <- append(standard_stopwords, additional_stopwords)
  } else {
    all_stopwords <- standard_stopwords
  }
  corpus <- assemble_corpus(data, stopwords = all_stopwords, remove_twitter = remove_twitter)
  cleanDFM <- clean_dfm(corpus, minimum_term_frequency, min_terms)
  print(cleanDFM)
  compare_frame <- process_cutdata(data, corpus, min_terms)
  devVects <- deviationalize(cleanDFM)
  clusters <- cluster_text(devVects$MAT, devVects$DEV_MAT, n_clusters, cleanDFM, num_terms)
  results <- invisible(list(cluster = clusters, compare = compare_frame))
  results
}

#' Compare wrapper function
#'
#' @param compare_which A factor variable of the groups of interest for comparison.
#' @param clustering_solution The output from the \code{cluster} function.
#' @param which_clusters Clusters to be included in the comparison. Default is all clusters.
#' @param which_groups Levels of the grouping factor to be included in the comparison. Default is all levels.
#'
#' @details Function for comparing clustering solution between subgroups.  Output is contingency table for the specified groups and clusters.
#' @examples
#' library(clustRcompaR)
#' library(dplyr)
#' library(quanteda)
#'
#' d <- inaugural_addresses
#' d <- mutate(d, century = ifelse(Year < 1800, "17th",
#'                                 ifelse(Year >= 1800 & Year < 1900, "18th",
#'                                        ifelse(Year >= 1900 & Year < 2000, "19th", "20th"))))
#'
#' three_clusters <- cluster(d, century, n_clusters = 3)
#' extract_terms(three_clusters)
#'
#' three_clusters_comparison <- compare(three_clusters, "century")
#' compare_plot(three_clusters_comparison)
#' @export

compare <- function(clustering_solution, compare_which, which_clusters = NULL, which_groups = NULL){
  # Compare specified groups based on cluster frequencies
  # chi square differences for the groups

  clusters <- clustering_solution$cluster$clusters$cluster
  comparison_table <- table(clusters, eval(parse(text = paste("clustering_solution$compare", compare_which, sep = "$"))), dnn = c("clusters", deparse(substitute(compare_which))))

  if (is.null(which_clusters)) {
    which_clusters = 1:length(unique(clusters))
  }

  if (is.null(which_groups)) {
    which_groups = 1:ncol(comparison_table)
  }

  invisible(comparison_table[which_clusters, which_groups])
}

#' Compare plot function
#'
#' @param comparison_table The table output from the \code{compare} function
#' @details Creates a plot visualizing group clustering differences across the groups and clusters specified in the \code{compare} function. Creates a \code{ggplot} object, so default parameters can be overridden by adding layers to this object.
#' @export
compare_plot <- function(comparison_table){
  to_plot <- t(t(comparison_table)/colSums(comparison_table))
  to_plot <- as.data.frame(to_plot)
  names(to_plot)[2] <- "group"
  plot <- ggplot2::ggplot(to_plot, ggplot2::aes(x = to_plot$group, y = to_plot$Freq, fill = to_plot$clusters)) +
    # ggplot2::geom_line(ggplot2::aes(group = to_plot$clusters), size = .75) +
    # ggplot2::geom_point() +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::theme_minimal() +
    ggplot2::ylab("Proportion of Responses") +
    ggplot2::xlab("Group") +
    ggplot2::scale_fill_brewer("Cluster", type = "qual", palette = 6) +
    ggplot2::theme_bw()
  plot
}

#' Compare test function
#'
#' @param comparison_table The table output from the \code{compare} function
#'
#' @details Performs a chi-squared test across the groups and clusters specified in the \code{compare} function. Output gives omnibus test results and a table indicating significant individual chi-squared differences.
#' @export
compare_test <- function(comparison_table){
  chisq_p <- stats::chisq.test(comparison_table)
  chisq_p$stdres[chisq_p$stdres > 1.96] <- "Sig. Greater"
  chisq_p$stdres[chisq_p$stdres < -1.96] <- "Sig. Lesser"
  chisq_p$stdres[chisq_p$stdres < 1.96 & chisq_p$stdres > -1.96] <- "Not Sig."
  results <- list(chisq_p, chisq_p$stdres)
  results
}

#' Extracts terms and term frequencies
#'
#' @param object output from the cluster() function
#' @details Extracts the terms and term frequencies from the output of the cluster() function
#' @export

extract_terms <- function(object) {
  object[[1]]$terms
}
