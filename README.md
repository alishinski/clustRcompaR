# clustRcompaR

An `R` package to cluster and compare text data.

## Background 
[Document clustering](https://en.wikipedia.org/wiki/Document_clustering) is a common technique to discover topics in a corpus of texts. This package uses functions from the [`quanteda`](https://github.com/kbenoit/quanteda) `R` package as the basis for two functions, `cluster()` and `compare(), to make document clustering and comparing topics identified through document clustering across factors straightforward.

## Workflow

* First, use `cluster()` on a `data.frame` with the first column a `vector` of `strings` and any subsequent columns `vectors` of `factors`.
 
* Optional arguments to the `cluster()` function include parameters for the minimum frequency with which a term must occur to be included in the analysis, the minimum number of terms in each document after processing, and additional stopwords. The output from the `cluster()` function can then be inspected to determine the interpretability of clusters and the suitability of the clustering solution.

* Next, use `compare()` with the output from the `cluster()` function along with a `string` for the factor to compare the frequency of clusters to. 
 
* This output can be examined and used on its own, or be passed to two functions, `compare_plot()`, which plots the table using [`ggplot2`](https://github.com/hadley/ggplot2), or `compare_test()`, which performs a chi-square test of proportions on the table and indicates across which levels of the factor clusters appear more or less likely than expected.
