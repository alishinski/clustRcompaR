# clustRcompaR

An R package to cluster and compare text data.

The overall approach, which is adapted from [Sherin (2013)](http://www.tandfonline.com/doi/abs/10.1080/10508406.2013.836654#.VcazbRNViko) is similar to that in [Latent Semantic Analysis](https://en.wikipedia.org/wiki/Latent_semantic_analysis), but instead of using singular value decomposition to identify topics in texts, it uses a two-step (hierarchical and k-means) clustering process.
Here is the overall process:

1. Pre-process text (remove punctuation, stopwords, content-specific terms, and stem terms)
2. Create a term document matrix
3. Filter the term document matrix
4. Process the documents by using deviation vectors 
5. Cluster the documents using hierarchical and then k-means clustering
6. Compare each cluster / topic to each group using frequencies and chi-square test of proportion tests
7. Examine output

