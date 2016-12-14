#################################
### clustRcompaR Testing Code ###
#################################

library(ppls)
library(quanteda)
library(ggplot2)
library(tidyr)
devtools::load_all()

#grab the data
# setwd("/home/alex/Dropbox/clustRcompaR") # Alex
setwd("~/dropbox/research/clustRcompaR") # Josh
data <- read.csv("scip_data.csv", header = T)
cutdata <- data.frame(data$purpose, data$grade, data$teacher, data$time, stringsAsFactors = F)
cutdata$data.purpose <- as.character(cutdata$data.purpose)
str(cutdata

# clustering test
clusteredDataNew <- cluster(cutdata, data.grade, n_clusters = 5)
clusteredDataNew[[1]]
cluster_text
colSums
quanteda::colSums
showMethods("quanteda::colSums")
#raw clustering output
clusteredDataNew$cluster$clusters$cluster
#print output
clusteredDataNew$cluster$terms
#compare data frame
clusteredDataNew$compare

# Compare functions testing
out <- compare(clusteredDataNew, "data.grade", 1:5, 1:2)
out

# test testing code
compare_test(out)
# test plotting code
compare_plot(out)
