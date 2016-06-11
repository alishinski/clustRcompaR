#################################
### clustRcompaR Testing Code ###
#################################

library(ppls)
library(quanteda)
library(ggplot2)
library(tidyr)

#grab the data
setwd("/home/alex/Dropbox/clustRcompaR") # Alex
#setwd("~/dropbox/research/clustRcompaR") # Josh
data <- read.csv("scip_data.csv", header = T)
cutdata <- data.frame(data$purpose, data$grade, data$teacher, data$time)

# clustering test
clusteredDataNew <- cluster(cutdata, data.grade, data.teacher, data.time, n_clusters = 5)

#raw clustering output
clusteredDataNew$cluster$clusters$cluster
#print output
clusteredDataNew$cluster$terms
#compare data frame
clusteredDataNew$compare

# Compare functions testing
out <- compare("data.teacher", clusteredDataNew)
out

# test testing code
compare_test(out)
# test plotting code
compare_plot(out)
