#################################
### clustRcompaR Testing Code ###
#################################

# library(ppls)
library(testthat)
library(quanteda)
library(ggplot2)
# library(tidyr)
devtools::load_all()

install.packages("quanteda")

#library(clustRcompaR)
library(dplyr)

# inaugural addresses
d <- inaugural_addresses

#d <- data("inaugural_addresses")
d <- dplyr::mutate(d, century = ifelse(Year < 1800, "17th",
                                ifelse(Year >= 1800 & Year < 1900, "18th",
                                       ifelse(Year >= 1900 & Year < 2000, "19th", "20th"))))



three_clusters <- cluster(d, n_clusters = 3)
extract_terms(three_clusters)
three_clusters_comparison <- compare(three_clusters, "century")
compare_plot(three_clusters_comparison)
compare_test(three_clusters_comparison)

# scip
d <- readr::read_csv("scip_data.csv")
d <- select(d, audience2, everything())
four_cluster_solution <- cluster(d, n_clusters = 4)
four_cluster_comparison <- compare(four_cluster_solution, "teacher")
compare_plot(four_cluster_comparison)
compare_test(four_cluster_solution)

dtmp <- tempfile()
# The first run always succeeds
expect_known_output(mtcars[1:10, ], tmp, print = TRUE)
# Subsequent runs will suceed only if the file is unchanged
# This will succeed:
expect_known_output(mtcars[1:10, ], tmp, print = TRUE)
## Not run:
# This will fail
expect_known_output(mtcars[1:9, ], tmp, print = TRUE)
cluster()

tmp
expect_equal_to_reference()



new  <- cluster(d, n_clusters = 4)

expect_known_output(cluster(d, n_clusters = 4), "four_cluster.rds")

readRDS("four_cluster.rds")
cluster(d, n_clusters = 4)
saveRDS(object = four_cluster_solution, file = "four_cluster.rds")
# #grab the data
# # setwd("/home/alex/Dropbox/clustRcompaR") # Alex
# # setwd("~/dropbox/research/clustRcompaR") # Josh
# d <- inaugural_addresses
# # cutdata <- data.frame(data$purpose, data$grade, data$teacher, data$time, stringsAsFactors = F)
# # cutdata$data.purpose <- as.character(cutdata$data.purpose)
#
# d <- dplyr::mutate(year_before_1900 = ifelse(Year < 1900, 1, 0))
#
# # clustering test
# clusteredDataNew <- cluster(d, year_before_1900, n_clusters = 2)
# clusteredDataNew[[1]]$terms
#
# # #raw clustering output
# # clusteredDataNew$cluster$clusters$cluster
# # #print output
# # clusteredDataNew$cluster$terms
# # #compare data frame
# # clusteredDataNew$compare
#
# # Compare functions testing
# out <- compare(clusteredDataNew, "Year", 1:2, )
# out
#
# # test testing code
# compare_test(out)
# # test plotting code
# compare_plot(out)
