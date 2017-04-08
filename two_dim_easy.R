rm(list = ls())
source("cluster_functions.R")
library(ggplot2)

set.seed(1)
k = 2
# receive and check the argument from the terminal 
args = commandArgs(trailingOnly = TRUE)
if(length(args) == 0) {
  k <- 2
} else if(length(args) == 1){
  k <- as.integer(args[1])
} else {
  stop("At most one argument can be supplied.", call.=FALSE)
}

easy <- read.csv("TwoDimEasy.csv", head = TRUE)
easy_cluster <- clustering(easy[,2:3], k)
true_cluster <- easy$cluster

# compute SSE for each cluster
sse <- double(k)
sse_true <- double(k)
for(i in 1:k) {
  sse[i] <- SSE(easy[easy_cluster == i, 2:3])
  sse_true[i] <- SSE(easy[easy$cluster == i, 2:3])
}

ssb <- SSB(easy[, 2:3], easy_cluster, k)
ssb_true <- SSB(easy[,2:3], easy$cluster, k)

if(k == 2) {
  true_centroids <- matrix(0, nrow = 2, ncol = 2)
  my_centroids <- matrix(0, nrow = 2, ncol = 2)
  cluster_rename <- integer(nrow(easy))
  for(i in 1:2) {
    true_centroids[i,] <- apply(easy[easy$cluster == i, 2:3], 2, mean)
    my_centroids[i,] <- apply(easy[easy_cluster == i, 2:3], 2, mean)
  }
  map <- integer(2)
  for(i in 1:2) {
    dist <- integer(2)
    for(j in 1:2) {
      dist[j] <- d(my_centroids[i,], true_centroids[j,])
    }
    map[i] <- which.min(dist)
  }
  for(i in 1:nrow(easy)) {
    easy_cluster[i] <- map[easy_cluster[i]]
  }
}

print(table(easy_cluster, easy$cluster))
print("Overall SSE:")
print(sum(sse))
print("SSB:")
print(ssb)



