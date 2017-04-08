rm(list = ls())
source("cluster_functions.R")
library(ggplot2)


set.seed(1)
k = 4
# receive and check the argument from the terminal 
args = commandArgs(trailingOnly = TRUE)
if(length(args) == 0) {
  k <- 4
} else if(length(args) == 1){
  k <- as.integer(args[1])
} else {
  stop("At most one argument can be supplied.", call.=FALSE)
}

hard <- read.csv("TwoDimHard.csv", head = TRUE)
hard_cluster <- clustering(hard[,2:3], k)
true_cluster <- hard$cluster
#table(hard$cluster, hard_cluster)

# compute SSE for each cluster
sse <- double(k)
sse_true <- double(k) 

for(i in 1:k) {
  sse[i] <- SSE(hard[hard_cluster == i, 2:3])
  sse_true[i] <- SSE(hard[hard$cluster == i, 2:3])
}

ssb <- SSB(hard[, 2:3], hard_cluster, k)
ssb_true <- SSB(hard[,2:3], hard$cluster, k)

hard_cluster = as.factor(hard_cluster)

if(k == 4) {
  true_centroids <- matrix(0, nrow = 4, ncol = 2)
  my_centroids <- matrix(0, nrow = 4, ncol = 2)
  cluster_rename <- integer(nrow(hard))
  for(i in 1:4) {
    true_centroids[i,] <- apply(hard[hard$cluster == i, 2:3], 2, mean)
    my_centroids[i,] <- apply(hard[hard_cluster == i, 2:3], 2, mean)
  }
  map <- integer(4)
  for(i in 1:4) {
    dist <- integer(4)
    for(j in 1:4) {
      dist[j] <- d(my_centroids[i,], true_centroids[j,])
    }
    map[i] <- which.min(dist)
  }
  for(i in 1:nrow(hard)) {
    hard_cluster[i] <- map[hard_cluster[i]]
  }
}

print(table(hard_cluster, hard$cluster))
print("Overall SSE:")
print(sum(sse))
print("SSB:")
print(ssb)

# ggplot(hard, aes(X.1, X.2, color = hard_cluster)) + geom_point()
# ggplot(hard, aes(X.1, X.2, color = as.factor(cluster))) + geom_point()
# ggplot(hard, aes(X.1, X.2, color = (cluster == hard_cluster), shape = as.factor(cluster))) + geom_point()


