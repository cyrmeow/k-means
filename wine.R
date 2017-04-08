rm(list = ls())
source("cluster_functions.R")
library(ggplot2)
set.seed(1)


wine <- read.csv("wine.csv", header = TRUE)
wine.std <- scale(wine[,2:12])
# receive and check the argument from the terminal 
args = commandArgs(trailingOnly = TRUE)
if(length(args) == 0) {
  k <- 6
} else if(length(args) == 1){
  k <- as.integer(args[1])
} else {
  stop("At most one argument can be supplied.", call.=FALSE)
}
cluster.wine <- clustering(wine.std, k)

sse <- double(k) 

for(i in 1:k) {
  sse[i] <- SSE(wine[cluster.wine == i, 2:3])
}

ssb <- SSB(wine.std, cluster.wine, k)

print(table(cluster.wine, wine$quality))
print("Overall SSE:")
print(sum(sse))
print("SSB")
print(ssb)


