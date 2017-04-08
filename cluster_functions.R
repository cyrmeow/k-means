d <- function(x1, x2){
  return(sum((x1-x2)^2))
}

clustering <- function(data, k) {
  N       <- nrow(data)
  MAX     <- 100
  e       <- 10^-8
  centroids.origin  <- data[sample(1:N, k, replace = FALSE), ]
  dist    <- 100
  iter    <- 0
  centroids.new     <- matrix(0, k, ncol(data))
  cluster <- rep(0, N)
  
  while(dist > e & iter <= MAX){
    # calculate cluster
    for (i in 1:N){
      cluster[i] <- which.min(apply(centroids.origin, 1, function(x) d(x, data[i,])))
    }
    # calculate new centroids
    for (i in 1:k){
      centroids.new[i,] <- apply(data[cluster == i, ], 2, mean)
    }
    dist = d(centroids.origin[1,], centroids.new[1,]) + d(centroids.origin[2,], centroids.new[2,])
    print(dist)
    iter <- iter + 1
    centroids.origin <- centroids.new
  }
  return(cluster)
}

SSE <- function(cluster) {
  center <- apply(cluster, 2, mean)
  dists <- apply(cluster, 1, function(x) {d(x, center)})
  return (sum(dists))
}

SSB <- function(data, clusterList, nCluster) {
  center <- apply(data, 2, mean)
  centroid <- matrix(0, nCluster, ncol(data))
  size <- integer(nCluster)
  dist <- integer(nCluster)
  for(i in 1:nCluster) {
    centroid[i,] <- apply(data[which(clusterList == i), ], 2, mean)
  }
  for(i in 1:nCluster) {
    dist[i] <- nrow(data[which(clusterList == i), ]) * d(centroid[i, ], center)
  }
  
  return (sum(dist))
}