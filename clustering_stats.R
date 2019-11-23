# Katherine M. Prioli
# CSC 8515 Final Project - working file for tinkering with logic to extract clustering statistics
# Sat Nov 23 17:17:20 2019 ------------------------------


clust_stats <- function(approach, k) {
  # metrics <- c("cluster.number",  # Could use this to make a for-loop (to clean up the loop below)
  #              "withinss",
  #              "avewithin",
  #              "asw")
  
  metrics_prett <- c("Cluster No.",
                     "Within-cluster Sum of Squares",
                     "Average Within-Cluster Distance",
                     "Average Silhouette Width")
  
  stats_mat <- matrix(nrow = 4, ncol = k - 1)
  rownames(stats_mat) <- metrics_prett
  
  iterations <- c() %>% as.vector()
  
  for(i in 2:k){
    iterations[i - 1] <- paste0("Iteration ", i - 1)
    
    stats_mat[1, i - 1] <- round(cqcluster.stats(d = dissim_mat, clustering = cutree(approach, i))$cluster.number, digits = 0)
    stats_mat[2, i - 1] <- round(cqcluster.stats(d = dissim_mat, clustering = cutree(approach, i))$withinss, digits = 3)
    stats_mat[3, i - 1] <- round(cqcluster.stats(d = dissim_mat, clustering = cutree(approach, i))$avewithin, digits = 3)
    stats_mat[4, i - 1] <- round(cqcluster.stats(d = dissim_mat, clustering = cutree(approach, i))$asw, digits = 3)
    
    # for(j in 1:length(metrics_prett)){   # Flesh this logic out per above note
    #   stats_mat[j, i - 1] <- cqcluster.stats(d = dissim_mat, clustering = cutree(approach, i))
    # }
  }
  
  colnames(stats_mat) <- iterations
  
  cluster_sizes <- cqcluster.stats(d = dissim_mat, clustering = cutree(approach, k))$cluster.size
  
  return(stats_mat)
  return(cluster_sizes)
  
}

agnes_stats <- clust_stats(nhanes_agnes, 10)

#diana_stats <- clust_stats(nhanes_diana, 10)