# Katherine M. Prioli
# CSC 8515 Final Project - finding clustering statistics
# Sat Nov 23 23:57:03 2019 ------------------------------


clust_stats <- function(approach, k) {
  metrics <- c("cluster.number",
               "withinss",
               "asw")
  
  metrics_prett <- c("Number of Clusters",
                     "Within-Cluster Sum of Squares",
                     "Average Silhouette Width")
  
  stats_mat <- matrix(nrow = length(metrics), ncol = k - 1)
  rownames(stats_mat) <- metrics_prett
  
  iterations <- c() %>% as.vector()
  
  for(i in 2:k){
    iterations[i - 1] <- paste0("Iter. ", i - 1)
    
    for(j in 1:length(metrics_prett)){
      stats_mat[j, i - 1] <- unlist(cqcluster.stats(d = dissim_mat, clustering = cutree(approach, i)))[metrics[j]] %>% round(digits = 3)
    }
  }
  
  colnames(stats_mat) <- iterations
  cluster_stats <- stats_mat
  
  cluster_sizes <- cqcluster.stats(d = dissim_mat, clustering = cutree(approach, k))$cluster.size %>% 
    as_tibble() %>% 
    mutate(cluster_no = row_number()) %>% 
    rename(cluster_size = value) %>% 
    select(cluster_no, cluster_size)
  
  results <- list(cluster_stats = cluster_stats, cluster_sizes = cluster_sizes)
  return(results)
  
}

agnes_stats <- clust_stats(nhanes_agnes, 15)
diana_stats <- clust_stats(nhanes_diana, 15)

agnes_screedata <- agnes_stats$cluster_stats %>% 
  t() %>% 
  as_tibble() %>% 
  rename(n_clusters = "Number of Clusters",
         withinss = "Within-Cluster Sum of Squares",
         asw = "Average Silhouette Width")

diana_screedata <- diana_stats$cluster_stats %>% 
  t() %>% 
  as_tibble() %>% 
  rename(n_clusters = "Number of Clusters",
         withinss = "Within-Cluster Sum of Squares",
         asw = "Average Silhouette Width")