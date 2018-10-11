
library(igraph)
source("work/generate_graph.R")
devtools::load_all()


# Generate random graph
# Assign group randomly with different fractions. -> no seggregation
# Calculate measures: 
# - SSI
# - Modularity
# - Community detection and 



n <- c(500, 1000, 2000)
p <- seq(0.05, 0.5, by = 0.05)
pdrop <- seq(0, 0.9, by = 0.1)


pars <- expand.grid(n = n, p = p, pdrop = pdrop)

res <- pars
res$mean_ssi1   <- as.numeric(NA)
res$median_ssi1 <- as.numeric(NA)
res$max_ssi1    <- as.numeric(NA)
res$min_ssi1    <- as.numeric(NA)
res$mean_ssi2   <- as.numeric(NA)
res$median_ssi2 <- as.numeric(NA)
res$max_ssi2    <- as.numeric(NA)
res$min_ssi2    <- as.numeric(NA)
res$modularity  <- as.numeric(NA)


pb <- txtProgressBar(max = nrow(res), style = 3)
for (i in seq_len(nrow(pars))) {
  
  g <- generate_network(n = pars$n[i], p = pars$p[i], pdrop = pars$pdrop[i])
  
  # Plot
  # if (n <= 100) plot(g, vertex.color = V(g)$group)
  
  # Create vertex and edge list
  vertices <- data.frame(
    id = as.numeric(V(g)), 
    group = V(g)$group
  )
  edges <- as.data.frame(as_edgelist(g))
  
  
  ssi1 <- ssi(vertices, edges, group = 1)
  ssi2 <- ssi(vertices, edges, group = 2)

  res$mean_ssi1[i]   <- mean(ssi1$ssi)
  res$median_ssi1[i] <- median(ssi1$ssi)
  res$max_ssi1[i]    <- max(ssi1$ssi)
  res$min_ssi1[i]    <- min(ssi1$ssi)
  res$mean_ssi2[i]   <- mean(ssi2$ssi)
  res$median_ssi2[i] <- median(ssi2$ssi)
  res$max_ssi2[i]    <- max(ssi2$ssi)
  res$min_ssi2[i]    <- min(ssi2$ssi)
  res$modularity[i]  <- modularity(as.undirected(g), V(g)$group)
  
  randwalk <- random_walk_segregation(vertices, edges, alpha = 0.85)
  
  res$rw1 <- randwalk[[1]]$segregation
  res$rw2 <- randwalk[[2]]$segregation
  res$rw1_norm <- randwalk[[1]]$segregation_norm
  res$rw2_norm <- randwalk[[2]]$segregation_norm
  
  setTxtProgressBar(pb, i)
}
close(pb)

saveRDS(res, "work/results.RDS")
