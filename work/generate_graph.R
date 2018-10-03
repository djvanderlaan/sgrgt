
generate_network <- function(n, p, pdrop, type = c("fitness", "pa")) {

  # Generate random groups
  groups <- 0
  while (sum(groups) == 0) groups <- rbinom(n, size = 1, prob = p) 
  groups <- groups + 1
  
  type <- match.arg(type)
  
  if (type == "pa") {
    g <- sample_pa(n, directed = FALSE)
  } else if (type == "fitness") {
    g <- sample_fitness_pl(n, n*min(n/5, 50), exponent.out = 2)
  }
  g <- set_vertex_attr(g, "group", value = groups)
  
  
  
  # Drop a fraction of between group edges
  c1 <- tail_of(g, E(g))$group
  c2 <- head_of(g, E(g))$group
  drop <- rbinom(length(c1), size = 1, prob = pdrop) == 1
  drop <- (c1 != c2) & drop
  g <- subgraph.edges(g, E(g)[!drop], delete.vertices = FALSE)
  as.directed(g)
}
