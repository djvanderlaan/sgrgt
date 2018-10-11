
# group <- "purple"
# edges_weight <- "weight"
# vertices_id <- 1
# vertices_group <- 2
# verbose <- 0

#' Random Walk Segregation Index
#'
#' @export
random_walk_segregation <- function(vertices, edges, alpha = 0.85,
    edges_weight = "weight", vertices_id = 1, vertices_group = 2, verbose = 0) {
  
   # Prep edges
  if (verbose > 0) message("Prep edges")
  if (is.null(edges[[edges_weight]])) {
    edges <- edges[1:2]
    edges[[edges_weight]] <- 1.0
  } else {
    edges <- edges[c(names(edges[1:2]), edges_weight)]
  }
  names(edges)[1:2] <- c("src", "dst")
  
  # Check for duplicate edges and make sure that for every edge a->b there is a 
  # edge b->a
  el <- paste0(edges$src, "-", edges$dst)
  stopifnot(all(!duplicated(el)))
  # el_rev <- paste0(edges$dst, "-", edges$src)
  # missing_edges <- !(el_rev %in% el)
  # if (any(missing_edges)) {
  #   warning(paste0("Not all edges a->b have corresponding edge b->a;", 
  #     " generating corresponding edges. "))
  #   missing_edges <- data.frame(
  #     src = edges$dst[missing_edges],
  #     dst = edges$src[missing_edges],
  #     weight = edges$weight[missing_edges],
  #     stringsAsFactors = FALSE
  #   )
  #   warning("Generated ", nrow(missing_edges), " edges.")
  #   edges <- dplyr::bind_rows(edges, missing_edges)
  # }
  
  # Calculate weight for each edge: each person has a total weight of 1
  edges <- edges %>% dplyr::group_by(src) %>% 
    dplyr::mutate(weight = weight/sum(weight)) %>% 
    dplyr::ungroup()
  
  # Prep vertices
  if (verbose > 0) message("Prep vertices")
  vertices <- data.frame(id = vertices[[vertices_id]], 
    group = vertices[[vertices_group]], stringsAsFactors = FALSE)
  
  # Check ids
  stopifnot(all(edges$src %in% vertices$id))
  stopifnot(all(edges$dst %in% vertices$id))

  # Create adjacency matrix
  G <- Matrix::Matrix(0, nrow = nrow(vertices), ncol = nrow(vertices), 
    sparse = TRUE)
  row <- match(edges$src, vertices$id)
  col <- match(edges$dst, vertices$id)
  G[cbind(row, col)] <- edges$weight
  
  
  groups <- unique(vertices$group)
  rw <- vector("list", length(groups))
  names(rw) <- groups
  for (group in groups) {
    # Calculate segregation of group
    c <- 1.0 * (vertices$group == group)
    b <- c / sum(c)
    v <- solve(diag(nrow(G)) - alpha * G, (1-alpha)*as.numeric(G %*% c))
    seg <- as.numeric(b %*% v)
    f <- sum(c)/length(c)
    # Add individual segregation to vertices
    vert <- vertices[vertices$group == group, , drop = FALSE]
    vert$randwalk <- v[vertices$group == group]
    vert$randwalk_norm <- vert$randwalk / f
    # Add to result list
    rw[[group]] <- list(
      segregation = seg,
      segregation_norm = seg / f,
      vertices = vert
    )
  }
  if (verbose > 0) message("Done!")
  rw
}

