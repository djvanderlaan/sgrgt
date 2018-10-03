

# group <- "purple"
# edges_weight <- "weight"
# vertices_id <- 1
# vertices_group <- 2
# verbose <- 0

#' Spectral Segregation Index
#'
#' @export
ssi <- function(vertices, edges, group, edges_weight = "weight", vertices_id = 1, 
    vertices_group = 2, verbose = 0) {
  
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
  el_rev <- paste0(edges$dst, "-", edges$src)
  missing_edges <- !(el_rev %in% el)
  if (any(missing_edges)) {
    warning(paste0("Not all edges a->b have corresponding edge b->a;", 
      " generating corresponding edges. "))
    missing_edges <- data.frame(
      src = edges$dst[missing_edges],
      dst = edges$src[missing_edges],
      weight = edges$weight[missing_edges],
      stringsAsFactors = FALSE
    )
    warning("Generated ", nrow(missing_edges), " edges.")
    edges <- dplyr::bind_rows(edges, missing_edges)
  }
  
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
  
  # Check if group present in groupvar
  stopifnot(all(group %in% vertices$group))
  stopifnot(length(group) == 1)
  sel_group <- group
  
  # Select edges of given group
  group_vertices <- dplyr::filter(vertices, group == sel_group) %>% 
    dplyr::select(-group)
  group_edges <- edges %>% 
    dplyr::inner_join(group_vertices, by = c("src" = "id")) %>%
    dplyr::inner_join(group_vertices, by = c("dst" = "id")) 
  
  
  # Determine connected components
  if (verbose > 0) message("Determine connected components")
  g <- igraph::graph_from_data_frame(group_edges[1:2], 
    vertices = group_vertices)
  comp <- igraph::components(g, mode = "weak")
  group_vertices$component <- comp$membership
  group_edges <- dplyr::left_join(group_edges, group_vertices, by = c("src" = "id"))
  if (verbose > 0) message("Number of components found: ", comp$no)
  
  # For each component:
  # - create adjacency matrix
  # - calculate largest eigenvalue
  components <- unique(group_vertices$component)
  ssi <- vector("list", length(components))
  
  for (i in seq_along(components)) {
    if (verbose > 1) message("Processing component ", i)
    # Select edges and vertices of current component
    eg <- dplyr::filter(group_edges, component == components[i])
    vg <- dplyr::filter(group_vertices, component == components[i])
    # Handle edgeless edge case 
    if (nrow(eg) == 0) {
      vg$ssi_component <- 0
      vg$ssi <- 0
      ssi[[i]] <- vg
      next
    }
    # Create adjacency matrix for component
    g  <- igraph::graph_from_data_frame(eg, directed = TRUE, vertices = vg)
    M  <- igraph::as_adjacency_matrix(g, attr = "weight", sparse = TRUE)
    # Calculate eigenvalues
    e <- if (ncol(M) < 3) {
      e <- eigen(M)
    } else {
      e <- RSpectra::eigs(M, 1)
    }
    eigenv <- abs(e$values[1])
    v <- abs(e$vectors)[,1]
    # Scale v
    v <- eigenv/mean(v)*v
    # Store results
    vg$ssi_component <- eigenv
    vg$ssi <- v
    ssi[[i]] <- vg
  }
  ssi <- dplyr::bind_rows(ssi)
  # ... and done
  if (verbose > 0) message("Done!")
  ssi
}

