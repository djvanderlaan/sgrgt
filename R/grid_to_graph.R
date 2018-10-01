

#' Convert grid to proximity graph
#'
#' @examples
#' grid <- as.matrix(read.table(textConnection("
#' 0 0 0 0 0 0
#' 1 1 0 0 1 0
#' 1 1 0 0 1 0
#' 1 1 1 1 0 0
#' 0 0 0 0 0 0
#' ")))
#' 
#' g <- grid_to_graph(grid)
#' ssi(g$vertices, g$edges, group = 1)
#' 
#' @export
grid_to_graph <- function(grid) {
  n <- prod(dim(grid))
  ids <- matrix(seq_len(n), nrow = nrow(grid), ncol = ncol(grid))
  
  edges <- vector("list", 4)
  edges[[1]] <- data.frame(src = as.numeric(ids), dst = shift_cor(ids, x = 1), 
    stringsAsFactors = FALSE)
  edges[[2]] <- data.frame(src = as.numeric(ids), dst = shift_cor(ids, x = -1), 
    stringsAsFactors = FALSE)
  edges[[3]] <- data.frame(src = as.numeric(ids), dst = shift_cor(ids, y = 1), 
    stringsAsFactors = FALSE)
  edges[[4]] <- data.frame(src = as.numeric(ids), dst = shift_cor(ids, y = -1), 
    stringsAsFactors = FALSE)
  edges <- dplyr::bind_rows(edges)
  edges <- dplyr::filter(edges, !is.na(src), !is.na(dst))
  vertices <- data.frame(ids = as.numeric(ids), group = as.numeric(grid),
    stringsAsFactors = FALSE)
  list(vertices = vertices, edges = edges)
}

shift_cor <- function(M, x = 0, y = 0) {
  N <- prod(dim(M))
  C <- arrayInd(seq_len(N), .dim = dim(M))
  C[,1] <- C[,1] + y
  C[,2] <- C[,2] + x
  oob <- (C[,1] < 1) | (C[,1] > dim(M)[1]) | 
    (C[,2] < 1) | (C[,2] > dim(M)[2])
  C[oob, ] <- NA
  M[C]
}

