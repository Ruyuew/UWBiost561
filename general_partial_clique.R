#' Generate Partial Clique
#'
#' This function generates a random adjacency matrix that includes a partial clique.
#' The matrix is symmetric, with specified fractions of nodes forming a partial clique,
#' and a defined edge density within that clique.
#'
#' @param n Integer, the total number of nodes in the graph.
#' @param clique_fraction Numeric, the fraction of the total nodes that will form the partial clique.
#' @param clique_edge_density Numeric, the density of the edges within the partial clique.
#'
#' @return A list containing:
#' \itemize{
#'   \item{adj_mat}{The adjacency matrix, a symmetric matrix with 1s along the diagonal, 
#'                  and no row or column names.}
#' }
#'
#' @examples
#' set.seed(123)  # For reproducibility
#' result <- generate_partial_clique(n = 10, clique_fraction = 0.5, clique_edge_density = 0.5)
#' print(result$adj_mat)
#'
#' @export
generate_partial_clique <- function(n = 10, clique_fraction = 0.5, clique_edge_density = 0.5) {
  adj_mat <- matrix(sample(c(0, 1), size = n^2, prob = c(1 - clique_edge_density, clique_edge_density), replace = TRUE), nrow = n, ncol = n)
  adj_mat <- (adj_mat + t(adj_mat)) > 0
  diag(adj_mat) <- 1
  clique_size <- round(n * clique_fraction)
  # Ensuring the clique
  for (i in 1:clique_size) {
    for (j in i:clique_size) {
      adj_mat[i, j] <- 1
      adj_mat[j, i] <- 1
    }
  }
  return(list(adj_mat = adj_mat))
}

