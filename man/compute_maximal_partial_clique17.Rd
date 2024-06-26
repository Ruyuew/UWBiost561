% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute_maximal_partial_clique17.R
\name{compute_maximal_partial_clique17}
\alias{compute_maximal_partial_clique17}
\title{Compute the maximal partial clique in an adjacency matrix}
\usage{
compute_maximal_partial_clique17(adj_mat, alpha)
}
\arguments{
\item{adj_mat}{a symmetric adjacency matrix}

\item{alpha}{the edge density of the adjacency matrix}
}
\value{
a list containing the indices of the maximal partial clique, the edge density of the maximal partial clique, and all partial cliques in the adjacency matrix
}
\description{
Enumerate over each row to find where the edges are
Start with the first row as the first partial clique
For each subsequent row, check if it connects with any of the existing partial cliques.
If it does, append to the existing partial cliques; if it doesn't, create a new partial clique for this row.
Return the partial clique with the most nodes
}
