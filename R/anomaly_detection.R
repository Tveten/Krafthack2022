#' Detect anomalies by CAPA-CC
#'
#' @param x Data matrix.
#'
#' @return A capacc object.
#'
#' @export
detect_anomalies_capacc <- function(x, est_band = 2, b = 1, b_point = 1, minsl = 2) {
  p <- ncol(x)
  adjacency_matrix <- capacc::adjacency_mat(
    capacc::banded_neighbours(est_band, p),
    sparse = FALSE
  )
  Q_hat <- capacc::robust_sparse_precision(x, adjacency_matrix)
  capacc::capa.cc(
    x,
    Q_hat,
    b = b,
    b_point = b_point,
    min_seg_len = minsl
  )
}
