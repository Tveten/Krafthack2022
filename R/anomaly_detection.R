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

#' @export
anomalies_from_cpt <- function(cpt, x, tol = 1) {
  if (length(cpt) == 0) {
    return(list("collective" = data.table(start = integer(0), end = integer(0)),
                "point"      = data.table(location = integer(0))))
  }
  n <- nrow(x)
  res <- data.table(location = c(0, cpt, n))
  res$mean_size <- 0
  for (i in 2:length(res$location)) {
    seg_mean <- colMeans(x[(res$location[i - 1] + 1):res$location[i], , drop = FALSE])
    res$mean_size[i] <- sign(sum(seg_mean)) * sqrt(sum(seg_mean^2))
  }
  starts <- integer(0)
  ends <- integer(0)
  in_anom <- FALSE
  curr_start_ind <- 0
  i <- 2
  while (i <= nrow(res)) {
    if (!in_anom && abs(res$mean_size[i]) >= tol) {
      curr_start_ind <- i - 1
      starts <- c(starts, res$location[curr_start_ind] + 1)
      in_anom <- TRUE
      i <- i + 1
    } else {
      if (in_anom) {
        end_anom <- is_in_interval(res$mean_size[i], c(-tol, tol))
        switch_anom <- (res$mean_size[curr_start_ind + 1] < 0 && res$mean_size[i] > tol) ||
          (res$mean_size[curr_start_ind + 1] > 0 && res$mean_size[i] < - tol)
        if (end_anom) {
          ends <- c(ends, res$location[i - 1])
          in_anom <- FALSE
        } else if (switch_anom) {
          ends <- c(ends, res$location[i - 1])
          curr_start_ind <- i - 1
          starts <- c(starts, res$location[curr_start_ind] + 1)
        }
      }
      i <- i + 1
    }
  }
  if (in_anom) ends <- c(ends, res[.N, location])
  if (length(starts) != length(ends)) {
    print(starts)
    print(ends)
    stop("Bug when extracting inspect anomalies. Unequal number of start and end points.")
  }
  anoms <- data.table(start = starts, end = ends)
  return(list("collective" = anoms[start != end],
              "point"      = data.table(location = anoms[start == end, start])))
}
