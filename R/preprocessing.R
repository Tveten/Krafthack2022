#' @export
find_burnin_and_shutdown <- function(dt, tol = 3 * 60) {
  dt[, sec := as.numeric(difftime(timepoints, timepoints[1]))]
  dt[, sec_to_previous := c(1, sec[-1] - sec[-.N])]

  segment_start <- c(1, dt[, which(sec_to_previous > tol)], nrow(dt) + 1)
  n_segments <- length(segment_start) - 1
  seg_nr <- rep(1:n_segments, times = diff(segment_start))
  dt[, segment_nr := seg_nr]

  dt[mode == "operation", dpower := c(0, diff(Unit_4_Power)), by = segment_nr]
  dt[
    mode == "operation",
    burnin_stop := .SD[, which(dpower < 0)[1]],
    by = segment_nr
  ]
  dt[
    mode == "operation",
    shutdown_start := .SD[, tail(which(dpower >= 0), 1)],
    by = segment_nr
  ]
  dt[mode == "operation", "within_segment_index" := 1:.N, by = segment_nr]
  dt[within_segment_index < burnin_stop, mode := "burnin"]
  dt[within_segment_index > shutdown_start, mode := "shutdown"]
  dt
}
