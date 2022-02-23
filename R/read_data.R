# The function to read the actual Krafthack data.
read_data <- function() {

}

read_synthetic_data <- function(n = 1000, p_response = 5, p_covariates = 5) {
  p <- p_response + p_covariates
  x <- matrix(rnorm(n * p), n, p)
  x[200:250, 1:2] <- x[200:250, 1:2] + 3  # Add anomaly.
  x <- as.data.table(x)
  setnames(x, c(
      paste0("response", 1:p_response),
      paste0("covariate", 1:p_covariates)
    )
  )
  x
}
