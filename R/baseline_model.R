#' Train a linear model
#'
#' @param x Data matrix.
#'
#' @return An lm object.
#'
#' @export
linear_model <- function(x, response, covariates) {
  covariates_formula <- paste(covariates, collapse = "+")
  model_formula <- formula(paste0(response, " ~ ", covariates_formula))
  lm(model_formula, data = x)
}

robust_linear_model <- function() {

}

