linear_model <- function(x, response, covariates) {
  covariates_formula <- paste(covariates, collapse = "+")
  model_formula <- formula(paste0(response, " ~ ", covariates_formula))
  lm_obj <- lm(model_formula, data = x)
}

make_residuals <- function(data, response) {
  covariates <- " ~ power + speed + flow_rate"  # And other context variables.
  model_formula <- formula(paste0(response, " ~ ", covariates))
  lm_obj <- lm(model_formula, data = data)
  predictions <- predict(lm_obj, newdata = data)
  data[[response]] - predictions
}

residuals
