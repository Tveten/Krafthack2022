library(Krafthack2022)
library(capacc)

# raw_data <- read_data()
# data <- preprocess(raw_data)  # Probably need some preprocessing of raw data.
# As a simple example:
n <- 1000
p_response <- 5
p_covariates <- 5
data <- read_synthetic_data(n, p_response, p_covariates)  # all iid normal.

# Train a model to represent baseline behaviour.
# Here: One linear model per response. See R/baseline_model.R for linear_model()
responses <- paste0("response", 1:p_response)  # Several types of sensor data.
covariates <- paste0("covariate", 1:p_covariates)  # Context variables.
models <- lapply(responses, linear_model, x = data, covariates = covariates)

# Predict and get residuals.
# Could also be for new testing data.
predictions <- do.call("cbind", lapply(models, predict, newdata = data))
response_data <- as.matrix(data[, ..responses])
residuals <- response_data - predictions



# chgpt detection on residuals using inspect:
p = dim(residuals)[2]
xi = 4*sqrt(log(p*n))
#lambda = sqrt(log(p*log(n)))
lambda = xi
res = Inspect(t(residuals), lambda = lambda, xi=xi, alpha = 1.2, K = 10,eps=1e-10,
                maxiter=10000,debug=FALSE)

inspect_anomalies =anomalies_from_inspect(res, residuals)

plot_capacc_external(inspect_anomalies)

