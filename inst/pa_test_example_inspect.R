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
res = myInspectR(t(residuals), threshold=2, adaptive_threshold=FALSE, alpha = 1.2, K = 10,eps=1e-10,
                lambda = 6, maxiter=10000,debug=FALSE)

# Anomaly detection in residuals.
# Here: CAPA-CC. But we could add several methods to try in R/anomaly_detection.R
precision_matrix_band <- 4
penalty_scaling <- 1.5
anomaly_results <- detect_anomalies_capacc(
  residuals,
  est_band = precision_matrix_band,
  b = penalty_scaling
)
plot(anomaly_results)
