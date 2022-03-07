#' Read the Krafthack data
#'
#' @return A data.table with the data.
#'
#' @export
#'
read_krafthack_data <- function(directory){
  input2 = as.data.table(arrow::read_parquet(gsub(" ", "", paste(directory, "/", "input_dataset-2.parquet"))))
  input1 = as.data.table(arrow::read_parquet(gsub(" ", "", paste(directory, "/", "input_dataset-1.parquet"))))
  test = as.data.table(arrow::read_parquet(gsub(" ", "", paste(directory, "/", "prediction_input.parquet"))))
  names = (names(input2)[((names(input2) %in%  names(test) )| (names(input2) %in% c("Bolt_1_Tensile", "Bolt_2_Tensile","Bolt_3_Tensile","Bolt_4_Tensile","Bolt_5_Tensile","Bolt_6_Tensile")))])
  input2 <- input2[, ..names] # removing vibration variables

  input2 = na.omit(input2)
  input1 = na.omit(input1)
  names(input2) <- gsub("\\s+", "_", names(input2))
  names(input1) <- gsub("\\s+", "_", names(input1))
  names(test) <- gsub("\\s+", "_", names(test))
  input2$mode = as.factor(input2$mode)
  input1$mode = as.factor(input1$mode)
  test$mode = as.factor(test$mode)
  return(list(input1, input2, test))
}



#' Read some synthetic data.
#'
#' @return A data.table with the synthetic data.
#'
#' @export
#'
read_synthetic_data <- function(n = 1000, p_response = 5, p_covariates = 5) {
  set.seed(4545)
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
