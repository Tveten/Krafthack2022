#' Evaluate mean percentage error
#'
#' @return Mean percentage error
#'
#' @export
#'
mpe = function(pred, true){
  return(100* mean((true - pred)/true))
}
