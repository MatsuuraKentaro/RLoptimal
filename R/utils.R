#' @importFrom R6 R6Class
NULL

#' @importFrom utils globalVariables
NULL

globalVariables("MCPModEnv")

softmax <- function(x) {
  exp_x <- exp(x - max(x))
  return(exp_x / sum(exp_x))
}

compute_MAE <- function(estimated_response, true_response) {
  shifted_estimates <- estimated_response - estimated_response[1L]
  shifted_true <- true_response - true_response[1L]
  errors <- shifted_estimates[-1L] - shifted_true[-1L]
  return(mean(abs(errors)))
}
