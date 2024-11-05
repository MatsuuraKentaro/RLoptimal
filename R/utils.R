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

sample_variance <- function(x) {
  var(x) * (length(x) - 1L) / length(x)
}

sample_standard_deviation <- function(x) {
  sqrt(sample_variance(x))
}

compute_state <- function(actions, resps, N_total) {
  # Check argument
  count_per_action <- tapply(resps, actions, length)
  stopifnot("the number of allocated subjects at each dose should be >= 2" = count_per_action >= 2L)
  
  mean_resps <- tapply(resps, actions, mean)
  shifted_mean_resps <- mean_resps[-1L] - mean_resps[1L]
  sd_resps <- tapply(resps, actions, sample_standard_deviation)
  proportion_per_action <- count_per_action / N_total
  state <- as.array(unname(c(shifted_mean_resps, sd_resps, proportion_per_action)))
  state
}

is_apple_silicon <- function() {
  sys_info <- Sys.info()
  sys_info["sysname"] == "Darwin" && sys_info["machine"] == "arm64"
}
