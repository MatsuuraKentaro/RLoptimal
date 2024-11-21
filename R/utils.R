#' @importFrom R6 R6Class
#' @importFrom utils globalVariables
#' @importFrom stats var

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

sample_std_dev <- function(x) {
  sqrt(sample_variance(x))
}

# The state s is described in Section 2.3 of the original paper
compute_state <- function(actions, resps, N_total) {
  stopifnot(length(actions) == length(resps))
  
  resps_per_action <- split(resps, actions)
  count_per_action <- vapply(resps_per_action, length, integer(1L), USE.NAMES = FALSE)
  stopifnot("the number of allocated subjects at each dose should be >= 2" = count_per_action >= 2L)
  
  mean_resps <- vapply(resps_per_action, mean, double(1L), USE.NAMES = FALSE)
  shifted_mean_resps <- mean_resps[-1L] - mean_resps[1L]
  sd_resps <- vapply(resps_per_action, sample_std_dev, double(1L), USE.NAMES = FALSE)
  proportion_per_action <- count_per_action / N_total
  
  state <- as.array(c(shifted_mean_resps, sd_resps, proportion_per_action))
  state
}

is_apple_silicon <- function() {
  sys_info <- Sys.info()
  sys_info["sysname"] == "Darwin" && sys_info["machine"] == "arm64"
}
