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

.ray_version <- NULL

ray_version <- function() {
  if (is.null(.ray_version)) {
    list_packages <- reticulate::py_list_packages()
    ray_version <- list_packages[list_packages$package == "ray", "version"]
    if (length(ray_version) == 0L) stop("Ray is not found.")
    package_namespace <- asNamespace(.packageName)
    unlockBinding(".ray_version", package_namespace)
    assign(".ray_version", ray_version, envir = package_namespace)
    lockBinding(".ray_version", package_namespace)
  }
  .ray_version
}
