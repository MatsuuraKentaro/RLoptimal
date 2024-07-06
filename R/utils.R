#' @importFrom R6 R6Class
NULL

#' @importFrom utils globalVariables
NULL

globalVariables("MCPModEnv")

softmax <- function(x) {
  exp_x <- exp(x - max(x))
  return(exp_x / sum(exp_x))
}

ray_version <- function() {
  list_packages <- reticulate::py_list_packages()
  ray_version <- list_packages[list_packages$package == "ray", "version"]
  if (length(ray_version) == 0L) stop("Ray is not found.")
  ray_version
}
