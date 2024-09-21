virtualenv_name <- "RLoptimal"

#' Setting up a Python Virtual Environment
#'
#' Setting up a Python virtual environment for the Ray package, which includes
#' the RLlib library for reinforcement learning.
#'
#' @export
setup_python <- function() {
  py_version <- "3.11"
  required_packages <- c(
    # "numpy>=2.1.1", 
    "dm-tree==0.1.8", "lz4==4.3.3", "pyarrow==17.0.0",
    "gymnasium==0.29.1", "torch==2.4.1", "scikit-image==0.24.0", 
    "pandas==2.2.3", "typer==0.12.5", "ray==2.36.0")

  if (is.null(reticulate::virtualenv_starter(py_version))) {
    reticulate::install_python(py_version)
  }
  if (reticulate::virtualenv_exists(virtualenv_name)) {
    installed_packages <- reticulate::py_list_packages(virtualenv_name)$requirement
    required_packages <- setdiff(required_packages, installed_packages)
  } else {
    reticulate::virtualenv_create(virtualenv_name, version = py_version)
  }
  if (length(required_packages) > 0) {
    py_install <- function(package) {
      reticulate::py_install(package, virtualenv_name)
    }
    Map(py_install, required_packages)
    message("The setup process for Python is complete.")
  }
  reticulate::use_virtualenv(virtualenv_name)
  invisible(NULL)
}

#' Clean the Python Virtual Environment
#' 
#' @export
clean_python_settings <- function() {
  reticulate::virtualenv_remove(virtualenv_name, confirm = TRUE)
}
