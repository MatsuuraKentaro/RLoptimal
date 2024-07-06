#' @export
setup_python <- function() {
  py_version <- "3.11"
  virtualenv_name <- "RLoptimal"
  required_packages <- c("numpy==1.26", "dm-tree", "lz4", "pyarrow", "gymnasium", "torch",
                         "scikit-image", "pandas", "typer", "ray")

  if (is.null(reticulate::virtualenv_starter(py_version))) {
    reticulate::install_python(py_version)
  }
  if (reticulate::virtualenv_exists(virtualenv_name)) {
    installed_packages <- reticulate::py_list_packages(virtualenv_name)$package
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
