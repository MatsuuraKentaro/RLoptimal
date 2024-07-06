.onAttach <- function(libname, pkgname) {
  if (interactive()) {
    loadNamespace("reticulate")
    packageStartupMessage(setup_python())
  }
}
