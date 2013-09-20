##
.onLoad <- function(libname, pkgname) {
  options(verbose = FALSE)
  options(reutils.verbose.queries = FALSE)
}

.onUnload <- function(libpath) {
  options(verbose = FALSE)
  library.dynam.unload("ncbi", libpath)
}
