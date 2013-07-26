##
.onLoad <- function(libname, pkgname) {
  options(verbose = FALSE)
}

.onUnload <- function(libpath) {
  options(verbose = FALSE)
  library.dynam.unload("ncbi", libpath)
}
