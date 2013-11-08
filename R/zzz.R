#' @section Package options:
#'
#' \emph{ncbi} uses the following \code{\link{options}} to configure behaviour:
#'
#' \itemize{
#'   \item \code{ncbi.taxonomy.path}: Path to a local installation of the NCBI
#'   taxonomy database and (optionally) a GI_to_TaxId database. Defaults to
#'   "$HOME/local/db/taxonomy/". You can override the default by setting this
#'   option in your .Rprofile file. Run\code{\link{createTaxonDB}} and
#'   \code{\link{createGeneidDB}} to install these databases.
#' }
#' 
#' @docType package
#' @name ncbi
NULL

.onLoad <- function(libname, pkgname) {
  ## set global options
  options(verbose = FALSE)
  options(reutils.verbose.queries = FALSE)
  op <- options()
  op.ncbi <- list(
    ncbi.taxonomy.path = normalizePath("~/local/db/taxonomy", mustWork=FALSE)
  )
  toset <- !(names(op.ncbi) %in% names(op))
  if (any(toset)) {
    options(op.ncbi[toset])
  }
  invisible()
}

.onUnload <- function(libpath) {
  options(verbose = FALSE)
  library.dynam.unload("ncbi", libpath)
}
