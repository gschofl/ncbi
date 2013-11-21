
#' @title Access pubmed objects
#' 
#' @description Access the various slots of
#' \code{\linkS4class{pubmed}} objects.
#' 
#' @param x A \code{\linkS4class{pubmed}} object.
#' @param ... Further arguments passed on to methods. 
#' @rdname pubmed-methods
#' @export
#' @docType methods
setGeneric("getAuthor", function(x, ...) standardGeneric("getAuthor"))

#' @rdname pubmed-methods
#' @export
#' @docType methods
setGeneric("getTitle", function(x, ...) standardGeneric("getTitle"))

#' @rdname pubmed-methods
#' @export
#' @docType methods
setGeneric("getAbstract", function(x, ...) standardGeneric("getAbstract"))

#' @rdname pubmed-methods
#' @export
#' @docType methods
setGeneric("getJournal", function(x, ...) standardGeneric("getJournal"))

#' @rdname pubmed-methods
#' @export
#' @docType methods
setGeneric("getIssue", function(x, ...) standardGeneric("getIssue"))

#' @rdname pubmed-methods
#' @export
#' @docType methods
setGeneric("getPmid", function(x, ...) standardGeneric("getPmid"))

#' @rdname pubmed-methods
#' @export
#' @docType methods
setGeneric("getDoi", function(x, ...) standardGeneric("getDoi"))

#' @rdname pubmed-methods
#' @export
#' @docType methods
setGeneric("getCites", function(x, ...) standardGeneric("getCites"))

#' @rdname pubmed-methods
#' @export
#' @docType methods
setGeneric("browsePubmed", function(x, ...) standardGeneric("browsePubmed"))

