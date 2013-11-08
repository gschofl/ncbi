
#' @title PubMed accessors
#' 
#' @description Access the various slots of
#' \code{\linkS4class{pubmed}} objects.
#' 
#' @param x A \code{\linkS4class{pubmed}} object.
#' @param ... Further arguments passed on to methods. 
#' @rdname Pubmed-accessors
#' @export
#' @genericMethods
setGeneric("getAuthor", function(x, ...) standardGeneric("getAuthor"))

#' @rdname Pubmed-accessors
#' @export
#' @genericMethods
setGeneric("getTitle", function(x, ...) standardGeneric("getTitle"))

#' @rdname Pubmed-accessors
#' @export
#' @genericMethods
setGeneric("getAbstract", function(x, ...) standardGeneric("getAbstract"))

#' @rdname Pubmed-accessors
#' @export
#' @genericMethods
setGeneric("getJournal", function(x, ...) standardGeneric("getJournal"))

#' @rdname Pubmed-accessors
#' @export
#' @genericMethods
setGeneric("getIssue", function(x, ...) standardGeneric("getIssue"))

#' @rdname Pubmed-accessors
#' @export
#' @genericMethods
setGeneric("getPmid", function(x, ...) standardGeneric("getPmid"))

#' @rdname Pubmed-accessors
#' @export
#' @genericMethods
setGeneric("getDoi", function(x, ...) standardGeneric("getDoi"))

#' @rdname Pubmed-accessors
#' @export
#' @genericMethods
setGeneric("getCites", function(x, ...) standardGeneric("getCites"))

#' @rdname Pubmed-accessors
#' @export
#' @genericMethods
setGeneric("browsePubmed", function(x, ...) standardGeneric("browsePubmed"))

