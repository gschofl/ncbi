
#' @title PubMed accessors
#' 
#' @description
#' Access the various slots of \code{\link[utils]{bibentry}} instances.
#' 
#' @param x A \code{\link[utils]{bibentry}} instance.
#' @param ... Further arguments passed on to methods. 
#' @rdname pubmed-accessors
#' @export
#' @genericMethods
setGeneric("author", function(x, ...) standardGeneric("author"))

#' @rdname pubmed-accessors
#' @export
#' @genericMethods
setGeneric("title", function(x, ...) standardGeneric("title"))

#' @rdname pubmed-accessors
#' @export
#' @genericMethods
setGeneric("abstract", function(x, ...) standardGeneric("abstract"))

#' @rdname pubmed-accessors
#' @export
#' @genericMethods
setGeneric("journal", function(x, ...) standardGeneric("journal"))

#' @rdname pubmed-accessors
#' @export
#' @genericMethods
setGeneric("issue", function(x, ...) standardGeneric("issue"))

#' @rdname pubmed-accessors
#' @export
#' @genericMethods
setGeneric("pmid", function(x, ...) standardGeneric("pmid"))

#' @rdname pubmed-accessors
#' @export
#' @genericMethods
setGeneric("doi", function(x, ...) standardGeneric("doi"))

#' @rdname pubmed-accessors
#' @export
#' @genericMethods
setGeneric("cites", function(x, ...) standardGeneric("cites"))

#' @rdname pubmed-accessors
#' @export
#' @genericMethods
setGeneric("browsePubmed", function(x, ...) standardGeneric("browsePubmed"))

