#' @include utils.R
#' @include taxonomy.R
NULL


#' @title Taxon accessors
#' 
#' @description
#' Access the various slots of \linkS4class{taxon} or \linkS4class{taxonList}
#' instances.
#' 
#' @param x A \linkS4class{taxon} or \linkS4class{taxonList}.
#' @param ... Further arguments passed on to methods. 
#' @rdname taxon-accessors
#' @export
#' @genericMethods
setGeneric("taxId", function(x, ...) standardGeneric("taxId"))

#' @rdname taxon-accessors
#' @export
#' @genericMethods
setGeneric("sciName", function(x, ...) standardGeneric("sciName"))

#' @rdname taxon-accessors
#' @export
#' @genericMethods
setGeneric("taxRank", function(x, ...) standardGeneric("taxRank"))

#' @rdname taxon-accessors
#' @export
#' @genericMethods
setGeneric("synonym", function (x, ...) standardGeneric("synonym"))

#' @rdname taxon-accessors
#' @export
#' @genericMethods
setGeneric("authority", function (x, ...) standardGeneric("authority"))

#' @rdname taxon-accessors
#' @export
#' @genericMethods
setGeneric("lineage", function(x, ...) standardGeneric("lineage"))

#' @rdname taxon-accessors
#' @export
#' @genericMethods
setGeneric("parent", function (x, ...) standardGeneric("parent"))
