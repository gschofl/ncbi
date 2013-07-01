#' @include Lineage-class.R
NULL

setOldClass("list")

## Validator
.valid_LineageList <- listclassValidator('LineageList', 'Lineage')

#' @rdname TaxonList
#' @export
#' @classHierarchy
#' @classMethods
setClass("LineageList", contains="list", validity=.valid_LineageList)

## Constructor
LineageList <- listclassConstructor('LineageList', 'Lineage')

## show method
.show_LineageList <- listclassShower(.show_Lineage, numOfElements=6, linesPerElement=2)
setMethod("show", "LineageList", function (object) {
  .show_LineageList(object) 
})

setMethod(".DB", "LineageList", function (x) x[[1]]@db )

# Accessors --------------------------------------------------------------


setMethod("getByRank", "LineageList", function(x, rank, value = NULL) {
  rank <- match.arg(rank, .ranks)
  i <- vapply(x, function (x) which(getRank(x) == rank) %||% NA_integer_, integer(1))
  
  if (!is.null(value)) {
    value <- match.arg(value, c("TaxId", "ScientificName"))
    unlist(Map(`[`, x=x, i=i, value=value))
  }
  else {
    taxids <- unlist(Map(`[`, x=x, i=i, value='TaxId'))
    new_taxon_db(taxids, .DB(x))
  }
})


# Subsetting -------------------------------------------------------------


setMethod("[", "LineageList",
          function(x, i, j, ..., drop) {
            LineageList( callNextMethod() )
          })

