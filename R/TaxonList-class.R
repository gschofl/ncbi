#' @include Taxon_full-class.R
NULL

setOldClass("list")

## Validator
.valid_TaxonList <- listclassValidator('TaxonList', 'Taxon')

#' TaxonList and LineageList
#' 
#' \dQuote{TaxonList} and \dQuote{LineageList} are lists of \linkS4class{Taxon}
#' and \linkS4class{Lineage} objects, respectively.
#' 
#' @rdname TaxonList
#' @export
#' @classHierarchy
#' @classMethods
setClass("TaxonList", contains="list", validity=.valid_TaxonList)

## Constructor
TaxonList <- listclassConstructor('TaxonList', 'Taxon')

## show method
.show_TaxonList <- listclassShower(.show_Taxon, numOfElements=12, linesPerElement=NULL)
setMethod("show", "TaxonList", function (object) {
  .show_TaxonList(object)
})

setMethod(".DB", "TaxonList", function (x) x[[1]]@db )

# Accessor methods ----------------------------------------------------------


#' @autoImports
setMethod("getTaxId", "TaxonList", function (x, use.names = FALSE) {
  if (use.names)
    setNames(vapply(x, getTaxId, FUN.VALUE=character(1)),
             nm=getScientificName(x))
  else 
    vapply(x, getTaxId, FUN.VALUE=character(1))
})


#' @autoImports
setMethod("getScientificName", "TaxonList", function (x) {
  vapply(x, getScientificName, FUN.VALUE=character(1))
})


setMethod("getRank", "TaxonList", function(x) {
  vapply(x, getRank, FUN.VALUE=character(1))
})


setMethod("getLineage", "TaxonList", function(x) {
  LineageList(lapply(x, getLineage))
})


setMethod("getByRank", "TaxonList", function(x, rank, value = NULL) { 
  getByRank(getLineage(x), rank=rank, value=value)
})


setMethod("[", "TaxonList",
          function(x, i, j, ..., drop) {
              TaxonList( callNextMethod() )
          })


#' @importFrom BiocGenerics table
#' @export
setGeneric("table")
setMethod("table", "TaxonList", 
          function(...) {
            sciNames <- getScientificName(...)
            tbl <- table(sciNames)
            names(attr(tbl, "dimnames")) <- "Scientific Names"
            tbl
          })



