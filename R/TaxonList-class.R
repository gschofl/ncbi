#' @include Taxon-class.R
#' @importFrom rmisc as.list
NULL


# Class-definitions ------------------------------------------------------


## Validators
#' @importFrom rmisc collectionValidator
.valid_TaxonList <- collectionValidator('TaxonList')
.valid_LineageList <- collectionValidator('LineageList')

#' TaxonList and LineageList
#' 
#' \dQuote{\bold{TaxonList}} and \dQuote{\bold{LineageList}}
#' are lists of \linkS4class{Taxon} and \linkS4class{Lineage}
#' objects, respectively.
#' 
#' @importClassesFrom rmisc Collection
#' @rdname TaxonList
#' @export
#' @classHierarchy
#' @classMethods
setClass("TaxonList", contains="Collection",
         prototype=prototype(elementType="Taxon"),
         validity=.valid_TaxonList)

#' @rdname TaxonList
#' @export
#' @classHierarchy
#' @classMethods
setClass("LineageList", contains="Collection",
         prototype=prototype(elementType="Taxon"),
         validity=.valid_LineageList)

## Constructors
#' @importFrom rmisc collectionConstructor
TaxonList <- collectionConstructor('TaxonList')
LineageList <- collectionConstructor('LineageList')

## show methods
#' @importFrom rmisc collectionShower
.show_TaxonList <- collectionShower(.show_Taxon, numOfElements=12, linesPerElement=NULL)
setMethod("show", "TaxonList", function(object) {
  .show_TaxonList(object)
})

.show_LineageList <- collectionShower(.show_Lineage, numOfElements=6, linesPerElement=2)
setMethod("show", "LineageList", function(object) {
  .show_LineageList(object) 
})


# Accessors --------------------------------------------------------------


setMethod("getByRank", "LineageList", function(x, rank, value=NULL) {
  rank <- match.arg(rank, ncbi:::.ranks)
  i <- vapply(x, function(x) which(getRank(x) == rank) %||% NA_integer_, integer(1))
  
  if (!is.null(value)) {
    value <- match.arg(value, c("TaxId", "ScientificName"))
    unlist(Map(`[`, x=x, i=i, value=value))
  }
  else {
    taxids <- unlist(Map(`[`, x=x, i=i, value='TaxId'))
    new_taxon(taxids, shared(x))
  }
})


setMethod("getTaxID", "TaxonList", function(x, use.names=FALSE) {
  if (use.names) {
    setNames(vapply(x, getTaxID, ""), getScientificName(x))
  } else {
    vapply(x, getTaxID, character(1))
  }
})


setMethod("getScientificName", "TaxonList", function(x) {
  vapply(x, getScientificName, character(1))
})


setMethod("getRank", "TaxonList", function(x) {
  vapply(x, getRank, character(1))
})


setMethod("getParentTaxID", "TaxonList", function(x) {
  if (!is(x[[1]], 'Taxon_full')) {
    x <- new_taxon(taxid, shared(x), full=TRUE)
  }
  vapply(x, getParentTaxID, character(1))
})


setMethod("getOtherName", "TaxonList", function(x) {
  if (!is(x[[1]], 'Taxon_full')) {
    x <- new_taxon(taxid, shared(x), full=TRUE)
  }
  lapply(x, getOtherName)
})


setMethod("getAuthority", "TaxonList", function(x) {
  if (!is(x[[1]], 'Taxon_full')) {
    x <- new_taxon(taxid, shared(x), full=TRUE)
  }
  lapply(x, getAuthority)
})


setMethod("getLineage", "TaxonList", function(x) {
  if (!is(x[[1]], 'Taxon_full')) {
    x <- new_taxon(taxid, shared(x), full=TRUE)
  }
  LineageList(lapply(x, getLineage), shared=shared(x))
})


setMethod("getByRank", "TaxonList", function(x, rank, value=NULL) {
  if (!is(x[[1]], 'Taxon_full')) {
    x <- new_taxon(taxid, shared(x), full=TRUE)
  }
  getByRank(getLineage(x), rank=rank, value=value)
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


setMethod("is.na", "TaxonList", function(x) {
  vapply(x, is.na, logical(1))
})



