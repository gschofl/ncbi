#' @include Taxon_full-class.R
NULL

setOldClass("list")

#' TaxonList and LineageList
#' 
#' \dQuote{TaxonList} and \dQuote{LineageList} are lists of \linkS4class{Taxon}
#' and \linkS4class{Lineage} objects, respectively.
#' 
#' @rdname TaxonList
#' @export
#' @classHierarchy
#' @classMethods
setClass("TaxonList", contains="list")
setValidity("TaxonList", function (object) {
  if (!all(vapply(object@.Data, is, "Taxon", FUN.VALUE=logical(1))))
    return("All elements in a TaxonList must be Taxon objects")
  
  TRUE
})


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
  LineageData <- getLineage(x)
  getByRank(LineageData, rank = rank, value = value)
})


setMethod("show", "TaxonList",
          function (object) {
            n <- 12
            olen <- length(object)
            cat(sprintf("A %s instance of length %s\n",
                        sQuote(class(object)), olen), sep="")
            if (olen > 2*n) {
              hd_idx <- seq_len(n)
              hd <- object[hd_idx]
              show_hd <- sprintf("[[%s]] %s", hd_idx,
                                 vapply(hd, .show.Taxon,
                                        width = getOption("width") - 8,
                                        FUN.VALUE = character(1)))
              tl_idx <- seq.int(to = olen, length.out = min(n, olen))
              tl <- object[tl_idx]
              show_tl <- sprintf("[[%s]] %s", tl_idx,
                                 vapply(tl, .show.Taxon,
                                        width = getOption("width") - 8,
                                        FUN.VALUE = character(1)))
              showme <- c(show_hd, "...", show_tl)
            } else {
              showme <- sprintf("[[%s]] %s", vapply(object, .show.Taxon,
                                                    width = getOption("width") - 8,
                                                    FUN.VALUE = character(1)))
            }
            
            cat(showme, sep="\n")
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


TaxonList <- function (...) {
  listData <- list(...)
  if (length(listData) == 0L) {
    new('TaxonList', .Data = list(new("Taxon_minimal")))
  } else {
    if (length(listData) == 1L && is.list(listData[[1L]])) 
      listData <- listData[[1L]]
    if (!all(vapply(listData, is, "Taxon", FUN.VALUE=logical(1)))) 
      stop("All elements in '...' must be Taxon objects")
    new('TaxonList', .Data = listData)
  }
}
