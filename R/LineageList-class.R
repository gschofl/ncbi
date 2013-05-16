#' @include Taxon_minimal-class.R
NULL

setOldClass("list")

#' @rdname TaxonList
#' @export
#' @classHierarchy
#' @classMethods
setClass("LineageList", contains="list")
setValidity("LineageList", function (object) {
  if (!all(vapply(object@.Data, is, "Lineage", FUN.VALUE=logical(1))))
    return("All elements in a 'LineageList' must be 'Lineage' objects")
  
  TRUE
})


# Accessors --------------------------------------------------------------


setMethod("getByRank", "LineageList", function(x, rank, value = NULL) {
  listData <- lapply(x, getByRank, rank = rank, value = value)
  if (is.null(value))
    TaxonList(listData)
  else
    unlist(listData)
})


# Subsetting -------------------------------------------------------------


setMethod("[", "LineageList",
          function(x, i, j, ..., drop) {
            LineageList( callNextMethod() )
          })


# Constructor ------------------------------------------------------------


LineageList <- function (...) {
  listData <- list(...)
  if (length(listData) == 0L) {
    new('LineageList', .Data = list(new("Lineage")))
  } else {
    if (length(listData) == 1L && is.list(listData[[1L]])) 
      listData <- listData[[1L]]
    if (!all(vapply(listData, is, "Lineage", FUN.VALUE=logical(1)))) 
      stop("All elements in '...' must be Lineage objects")
    new('LineageList', .Data = listData)
  }
}


setMethod("show", "LineageList",
          function (object) {
            n <- 4
            olen <- length(object)
            cat(sprintf("A %s instance of length %s\n",
                        sQuote(class(object)), olen), sep="")
            if (olen > 2*n) {
              hd_idx <- seq_len(n)
              hd <- object[hd_idx]
              show_hd <- sprintf("[[%s]] %s", hd_idx,
                                 vapply(hd, .show.Lineage,
                                        width = 2*getOption("width") - 15,
                                        FUN.VALUE = character(1)))
              tl_idx <- seq.int(to = olen, length.out = min(n, olen))
              tl <- object[tl_idx]
              show_tl <- sprintf("[[%s]] %s", tl_idx,
                                 vapply(tl, .show.Lineage,
                                        width = 2*getOption("width") - 15,
                                        FUN.VALUE = character(1)))
              showme <- c(show_hd, "...", show_tl)
            } else {
              showme <- sprintf("[[%s]] %s", seq_len(olen),
                                 vapply(object, .show.Lineage,
                                        width = 2*getOption("width") - 15,
                                        FUN.VALUE = character(1)))
            }
            
            cat(showme, sep="\n")
          })

