#' @include utils.R
#' @include taxonomy.R
#' @include taxonomy-generics.R
NULL


# taxId methods ----------------------------------------------------------


#' @autoImports
setMethod("taxId", "taxon", function (x, use.names = TRUE) {
  if (use.names)
    setNames(x@taxId, nm=sciName(x))
  else
    x@taxId
})

#' @autoImports
setMethod("taxId", "taxonList", function (x, use.names = TRUE) {
  if (use.names)
    setNames(vapply(x, taxId, FUN.VALUE=character(1)),
             nm=sciName(x))
  else 
    vapply(x, taxId, FUN.VALUE=character(1))
})


# sciName methods --------------------------------------------------------


setMethod("sciName", "taxon", function (x) x@scientificName)

setMethod("sciName", "taxonList", function (x) {
  vapply(x, sciName, FUN.VALUE=character(1))
})


# taxRank methods --------------------------------------------------------


setMethod("taxRank", "taxon", function(x) x@rank)

setMethod("taxRank", "taxonList", function(x) {
  vapply(x, taxRank, FUN.VALUE=character(1))
})


# other accessors --------------------------------------------------------


setMethod("synonym", "taxon", function(x) x@otherName)

setMethod("authority", "taxon", function(x) x@authority)

setMethod("lineage", "taxon", function(x) x@lineage)

setMethod("parent", "taxon", function(x) {
  taxon(x@parentTaxId)
})


# show methods -----------------------------------------------------------


#' @autoImports
setMethod("show", "taxon",
          function (object) {
            lin <- paste(sciName(lineage(object)), collapse="; ")
            showme <- sprintf("%s\nTaxonomy Id: %s; Rank: %s\n",
                              sciName(object), sQuote(taxId(object, FALSE)),
                              sQuote(taxRank(object))) 
            cat(showme)
            
            if (not_empty(lin))
              cat(sprintf("Lineage: %s\n", lin))
          })


setMethod("show", "taxonList",
          function (object) {
            lo <- length(object)
            cat(sprintf("A %s instance of length %s", sQuote(class(object)), lo))
            showme <- sprintf("\n%s (%s; %s)", sciName(object), taxId(object, FALSE),
                              taxRank(object))
            cat(showme)
          })


setMethod("show", "Lineage",
          function (object) {
            lo <- length(object)
            lin <- paste(sciName(object), collapse="; ")
            cat(sprintf("A %s of length %s\n%s", sQuote(class(object)), lo, lin))   
            return(invisible(lin))
          })

