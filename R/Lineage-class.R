#' @include Taxon-class.R
NULL


# Class definition - Lineage ---------------------------------------------


.valid_Lineage <- function (object) {
  errors <- character()
  n <- length(getTaxId(object, use.names = FALSE))
  lengths <- c(length(getScientificName(object)), length(getRank(object)))
  
  if (any(lengths != n)) {
    msg <- paste0("Inconsistent lengths: TaxId = ", n, 
                  ", ScientificName = ", lengths[1], 
                  ", Rank = ", lengths[2])
    errors <- c(errors, msg)
  }
  
  if (length(errors) == 0) 
    TRUE
  else
    errors
}


#' @rdname Taxon
#' @export
#' @classHierarchy
#' @classMethods 
setClass("Lineage", contains="Taxon_minimal", validity = .valid_Lineage)


# Contructor -------------------------------------------------------------


Lineage <- function (db, ...) {
  listData <- list(...)
  if (all_empty(listData)) {
    new('Lineage', db = db)
  } else {
    if (length(listData) == 1L && ( is.list(listData[[1L]]) || is.matrix(listData[[1L]] ))) 
      listData <- listData[[1L]]
    if (all(vapply(listData, is, "Taxon", FUN.VALUE=logical(1)))) {
      new('Lineage', db = db,
          TaxId = vapply(listData, getTaxId, character(1)),
          ScientificName = vapply(listData, getScientificName, character(1)),
          Rank = vapply(listData, getRank, character(1)))
    } else if (all(colnames(listData) %in% c("tax_id", "tax_name", "rank"))) {
      new('Lineage', db = db,
          TaxId = listData[, "tax_id"] %||% NA_character_,
          ScientificName = listData[, "tax_name"] %||% NA_character_,
          Rank = listData[, "rank"] %||% NA_character_)
    } else {
      stop("All elements in '...' must be 'Taxon' objects or a named list ",
           "containing 'tax_id', 'tax_name', and 'rank'.")
    }
  }
}


# Subsetting -------------------------------------------------------------


setMethod("[", "Lineage",
          function(x, i, j, ... , drop=FALSE) {
            if (!missing(j))
              stop("invalid subsetting")
            if (!missing(i)) {
              if (is.character(i))
                stop("Cannot subset a Lineage object by character")
              
              value <- list(...)$value
              if (is.null(value)) {
                x@TaxId <- x@TaxId[i] %||% NA_character_
                x@ScientificName <- x@ScientificName[i] %||% NA_character_
                x@Rank <- x@Rank[i] %||% NA_character_
              } 
              else {
                x <- slot(x, value)[i] %||% NA_character_
              }
            }
            if (drop && !is.null(value))
              data.frame(TaxId = x@TaxId, ScientificName = x@ScientificName,
                         Rank = x@Rank, stringsAsFactors=FALSE)
            else x
          })

#' Extract taxa from a lineage by their rank
#'
#' Valid rank designations are:
#' superkingdom, kingdom, subkingdom, superphylum,
#' phylum, subphylum, superclass, class, subclass, infraclass, 
#' superorder, order, suborder, parvorder, infraorder, 
#' superfamily, family, subfamily, tribe, subtribe, genus, 
#' subgenus, species group, species subgroup, species, subspecies, 
#' varietas, and forma.
#'
#' @param x A \code{\linkS4class{Taxon}, \code{\linkS4class{TaxonList}},
#' \code{\linkS4class{Lineage}}, or \code{\linkS4class{LineageList}} object.
#' @param rank One of the valid ranks for NCBI taxonomies (see Details).
#' @param value One of \code{NULL}, \sQuote{TaxId}, or \sQuote{ScientificName}.
#' If \code{NULL}, \code{Taxon} objects are returned, otherwise a character
#' vector of TaxIds or scientific names, respectively.
#'
#' @rdname getByRank
#' @export
#' @genericMethods
setGeneric("getByRank", function(x, rank, value = NULL, ...) standardGeneric("getByRank"))
setMethod("getByRank", "Lineage", function (x, rank, value = NULL, drop = FALSE) {
  rank <- match.arg(rank, .ranks)
  i <- which(getRank(x) == rank)
  
  if (!is.null(value)) {
    value <- match.arg(value, c("TaxId", "ScientificName"))
    x[i, value = value, drop = drop]
  }
  else {
    new_taxon_db(taxid=x[i, value = "TaxId"], .DB(x))
  }
})


# show -------------------------------------------------------------------


.show_Lineage <- function (x, width = getOption("width"), ellipsis = " ... ") {
  ellipsize(paste(getScientificName(x), collapse="; "), width = width,
            ellipsis = ellipsis)
}
setMethod("show", "Lineage",
          function (object) {
            lo <- length(getTaxId(object))
            showme <- sprintf("A %s of length %s\n%s",
                              sQuote(class(object)), lo,
                              .show_Lineage(object))
            cat(showme, sep="")
          })
