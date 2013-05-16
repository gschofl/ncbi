#' @include utils.R
NULL

.ranks  <- c("root","superkingdom","kingdom","subkingdom","superphylum",
             "phylum","subphylum","superclass","class","subclass","infraclass",
             "superorder","order","suborder","parvorder","infraorder",
             "superfamily","family","subfamily","tribe","subtribe","genus",
             "subgenus","species group","species subgroup","species","subspecies",
             "varietas","forma","no rank")


# Class definition - Taxon -----------------------------------------------


#' Retrieve records from the NCBI Taxonomy database.
#'
#' \dQuote{Taxon} is a virtual class (with no slots) that provides a
#' container for records retrieved from the NCBI Taxonomy database.
#' It is extended by the class \dQuote{Taxon_minimal} with three slots
#' \emph{TaxId}, \emph{ScientificName}, and \emph{Rank}, and by class
#' \dQuote{Taxon_full}, with the additional slots \emph{ParentTaxId},
#' \emph{OtherName}, \emph{Authority}, \emph{TypeMaterial}, and
#' \emph{Lineage}.
#'  
#' @rdname Taxon
#' @export
#' @classHierarchy
#' @classMethods 
setClass("Taxon", representation("VIRTUAL") )


#' @rdname Taxon
#' @export
#' @classHierarchy
#' @classMethods
setClass("Taxon_minimal",
         contains="Taxon",
         representation(TaxId = "character",
                        ScientificName = "character",
                        Rank = "character"),
         prototype(TaxId = NA_character_,
                   ScientificName = NA_character_,
                   Rank = NA_character_))

.valid.Taxon_minimal <- function (object) {
  if (!all(grepl("^\\d+$", getTaxId(object))) &&
      !all(is.na(getTaxId(object))))
    return("TaxIds should contain only digits or NA")
  if (!all(getRank(object) %in% .ranks) &&
      !all(is.na(getRank(object)))) 
    return(paste0("Invalid rank designation ",
                  sQuote(getRank(object)[!getRank(object)%in%.ranks])))
  
  TRUE
}

setValidity("Taxon_minimal", .valid.Taxon_minimal)


# Accessors --------------------------------------------------------------


#' @title Taxon accessors
#' 
#' @description
#' Access the slots of \linkS4class{Taxon} or \linkS4class{TaxonList}
#' instances.
#' 
#' @param x A \linkS4class{Taxon} or \linkS4class{TaxonList}.
#' @param ... Further arguments passed on to methods. 
#' @rdname Taxon-accessors
#' @export
#' @genericMethods
setGeneric("getTaxId", function(x, ...) standardGeneric("getTaxId"))
setMethod("getTaxId", "Taxon", function (x, use.names = TRUE) {
  if (use.names)
    setNames(x@TaxId, nm=getScientificName(x))
  else
    x@TaxId
})


#' @rdname Taxon-accessors
#' @export
#' @genericMethods
setGeneric("getScientificName", function(x, ...) standardGeneric("getScientificName"))
setMethod("getScientificName", "Taxon", function (x)  x@ScientificName )


#' @rdname Taxon-accessors
#' @export
#' @genericMethods
setGeneric("getRank", function(x, ...) standardGeneric("getRank"))
setMethod("getRank", "Taxon", function(x) x@Rank)


setMethod("is.na", "Taxon", function (x) {
  is.na(getTaxId(x)) && is.na(getScientificName(x)) && is.na(getRank(x))
})


# Class definition - Lineage ---------------------------------------------


#' \dQuote{Lineage} is a class (with no slots) that holds the taxonomic
#' lineage from a \emph{taxid} to the root node of the taxonomy.
#'  
#' @rdname Taxon
#' @export
#' @classHierarchy
#' @classMethods 
setClass("Lineage", contains="Taxon_minimal")

.valid.Lineage <- function (object) {
  n <- length(getTaxId(object))
  if ((length(getScientificName(object)) != n) || (length(getRank(object)) != n))
    return("slot lengths are not all equal")
  
  TRUE
}
  
setValidity("Lineage", .valid.Lineage)


# Contructor -------------------------------------------------------------


Lineage <- function (...) {
  listData <- list(...)
  if (all_empty(listData)) {
    new('Lineage')
  } else {
    if (length(listData) == 1L && ( is.list(listData[[1L]]) || is.matrix(listData[[1L]] ))) 
      listData <- listData[[1L]]
    if (all(vapply(listData, is, "Taxon", FUN.VALUE=logical(1)))) {
      TaxId <- vapply(listData, getTaxId, character(1))
      ScientificName <- vapply(listData, getScientificName, character(1))
      Rank <- vapply(listData, getRank, character(1))
      new('Lineage', TaxId = TaxId, ScientificName = ScientificName, Rank = Rank)
    } else if (all(colnames(listData) %in% c("tax_id", "tax_name", "rank"))) {
      new('Lineage',
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
              
              x@TaxId <- x@TaxId[i] %||% NA_character_
              x@ScientificName <- x@ScientificName[i] %||% NA_character_
              x@Rank <- x@Rank[i] %||% NA_character_
            }
            if (drop)
              data.frame(TaxId = x@TaxId, ScientificName = x@ScientificName,
                         Rank = x@Rank, stringsAsFactors=FALSE)
            else x
          })


#' @rdname Taxon-accessors
#' @export
#' @genericMethods
setGeneric("getByRank", function(x, rank, value = NULL, ...) standardGeneric("getByRank"))
setMethod("getByRank", "Lineage", function (x, rank, value = NULL, drop = FALSE) {
  rank <- match.arg(rank, .ranks)
  FILTER <- function (x) x
  if (!is.null(value)) {
    value <- match.arg(value, c("TaxId", "ScientificName"))
    FILTER <- match.fun(paste0("get", value))
  }
  i <- which(getRank(x) == rank)
  FILTER(x[i, drop = drop])
})


# show -------------------------------------------------------------------


.show.Lineage <- function (x, width = getOption("width"), ellipsis = "...") {
  ellipsize(paste(getScientificName(x), collapse="; "), width = width,
            ellipsis = ellipsis)
}


setMethod("show", "Lineage",
          function (object) {
            lo <- length(getTaxId(object))
            showme <- sprintf("A %s of length %s\n%s",
                              sQuote(class(object)), lo,
                              .show.Lineage(object))
            cat(showme, sep="")
          })

