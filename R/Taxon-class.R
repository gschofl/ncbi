#' @include utils.R
NULL

## valid rank designations on NCBI
.ranks  <- c("root","superkingdom","kingdom","subkingdom","superphylum",
             "phylum","subphylum","superclass","class","subclass","infraclass",
             "superorder","order","suborder","parvorder","infraorder",
             "superfamily","family","subfamily","tribe","subtribe","genus",
             "subgenus","species group","species subgroup","species","subspecies",
             "varietas","forma","no rank")


# Class definition - Taxon -----------------------------------------------


#' Retrieve records from the NCBI Taxonomy database.
#'
#' \dQuote{Taxon} is an S4 class that provides a container for records
#' retrieved from the NCBI Taxonomy database.
#' It is extended by the classes \dQuote{Taxon_minimal} and \dQuote{Lineage}
#' with three slots \emph{TaxId}, \emph{ScientificName}, and \emph{Rank},
#' and by class \dQuote{Taxon_full}, with the additional slots
#' \emph{ParentTaxId}, \emph{OtherName}, \emph{Authority},
#' \emph{TypeMaterial}, and \emph{Lineage}.
#'  
#' @rdname Taxon
#' @export
#' @classHierarchy
#' @classMethods 
setClass("Taxon", representation(db = "environment"),
         prototype(db = new.env(parent=emptyenv())))


## extract the database environment from objects
setGeneric(".DB", function(x) standardGeneric(".DB"))
setMethod(".DB", "Taxon", function (x) x@db)


.valid_Taxon_minimal <- function (object) {
  errors <- character()
  if (!all(grepl("^\\d+$", getTaxId(object))) &&
      !all(is.na(getTaxId(object)))) {
    msg <- "TaxIds must contain only digits or NA"
    errors <- c(errors, msg)
  }
    
  if (!all(getRank(object) %in% .ranks) &&
      !all(is.na(getRank(object)))) {
    msg <- paste0("Invalid rank designation ",
                  sQuote(getRank(object)[!getRank(object)%in%.ranks]))
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
setClass("Taxon_minimal", contains="Taxon",
         representation(TaxId = "character",
                        ScientificName = "character",
                        Rank = "character"),
         prototype(TaxId = NA_character_,
                   ScientificName = NA_character_,
                   Rank = NA_character_),
         validity = .valid_Taxon_minimal)


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
