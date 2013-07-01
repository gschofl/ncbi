#' @include LineageList-class.R
NULL

# Class definition -------------------------------------------------------

#' @slot TaxId The Taxonomy Identifier, a stable unique identifier for
#' each taxon in the NCBI Taxonomy database.
#' @slot ScientificName The unique scientific name of the taxon.
#' @slot Rank The taxonomic rank of the taxon.
#'  
#' @slot ParentTaxId The Taxonomy Identifier of the parental taxon.
#' @slot OtherName A named character vector holding synonyms
#' or GenBankSynonyms.
#' @slot Authority
#' @slot TypeMaterial
#' @slot Lineage
#' 
#' @rdname Taxon
#' @export
#' @classHierarchy
#' @classMethods
setClass("Taxon_full",
         contains = "Taxon_minimal",
         representation(ParentTaxId = "character",
                        OtherName = "character",
                        Authority = "character",
                        TypeMaterial = "character",
                        Lineage = "Lineage"),
         prototype(ParentTaxId = NA_character_,
                   OtherName = NA_character_,
                   Authority = NA_character_,
                   TypeMaterial = NA_character_,
                   Lineage = new("Lineage")))


# Accessors --------------------------------------------------------------


#' @rdname Taxon-accessors
#' @export
#' @genericMethods
setGeneric("getParentTaxId", function (x, ...) standardGeneric("getParentTaxId"))
setMethod("getParentTaxId", "Taxon_full", function(x) x@ParentTaxId)


#' @rdname Taxon-accessors
#' @export
#' @genericMethods
setGeneric("getOtherName", function (x, ...) standardGeneric("getOtherName"))
setMethod("getOtherName", "Taxon_full", function(x) x@OtherName)


#' @rdname Taxon-accessors
#' @export
#' @genericMethods
setGeneric("getAuthority", function (x, ...) standardGeneric("getAuthority"))
setMethod("getAuthority", "Taxon_full", function(x) x@Authority)


#' @rdname Taxon-accessors
#' @export
#' @genericMethods
setGeneric("getLineage", function(x, ...) standardGeneric("getLineage"))
setMethod("getLineage", "Taxon_full", function(x) x@Lineage)


#' @rdname Taxon-accessors
#' @export
#' @genericMethods
setMethod("getByRank", "Taxon_full", function (x, rank, value = NULL) {
  getByRank(getLineage(x), rank=rank, value=value)
})


# Constructors -----------------------------------------------------------


#' @details
#' See the documentation at 
#' \href{http://www.ncbi.nlm.nih.gov/books/NBK21100/}{NCBI}
#' for more information.
#' 
#' @param taxid \sQuote{taxids} or a valid NCBI search term.
#' @param rettype Which type of data should be retrieved? Full records
#' (default: \code{NULL}) or an \sQuote{uilist}. 
#' @param retmax Maximal number of records to be retrieved (default: 25).
#' @param parse Should the retrieved data be parsed?
#' @param ... Parameters passed on to the underlying \code{\link{efetch}}
#' query.
#'
#' @return An \linkS4class{XMLInternalDocument} or if parsed a
#'  \linkS4class{taxon} or \linkS4class{TaxonList} instance.
#' @rdname Taxon
#' @export
#' @autoImports
taxon <- function (taxid, rettype = NULL, retmax = 25, parse = TRUE, ...) {
  if (missing(taxid)) {
    return(new("Taxon_full"))
  }
  
  if (is(taxid, "esearch")) {
    if (database(taxid) != 'taxonomy')
      stop("Database ", sQuote(database(taxid)), " not supported")
    if (!has_webenv(taxid))
      taxid <- idList(taxid)
  }
  
  args <- getArgs(id=taxid, db="taxonomy", rettype, retmax, ...)
  response <- fetch_records(args, 500)
  if (parse) {
    switch(args$rettype %|null|% "xml",
           xml = parseTaxon(response),
           uilist = parseUilist(response),
           response)
  } else {
    response
  }
}


#' @keywords internal
new_taxon_db <- function(taxid, db, full = TRUE) {
  assert_that(!is.null(db$taxonDBcon))
  if (full) {
    tx <- lapply(taxid, dbGetTaxon, db=db)
  } else {
    tx <- lapply(taxid, dbGetTaxonMinimal, db=db)
  }
  if (length(tx) == 1) {
    return( tx[[1]] )
  } else {
    return( TaxonList(tx) )
  }
}


#' @param taxid A vector of valid NCBI Taxonomy Identifiers.
#' @param dbPath Path to a valid Taxon.db 
#' @param full if \code{FALSE} a minimal taxonomic description is extracted
#' (TaxId, ScientificName, Rank).
#'
#' @return A \linkS4class{Taxon} or \linkS4class{TaxonList} instance.
#' @rdname Taxon
#' @export
taxonDB <- function (taxid, dbPath=NULL, full=TRUE) {
  if (missing(taxid)) {
    return( new("Taxon_full") )
  }
  db <- new.env(parent=emptyenv())
  db$taxonDBcon <- taxonDBConnect(dbPath)
  new_taxon_db(taxid, db, full=full)
}


#' @keywords internal
new_taxon_by_geneid <- function(geneid, db, full=TRUE) {
  assert_that(!is.null(db$taxonDBcon))
  assert_that(!is.null(db$geneidDBcon))
  
  if (length(getTaxidByGeneID(db$geneidDBcon, 2)) == 0) {
    stop("'genes' table is empty. Run 'createTaxonDB()' setting 'with_geneid = TRUE'")
  }
  
  if (full) {
    tx <- lapply(geneid, dbGetTaxonByGeneID, db=db)
  } else {
    tx <- lapply(geneid, dbGetTaxonMinimalByGeneID, db=db)
  }
  
  if (length(tx) == 1) {
    return( tx[[1]] )
  } else {
    return( TaxonList(tx) )
  }
}


#' @param geneid A vector of valid NCBI Gene Identifiers.
#' @param dbPath 
#' @param full if \code{FALSE} a minimal taxonomic description is extracted
#' (TaxId, ScientificName, Rank).
#'
#' @return A \linkS4class{Taxon} or \linkS4class{TaxonList} instance.
#' @rdname Taxon
#' @export
taxonByGeneID <- function (geneid, dbPath=NULL, full=TRUE) {
  if (missing(geneid)) {
    return( new("Taxon_full") )
  }

  db <- new.env(parent=emptyenv())
  db$taxonDBcon <- taxonDBConnect(dbPath)
  db$geneidDBcon <- geneidDBConnect(dbPath)
  new_taxon_by_geneid(taxid, db, full=full)
}


# Parser -----------------------------------------------------------------


#' @export
#' @autoImports
parseTaxon <- function (taxaSet = response) {
  
  if (is(taxaSet, "efetch")) {
    taxaSet <- content(taxaSet)
  }
  catchEFetchError(taxaSet)
  if (!is(taxaSet, "XMLInternalDocument")) {
    return(taxaSet)
  }
  taxaSet <- getNodeSet(xmlRoot(taxaSet), '//TaxaSet/Taxon')
  if (all_empty(taxaSet)) {
    stop("No 'TaxaSet' provided")
  }
  
  tx <- base::lapply(taxaSet, function (taxon) {
    # taxon <- xmlDoc(taxaSet[[1]])
    taxon <- xmlDoc(taxon)
    taxMin <- new("Taxon_minimal",
                  TaxId = xvalue(taxon, '/Taxon/TaxId'),
                  ScientificName = xvalue(taxon, '/Taxon/ScientificName'),
                  Rank = xvalue(taxon, '/Taxon/Rank'))
    ParentTaxId <- xvalue(taxon, '/Taxon/ParentTaxId')
    nm <- xname(taxon, '//OtherNames/*')
    obj <- xvalue(taxon, '//OtherNames/*')[nm != "Name"]
    OtherName <- setNames(obj, nm[nm != "Name"]) %||% NA_character_
    classCDE <- xvalue(taxon, '//OtherNames/Name/ClassCDE')
    dispName <- xvalue(taxon, '//OtherNames/Name/DispName')
    Authority <-  dispName[classCDE == "authority"] %||% NA_character_
    TypeMaterial <- dispName[classCDE == "type material"] %||% NA_character_
    Lineage <- Lineage(
      base::lapply(getNodeSet(taxon, "//LineageEx/Taxon"), function (l) {
        l <- xmlDoc(l)
        new("Taxon_minimal",
            TaxId = xvalue(l, '/Taxon/TaxId'),
            ScientificName = xvalue(l, '/Taxon/ScientificName'),
            Rank = xvalue(l, '/Taxon/Rank'))
      }))
    free(taxon)
    new("Taxon_full", taxMin, ParentTaxId = ParentTaxId,
        OtherName = OtherName, Authority = Authority,
        TypeMaterial = TypeMaterial, Lineage = Lineage)
  })
  
  if (length(tx) == 1) {
    return( tx[[1]] )
  } else {
    return( TaxonList(tx) )
  }
}


# show -------------------------------------------------------------------


.show_Taxon <- function (x, width = getOption("width"), ellipsis = "...") {
  ellipsize(sprintf("%s (%s; %s)", getTaxId(x, FALSE), getScientificName(x),
                    getRank(x)), width = width, ellipsis = ellipsis)
}
setMethod("show", "Taxon",
          function (object) {
            showme <- .show_Taxon(object)
            cat(showme, sep="\n")
            if (is(object, "Taxon_full")) {
              lin <- .show_Lineage(getLineage(object),
                                   width = getOption("width") - 10) %||% NA_character_
              cat(sprintf("Lineage: %s\n", lin), sep="")
            }
          })

