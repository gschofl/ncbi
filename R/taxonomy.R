#' @include utils.R
NULL


setOldClass("list")


#' taxonList
#' 
#' An extension of \link{list} that holds only \linkS4class{taxon}
#' instances.
#' 
#' @export
#' @classHierarchy
#' @classMethods
setClass("taxonList", contains="list")


setClass("Lineage", contains="taxonList")


#' Retrieve records from the Taxonomy database.
#'
#' \dQuote{taxon} is an S4 class that provides a container for
#' records retrieved from the NCBI Taxonomy database.
#' 
#' @slot taxId A \code{character} vector containing the Taxonomy
#'  Identifier, a stable unique identifier for each taxon in
#'  the NCBI Taxonomy database.
#' @slot parentTaxId The Taxonomy Identifier of the parental taxon.
#' @slot scientificName The unique scientific name of the taxon.
#' @slot otherName A named character vector holdoing synonyms
#'  or GenBankSynonyms.
#' @slot authority
#' @slot typeMaterial
#' @slot rank
#' @slot lineage
#'  
#' @rdname taxon
#' @export
#' @classHierarchy
#' @classMethods
setClass("taxon",
         representation(taxId = "character",
                        parentTaxId = "character",
                        scientificName = "character",
                        otherName = "character",
                        authority = "character",
                        typeMaterial = "character",
                        rank = "character",
                        lineage = "Lineage"),
         prototype(taxId = NA_character_,
                   parentTaxId = NA_character_,
                   scientificName = NA_character_,
                   otherName = NA_character_,
                   authority = NA_character_,
                   typeMaterial = NA_character_,
                   rank = NA_character_,
                   lineage = new("Lineage"))
)


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
#' @return An \linkS4class{XMLInternalDocument} or if #' parsed a
#'  \linkS4class{taxon} or \linkS4class{taxonList} instance.
#' @rdname taxon
#' @export
#' @autoImports
taxon <- function (taxid, rettype = NULL, retmax = 25, parse = TRUE, ...) {
  if (missing(taxid)) {
    return(new("taxon"))
  }
  
  if (is(taxid, "esearch")) {
    if (database(taxid) != 'taxonomy')
      stop("Database ", sQuote(database(taxid)), " not supported")
    if (!has_webenv(taxid))
      taxid <- idList(taxid)
  }
  
  args <- getArgs(taxid, "taxonomy", rettype, retmax, ...)
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
  
  tx <- lapply(taxaSet, function (taxon) {
    # taxon <- xmlDoc(taxaSet[[1]])
    taxon <- xmlDoc(taxon)
    taxId <- xvalue(taxon, '/Taxon/TaxId')
    parentTaxId <- xvalue(taxon, '/Taxon/ParentTaxId')
    sciName <- xvalue(taxon, '/Taxon/ScientificName')
    rank <- xvalue(taxon, '/Taxon/Rank')
    
    nm <- xname(taxon, '//OtherNames/*')
    obj <- xvalue(taxon, '//OtherNames/*')[nm != "Name"]
    otherName <- setNames(obj, nm[nm != "Name"]) %||% NA_character_
    
    classCDE <- xvalue(taxon, '//OtherNames/Name/ClassCDE')
    dispName <- xvalue(taxon, '//OtherNames/Name/DispName')
    authority <-  dispName[classCDE == "authority"] %||% NA_character_
    typeMaterial <- dispName[classCDE == "type material"] %||% NA_character_
    
    lineage <- lapply(getNodeSet(taxon, "//LineageEx/Taxon"), function (l) {
      l <- xmlDoc(l)
      new("taxon", taxId = xvalue(l, '//TaxId'),
          scientificName = xvalue(l, '//ScientificName'),
          rank = xvalue(l, '//Rank'))
    })
    
    free(taxon)
    
    new("taxon", taxId = taxId, parentTaxId = parentTaxId,
        scientificName = sciName, otherName = otherName,
        authority = authority, typeMaterial = typeMaterial,
        rank = rank, lineage = new("Lineage", lineage))
  })
  
  if (length(tx) > 1) {
    return(new("taxonList", tx))
  } else {
    return(tx[[1]])
  }  
}
