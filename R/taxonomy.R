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
  args <- taxonomy_args(taxid, rettype, retmax, ...)
  response <- fetch_records(args, 500)
  if (parse) {
    switch(args$rettype %||% "xml",
           xml = parseTaxon(taxaSet=response),
           uilist = parseUilist(response),
           response)
  } else {
    response
  }
}


#' @export
#' @importFrom XML xmlName
#' @autoImports
parseTaxon <- function (taxaSet) {
  
  if (is(taxaSet, "efetch")) {
    taxaSet <- content(taxaSet)
  }
  catchEFetchError(taxaSet)
  if (!is(taxaSet, "XMLInternalDocument")) {
    return(taxaSet)
  }
  taxaSet <- getNodeSet(xmlRoot(taxaSet), '//TaxaSet/Taxon')
  if (is_empty(taxaSet)) {
    stop("No 'TaxaSet' provided")
  }
  
  tx <- lapply(taxaSet, function (taxon) {
    # taxon <- xmlDoc(taxaSet[[1]])
    taxon <- xmlDoc(taxon)
    taxId <- unlist(xpathApply(taxon, "/Taxon/TaxId", xmlValue))
    parentTaxId <- unlist(xpathApply(taxon, "/Taxon/ParentTaxId", xmlValue))
    sciName <- unlist(xpathApply(taxon, "/Taxon/ScientificName", xmlValue))
    rank <- unlist(xpathApply(taxon, "/Taxon/Rank", xmlValue))
    
    nm <- xpathSApply(taxon, "//OtherNames/*", xmlName)
    obj <- xpathSApply(taxon, "//OtherNames/*", xmlValue)[nm != "Name"]
    otherName <- setNames(obj, nm[nm != "Name"]) %||% NA_character_
    
    classCDE <- xpathSApply(taxon, "//OtherNames/Name/ClassCDE", xmlValue)
    dispName <- xpathSApply(taxon, "//OtherNames/Name/DispName", xmlValue)
    authority <-  dispName[classCDE == "authority"] %||% NA_character_
    typeMaterial <- dispName[classCDE == "type material"] %||% NA_character_
    
    lineage <- lapply(getNodeSet(taxon, "//LineageEx/Taxon"), function (l) {
      l <- xmlDoc(l)
      new("taxon", taxId = xpathSApply(l, "//TaxId", xmlValue),
          scientificName = xpathSApply(l, "//ScientificName", xmlValue),
          rank = xpathSApply(l, "//Rank", xmlValue))
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
