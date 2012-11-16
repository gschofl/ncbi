#' @include utils.R
NULL


setOldClass("bibentry")


#' Retrieve records from the PubMed database.
#'
#' @details
#' See the documentation at
#' \href{http://www.ncbi.nlm.nih.gov/books/NBK3827/#pubmedhelp.PubMed_Quick_Start}{NCBI}
#' for more information.
#' 
#' @param pmid \sQuote{PMID}s or a valid NCBI search term.
#' @param rettype Which type of data should be retrieved? Full records
#' (default: \code{NULL}), \sQuote{medline}, \sQuote{uilist}, or
#' \sQuote{abstract}. 
#' @param retmax Maximal number of records to be retrieved (default: 25).
#' @param parse Should the retrieved data be parsed?
#' @param ... Parameters passed on to the underlying \code{\link{efetch}}
#' query.
#'
#' @return An \linkS4class{XMLInternalDocument}, a character vector, or if
#' parsed a (list of) \linkS4class{bibentry} instance(s).
#' @rdname pubmed
#' @export
pubmed <- function(pmid, rettype = NULL, retmax = 25, parse = TRUE, ...) {
  
  if (is(pmid, "esearch")) {
    if (database(pmid) != 'pubmed')
      stop("Database ", sQuote(database(pmid)), " not supported")
    if (!has_webenv(pmid))
      pmid <- idList(pmid)
  }
  
  args <- getArgs(pmid, "pubmed", rettype, retmax, ...)
  response <- fetch_records(args, 500)
  if (parse) {
    switch(args$rettype %||% "xml",
           xml = parsePubmed(response),
           uilist = parseUilist(response),
           response)
  } else {
    response
  }
}


#' @export
#' @autoImports
parsePubmed <- function (pmArticleSet) {
  
  if (is(pmArticleSet, "efetch")) {
    pmArticleSet <- content(pmArticleSet)
  }
  catchEFetchError(pmArticleSet)
  if (!is(pmArticleSet, "XMLInternalDocument")) {
    return(pmArticleSet)
  }
  pmArtSet <- getNodeSet(xmlRoot(pmArticleSet), '//PubmedArticleSet/PubmedArticle')
  if (is_empty(pmArtSet)) {
    stop("No 'PubmedArticleSet' provided")
  }
  
  reff <- lapply(pmArtSet, function (art) {
    # art <- xmlDoc(pmArtSet[[1]])
    art <- xmlDoc(art)
    author <- {
      lastName <- xpathApply(art, "//AuthorList//LastName", xmlValue)
      foreName <- xpathApply(art, "//AuthorList//ForeName", xmlValue)
      list(author = do.call(personList,
                            Map(person, given=foreName, family=lastName)))
    }
    issue <- list(
      volume = xpathSApply(art, '//JournalIssue/Volume', xmlValue),
      number = xpathSApply(art, '//JournalIssue/Issue', xmlValue),
      year = {
        year <- xpathSApply(art, '//JournalIssue/PubDate/Year', xmlValue)
        medlineDate <- xpathSApply(art, '//JournalIssue/PubDate/MedlineDate', xmlValue)
        if (length(year) > 0) year else medlineDate
      },
      month = xpathSApply(art, '//JournalIssue/PubDate/Month', xmlValue),
      pages = xpathSApply(art, '//Pagination/MedlinePgn', xmlValue)
    )
    journal <- list(
      issn = xpathSApply(art, '//Journal/ISSN', xmlValue),
      journal = xpathSApply(art, '//Journal/Title', xmlValue),
      abbrev = xpathSApply(art, '//Journal/ISOAbbreviation', xmlValue) 
    )
    article <- list(
      title = xpathSApply(art, '//ArticleTitle', xmlValue),
      abstract = {
        abs <- xpathSApply(art, '//Abstract/AbstractText', xmlValue)
        headers <- xpathSApply(art, '//Abstract/AbstractText', xmlGetAttr, "Label")
        if (is.null(headers[[1]])) {
          abs
        } else {
          paste0(headers, ": ", abs, collapse="\n")
        }
      },
      doi = xpathSApply(art, '//ArticleIdList/ArticleId[@IdType="doi"]', xmlValue),
      pii = xpathSApply(art, '//ArticleIdList/ArticleId[@IdType="pii"]', xmlValue),
      pmid = xpathSApply(art, '//ArticleIdList/ArticleId[@IdType="pubmed"]', xmlValue),
      pmc = xpathSApply(art, '//ArticleIdList/ArticleId[@IdType="pmc"]', xmlValue)
    )
    affiliation <- list(
      affiliation = xpathSApply(art, "//Affiliation", xmlValue)
    )
    
    issue[vapply(issue, is_empty, logical(1))] <- ""
    journal[vapply(journal, is_empty, logical(1))] <- ""
    article[vapply(article, is_empty, logical(1))] <- ""
    affiliation[vapply(affiliation, is_empty, logical(1))] <- ""
    
    free(art)
    ref <- bibentry('Article', other=c(author, article, journal, issue, affiliation))
    ref
    
  })
  
  reff <- do.call(c, reff)
  reff
}
