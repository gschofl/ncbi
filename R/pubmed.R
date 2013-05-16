#' @include utils.R
NULL

setOldClass("bibentry")

setClass("doi", 
         representation(doi = "character"),
         prototype(doi = NA_character_))

setClass("pubmed",
         representation(pmid = "character",
                        doi = "doi",
                        cites = "list",
                        date = "list",
                        ref = "bibentry"))


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
#' parsed a (list of) \linkS4class{pubmed} instance(s).
#' @rdname pubmed
#' @export
pubmed <- function(pmid, rettype = NULL, retmax = 25, parse = TRUE, ...) {
  
  if (is(pmid, "esearch")) {
    if (database(pmid) != 'pubmed')
      stop("Database ", sQuote(database(pmid)), " not supported")
    if (!has_webenv(pmid))
      pmid <- idList(pmid)
  }
  
  args <- getArgs(id=pmid, db="pubmed", rettype, retmax, ...)
  response <- fetch_records(args, 500)
  if (parse) {
    switch(args$rettype %|null|% "xml",
           xml = parsePubmed(response),
           uilist = parseUilist(response),
           response)
  } else {
    response
  }
}


#' @rdname pubmed
#' @export
#' @autoImports
parsePubmed <- function (pmArticleSet = response) {
  
  if (is(pmArticleSet, "efetch")) {
    pmArticleSet <- content(pmArticleSet)
  }
  catchEFetchError(pmArticleSet)
  if (!is(pmArticleSet, "XMLInternalDocument")) {
    return(pmArticleSet)
  }
  pmArticleSet <- xmlRoot(pmArticleSet)
  pmArtSet <- getNodeSet(pmArticleSet, '//PubmedArticleSet/PubmedArticle')
  if (all_empty(pmArtSet)) {
    stop("No 'PubmedArticleSet' provided")
  }
  
  # art <- pmArtSet[[1]]
  reff <- base::lapply(pmArtSet, function (art) {
    art <- xmlDoc(art)
    pmid <- xvalue(art, '//MedlineCitation/PMID')
    if (!is.na(xvalue(art, '//ELocationID[@EIdType="doi"]'))) {
      doi <- new("doi",
               doi=xvalue(art, '//ELocationID[@EIdType="doi"]'))
    } else {
      doi <- new("doi",
               doi=xvalue(art, '//ArticleId[@IdType="doi"]'))
    }
    
    dateCreated <- as.POSIXlt(xvalue(art, '//MedlineCitation/DateCreated', NA),
                              format="%Y%m%d")
    
    cites <- xvalue(art, '//CommentsCorrections[ @RefType="Cites"]/PMID', NA)
  
    # bibentry
    author <- {
      lastName <- xvalue(art, '//AuthorList//LastName')
      foreName <- xvalue(art, '//AuthorList//ForeName')
      person(given=base::as.list(foreName), family=base::as.list(lastName))
    }
    abstract <- {
      abs <- xvalue(art, '//Abstract/AbstractText', '')
      headers <- xattr(art, '//Abstract/AbstractText', 'Label')
      if (headers == "NULL" || is.na(headers)) {
        abs
      } else {
        paste0(headers, ": ", abs, collapse="\n")
      }
    }
    article <- list(
      title = xvalue(art, '//ArticleTitle', ''),
      abstract = abstract,
      doi = xvalue(art, '//ArticleIdList/ArticleId[@IdType="doi"]', ''),
      pii = xvalue(art, '//ArticleIdList/ArticleId[@IdType="pii"]', ''),
      pmid = xvalue(art, '//ArticleIdList/ArticleId[@IdType="pubmed"]', ''),
      pmc = xvalue(art, '//ArticleIdList/ArticleId[@IdType="pmc"]', '')
    )
    journal <- list(
      issn = xvalue(art, '//Journal/ISSN', ''),
      journal = xvalue(art, '//Journal/Title', ''),
      abbrev = xvalue(art, '//Journal/ISOAbbreviation', ''),
      volume = xvalue(art, '//JournalIssue/Volume', ''),
      number = xvalue(art, '//JournalIssue/Issue', ''),
      year = {
        year <- xvalue(art, '//JournalIssue/PubDate/Year', '')
        medlineDate <- xvalue(art, '//JournalIssue/PubDate/MedlineDate', '')
        if (nzchar(year)) year else medlineDate
      },
      month = xvalue(art, '//JournalIssue/PubDate/Month', ''),
      pages = xvalue(art, '//Pagination/MedlinePgn', '')
    )
    affiliation <- list(
      affiliation = xvalue(art, '//Affiliation', '')
    )
    
    free(art)
    key <- paste0(author[1]$family, journal$year)
    ref <- bibentry('Article', key=key, author=author,
                    other=c(article, journal, affiliation))
    pm <- new("pubmed", pmid = pmid, doi = doi,
              cites = list(cites), date = list(dateCreated),
              ref = ref)
    pm
  })
  
  reff <- do.call(c, reff)
  reff
}

