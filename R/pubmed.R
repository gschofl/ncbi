#' @include utils.R
NULL
#' @importFrom reutils xattr xset
NULL
#' @importFrom utils person bibentry
NULL

setOldClass("bibentry")


new_doi <- setClass(
  Class="doi",
  slots=c(doi="character"),
  prototype=prototype(doi=NA_character_)
)


new_pubmed <- setClass(
  Class="pubmed",
  slots=c(pmid="character",
          doi="doi",
          cites="list",
          date="list",
          ref="bibentry")
)

#' Retrieve records from the PubMed database.
#'
#' @details
#' See the documentation at
#' \href{http://www.ncbi.nlm.nih.gov/books/NBK3827/#pubmedhelp.PubMed_Quick_Start}{NCBI}
#' for more information.
#' 
#' @param pmid \sQuote{PMID}s or a valid NCBI search term.
#' @param rettype Which type of data should be retrieved? Full records
#' (default: \code{NULL}), \sQuote{medline}, \sQuote{uilist},
#' \sQuote{abstract}, or \sQuote{docsum}. 
#' @param retmax Maximal number of records to be retrieved (default: 25).
#' @param parse Should the retrieved data be parsed?
#' @param ... Parameters passed on to the underlying \code{\link{efetch}}
#' query.
#'
#' @return An \linkS4class{XMLInternalDocument}, a character vector, or if
#' parsed a (list of) \linkS4class{pubmed} instance(s).
#' @rdname pubmed
#' @export
pubmed <- function(pmid, rettype=NULL, retmax=25, parse=TRUE, ...) {
  if (is(pmid, "esearch") && database(pmid) != 'pubmed') {
    stop("Database ", sQuote(database(pmid)), " not supported")
  }
  args <- get_args(uid=pmid, db="pubmed", rettype, retmax, ...)
  response <- content(do.call("efetch", args))
  if (parse) {
    switch(args$rettype %||% "xml",
           xml=parsePubmed(response),
           uilist=parseUilist(response),
           response)
  } else {
    response
  }
}


#' @rdname pubmed
#' @export
parsePubmed <- function(pmArticleSet=response) {
  if (is(pmArticleSet, "efetch")) {
    pmArticleSet <- content(pmArticleSet)
  }
  catchEFetchError(pmArticleSet)
  if (!is(pmArticleSet, "XMLInternalDocument")) {
    return(pmArticleSet)
  }
  pmArticleSet <- xset(pmArticleSet, '//PubmedArticleSet/PubmedArticle')
  if (length(pmArticleSet) == 0) {
    stop("No 'PubmedArticleSet' provided")
  }
  reff <- lapply(pmArticleSet, function(art) {
    art <- xmlDoc(art)
    pmid <- xvalue(art, '//MedlineCitation/PMID')
    if (!is.na(xvalue(art, '//ELocationID[@EIdType="doi"]'))) {
      doi <- new_doi(doi=xvalue(art, '//ELocationID[@EIdType="doi"]'))
    } else {
      doi <- new_doi(doi=xvalue(art, '//ArticleId[@IdType="doi"]'))
    }
    dateCreated <- as.POSIXlt(xvalue(art, '//MedlineCitation/DateCreated', default=NA),
                              format="%Y%m%d")
    
    cites <- xvalue(art, '//CommentsCorrections[ @RefType="Cites"]/PMID', default=NA)
    
    # bibentry
    author <- {
      lastName <- xvalue(art, '//AuthorList//LastName')
      foreName <- xvalue(art, '//AuthorList//ForeName')
      person(given=as.list(foreName), family=as.list(lastName))
    }
    abstract <- {
      abs <- xvalue(art, '//Abstract/AbstractText', default='')
      headers <- xattr(art, '//Abstract/AbstractText', 'Label')
      if (headers == "NULL" || is.na(headers)) {
        abs
      } else {
        paste0(headers, ": ", abs, collapse="\n")
      }
    }
    article <- list(
      title=xvalue(art, '//ArticleTitle', default=''),
      abstract=abstract,
      doi=xvalue(art, '//ArticleIdList/ArticleId[@IdType="doi"]', default=''),
      pii=xvalue(art, '//ArticleIdList/ArticleId[@IdType="pii"]', default=''),
      pmid=xvalue(art, '//ArticleIdList/ArticleId[@IdType="pubmed"]', default=''),
      pmc=xvalue(art, '//ArticleIdList/ArticleId[@IdType="pmc"]', default='')
    )
    journal <- list(
      issn=xvalue(art, '//Journal/ISSN', default=''),
      journal=xvalue(art, '//Journal/Title', default=''),
      abbrev=xvalue(art, '//Journal/ISOAbbreviation', default=''),
      volume=xvalue(art, '//JournalIssue/Volume', default=''),
      number=xvalue(art, '//JournalIssue/Issue', default=''),
      year={
        year <- xvalue(art, '//JournalIssue/PubDate/Year', default='')
        medlineDate <- xvalue(art, '//JournalIssue/PubDate/MedlineDate', default='')
        if (nzchar(year)) year else medlineDate
      },
      month=xvalue(art, '//JournalIssue/PubDate/Month', default=''),
      pages=xvalue(art, '//Pagination/MedlinePgn', default='')
    )
    affiliation <- list(
      affiliation=xvalue(art, '//Affiliation', default='')
    )
    
    free(art)
    key <- paste0(author[1]$family, journal$year)
    ref <- bibentry('Article', key=key, author=author, other=c(article, journal, affiliation))
    new_pubmed(pmid=pmid, doi=doi, cites=list(cites), date=list(dateCreated), ref=ref)
  })
  
  reff <- do.call(c, reff)
  reff
}

