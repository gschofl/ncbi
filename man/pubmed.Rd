\name{pubmed}
\alias{parsePubmed}
\alias{pubmed}
\title{Retrieve records from the PubMed database.}
\usage{
  pubmed(pmid, rettype = NULL, retmax = 25, parse = TRUE,
    ...)

  parsePubmed(pmArticleSet = response)
}
\arguments{
  \item{pmid}{\sQuote{PMID}s or a valid NCBI search term.}

  \item{rettype}{Which type of data should be retrieved?
  Full records (default: \code{NULL}), \sQuote{medline},
  \sQuote{uilist}, \sQuote{abstract}, or \sQuote{docsum}.}

  \item{retmax}{Maximal number of records to be retrieved
  (default: 25).}

  \item{parse}{Should the retrieved data be parsed?}

  \item{...}{Parameters passed on to the underlying
  \code{\link{efetch}} query.}
}
\value{
  An \linkS4class{XMLInternalDocument}, a character vector,
  or if parsed a (list of) \linkS4class{pubmed} objects.
}
\description{
  Retrieve records from the PubMed database.
}
\details{
  See the documentation at
  \href{http://www.ncbi.nlm.nih.gov/books/NBK3827/#pubmedhelp.PubMed_Quick_Start}{NCBI}
  for more information.
}

