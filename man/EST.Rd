\name{EST}
\alias{EST}
\title{Retrieve sequences and annotations from the EST database}
\usage{
  EST(gi, rettype="fasta", retmax=25, parse=TRUE, ...)
}
\arguments{
  \item{gi}{\sQuote{GI}s or a valid NCBI search term.}

  \item{rettype}{Which type of data should be retrieved?
  \sQuote{fasta} (default), \sQuote{gp}, \sQuote{acc},
  \sQuote{seqid}, \sQuote{est}, \sQuote{native}, or
  \code{NULL} (text ASN.1).}

  \item{retmax}{Maximal number of records to be retrieved
  (default: 25).}

  \item{parse}{Should the retrieved data be parsed?}

  \item{...}{Parameters passed on to the underlying
  \code{\link{efetch}} query.}
}
\value{
  A \linkS4class{gbRecord} or an \linkS4class{XStringSet}
  instance.
}
\description{
  Retrieve sequences and annotations from the EST database
}
\details{
  See the documentation at
  \href{http://www.ncbi.nlm.nih.gov/books/NBK44863/}{NCBI}
  for more information.
}

