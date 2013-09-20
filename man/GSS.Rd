% Built by roxygen3 0.1
\name{GSS}
\alias{GSS}
\title{Retrieve sequences and annotations from the GSS database}
\usage{
  GSS(gi, rettype="fasta", retmax=25, parse=TRUE, ...)
}
\arguments{
  \item{gi}{\sQuote{GI}s or a valid NCBI search term.}

  \item{rettype}{Which type of data should be retrieved? \sQuote{fasta}
(default), \sQuote{gb}, \sQuote{acc}, \sQuote{seqid}, \sQuote{gss},
\sQuote{native}, or \code{NULL} (text ASN.1).}

  \item{retmax}{Maximal number of records to be retrieved (default: 25).}

  \item{parse}{Should the retrieved data be parsed?}

  \item{...}{Parameters passed on to the underlying \code{\link{efetch}}
query.}
}

\value{
A \linkS4class{gbRecord} or an \linkS4class{XStringSet} instance.
}
\description{
Retrieve sequences and annotations from the GSS database
}
\details{
See the documentation at
\href{http://www.ncbi.nlm.nih.gov/books/NBK44863/}{NCBI}
for more information.
}
