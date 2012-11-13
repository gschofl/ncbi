% Built by roxygen3 0.1
\name{GSS}
\alias{GSS}
\title{Retrieve sequences and annotations from the GSS database}
\usage{
  GSS(term, rettype = "fasta", retmax = 25, parse = TRUE,
    ...)
}
\arguments{
  \item{term}{A valid NCBI search term or \sQuote{GI} numbers.}

  \item{rettype}{}

  \item{retmax}{}

  \item{parse}{}

  \item{...}{Parameters passed on to the underlying \code{\link{esearch}}
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
