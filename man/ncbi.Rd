% Built by roxygen3 0.1
\docType{package}
\name{ncbi}
\alias{ncbi}
\alias{ncbi-package}
\title{Retrieve and parse data from NCBI databases.}
\description{
This package provides an R interface to retrieve data from the following NCBI
databases: \emph{nucleotide}, \emph{protein}, \emph{pubmed}, \emph{taxonomy},
\emph{nucgss}, and \emph{nucest}.
}
\section{Important functions}{


  \itemize{
  
    \item \code{\link{taxon}}: Retrieve taxonomy records remotely from NCBI's
    \emph{Taxonomy} database.
    \item \code{\link{craeateTaxonDB}}: Install NCBI's \emph{Taxonomy}
    database locally.
    \item \code{\link{taxonDBConnect}}: Connect to a local install of the
    \emph{Taxonomy} database.
    \item \code{\link{taxonDB}}: Retrieve records from a local install of
    the \emph{Taxonomy} database.
    \item \code{\link{taxonByGeneID}}: Retrieve records from a local install of
    the \emph{Taxonomy} database via NCBI GI numbers.
    \item \code{\link{pubmed}}: Retrieve records from the \emph{PubMed}
    database.
    \item \code{\link{protein}}: Retrieve records from the \emph{protein}
    database.
    \item \code{\link{nucleotide}}: Retrieve records from the
    \emph{nuccore} database.
    \item \code{\link{GSS}}: Retrieve records from the
    \emph{nucgss} database.   
    \item \code{\link{EST}}: Retrieve records from the
    \emph{nucest} database.
          
  }
}

\author{
Gerhard Schöfl \email{gschofl@yahoo.de}
}
\keyword{package}
