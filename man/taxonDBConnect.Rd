% Built by roxygen3 0.1
\name{taxonDBConnect}
\alias{geneidDBConnect}
\alias{taxonDBConnect}
\title{Create a connection to a local NCBI Taxonomy or Gene ID database}
\usage{
  taxonDBConnect(db_path = NULL)

  geneidDBConnect(db_path = NULL)
}
\arguments{
  \item{db_path}{Path to the taxonomy or gene ID database. If \code{NULL} the
databases are looked for in the \code{extdata} directory of the
installed \code{ncbi} package. This is the place where they are installed
by default by \code{\link{createTaxonDB}}.}
}

\description{
Create a connection to a local NCBI Taxonomy or Gene ID database
}
\seealso{
\code{\link{taxonDB}}, \code{\link{taxonByGeneID}},
}
