% Built by roxygen3 0.1
\name{taxonDBConnect}
\alias{geneidDBConnect}
\alias{taxonDBConnect}
\title{Create a connection to a local NCBI Taxonomy or Gene ID database}
\usage{
  taxonDBConnect(db_path = getOption("ncbi.taxonomy.path"))

  geneidDBConnect(db_path = getOption("ncbi.taxonomy.path"))
}
\arguments{
  \item{db_path}{Path to the directory holding the local taxonomy and
gene ID database. This path can be permanently configured by setting
the option \code{ncbi.taxonomy.path}. Run the command
\code{\link{createTaxonDB}} to create a new taxonomy database.}
}

\value{
A \code{\linkS4class{TaxonDBConnection}} or a
\code{\linkS4class{GeneidDBConnection}}, respectively
}
\description{
Create a connection to a local NCBI Taxonomy or Gene ID database
}
\seealso{
\code{\link{taxonDB}}, \code{\link{taxonByGeneID}},
}
