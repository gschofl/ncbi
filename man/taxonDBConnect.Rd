\name{taxonDBConnect}
\alias{geneidDBConnect}
\alias{taxonDBConnect}
\title{Create connections to a local NCBI Taxonomy or GeneID database}
\usage{
  taxonDBConnect(db_path=getOption("ncbi.taxonomy.path")

  geneidDBConnect(db_path=getOption("ncbi.taxonomy.path")
}
\arguments{
  \item{db_path}{Path to the directory containing the local
  taxonomy and GeneID databases. This path cshould be
  configured by setting the option
  \code{ncbi.taxonomy.path}.}
}
\value{
  A \code{\linkS4class{TaxonDBConnection}} or a
  \code{\linkS4class{GeneidDBConnection}}, respectively.
}
\description{
  These functions are not typically invoked by the user.
  The path to the taxonomy and GeneID databases is
  specified rather by setting the global option
  \code{ncbi.taxonomy.path}.
}
\details{
  Run the code{\link{createTaxonDB}} and
  ode{\link{createGeneidDB}} to install the taxonomy and
  geneid databases locally.
}
\seealso{
  \code{\link{taxonDB}}, \code{\link{taxonByGeneID}}
}
\keyword{internal}

