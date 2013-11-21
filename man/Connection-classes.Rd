\name{TaxonDBConnection-class}
\alias{GeneidDBConnection-class}
\alias{TaxonDBConnection-class}
\title{Database Connections}
\usage{
  new_TaxonDBConnection(...)

  new_GeneidDBConnection(...)
}
\description{
  \sQuote{\bold{TaxonDBConnection}}: A connection to an
  SQLite database containing the NCBI taxonomy organised in
  two tables:
}
\details{
  \bold{nodes} with fields:

  \itemize{ \item tax_id CHAR(10) Primary key \item
  parent_id CHAR(10) \item rank VARCHAR(50) \item embl_code
  CHAR(2) \item division_id CHAR(2) }

  \bold{names} with fields:

  \itemize{ \item tax_id CHAR(10) Primary key \item
  tax_name VARCHAR(200) \item unique_name VARCHAR(100)
  \item class VARCHAR(50) }

  \sQuote{\bold{GeneidDBConnection}}: A connection to an
  SQLite database linking NCBI Gene IDs to Taxids.
}
\seealso{
  The constructors \code{\link{taxonDBConnect}},
  \code{\link{geneidDBConnect}}.
}
\keyword{classes}
\keyword{internal}

