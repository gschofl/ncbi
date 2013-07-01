% Built by roxygen3 0.1
\docType{class}
\name{TaxonDB-class}
\alias{TaxonDB-class}
\title{TaxonDB-class}
\description{
\sQuote{\code{TaxonDB}} is an S4 class that represents a connection
to an SQLite database holding the NCBI taxonomy organised in two tables:
}
\details{
\bold{nodes} with fields:

\itemize{
   \item tax_id        CHAR(10)    Primary key
   \item parent_id     CHAR(10)
   \item rank          VARCHAR(50)
   \item embl_code     CHAR(2)
   \item division_id   CHAR(2)
}

\bold{names} with fields:

\itemize{
   \item tax_id        CHAR(10)     Primary key
   \item tax_name      VARCHAR(200)    
   \item unique_name   VARCHAR(100)
   \item class         VARCHAR(50)
}
}
\seealso{
The constructor \code{\link{TaxonDB}}.
}
\keyword{classes}
