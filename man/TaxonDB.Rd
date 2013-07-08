% Built by roxygen3 0.1
\name{createTaxonDB}
\alias{createGeneidDB}
\alias{createTaxonDB}
\alias{updateTaxonDB}
\title{Create a local install of the NCBI Taxonomy database.}
\usage{
  createTaxonDB(db_path = file.path(path.package("ncbi"), "extdata"))

  updateTaxonDB(db_path = file.path(path.package("ncbi"), "extdata"))

  createGeneidDB(db_path = file.path(path.package("ncbi"), "extdata"))
}
\arguments{
  \item{db_path}{Parent directory for SQLite database files.}
}

\description{
Create a local install of the NCBI Taxonomy database.
}
\details{
From the commandline:
\code{R -q -e "require(ncbi); createTaxonDB('/path/to/db');createGeneidDB('/path/to/db')"}
}
\seealso{
\code{\link{taxonDBConnext}}, \code{\link{geneidDBConnext}}
}
