% Built by roxygen3 0.1
\name{createTaxonDB}
\alias{createGeneidDB}
\alias{createTaxonDB}
\alias{updateGeneidDB}
\alias{updateTaxonDB}
\title{Create a local install of the NCBI Taxonomy database.}
\usage{
  createTaxonDB(db_path = "~/taxonomy")

  updateTaxonDB(db_path = "~/taxonomy")

  createGeneidDB(db_path = "~/taxonomy")

  updateGeneidDB(db_path = "~/taxonomy")
}
\arguments{
  \item{db_path}{Parent directory for SQLite database files.}
}

\description{
Create a local install of the NCBI Taxonomy database.
}
\details{
From the commandline run:
\code{R -q -e "require(ncbi);createTaxonDB('/path/to/db');createGeneidDB('/path/to/db')"}
}
\seealso{
\code{\link{taxonDBConnext}}, \code{\link{geneidDBConnext}}
}
