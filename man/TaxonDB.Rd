% Built by roxygen3 0.1
\name{createTaxonDB}
\alias{createTaxonDB}
\alias{updateTaxonDB}
\title{Create a local install of the NCBI Taxonomy database.}
\usage{
  createTaxonDB(dbPath = file.path(path.package("ncbi"), "extdata"),
    with_geneid = TRUE)

  updateTaxonDB(dbPath = file.path(path.package("ncbi"), "extdata"),
    with_geneid = TRUE)
}
\arguments{
  \item{dbPath}{Source directory for SQLite database files.}

  \item{with_geneid}{Include mappings of Gene ID to Taxonomy ID (very large table!)}
}

\description{
Create a local install of the NCBI Taxonomy database.
}
\details{
From the commandline:
\code{R -q -e "require(ncbi);createTaxonDB('/path/to/db')"}
}
