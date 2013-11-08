% Built by roxygen3 0.1
\name{createTaxonDB}
\alias{createGeneidDB}
\alias{createTaxonDB}
\alias{updateGeneidDB}
\alias{updateTaxonDB}
\title{Create a local install of the NCBI Taxonomy database.}
\usage{
  createTaxonDB(db_path = getOption("ncbi.taxonomy.path"))

  updateTaxonDB(db_path = getOption("ncbi.taxonomy.path"))

  createGeneidDB(db_path = getOption("ncbi.taxonomy.path"))

  updateGeneidDB(db_path = getOption("ncbi.taxonomy.path"))
}
\arguments{
  \item{db_path}{Parent directory for SQLite database files.}
}

\description{
Create a local install of the NCBI Taxonomy database.
}
\details{
From the commandline run:
\code{R -q -e "require(ncbi);createTaxonDB();createGeneidDB()"}

This will install two SQLite databases "taxon.db" and "geneid.db"
in "$HOME/local/db/taxonomy/". To override the default installation
directory set the option \code{ncbi.taxonomy.path} in your .Rprofile.
}
\seealso{
\code{\link{taxonDBConnect}}, \code{\link{geneidDBConnect}}
}
