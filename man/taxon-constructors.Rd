% Built by roxygen3 0.1
\name{taxon}
\alias{taxon}
\alias{taxonByGeneID}
\alias{taxonDB}
\title{Retrieve records from the NCBI Taxonomy database (locally or remote)}
\usage{
  taxon(taxid, rettype = NULL, retmax = 25, parse = TRUE,
    ...)

  taxonDB(taxid, full = TRUE)

  taxonByGeneID(geneid, full = TRUE)
}
\arguments{
  \item{taxid}{\sQuote{taxids} or a valid NCBI search term.}

  \item{rettype}{Which type of data should be retrieved? Full records
(default: \code{NULL}) or an \sQuote{uilist}.}

  \item{retmax}{Maximal number of records to be retrieved (default: 25).}

  \item{parse}{Should the retrieved data be parsed?}

  \item{...}{Parameters passed on to the underlying \code{\link{efetch}}
query.}

  \item{taxid}{A vector of valid NCBI Taxonomy Identifiers.}

  \item{full}{if \code{FALSE} a minimal taxonomic description is extracted
(TaxId, ScientificName, Rank).}

  \item{geneid}{A vector of valid NCBI Gene Identifiers.}
}

\value{
An \linkS4class{XMLInternalDocument} or if parsed a
 \linkS4class{taxon} or \linkS4class{TaxonList} instance.
}
\description{
Retrieve records from the NCBI Taxonomy database (locally or remote)
}
\details{
\code{taxonDB} and \code{taxonByGeneID} require a local installation
of the NCBI taxonomy database and a database providing the GI to TaxId
mapping. These databases are created using \code{\link{createTaxonDB}} and
\code{\link{createGeneidDB}}, respectively, and kept up to date with
\code{\link{updateTaxonDB}} and \code{\link{updateGeneidDB}}.

The install path for these custom databases \bold{taxon.db} and
\bold{geneid.db} is specified by setting the global option
\code{ncbi.taxonomy.path}. Currently, it defaults to "$HOME/local/db/taxonomy/".
The default path can be overridden permanently by setting this option
in the .Rprofile file.

Note that the \bold{geneid.db} file can get fairly large (currently ~6GB) and
takes a long time to create.

See the documentation at 
\href{http://www.ncbi.nlm.nih.gov/books/NBK21100/}{NCBI}
for more information on the NCBI Taxonomy database.
}
\seealso{
\code{\link{createTaxonDB}}, \code{\link{updateTaxonDB}}
}
