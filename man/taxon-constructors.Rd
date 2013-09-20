% Built by roxygen3 0.1
\name{taxon}
\alias{taxon}
\alias{taxonByGeneID}
\alias{taxonDB}
\title{Retrieve records from the NCBI Taxonomy database (locally or remote)}
\usage{
  taxon(taxid, rettype = NULL, retmax = 25, parse = TRUE,
    ...)

  taxonDB(taxid, taxon_db = NULL, full = TRUE)

  taxonByGeneID(geneid, geneid_db = NULL, taxon_db = NULL,
    full = TRUE)
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

  \item{taxon_db}{A \code{\linkS4class{taxonDBConnection}}.}

  \item{full}{if \code{FALSE} a minimal taxonomic description is extracted
(TaxId, ScientificName, Rank).}

  \item{geneid}{A vector of valid NCBI Gene Identifiers.}

  \item{geneid_db}{A \code{\linkS4class{geneidDBConnection}}.}
}

\value{
An \linkS4class{XMLInternalDocument} or if parsed a
 \linkS4class{taxon} or \linkS4class{TaxonList} instance.
}
\description{
Retrieve records from the NCBI Taxonomy database (locally or remote)
}
\details{
If no \code{geneid_db} or \code{taxon_db} are provided the databases are
searched in the \code{extdata} directory of the installed \code{ncbi}
package. To create these databases in the default location, run
\code{createTaxonDB(with_geneid=TRUE)}.

The \bold{geneid.db} file, however, gets fairly large (currently ~6GB) and
takes a long time to create. It might be advisable to provide a custom 
install path when creating these databases.

See the documentation at 
\href{http://www.ncbi.nlm.nih.gov/books/NBK21100/}{NCBI}
for more information on the NCBI Taxonomy database.
}
\seealso{
\code{\link{createTaxonDB}}, \code{\link{updateTaxonDB}}
}
