% Built by roxygen3 0.1
\docType{class}
\name{Taxon-class}
\alias{Lineage-class}
\alias{Taxon-class}
\alias{Taxon_full-class}
\alias{Taxon_minimal-class}
\alias{taxon}
\alias{taxonByGeneID}
\alias{taxonDB}
\title{Retrieve records from the NCBI Taxonomy database.}
\usage{
  taxon(taxid, rettype = NULL, retmax = 25, parse = TRUE,
    ...)

  taxonDB(taxid, dbPath = NULL, full = TRUE)

  taxonByGeneID(geneid, dbPath = NULL, full = TRUE)
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

  \item{dbPath}{Path to a valid Taxon.db}

  \item{full}{if \code{FALSE} a minimal taxonomic description is extracted
(TaxId, ScientificName, Rank).}

  \item{geneid}{A vector of valid NCBI Gene Identifiers.}

  \item{dbPath}{}

  \item{full}{if \code{FALSE} a minimal taxonomic description is extracted
(TaxId, ScientificName, Rank).}
}

\value{
An \linkS4class{XMLInternalDocument} or if parsed a
 \linkS4class{taxon} or \linkS4class{TaxonList} instance.

A \linkS4class{Taxon} or \linkS4class{TaxonList} instance.

A \linkS4class{Taxon} or \linkS4class{TaxonList} instance.
}
\description{
\dQuote{Taxon} is an S4 class that provides a container for records
retrieved from the NCBI Taxonomy database.
It is extended by the classes \dQuote{Taxon_minimal} and \dQuote{Lineage}
with three slots \emph{TaxId}, \emph{ScientificName}, and \emph{Rank},
and by class \dQuote{Taxon_full}, with the additional slots
\emph{ParentTaxId}, \emph{OtherName}, \emph{Authority},
\emph{TypeMaterial}, and \emph{Lineage}.
}
\details{
See the documentation at 
\href{http://www.ncbi.nlm.nih.gov/books/NBK21100/}{NCBI}
for more information.
}
\section{Class hierarchy for Taxon}{
Sub classes:
\itemize{
  \item \linkS4class{Taxon_minimal}
  \item \linkS4class{Lineage}
  \item \linkS4class{Taxon_full}
}

}

\section{Generics with methods for Taxon}{
\itemize{
  \item \code{\link{[}}* (1 methods defined for subclasses)
  \item \code{\link{getAuthority}}* (1 methods defined for subclasses)
  \item \code{\link{getByRank}}* (2 methods defined for subclasses)
  \item \code{\link{getLineage}}* (1 methods defined for subclasses)
  \item \code{\link{getOtherName}}* (1 methods defined for subclasses)
  \item \code{\link{getParentTaxId}}* (1 methods defined for subclasses)
  \item \code{\link{getRank}}* (1 methods defined for subclasses)
  \item \code{\link{getScientificName}}* (1 methods defined for subclasses)
  \item \code{\link{getTaxId}}* (1 methods defined for subclasses)
  \item \code{\link{is.na}}* (1 methods defined for subclasses)
  \item \code{\link{shared}}
  \item \code{\link{show}} (1 methods defined for subclasses)
}

* = methods only defined for subclasses
}

\section{Class hierarchy for Taxon_minimal}{
Sub classes:
\itemize{
  \item \linkS4class{Lineage}
  \item \linkS4class{Taxon_full}
}
Super classes:
\itemize{
  \item \linkS4class{Taxon}
}

}

\section{Generics with methods for Taxon_minimal}{
\itemize{
  \item \code{\link{[}}* (1 methods defined for subclasses)
  \item \code{\link{getAuthority}}* (1 methods defined for subclasses)
  \item \code{\link{getByRank}}* (2 methods defined for subclasses)
  \item \code{\link{getLineage}}* (1 methods defined for subclasses)
  \item \code{\link{getOtherName}}* (1 methods defined for subclasses)
  \item \code{\link{getParentTaxId}}* (1 methods defined for subclasses)
  \item \code{\link{getRank}}
  \item \code{\link{getScientificName}}
  \item \code{\link{getTaxId}}
  \item \code{\link{is.na}}
  \item \code{\link{show}}* (1 methods defined for subclasses)
}

* = methods only defined for subclasses
}

\section{Class hierarchy for Lineage}{
Super classes:
\itemize{
  \item \linkS4class{Taxon_minimal}
  \item \linkS4class{Taxon}
}

}

\section{Generics with methods for Lineage}{
\itemize{
  \item \code{\link{[}}
  \item \code{\link{getByRank}}
  \item \code{\link{show}}
}

}

\section{Class hierarchy for Taxon_full}{
Super classes:
\itemize{
  \item \linkS4class{Taxon_minimal}
  \item \linkS4class{Taxon}
}

}

\section{Generics with methods for Taxon_full}{
\itemize{
  \item \code{\link{getAuthority}}
  \item \code{\link{getByRank}}
  \item \code{\link{getLineage}}
  \item \code{\link{getOtherName}}
  \item \code{\link{getParentTaxId}}
}

}

\keyword{classes}
\section{Slots}{
\describe{
  \item{TaxId}{The Taxonomy Identifier, a stable unique identifier for
each taxon in the NCBI Taxonomy database.}

  \item{ScientificName}{The unique scientific name of the taxon.}

  \item{Rank}{The taxonomic rank of the taxon.}

  \item{ParentTaxId}{The Taxonomy Identifier of the parental taxon.}

  \item{OtherName}{A named character vector holding synonyms
or GenBankSynonyms.}

  \item{Authority}{An object of class \linkS4class{character}}

  \item{TypeMaterial}{An object of class \linkS4class{character}}

  \item{Lineage}{An object of class \linkS4class{Lineage}}

}
}
