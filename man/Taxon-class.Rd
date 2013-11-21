\name{Taxon-class}
\alias{Lineage-class}
\alias{Taxon-class}
\alias{Taxon_full-class}
\alias{Taxon_minimal-class}
\title{Taxon-classes}
\usage{
  new_Taxon_minimal(...)

  new_Lineage(...)

  new_Taxon_full(...)
}
\description{
  \dQuote{\bold{Taxon}} is a (virtual) class that provides
  a container for records retrieved from the NCBI Taxonomy
  database. It is extended by the classes
  \dQuote{\bold{Taxon_minimal}} and \dQuote{\bold{Lineage}}
  with three slots \emph{TaxId}, \emph{ScientificName}, and
  \emph{Rank}, and by class \dQuote{\bold{Taxon_full}},
  with the additional slots \emph{ParentTaxId},
  \emph{OtherName}, \emph{Authority}, \emph{TypeMaterial},
  and \emph{Lineage}.
}
\section{Slots}{
  \describe{ \item{\code{TaxId}:}{The Taxonomy Identifier,
  a stable unique identifier for each taxon in the NCBI
  Taxonomy database.} \item{\code{ScientificName}:}{The
  unique scientific name of the taxon.}
  \item{\code{Rank}:}{The taxonomic rank of the taxon.}
  \item{\code{ParentTaxId}:}{The Taxonomy Identifier of the
  parental taxon.} \item{\code{OtherName}:}{A named
  character vector holding synonyms or GenBankSynonyms.}
  \item{\code{Authority}:}{Authority}
  \item{\code{TypeMaterial}:}{Type material}
  \item{\code{Lineage}:}{Lineage} }
}
\seealso{
  \code{\linkS4class{TaxonList}}
}

