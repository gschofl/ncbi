\docType{methods}
\name{getByRank}
\alias{getByRank}
\title{Extract taxa from a lineage by their rank}
\arguments{
  \item{x}{A \code{\linkS4class{Taxon}},
  \code{\linkS4class{TaxonList}},
  \code{\linkS4class{Lineage}}, or
  \code{\linkS4class{LineageList}} object.}

  \item{rank}{One of the valid ranks for NCBI taxonomies
  (see Details).}

  \item{value}{One of \code{NULL}, \sQuote{TaxId}, or
  \sQuote{ScientificName}. If \code{NULL}, \code{Taxon}
  objects are returned, otherwise a character vector of
  TaxIds or scientific names, respectively.}
}
\description{
  Valid rank designations are: superkingdom, kingdom,
  subkingdom, superphylum, phylum, subphylum, superclass,
  class, subclass, infraclass, superorder, order, suborder,
  parvorder, infraorder, superfamily, family, subfamily,
  tribe, subtribe, genus, subgenus, species group, species
  subgroup, species, subspecies, varietas, and forma.
}

