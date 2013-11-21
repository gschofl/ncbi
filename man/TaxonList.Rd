\name{TaxonList-class}
\alias{LineageList}
\alias{LineageList-class}
\alias{TaxonList}
\alias{TaxonList-class}
\title{TaxonList and LineageList}
\usage{
  TaxonList(..., shared = new.env(parent = emptyenv()))

  LineageList(..., shared = new.env(parent = emptyenv()))
}
\arguments{
  \item{...}{\code{\linkS4class{Taxon}} objects.}

  \item{shared}{A shared envionment containing connections
  to \bold{taxon.db} and (optionally) \bold{geneid.db}}
}
\value{
  A \linkS4class{TaxonList} or a \linkS4class{LineageList}.
}
\description{
  \dQuote{\bold{TaxonList}} and \dQuote{\bold{LineageList}}
  are lists of \linkS4class{Taxon} and
  \linkS4class{Lineage} objects, respectively.

  Construct a list of Taxa or Lineages
}
\keyword{internal}

