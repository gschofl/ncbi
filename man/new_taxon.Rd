\name{new_taxon}
\alias{new_taxon}
\alias{new_taxon_by_geneid}
\title{Extract taxon from taxon.db}
\usage{
  new_taxon(taxid, shared, full = TRUE, ...)

  new_taxon_by_geneid(geneid, shared, full = TRUE, ...)
}
\arguments{
  \item{taxid}{TaxId}

  \item{shared}{Shared environment containing a connection
  to \bold{taxon.db} and (optionally) \bold{geneid.db}.}

  \item{full}{Taxon_minimal or Taxon_full.}

  \item{...}{Further arguments.}

  \item{geneid}{GeneId (GI-number)}

  \item{shared}{Shared environment containing a connection
  to taxon.db and (optionally) geneid.db.}

  \item{full}{Taxon_minimal or Taxon_full.}

  \item{...}{Further arguments.}
}
\description{
  Extract taxon from taxon.db
}
\keyword{internal}

