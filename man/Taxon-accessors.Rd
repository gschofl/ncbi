% Built by roxygen3 0.1
\docType{methods}
\name{getTaxId}
\alias{getAuthority}
\alias{getByRank,Taxon_full-method}
\alias{getLineage}
\alias{getOtherName}
\alias{getParentTaxId}
\alias{getRank}
\alias{getScientificName}
\alias{getTaxId}
\title{Taxon accessors}
\usage{
  getTaxId(x, ...)

  getScientificName(x, ...)

  getRank(x, ...)

  getParentTaxId(x, ...)

  getOtherName(x, ...)

  getAuthority(x, ...)

  getLineage(x, ...)

  \S4method{getByRank}{Taxon_full}(x, rank, value = NULL,
    ...)
}
\arguments{
  \item{x}{A \linkS4class{Taxon} or \linkS4class{TaxonList}.}

  \item{...}{Further arguments passed on to methods.}

  \item{x}{A \code{\linkS4class{Taxon}, \code{\linkS4class{TaxonList}},
\code{\linkS4class{Lineage}}, or \code{\linkS4class{LineageList}} object.}

  \item{rank}{One of the valid ranks for NCBI taxonomies (see Details).}

  \item{value}{One of \code{NULL}, \sQuote{TaxId}, or \sQuote{ScientificName}.
If \code{NULL}, \code{Taxon} objects are returned, otherwise a character
vector of TaxIds or scientific names, respectively.}
}

\description{
Access the slots of \linkS4class{Taxon} or \linkS4class{TaxonList}
instances.
}
\section{Method signatures for generic getTaxId}{
\itemize{
  \item \linkS4class{Taxon_minimal}
  \item \linkS4class{TaxonList}
}

}

\section{Method signatures for generic getScientificName}{
\itemize{
  \item \linkS4class{Taxon_minimal}
  \item \linkS4class{TaxonList}
}

}

\section{Method signatures for generic getRank}{
\itemize{
  \item \linkS4class{Taxon_minimal}
  \item \linkS4class{TaxonList}
}

}

\section{Method signatures for generic getParentTaxId}{
\itemize{
  \item \linkS4class{Taxon_full}
}

}

\section{Method signatures for generic getOtherName}{
\itemize{
  \item \linkS4class{Taxon_full}
}

}

\section{Method signatures for generic getAuthority}{
\itemize{
  \item \linkS4class{Taxon_full}
}

}

\section{Method signatures for generic getLineage}{
\itemize{
  \item \linkS4class{Taxon_full}
  \item \linkS4class{TaxonList}
}

}

\keyword{methods}
