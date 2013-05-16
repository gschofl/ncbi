% Built by roxygen3 0.1
\name{getParentTaxId}
\alias{getAuthority}
\alias{getByRank}
\alias{getLineage}
\alias{getOtherName}
\alias{getParentTaxId}
\alias{getRank}
\alias{getScientificName}
\alias{getTaxId}
\title{Taxon accessors}
\usage{
  getParentTaxId(x, ...)

  getOtherName(x, ...)

  getAuthority(x, ...)

  getLineage(x, ...)

  getTaxId(x, ...)

  getScientificName(x, ...)

  getRank(x, ...)

  getByRank(x, rank, value = NULL, ...)
}
\arguments{
  \item{x}{A \linkS4class{Taxon} or \linkS4class{TaxonList}.}

  \item{...}{Further arguments passed on to methods.}
}

\description{
Access the slots of \linkS4class{Taxon} or \linkS4class{TaxonList}
instances.
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

\section{Method signatures for generic getTaxId}{
\itemize{
  \item \linkS4class{Taxon}
  \item \linkS4class{TaxonList}
}

}

\section{Method signatures for generic getScientificName}{
\itemize{
  \item \linkS4class{Taxon}
  \item \linkS4class{TaxonList}
}

}

\section{Method signatures for generic getRank}{
\itemize{
  \item \linkS4class{Taxon}
  \item \linkS4class{TaxonList}
}

}

\section{Method signatures for generic getByRank}{
\itemize{
  \item \linkS4class{Lineage}
  \item \linkS4class{LineageList}
  \item \linkS4class{TaxonList}
}

}

\keyword{methods}
