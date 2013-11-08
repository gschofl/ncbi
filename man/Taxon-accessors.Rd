% Built by roxygen3 0.1
\name{getTaxID}
\alias{getAuthority}
\alias{getLineage}
\alias{getOtherName}
\alias{getParentTaxID}
\alias{getRank}
\alias{getScientificName}
\alias{getTaxID}
\title{Taxon accessors}
\usage{
  getTaxID(x, ...)

  getScientificName(x, ...)

  getRank(x, ...)

  getParentTaxID(x, ...)

  getOtherName(x, ...)

  getAuthority(x, ...)

  getLineage(x, ...)
}
\arguments{
  \item{x}{A \linkS4class{Taxon} or \linkS4class{TaxonList}.}

  \item{...}{Further arguments passed on to methods.}
}

\description{
Access the slots of \linkS4class{Taxon} or
\linkS4class{TaxonList} objects.
}
\section{Method signatures for generic getTaxID}{
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

\section{Method signatures for generic getParentTaxID}{
\itemize{
  \item \linkS4class{Taxon_full}
  \item \linkS4class{TaxonList}
}

}

\section{Method signatures for generic getOtherName}{
\itemize{
  \item \linkS4class{Taxon_full}
  \item \linkS4class{TaxonList}
}

}

\section{Method signatures for generic getAuthority}{
\itemize{
  \item \linkS4class{Taxon_full}
  \item \linkS4class{TaxonList}
}

}

\section{Method signatures for generic getLineage}{
\itemize{
  \item \linkS4class{Taxon_full}
  \item \linkS4class{TaxonList}
}

}

\keyword{methods}
