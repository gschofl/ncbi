% Built by roxygen3 0.1
\docType{class}
\name{TaxonList-class}
\alias{LineageList-class}
\alias{TaxonList}
\alias{TaxonList-class}
\title{TaxonList and LineageList}
\usage{
  TaxonList(..., shared = new.env(parent = emptyenv()))
}
\description{
\dQuote{TaxonList} and \dQuote{LineageList} are lists of \linkS4class{Taxon}
and \linkS4class{Lineage} objects, respectively.
}
\section{Class hierarchy for TaxonList}{
Super classes:
\itemize{
  \item \linkS4class{Collection}
  \item \linkS4class{list}
  \item \linkS4class{vector}
  \item \linkS4class{vectorORfactor}
}

}

\section{Generics with methods for TaxonList}{
\itemize{
  \item \code{\link{getByRank}}
  \item \code{\link{getLineage}}
  \item \code{\link{getRank}}
  \item \code{\link{getScientificName}}
  \item \code{\link{getTaxId}}
  \item \code{\link{show}}
  \item \code{\link{table}}
}

}

\section{Class hierarchy for LineageList}{
Super classes:
\itemize{
  \item \linkS4class{Collection}
  \item \linkS4class{list}
  \item \linkS4class{vector}
  \item \linkS4class{vectorORfactor}
}

}

\section{Generics with methods for LineageList}{
\itemize{
  \item \code{\link{getByRank}}
  \item \code{\link{show}}
}

}

\keyword{classes}
