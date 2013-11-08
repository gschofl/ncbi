% Built by roxygen3 0.1
\docType{class}
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

  \item{shared}{A shared envionment containing connections to \bold{taxon.db}
and (optionally) \bold{geneid.db}}
}

\value{
A \linkS4class{TaxonList} or a \linkS4class{LineageList}.
}
\description{
\dQuote{\bold{TaxonList}} and \dQuote{\bold{LineageList}}
are lists of \linkS4class{Taxon} and \linkS4class{Lineage}
objects, respectively.
}
\section{Class hierarchy for TaxonList}{
Super classes:
\itemize{
  \item \linkS4class{Collection}
  \item \linkS4class{list}
  \item \linkS4class{vector}
}

}

\section{Generics with methods for TaxonList}{
\itemize{
  \item \code{\link{getAuthority}}
  \item \code{\link{getByRank}}
  \item \code{\link{getLineage}}
  \item \code{\link{getOtherName}}
  \item \code{\link{getParentTaxID}}
  \item \code{\link{getRank}}
  \item \code{\link{getScientificName}}
  \item \code{\link{getTaxID}}
  \item \code{\link{is.na}}
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
}

}

\section{Generics with methods for LineageList}{
\itemize{
  \item \code{\link{getByRank}}
  \item \code{\link{show}}
}

}

\keyword{classes}
\keyword{internal}
