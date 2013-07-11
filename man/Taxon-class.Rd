% Built by roxygen3 0.1
\docType{class}
\name{Taxon-class}
\alias{Lineage-class}
\alias{Taxon-class}
\alias{Taxon_full-class}
\alias{Taxon_minimal-class}
\title{Taxon-classes}
\description{
\dQuote{\bold{Taxon}} is a (virtual) class that provides a container for records
retrieved from the NCBI Taxonomy database.
It is extended by the classes \dQuote{\bold{Taxon_minimal}} and
\dQuote{\bold{Lineage}} with three slots \emph{TaxId}, \emph{ScientificName},
and \emph{Rank}, and by class \dQuote{\bold{Taxon_full}}, with the additional
slots \emph{ParentTaxId}, \emph{OtherName}, \emph{Authority},
\emph{TypeMaterial}, and \emph{Lineage}.
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
  \item \code{\link{is.na}}
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
