% Built by roxygen3 0.1
\docType{class}
\name{pubmed-class}
\alias{pubmed-class}
\title{pubmed-class}
\description{
\dQuote{\bold{pubmed}} is a class that provides a container for records
retrieved from the PubMed database.
}
\section{Slots}{

\describe{
   \item{\code{pmid}:}{\code{character}; A PubMed Identifier.}
   \item{\code{doi}:}{\code{\linkS4class{doi}}; A Digital
   Object Identifier.}
   \item{\code{cites}:}{\code{list}.}
   \item{\code{date}:}{\code{list}.}
   \item{\code{ref}:}{\code{\link[utils]{bibentry}}; A bibliografic entry}
}
}

\section{Generics with methods for pubmed}{
\itemize{
  \item \code{\link{[}}
  \item \code{\link{browsePubmed}}
  \item \code{\link{c}}
  \item \code{\link{getAbstract}}
  \item \code{\link{getAuthor}}
  \item \code{\link{getCites}}
  \item \code{\link{getDoi}}
  \item \code{\link{getIssue}}
  \item \code{\link{getJournal}}
  \item \code{\link{getPmid}}
  \item \code{\link{getTitle}}
  \item \code{\link{show}}
}

}

\keyword{classes}
\keyword{internal}
