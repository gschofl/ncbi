\name{pubmed-class}
\alias{pubmed-class}
\title{pubmed-class}
\usage{
  new_pubmed(...)
}
\description{
  \dQuote{\bold{pubmed}} is a class that provides a
  container for records retrieved from the PubMed database.
}
\section{Slots}{
  \describe{ \item{\code{pmid}:}{\code{character}; A PubMed
  Identifier.} \item{\code{doi}:}{\code{\linkS4class{doi}};
  A Digital Object Identifier.}
  \item{\code{cites}:}{\code{list}.}
  \item{\code{date}:}{\code{list}.}
  \item{\code{ref}:}{\code{\link[utils]{bibentry}}; A
  bibliografic entry} }
}
\keyword{classes}
\keyword{internal}

