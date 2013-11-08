% Built by roxygen3 0.1
\name{Lineage}
\alias{Lineage}
\title{Construct a Lineage}
\usage{
  Lineage(..., shared = new.env(parent = emptyenv()))
}
\arguments{
  \item{...}{\code{\linkS4class{Taxon}} objects that make up a Lineage}

  \item{shared}{A shared envionment containing connections to \bold{taxon.db}
and \bold{geneid.db}}
}

\value{
A \linkS4class{Lineage}.
}
\description{
Construct a Lineage
}
\keyword{internal}
