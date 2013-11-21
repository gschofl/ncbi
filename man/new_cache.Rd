\name{new_cache}
\alias{new_cache}
\title{Register a caching environment}
\usage{
  new_cache()
}
\description{
  Generate a \code{cache} object, with \code{set},
  \code{get}, \code{exists}, \code{ls}, and \code{rm}
  methods.
}
\examples{
cache <- new_cache()
cache$set("a", 1:10)
cache$set("b", list(1,2,3,4))
cache$ls()
cache$get("a")
cache$exists("b")
cache$exists("c")
cache$rm()
}
\keyword{internal}

