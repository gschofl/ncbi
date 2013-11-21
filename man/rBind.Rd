\name{rBind}
\alias{rBind}
\title{Bind a list of data frames}
\usage{
  rBind(x)
}
\arguments{
  \item{x}{A list of data frames.}
}
\value{
  A data frame.
}
\description{
  Bind a list of data.frames by row. Equivalent to
  \code{do.call( "rbind", x)}, but faster. This function
  performs \bold{no} checking whatsoever! Each component of
  the list must be a \code{data.frame} with the same number
  of columns and with all columns of equivalent class.
}
\details{
  Do not use factors: they will be converted to their
  internal integer representations.
}
\keyword{internal}

