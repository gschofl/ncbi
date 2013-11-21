\name{strip_ext}
\alias{strip_ext}
\title{Strip file extensions}
\usage{
  strip_ext(file, sep = "\\.", level = 0)
}
\arguments{
  \item{file}{The file name(s).}

  \item{sep}{specifies the seperator character (default
  ".").}

  \item{level}{How many extensions should be stripped. The
  default (0) strips all, 1 strips the last one, 2 strips
  the last two, and so on.}
}
\description{
  Strips the extension (or an arbitrary tag) from a file
  name.
}
\keyword{internal}

