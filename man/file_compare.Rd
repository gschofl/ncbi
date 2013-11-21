\name{file_compare}
\alias{file_compare}
\title{Compare local and remote file timestamp and/or size}
\usage{
  file_compare(file, url, time = TRUE, size = FALSE,
    .message = TRUE)
}
\arguments{
  \item{file}{Path to local file.}

  \item{url}{URL of remote source.}

  \item{time}{Compare the timestamps (default:
  \code{TRUE}).}

  \item{size}{Compare the file sizes (default:
  \code{FALSE}).}

  \item{.message}{Print a message}
}
\value{
  \code{TRUE} if the local file does not exist, if the
  remote source is more recent than the local file, or if
  the size of the remote source differs from the local
  file. Otherwise return \code{FALSE}.
}
\description{
  Compare local and remote file timestamp and/or size
}
\keyword{internal}

