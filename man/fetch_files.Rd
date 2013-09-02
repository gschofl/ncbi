% Built by roxygen3 0.1
\name{fetch_files}
\alias{fetch_files}
\title{Fetch files from a remote location conditional on a difference in
timestamp or content-length.}
\usage{
  fetch_files(path, url, files, check = FALSE,
    verbose = FALSE)
}
\arguments{
  \item{path}{Local file path.}

  \item{url}{Remote base URL.}

  \item{files}{File names.}

  \item{check}{if \code{TRUE}, fetch the file only if the
remote file is newer than the local version.}

  \item{verbose}{be verbose}
}

\description{
Fetch files from a remote location conditional on a difference in
timestamp or content-length.
}
\keyword{internal}
