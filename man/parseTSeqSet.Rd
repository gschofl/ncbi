% Built by roxygen3 0.1
\name{parseTSeqSet}
\alias{parseTSeqSet}
\title{Parse Tiny Bioseq FASTA files}
\usage{
  parseTSeqSet(tSeqSet = response)
}
\arguments{
  \item{tSeqSet}{An \linkS4class{efetch} or \linkS4class{XMLInternalDocument}
instance contining TSeqSet XML file.}
}

\value{
An \linkS4class{XStringSet} instance with additional innformation
in the \code{elementMetadata} slot.
}
\description{
Parse Tiny Bioseq FASTA files
}
