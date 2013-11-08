% Built by roxygen3 0.1
\docType{package}
\name{ncbi}
\alias{ncbi}
\alias{ncbi-package}
\title{Retrieve and parse data from NCBI databases.}
\description{
This package provides an R interface to retrieve data from the following NCBI
databases: \emph{nucleotide}, \emph{protein}, \emph{pubmed}, \emph{taxonomy},
\emph{nucgss}, and \emph{nucest}.
}
\section{Important functions}{


  \itemize{
    \item \code{\link{taxon}}: Retrieve taxonomy records remotely from NCBI's
    \emph{Taxonomy} database.
    \item \code{\link{createTaxonDB}}: Install NCBI's \emph{Taxonomy}
    database locally.
    \item \code{\link{taxonDB}}: Retrieve records from a local install of
    the \emph{Taxonomy} database.
    \item \code{\link{taxonByGeneID}}: Retrieve records from a local install of
    the \emph{Taxonomy} database via NCBI GI numbers.
    \item \code{\link{pubmed}}: Retrieve records from the \emph{PubMed}
    database.
    \item \code{\link{protein}}: Retrieve records from the \emph{protein}
    database.
    \item \code{\link{nucleotide}}: Retrieve records from the
    \emph{nuccore} database.
    \item \code{\link{GSS}}: Retrieve records from the
    \emph{nucgss} database.   
    \item \code{\link{EST}}: Retrieve records from the
    \emph{nucest} database.
  }
}

\section{Main classes}{


 \itemize{
   \item \code{\linkS4class{Taxon}}: A container for data retrieved from the
   NCBI Taxonomy database.
   \item \code{\linkS4class{TaxonList}}: A list of Taxa.
   \item \code{\linkS4class{taxonDBConnection}}: Connection to a local Taxonomy
   database.
 }
}

\section{Package options}{


\emph{ncbi} uses the following \code{\link{options}} to configure behaviour:

\itemize{
  \item \code{ncbi.taxonomy.path}: Path to a local installation of the NCBI
  taxonomy database and (optionally) a GI_to_TaxId database. Defaults to
  "$HOME/local/db/taxonomy/". You can override the default by setting this
  option in your .Rprofile file. Run\code{\link{createTaxonDB}} and
  \code{\link{createGeneidDB}} to install these databases.
}
}

\author{
Gerhard Schoefl \email{gschofl@yahoo.de}
}
\keyword{package}
