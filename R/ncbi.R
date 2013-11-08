#' Retrieve and parse data from NCBI databases.
#'
#' @description
#' This package provides an R interface to retrieve data from the following NCBI
#' databases: \emph{nucleotide}, \emph{protein}, \emph{pubmed}, \emph{taxonomy},
#' \emph{nucgss}, and \emph{nucest}.
#' 
#' @section Important functions:
#'
#'   \itemize{
#'     \item \code{\link{taxon}}: Retrieve taxonomy records remotely from NCBI's
#'     \emph{Taxonomy} database.
#'     \item \code{\link{createTaxonDB}}: Install NCBI's \emph{Taxonomy}
#'     database locally.
#'     \item \code{\link{taxonDB}}: Retrieve records from a local install of
#'     the \emph{Taxonomy} database.
#'     \item \code{\link{taxonByGeneID}}: Retrieve records from a local install of
#'     the \emph{Taxonomy} database via NCBI GI numbers.
#'     \item \code{\link{pubmed}}: Retrieve records from the \emph{PubMed}
#'     database.
#'     \item \code{\link{protein}}: Retrieve records from the \emph{protein}
#'     database.
#'     \item \code{\link{nucleotide}}: Retrieve records from the
#'     \emph{nuccore} database.
#'     \item \code{\link{GSS}}: Retrieve records from the
#'     \emph{nucgss} database.   
#'     \item \code{\link{EST}}: Retrieve records from the
#'     \emph{nucest} database.
#'   }
#'   
#' @section Main classes:
#' 
#'  \itemize{
#'    \item \code{\linkS4class{Taxon}}: A container for data retrieved from the
#'    NCBI Taxonomy database.
#'    \item \code{\linkS4class{TaxonList}}: A list of Taxa.
#'    \item \code{\linkS4class{taxonDBConnection}}: Connection to a local Taxonomy
#'    database.
#'  }
#'   
#' @author Gerhard Schoefl \email{gschofl@@yahoo.de}
#' @useDynLib ncbi
#' @docType package
#' @name ncbi
#' @aliases ncbi ncbi-package
#' @keywords package
NULL
