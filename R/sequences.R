#' @include utils.R
NULL


#' @autoImports
ncbi_sequences <- function (term, db, rettype = "fasta", retmax = 25,
                            parse = TRUE, ...) {
  
  rettype <- switch(db,
                    protein=match.arg(rettype, c("fasta", "gp", "gpc", "ft",
                                                 "seqid", "acc", "native")),
                    nuccore=match.arg(rettype, c("fasta", "gb", "gbc", "ft",
                                                 "seqid", "acc", "native")),
                    nucgss=match.arg(rettype, c("fasta", "gb", "gbc", "ft",
                                                "seqid", "acc", "native", "gss")),
                    nucest=match.arg(rettype, c("fasta", "gb", "gbc", "ft",
                                                "seqid", "acc", "native", "est")))
  
  args <- c(list(...), list(rettype = rettype, retmax = retmax))
  args <- c(args, list(id = getGI(term=term, db=db)))
  if (is.null(args[["retmode"]]))
    args <- c(args, list(retmode = switch(rettype, fasta = "xml", gp = "text",
                                          gpc = "xml", gb = "text", gbc = "text",
                                          ft = "text", seqid = "text", acc = "text",
                                          native = "xml", gss = "text", est = "text")))
            
  if (count(args$id) > 500 && args$retmax > 500) {
    response <- do.call(efetch.batch, args)
  } else {
    response <- do.call(efetch, args)
  }
  
  response <- content(response)
  if (parse) {
    switch(rettype,
           fasta = parseSeqSet(response),
           gp = gbRecord(textConnection(response, open="r")),
           response)
  } else {
    response
  }
}


#' @export
#' @autoImports
parseSeqSet <- function(seqSet) {
  if (!is(seqSet, "XMLInternalDocument"))
    return(seqSet)
  
  seqSet <- getNodeSet(xmlRoot(seqSet), '//TSeqSet/TSeq')
  
  if (is_empty(seqSet)) {
    stop("No 'TSeqSet' provided")
  }
  
  seqs <- lapply(seqSet, function (seq) {
#     seq <- xmlRoot(xmlDoc(seqSet[[1]]))
    seq <- xmlRoot(xmlDoc(seq))
    seqtype <- xmlGetAttr(seq[["TSeq_seqtype"]], name="value")
    gi <- xmlValue(seq[["TSeq_gi"]])
    acc <- xmlValue(seq[["TSeq_accver"]])
    taxid <- xmlValue(seq[["TSeq_taxid"]])
    orgname <- xmlValue(seq[["TSeq_orgname"]])
    defline <- xmlValue(seq[["TSeq_defline"]])
    sequence <- switch(seqtype,
                       protein=AAStringSet(xmlValue(seq[["TSeq_sequence"]])),
                       nucleotide=DNAStringSet(xmlValue(seq[["TSeq_sequence"]])))
    names(sequence) <- paste(acc, defline)
    elementMetadata(sequence) <- DataFrame(gi = gi, acc = acc,
                                           taxid = taxid, orgname = orgname,
                                           defline = defline)
    sequence
  })
  
  if (length(seqs) == 1) {
    return( seqs[[1]] )
  } else if (length(unique(vapply(seqs, class, character(1)))) == 1) {
    return( do.call(c, seqs) )
  } else {
    return( seqs )
  }
}


#' Retrieve sequences and annotations from the Protein database
#'
#' @details
#' See the documentation at
#' \href{http://www.ncbi.nlm.nih.gov/books/NBK44863/}{NCBI}
#' for more information.
#' 
#' @usage protein(term, rettype = "fasta", retmax = 25, parse = TRUE, ...)
#' 
#' @param term A valid NCBI search term or \sQuote{GI} numbers.
#' @param rettype
#' @param retmax
#' @param parse
#' @param ... Parameters passed on to the underlying \code{\link{esearch}}
#' query.
#'
#' @return A \linkS4class{gbRecord} or an \linkS4class{XStringSet} instance.
#' @rdname protein
#' @export
#' @importFrom rmisc Curry
protein <- Curry(ncbi_sequences, db="protein")


#' Retrieve sequences and annotations from the Nucleotide database
#'
#' @details
#' See the documentation at
#' \href{http://www.ncbi.nlm.nih.gov/books/NBK44863/}{NCBI}
#' for more information.
#' 
#' @usage nucleotide(term, rettype = "fasta", retmax = 25, parse = TRUE, ...)
#' 
#' @param term A valid NCBI search term or \sQuote{GI} numbers.
#' @param rettype
#' @param retmax
#' @param parse
#' @param ... Parameters passed on to the underlying \code{\link{esearch}}
#' query.
#'
#' @return A \linkS4class{gbRecord} or an \linkS4class{XStringSet} instance.
#' @rdname nucleotide
#' @export
nucleotide <- Curry(ncbi_sequences, db="nuccore")


#' Retrieve sequences and annotations from the GSS database
#'
#' @details
#' See the documentation at
#' \href{http://www.ncbi.nlm.nih.gov/books/NBK44863/}{NCBI}
#' for more information.
#' 
#' @usage GSS(term, rettype = "fasta", retmax = 25, parse = TRUE, ...)
#' 
#' @param term A valid NCBI search term or \sQuote{GI} numbers.
#' @param rettype
#' @param retmax
#' @param parse
#' @param ... Parameters passed on to the underlying \code{\link{esearch}}
#' query.
#'
#' @return A \linkS4class{gbRecord} or an \linkS4class{XStringSet} instance.
#' @rdname GSS
#' @export
GSS <- Curry(ncbi_sequences, db="nucgss")


#' Retrieve sequences and annotations from the EST database
#'
#' @details
#' See the documentation at
#' \href{http://www.ncbi.nlm.nih.gov/books/NBK44863/}{NCBI}
#' for more information.
#' 
#' @usage GSS(term, rettype = "fasta", retmax = 25, parse = TRUE, ...)
#' 
#' @param term A valid NCBI search term or \sQuote{GI} numbers.
#' @param rettype
#' @param retmax
#' @param parse
#' @param ... Parameters passed on to the underlying \code{\link{esearch}}
#' query.
#'
#' @return A \linkS4class{gbRecord} or an \linkS4class{XStringSet} instance.
#' @rdname EST
#' @export
EST <- Curry(ncbi_sequences, db="nucest")




