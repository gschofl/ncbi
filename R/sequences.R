#' @include utils.R
NULL
#' @importFrom XML xmlDoc free
NULL
#' @importFrom Biostrings AAStringSet DNAStringSet
NULL
#' @importFrom IRanges DataFrame "elementMetadata<-"
NULL


ncbi_sequences <- function(gi, db, rettype="fasta", retmax=100, parse=TRUE, ...) {
  .dbs <- c('nuccore', 'nucest','nucgss', 'protein', 'popset', 'nucleotide')
  if (is(gi, "esearch") && database(gi) %ni% .dbs) {
    stop("Database ", sQuote(database(gi)), " is not yet supported")
  }
  args <- get_args(gi, db, rettype, retmax, ...)
  response <- content(do.call("efetch", args))
  if (parse) {
    switch(args$rettype %||% "asn.1",
           fasta=parseTSeqSet(response),
           gb=parseGenBank(response),
           gp=parseGenBank(response),
           gbwithparts=parseGenBank(response),
           acc=parseAcc(response),
           response)
  } else {
    response
  }
}


#' Parse GenBank flatfiles
#' 
#' @param gb An \linkS4class{efetch} instance, a character vector
#' containing a GenBank flatfile, or a file path to a GenBank file.
#' 
#' @return A \linkS4class{gbRecord} instance.
#'
#' @export
parseGenBank <- function(gb) {
  stopifnot(require(biofiles))
  ## see if gb is a valid file path
  if (tryCatch(file.exists(gb), error=function(e) FALSE)) {
    return(gbRecord(gb))
  }
  ## or if it's an efetch instance
  else if (is(gb, "efetch")) {
    gb <- content(gb)
  }
  ## don't try to parse XML
  if (is(gb, "XMLInternalDocument")) {
    return(gb)
  }
  
  gbRecord(textConnection(gb, open="r"))
}

#' Parse Tiny Bioseq FASTA files
#' 
#' @param tSeqSet An \linkS4class{efetch} or \linkS4class{XMLInternalDocument}
#' instance contining TSeqSet XML file.
#' 
#' @return An \linkS4class{XStringSet} instance with additional innformation
#' in the \code{elementMetadata} slot.
#'
#' @export
parseTSeqSet <- function(tSeqSet) {
  if (is(tSeqSet, "efetch")) {
    tSeqSet <- content(tSeqSet)
  }
  catchEFetchError(tSeqSet)
  if (!is(tSeqSet, "XMLInternalDocument")) {
    return(tSeqSet)
  }
  tSeqSet <- xset(tSeqSet, '/TSeqSet/TSeq')
  if (length(tSeqSet) == 0) {
    stop("No 'tSeqSet' provided")
  }
  
  seqs <- lapply(tSeqSet, function(seq) {
    seq       <- xmlDoc(seq)
    seqtype   <- xattr(seq, '/TSeq/TSeq_seqtype', 'value')
    gi        <- xvalue(seq, '/TSeq/TSeq_gi') # optional
    accver    <- xvalue(seq, '/TSeq/TSeq_accver') # optional
    sid       <- xvalue(seq, '/TSeq/TSeq_sid') # optional
    local     <- xvalue(seq, '/TSeq/TSeq_local') # optional
    taxid     <- xvalue(seq, '/TSeq/TSeq_taxid') # optional
    orgname   <- xvalue(seq, '/TSeq/TSeq_orgname') # optional
    defline   <- xvalue(seq, '/TSeq/TSeq_defline')
    length    <- xvalue(seq, '/TSeq/TSeq_length', as="numeric")
    sequence  <- switch(seqtype,
                        protein=AAStringSet(xvalue(seq, '/TSeq/TSeq_sequence')),
                        nucleotide=DNAStringSet(xvalue(seq, '/TSeq/TSeq_sequence')))
    ## construct a defline
    df_gi_db  <- ifelse(is.na(gi), '', 'gi|')
    df_gi     <- gi %|NA|% ''
    df_acc_db <- ifelse(is.na(accver), '|', '|gb|')
    df_acc    <- accver %|NA|% sid
    names(sequence) <- paste0(df_gi_db, df_gi, df_acc_db, df_acc, ' ', defline)
    elementMetadata(sequence) <- DataFrame(gi=gi, accver=accver, sid=sid,
                                           local=local, taxid=taxid,
                                           orgname=orgname, defline=defline,
                                           length=length)
    sequence
  })
  
  if (length(seqs) == 1) {
    return(seqs[[1]])
  } else if (length(unique(vapply(seqs, class, ""))) == 1) {
    return(do.call("c", seqs))
  } else {
    return(seqs)
  }
}


#' Retrieve sequences and annotations from the Protein database
#'
#' @details
#' See the documentation at
#' \href{http://www.ncbi.nlm.nih.gov/books/NBK44863/}{NCBI}
#' for more information.
#' 
#' @usage protein(gi, rettype="fasta", retmax=25, parse=TRUE, ...)
#' 
#' @param gi \sQuote{GI}s or a valid NCBI search term.
#' @param rettype Which type of data should be retrieved? \sQuote{fasta}
#' (default), \sQuote{gp}, \sQuote{acc}, \sQuote{seqid}, \sQuote{ft},
#' \sQuote{native}, or \code{NULL} (text ASN.1).
#' @param retmax Maximal number of records to be retrieved (default: 25).
#' @param parse Should the retrieved data be parsed?
#' @param ... Parameters passed on to the underlying \code{\link{efetch}}
#' query.
#'
#' @return A \linkS4class{gbRecord} or an \linkS4class{XStringSet} instance.
#' @rdname protein
#' @export
protein <- Partial(ncbi_sequences, db="protein")


#' Retrieve sequences and annotations from the Nucleotide database
#'
#' @details
#' See the documentation at
#' \href{http://www.ncbi.nlm.nih.gov/books/NBK44863/}{NCBI}
#' for more information.
#' 
#' @usage nucleotide(gi, rettype="fasta", retmax=25, parse=TRUE, ...)
#' 
#' @param gi \sQuote{GI}s or a valid NCBI search term.
#' @param rettype Which type of data should be retrieved? \sQuote{fasta}
#' (default), \sQuote{gb}, \sQuote{gbwithparts}, \sQuote{acc}, \sQuote{seqid},
#' \sQuote{ft}, \sQuote{fasta_cds_na}, \sQuote{fasta_cds_aa}, \sQuote{native},
#' or \code{NULL} (text ASN.1).
#' @param retmax Maximal number of records to be retrieved (default: 25).
#' @param parse Should the retrieved data be parsed?
#' @param ... Parameters passed on to the underlying \code{\link{efetch}}
#' query.
#'
#' @return A \linkS4class{gbRecord} or an \linkS4class{XStringSet} instance.
#' @rdname nucleotide
#' @export
nucleotide <- Partial(ncbi_sequences, db="nuccore")


#' Retrieve sequences and annotations from the GSS database
#'
#' @details
#' See the documentation at
#' \href{http://www.ncbi.nlm.nih.gov/books/NBK44863/}{NCBI}
#' for more information.
#' 
#' @usage GSS(gi, rettype="fasta", retmax=25, parse=TRUE, ...)
#' 
#' @param gi \sQuote{GI}s or a valid NCBI search term.
#' @param rettype Which type of data should be retrieved? \sQuote{fasta}
#' (default), \sQuote{gb}, \sQuote{acc}, \sQuote{seqid}, \sQuote{gss},
#' \sQuote{native}, or \code{NULL} (text ASN.1).
#' @param retmax Maximal number of records to be retrieved (default: 25).
#' @param parse Should the retrieved data be parsed?
#' @param ... Parameters passed on to the underlying \code{\link{efetch}}
#' query.
#'
#' @return A \linkS4class{gbRecord} or an \linkS4class{XStringSet} instance.
#' @rdname GSS
#' @export
GSS <- Partial(ncbi_sequences, db="nucgss")


#' Retrieve sequences and annotations from the EST database
#'
#' @details
#' See the documentation at
#' \href{http://www.ncbi.nlm.nih.gov/books/NBK44863/}{NCBI}
#' for more information.
#' 
#' @usage EST(gi, rettype="fasta", retmax=25, parse=TRUE, ...)
#' 
#' @param gi \sQuote{GI}s or a valid NCBI search term.
#' @param rettype Which type of data should be retrieved? \sQuote{fasta}
#' (default), \sQuote{gp}, \sQuote{acc}, \sQuote{seqid}, \sQuote{est},
#' \sQuote{native}, or \code{NULL} (text ASN.1).
#' @param retmax Maximal number of records to be retrieved (default: 25).
#' @param parse Should the retrieved data be parsed?
#' @param ... Parameters passed on to the underlying \code{\link{efetch}}
#' query.
#'
#' @return A \linkS4class{gbRecord} or an \linkS4class{XStringSet} instance.
#' @rdname EST
#' @export
EST <- Partial(ncbi_sequences, db="nucest")


