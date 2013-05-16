#' @include utils.R
NULL

#' @autoImports
ncbi_sequences <- function (gi, db, rettype = "fasta", retmax = 100,
                            parse = TRUE, ...) {
  
  if (is(gi, "esearch")) {
    if (database(gi) %ni% c('nuccore', 'nucest','nucgss', 'protein', 'popset',
                            'nucleotide'))
      stop("Database ", sQuote(database(gi)), " not supported")
    if (!has_webenv(gi))
      gi <- idList(gi)
  }
  
  args <- getArgs(gi, db, rettype, retmax, ...)
  response <- fetch_records(args, 500)
  if (parse) {
    switch(args$rettype %|null|% "asn.1",
           fasta = parseTSeqSet(response),
           gb = parseGenBank(response),
           gp = parseGenBank(response),
           gbwithparts = parseGenBank(response),
           acc = parseAcc(response),
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
#' @autoImports
parseGenBank <- function(gb) {
  
  ## see if gb is a valid file path
  if (tryCatch(file.exists(gb), error=function(e) FALSE)) {
    return( gbRecord(gb) )
  ## or if it's an efetch instance
  } else if (is(gb, "efetch")) {
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
#' @autoImports
parseTSeqSet <- function(tSeqSet = response) {
  
  if (is(tSeqSet, "efetch")) {
    tSeqSet <- content(tSeqSet)
  }
  catchEFetchError(tSeqSet)
  if (!is(tSeqSet, "XMLInternalDocument")) {
    return(tSeqSet)
  }
  tSeqSet <- getNodeSet(xmlRoot(tSeqSet), '//TSeqSet/TSeq')
  if (all_empty(tSeqSet)) {
    stop("No 'tSeqSet' provided")
  }
  
  seqs <- base::lapply(tSeqSet, function (seq) {
    # seq <- xmlRoot(xmlDoc(tSeqSet[[1]]))
    seq <- xmlRoot(xmlDoc(seq))
    seqtype <- xattr(seq, '//TSeq_seqtype', "value")
    gi <- xvalue(seq, '//TSeq_gi') # optional
    accver <- xvalue(seq, '//TSeq_accver') # optional
    sid <- xvalue(seq, '//TSeq_sid') # optional
    local <- xvalue(seq, '//TSeq_local') # optional
    taxid <- xvalue(seq, '//TSeq_taxid') # optional
    orgname <- xvalue(seq, '//TSeq_orgname') # optional
    defline <- xvalue(seq, '//TSeq_defline')
    length <- xvalue(seq, '//TSeq_length', as="numeric")
    sequence <- switch(seqtype,
                       protein=AAStringSet(xvalue(seq, '//TSeq_sequence')),
                       nucleotide=DNAStringSet(xvalue(seq, '//TSeq_sequence')))
    names(sequence) <- base::paste(accver, defline)
    elementMetadata(sequence) <- DataFrame(gi = gi, accver = accver, sid = sid,
                                           local = local, taxid = taxid,
                                           orgname = orgname, defline = defline,
                                           length = length)
    sequence
  })
  
  if (length(seqs) == 1) {
    return( seqs[[1]] )
  } else if (length(base::unique(vapply(seqs, class, character(1)))) == 1) {
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
#' @usage protein(gi, rettype = "fasta", retmax = 25, parse = TRUE, ...)
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
#' @importFrom rmisc Curry
protein <- Curry("ncbi_sequences", db="protein")


#' Retrieve sequences and annotations from the Nucleotide database
#'
#' @details
#' See the documentation at
#' \href{http://www.ncbi.nlm.nih.gov/books/NBK44863/}{NCBI}
#' for more information.
#' 
#' @usage nucleotide(gi, rettype = "fasta", retmax = 25, parse = TRUE, ...)
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
nucleotide <- Curry("ncbi_sequences", db="nuccore")


#' Retrieve sequences and annotations from the GSS database
#'
#' @details
#' See the documentation at
#' \href{http://www.ncbi.nlm.nih.gov/books/NBK44863/}{NCBI}
#' for more information.
#' 
#' @usage GSS(gi, rettype = "fasta", retmax = 25, parse = TRUE, ...)
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
GSS <- Curry("ncbi_sequences", db="nucgss")


#' Retrieve sequences and annotations from the EST database
#'
#' @details
#' See the documentation at
#' \href{http://www.ncbi.nlm.nih.gov/books/NBK44863/}{NCBI}
#' for more information.
#' 
#' @usage EST(gi, rettype = "fasta", retmax = 25, parse = TRUE, ...)
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
EST <- Curry("ncbi_sequences", db="nucest")


