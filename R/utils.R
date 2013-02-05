#' @autoImports
getID <- function(id, db) {
  if (has_webenv(id)) {
    return( id )
  } else if (is.numeric(id) || !any(is.na(suppressWarnings(as.numeric(id))))) {
    return( epost(id, db) )
  } else {
    return( esearch(id, db, usehistory=TRUE) )
  }
}


#' @autoImports
getArgs <- function(id, db, rettype, retmax, ...) {
  args <- list(..., retmax = retmax)
  rtype <- set_record_type(db, rettype, args$retmode)
  args$retmode <- NULL
  args <- c(list(id = getID(id, db)), rtype, args)
  args
}
  

#' @importFrom rentrez efetch
#' @importFrom rentrez efetch.batch
#' @autoImports
fetch_records <- function(args, maxrec = 500) {
  if (count(args$id) > maxrec && args$retmax %||% Inf > maxrec) {
    response <- do.call(efetch.batch, c(args, list(chunk_size = 500)))
  } else {
    response <- do.call(efetch, args)
  }
  response <- content(response)
  response
}


#' @autoImports
catchEFetchError <- function (response) {
  if (is(response, "efetch")) {
    response <- content(response)
  }
  if (!is(response, "XMLInternalDocument")) {
    warning("No XML response", call.=FALSE, immediate.=TRUE)
    return(invisible(TRUE))
  }
  e <- xvalue(response, '//ERROR')
  if (not.na(e)) {
    stop("ERROR in efetch: ", paste(e, collapse=", "), call.=FALSE)  
  }
  invisible(TRUE)  
}


set_type <- function(x, as) {
  switch(as,
         character=as.character(x),
         numeric=as.numeric(x),
         integer=as.integer(x),
         double=as.double(x),
         logical=as.logical(x),
         complex=as.complex(x),
         x)
}


xvalue <- function(xdoc, path, alt = NA_character_, as = 'character') {
  v <- xpathSApply(xdoc, path, xmlValue) %||% alt
  set_type(v, as)
}


xname <- function(xdoc, path, alt = NA_character_, as = 'character') {
  n <- xpathSApply(xdoc, path, xmlName) %||% alt
  set_type(n, as)
}


xattr <- function(xdoc, path, name, alt = NA_character_, as = 'character') {
  a <- xpathSApply(xdoc, path, xmlGetAttr, name=name) %||% alt
  set_type(a, as)
}


#' @autoImports
has_webenv <- function (x) {
  if (is(x, "eutil") && not.na(webEnv(x)) && not.na(queryKey(x)))
    TRUE
  else
    FALSE
}


