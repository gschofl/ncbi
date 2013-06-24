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
  

#' @importFrom Rentrez efetch efetch.batch count
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
  if (!is.na(e)) {
    stop("ERROR in efetch: ", base::paste(e, collapse=", "), call.=FALSE)  
  }
  invisible(TRUE)  
}

#' @autoImports
has_webenv <- function (x) {
  is(x, "eutil") && !is.na(webEnv(x)) && !is.na(queryKey(x))
}


#' @autoImports
ellipsize <- function(obj, width = getOption("width"), ellipsis = "...") {
  str <- encodeString(obj)
  base::ifelse(base::nchar(str) > width - 1,
         paste0(base::substring(str, 1, width - base::nchar(ellipsis) - 1), ellipsis),
         str)
}

## vectorised %|na|%
#' @autoImports
"%|NA|%" <- Curry(`%|%`, filter = "is.na")
