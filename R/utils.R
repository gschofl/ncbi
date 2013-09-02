#' @importFrom Rentrez epost esearch efetch efetch.batch count set_record_type
NULL

getID <- function (id, db) {
  if (has_webenv(id)) {
    id
  }
  else if (is.numeric(id) || !any(is.na(suppressWarnings(as.numeric(id))))) {
    epost(id, db)
  }
  else {
    esearch(id, db, usehistory=TRUE)
  }
}


getArgs <- function (id, db, rettype, retmax, ...) {
  args <- list(..., retmax = retmax)
  rtype <- set_record_type(db, rettype, args$retmode)
  args$retmode <- NULL
  args <- c(list(id = getID(id, db)), rtype, args)
  args
}
  

#' @importFrom Rentrez content
fetch_records <- function (args, maxrec = 500) {
  if (count(args$id) > maxrec && args$retmax %||% Inf > maxrec) {
    response <- do.call("efetch.batch", c(args, list(chunk_size = 500)))
  }
  else {
    response <- do.call("efetch", args)
  }
  content(response)
}


#' @importFrom rmisc xvalue
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
    stop("ERROR in efetch: ", paste(e, collapse=", "), call.=FALSE)
  }
  invisible(TRUE)  
}

#' @importFrom Rentrez webEnv queryKey
has_webenv <- function (x) {
  is(x, "eutil") && !is.na(webEnv(x)) && !is.na(queryKey(x))
}


ellipsize <- function(obj, width = getOption("width"), ellipsis = "...") {
  str <- encodeString(obj)
  ifelse(nchar(str) > width - 1,
         paste0(substring(str, 1, width - nchar(ellipsis) - 1), ellipsis),
         str)
}

## vectorised %|na|%
#' @importFrom rmisc Partial "%|%"
"%|NA|%" <- Partial(`%|%`, filter = "is.na")

