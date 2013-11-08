#' @importFrom reutils epost esearch efetch webenv querykey database content xvalue
NULL
#' @importFrom assertthat assert_that "on_failure<-" is.string is.writeable is.dir
NULL
#' @importFrom rmisc rBind
NULL

Partial <- function(fn, ..., .env = parent.frame()) {
  assert_that(is.function(fn))
  fcall <- substitute(fn(...))
  if (!is.primitive(fn)) {
    fcall <- match.call(fn, fcall)
  }
  fcall[[length(fcall) + 1]] <- quote(...)
  args <- list("..." = quote(expr = ))
  
  eval(call("function", as.pairlist(args), fcall), .env)
}

is.empty <- function(x) {
  is.null(x) || length(x) == 0L || (length(x) == 1L && !nzchar(x))
}
on_failure(is.empty) <- function(call, env) {
  paste0(deparse(call$x), " is not empty.")
}

"%||%" <- function(a, b) {
  if (is.empty(a)) force(b) else a
}

"%|na|%" <- function(a, b) {
  if (is.null(a) || all(is.na(a))) force(b) else a
}

## Vectorized default operators
"%|%" <- function(a, b) ifelse(nzchar(a), a, b)

"%|NA|%" <- function(a, b) ifelse(is.na(a), b, a)

"%ni%" <- Negate(`%in%`)

compact <- function(x) {
  x[!vapply(x, is.empty, FALSE, USE.NAMES=FALSE)]
}

compactChar <- function(x) {
  x[vapply(x, nzchar, FALSE, USE.NAMES=FALSE)]
}

create_if_not_exists <- function(path, type="dir") {
  type <- match.arg(type, c("dir", "file"))
  assert_that(is.string(path))
  if (!file.exists(path)) {
    success <- tryCatch(
      switch(type, dir=dir.create(path), file=file.create(path)),
      warning = function(w) FALSE
    )
    return(success)
  }
  TRUE
}
on_failure(create_if_not_exists) <- function(call, env) {
  paste0("The file/directory ", deparse(call$path), " could not be created")
}

ncbi.taxonomy.path <- function(taxonomy.path = getOption("ncbi.taxonomy.path")) {
  msg <- paste0("No path to the taxonomy database provided. ",
                "Configure a path permanently by setting the",
                "option \"ncbi.taxonomy.path\" in your .Rprofile.")
  if (is.null(taxonomy.path)) {
    stop(msg, call.=FALSE)
  }
  if (is.null(getOption("ncbi.taxonomy.path"))) {
    options(ncbi.taxonomy.path = taxonomy.path)
  }
  assert_that(
    create_if_not_exists(taxonomy.path, "dir"),
    is.dir(taxonomy.path),
    is.readable(taxonomy.path)
  )
  invisible(taxonomy.path)
}

get_uids <- function(uid, db) {
  if (has_webenv(uid)) {
    uid
  } else if (is.numeric(uid) || !any(is.na(suppressWarnings(as.numeric(uid))))) {
    epost(uid, db)
  } else {
    esearch(uid, db, usehistory=TRUE)
  }
}


#' @importFrom reutils ncbi_retrieval_type
get_args <- function(uid, db, rettype, retmax, ...) {
  args <- list(..., retmax=retmax)
  rtype <- ncbi_retrieval_type(db, rettype, args$retmode)
  args$retmode <- NULL
  args <- c(list(uid=get_uids(uid, db)), rtype, args)
  args
}


catchEFetchError <- function(response) {
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


has_webenv <- function(x) {
  is(x, "eutil") && !is.na(webenv(x)) && !is.na(querykey(x))
}


ellipsize <- function(obj, width=getOption("width"), ellipsis="...") {
  str <- encodeString(obj)
  ifelse(nchar(str) > width - 1,
         paste0(substring(str, 1, width - nchar(ellipsis) - 1), ellipsis),
         str)
}

