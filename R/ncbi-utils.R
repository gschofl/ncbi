#' @include utils.R
#' @importFrom reutils epost esearch efetch webenv querykey database content xvalue
#' @importFrom reutils ncbi_retrieval_type
#' @importFrom RCurl basicTextGatherer curlPerform
#' @importFrom assertthat is.dir is.readable
NULL

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

#' Bind a list of data frames
#' 
#' Bind a list of data.frames by row. Equivalent to \code{do.call( "rbind", x)},
#' but faster. This function performs \bold{no} checking whatsoever! Each
#' component of the list must be a \code{data.frame} with the same number of
#' columns and with all columns of equivalent class.
#' 
#' Do not use factors: they will be converted to their internal integer
#' representations.
#' 
#' @param x A list of data frames.
#' @return A data frame.
#' @keywords internal
rBind <- function (x) {
  n_col <- length(x[[1L]])
  col_classes <- vapply(x[[1L]], class, character(1L), USE.NAMES=FALSE)
  res <- .Call("ncbi_bind_list", x, n_col, col_classes, PACKAGE = "ncbi")
  attr(res, "row.names")  <- seq_len(length(res[[1L]]))
  res
}

with_localtime <- function (new, code) {
  old <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", new)
  on.exit(Sys.setlocale("LC_TIME", old))
  force(code)
}

#' Compare local and remote file timestamp and/or size
#' 
#' @param file Path to local file.
#' @param url URL of remote source.
#' @param time Compare the timestamps (default: \code{TRUE}).
#' @param size Compare the file sizes (default: \code{FALSE}).
#' @param .message Print a message
#' @return \code{TRUE} if the local file does not exist,
#' if the remote source is more recent than the local file,
#' or if the size of the remote source differs from the local file.
#' Otherwise return \code{FALSE}.
#' @keywords internal
file_compare <- function(file, url, time = TRUE, size = FALSE, .message = TRUE) {
  assert_that(is.string(url), is.string(file))
  if (!file.exists(file)) {
    return(TRUE)
  }
  file_info <- file.info(file)
  local_time <- file_info$ctime
  local_size <- file_info$size
  
  h <- basicTextGatherer()
  failed <- FALSE
  status <- tryCatch(curlPerform(url = url, followlocation = TRUE, filetime=TRUE,
                                 headerfunction = h$update, nobody = TRUE),
                     COULDNT_RESOLVE_HOST = function(x) failed <<- TRUE, 
                     error = function(x) failed <<- TRUE)
  if (failed) {
    stop("Failed to connect to remote source: ", sQuote(url))
  }
  with_localtime("C", {
    header <- usp(h$value(), "\r\n")
    remote_time <- grep("Last-Modified: ", header, value=TRUE)
    remote_time <- as.POSIXct(strptime(sub("Last-Modified: ", "", remote_time),
                                       format="%a, %d %b %Y %H:%M:%S", tz="GMT"))
    remote_size <- grep("Content-Length: ", header, value=TRUE)
    remote_size <- as.numeric(sub("Content-Length: ", "", remote_size))
  })
  if (time && remote_time < local_time) {
    if (.message) {
      message("Local file is more recent than the remote source.")
    }
    FALSE
  } else if (size && remote_size != local_size) {
    if (.message) {
      message("Local file differs in size from the remote source.")
    }
    FALSE
  } else {
    TRUE
  }
}


