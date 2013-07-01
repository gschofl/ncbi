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


#' @importFrom assertthat assert_that is.string
listclassConstructor <- function (listClass, elemClass) {
  assert_that(is.string(listClass), is.string(elemClass))
  function (...) {
    listData <- list(...)
    if (length(listData) == 0L) {
      new(listClass, list(new(elemClass)))
    }
    else {
      if (length(listData) == 1L && is.list(listData[[1L]])) 
        listData <- listData[[1L]]
      if (!all(vapply(listData, is, elemClass, FUN.VALUE=logical(1L)))) 
        stop("All elements in '...' must be '", elemClass,"' objects")
      
      new(listClass, .Data = listData)
    }
  }
}


listclassValidator <- function (listClass, elemClass) {
  assert_that(is.string(listClass), is.string(elemClass))
  function (object) {
    errors <- character()
    elem_of_class <- vapply(S3Part(object, strictS3=TRUE), is, elemClass, FUN.VALUE=logical(1L))
    if (!all(elem_of_class)) {
      msg <- paste0("All elements in a '", listClass ,"' must be of class '",
                    elemClass, "'.")
      errors <- c(errors, msg)
    }
    
    if (length(errors) == 0L) TRUE else errors
  }
}

# elementShowFun <- .show_Lineage
#' @importFrom assertthat has_args
listclassShower <- function (elementShowFun, numOfElements, linesPerElement = NULL) {
  assert_that(is.function(elementShowFun), has_args(elementShowFun, c('x', 'width', 'ellipsis')))
  assert_that(is.numeric(numOfElements), length(numOfElements) == 1)
  
  .show_ListElements <- function(index, elems, lPerEl = NULL, ellipsis = ' ... ' ) {
    index_string <- paste0('[[', index, ']] ')
    if (is.null(lPerEl)) {
      width <- list(Inf)
    }
    else {
      indent <- nchar(index_string) + nchar(ellipsis) + 2L*lPerEl + 1L
      width <- lPerEl*getOption("width") - indent
    }
    object_string <- unlist(Map(elementShowFun, x=elems, width=width, ellipsis=list(ellipsis)))
    sprintf("%s%s", index_string, linebreak(object_string, indent = -nchar(index_string),
                                            offset=1L, FORCE=TRUE))
  }
  
  ## nOfEl: how many elements do we show as head and tail.
  ## lPerEl: how many lines to we show per element before we ellipsize.
  function (x, nOfEl = numOfElements, lPerEl = linesPerElement) {
    listLength <- length(x)
    cat(sprintf("A %s instance of length %s\n",
                sQuote(class(x)), listLength), sep="")
    
    if (listLength > 2*nOfEl) {
      head_index <- seq_len(nOfEl)
      head <- x[head_index]
      showHead <- .show_ListElements(head_index, head, lPerEl)
      tail_index <- seq.int(to=listLength, length.out = min(nOfEl, listLength))
      tail <- x[tail_index]
      showTail <- .show_ListElements(tail_index, tail, lPerEl)
      showme <- c(showHead, '...', showTail)
    }
    else {
      showme <- .show_ListElements(seq_along(x), x, lPerEl)
    }
    cat(showme, sep="\n")
  }
}

