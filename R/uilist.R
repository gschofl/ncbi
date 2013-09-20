#' @include utils.R
NULL

#' @keywords internal
#' @export
parseUilist <- function(uilist) {
  if (is(uilist, "efetch")) {
    uilist <- content(uilist)
  }
  if (!is(uilist, "XMLInternalDocument")) {
    if (grepl("(\\d+\\n)+", uilist)) {
      return( as.numeric(unlist(strsplit(uilist, "\\n"))) )
    } else {
      return( uilist )
    }
  }
  id <- xvalue(uilist, '//Id')
  if (length(id) == 0) {
    e <- xvalue(response, '//ERROR')
    if (!is.null(e)) {
      stop("ERROR in efetch: ", paste(e, collapse=", "))
    } else {
      stop("No 'uiliist' provided")
    }
  }
  as.numeric(id)
}


#' @keywords internal
#' @export
parseAcc <- function(acc) {
  if (is(acc, "efetch")) {
    acc <- content(acc)
  }
  if (!is(acc, "XMLInternalDocument")) {
    ACC <- "([a-zA-Z][a-zA-Z0-9_]*(\\.[a-zA-Z0-9]+)?)"
    if (grepl(paste0("(", ACC, "\\n)+"), acc)) {
      as.character(unlist(strsplit(acc, "\\n")))
    } else {
      acc
    }
  } else {
    acc
  }
}

