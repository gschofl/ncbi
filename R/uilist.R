#' @include utils.R
NULL

#' @keywords internal
#' @export
#' @importFrom rmisc usp
parseUilist <- function (uilist) {
  if (is(uilist, "efetch")) {
    uilist <- content(uilist)
  }
  if (!is(uilist, "XMLInternalDocument")) {
    if (grepl("(\\d+\\n)+", uilist)) {
      return(as.numeric(usp(uilist, "\\n")))
    }
    else {
      return(uilist)
    }
  }
  id <- xvalue(uilist, '//Id')
  if (all_empty(id)) {
    e <- xvalue(response, '//ERROR')
    if (!is.null(e)) {
      stop("ERROR in efetch: ", paste(e, collapse=", "))
    }
    else {
      stop("No 'uiliist' provided")
    }
  }
  
  return(as.numeric(id))
}


#' @keywords internal
#' @export
#' @autoImports
parseAcc <- function (acc) {
  if (is(acc, "efetch")) {
    acc <- content(acc)
  }
  if (!is(acc, "XMLInternalDocument")) {
    ACC <- "([a-zA-Z][a-zA-Z0-9_]*(\\.[a-zA-Z0-9]+)?)"
    if (grepl(paste0("(", ACC, "\\n)+"), acc)) {
      return(as.character(usp(acc, "\\n")))
    }
    else {
      return(acc)
    }
  }
  else {
    return(acc)
  }
}

