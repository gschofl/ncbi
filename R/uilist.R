#' @export
#' @autoImports
parseUilist <- function (uilist) {
  
  if (is(uilist, "efetch")) {
    uilist <- content(uilist)
  }
  
  if (!is(uilist, "XMLInternalDocument")) {
    if (grepl("(\\d+\\n)+", uilist)) {
      return(as.numeric(strsplit(uilist, "\\n")[[1]]))
    } else {
      return(uilist)
    }
  }

  id <- xpathSApply(uilist, "//Id", xmlValue)
  if (is_empty(id)) {
    e <- unlist(xpathApply(response, "//ERROR", xmlValue))
    if (not.null(e)) {
      stop("ERROR in efetch: ", paste(e, collapse=", "))
    } else {
      stop("No 'uiliist' provided")
    }
  }
  
  return(as.numeric(id))
}


#' @export
#' @autoImports
parseAcc <- function (acc) {
  
  if (is(acc, "efetch")) {
    acc <- content(acc)
  }
  
  if (!is(acc, "XMLInternalDocument")) {
    ACC <- "([a-zA-Z][a-zA-Z0-9_]*(\\.[a-zA-Z0-9]+)?)"
    if (grepl(paste0("(", ACC, "\\n)+"), acc)) {
      return(as.character(strsplit(acc, "\\n")[[1]]))
    } else {
      return(acc)
    }
  } else {
    return(acc)
  }
  
}




