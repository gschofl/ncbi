#' @autoImports
getGI <- function(term, db) {
  if (is.numeric(term) || !any(is.na(suppressWarnings(as.numeric(term))))) {
    epost(id=term, db=db)
  } else {
    esearch(term=term, db=db, usehistory=TRUE)
  }
}


