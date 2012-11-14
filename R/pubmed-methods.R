setMethod("doi", "bibentry", function(x) {
  return( x$doi )
})


setMethod("pmid", "bibentry", function(x) {
  return( x$pmid )
})


setMethod("author", "bibentry", function(x) {
  return( x$author )
})


setMethod("abstract", "bibentry", function(x) {
  return( x$abstract )
})


setMethod("title", "bibentry", function(x) {
  return( x$title )
})


setMethod("issue", "bibentry", function(x) {
  Map(function (v, n, y, m, p) {
    c(volume = v, number = n, year = y, month = m, pages = p)
  }, x$volume, x$number, x$year, x$month, x$pages)
})


setMethod("journal", "bibentry", function(x) {
  Map(function (j, a, i) {
    attr(j, "abbrev") <- a
    attr(j, "issn") <- i
    j
  }, j=x$journal, a=x$abbrev, i=x$issn)
})


setMethod("browsePubmed", "bibentry", function(x) {
  if (all(!nzchar(doi(x)))) {
    return("No doi available")
  }
  l <- lapply(Filter(nzchar, doi(x)), function (doi) {
    browseURL(paste0('http://dx.doi.org/', doi), browser = browser)
  })
  invisible(NULL)
})

