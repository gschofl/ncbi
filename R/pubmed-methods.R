setMethod("getPmid", "pubmed", function(x) x@pmid)

setMethod("getDoi", "pubmed", function(x) x@doi)

setMethod("getCites", "pubmed", function(x) x@cites)

setMethod("getAuthor", "pubmed", function(x) x@ref$author)

setMethod("getAbstract", "pubmed", function(x) x@ref$abstract)

setMethod("getTitle", "pubmed", function(x) x@ref$title)

setMethod("getIssue", "pubmed", function(x) {
  Map(function(v, n, y, m, p) {
    c(volume=v, number=n, year=y, month=m, pages=p)
  }, x@ref$volume, x@ref$number, x@ref$year, x@ref$month, x@ref$pages)
})

setMethod("getJournal", "pubmed", function(x) {
  Map(function(j, a, i) {
    attr(j, "abbrev") <- a
    attr(j, "issn") <- i
    j
  }, j=x@ref$journal, a=x@ref$abbrev, i=x@ref$issn)
})

setMethod("browsePubmed", "doi", function(x, browser=getOption("browser")) {
  if (all(is.na(x@doi))) {
    return("No doi available")
  }
  x <- initialize(x, doi=Filter(!is.na, x@doi))
  l <- Map(function(d) {
    browseURL(paste0('http://dx.doi.org/', d), browser=browser)
  }, d=x@doi)
  
  invisible(NULL)
})


setMethod("browsePubmed", "pubmed", function(x, browser=getOption("browser")) {
  browsePubmed(getDoi(x), browser=browser)
})


setMethod("c", "doi",  function(x, ..., recursive=FALSE) {
  args <- list(...)
  initialize(x, doi=c(x@doi, vapply(args, function(x) x@doi, "")))
})


setMethod("c", "pubmed", function(x, ..., recursive=FALSE) {
  args <- list(...)
  pmid <- c(getPmid(x), vapply(args, getPmid, ""))
  doi <- do.call(c, c(list(getDoi(x)), lapply(args, getDoi)))
  cites <- c(getCites(x), lapply(args, function(x) unlist(x@cites)))
  date <- c(x@date, sapply(args, function(x) x@date))
  ref <- do.call(c, c(list(x@ref), lapply(args, function(x) x@ref)))
  new_pubmed(pmid=pmid, doi=doi, cites=cites, date=date, ref=ref)
})

setMethod("show", "pubmed",
          function(object) {
            lo <- length(object@ref)
            cat(sprintf("A %s instance of length %s", sQuote(class(object)), lo))
            showme <- sprintf("\n[%s] Pmid: %s\tReferences: %s\n%s\n", seq_len(lo),
                              getPmid(object), vapply(getCites(object), length, numeric(1)),
                              dup("-", getOption("width") - 8))
            for(i in seq_len(lo)) {
              cat(showme[i])
              print(object@ref[[i]])
            }
          })

setMethod("show", "doi",
          function(object) {
            lo <- length(object@doi)
            showme <- sprintf("[%s] doi:%s\n", seq_len(lo), object@doi)
            showme[1] <- paste0(" ", showme[1])
            cat(showme)
          })


setMethod("[", "pubmed",
          function(x, i, j, ..., drop=TRUE) {
            initialize(x, pmid=getPmid(x)[i], doi=getDoi(x)[i],
                       cites=getCites(x)[i], date=x@date[i], ref=x@ref[i])
          })


setMethod("[", "doi",
          function(x, i, j, ..., drop=TRUE) {
            initialize(x, doi=x@doi[i])
          })











