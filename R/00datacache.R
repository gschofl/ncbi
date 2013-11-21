#' Register a caching environment
#' 
#' Generate a \code{cache} object, with \code{set}, \code{get},
#' \code{exists}, \code{ls}, and \code{rm} methods.
#' 
#' @keywords internal
#' @examples
#' cache <- new_cache()
#' cache$set("a", 1:10)
#' cache$set("b", list(1,2,3,4))
#' cache$ls()
#' cache$get("a")
#' cache$exists("b")
#' cache$exists("c")
#' cache$rm()     
new_cache <- function() {
  cache <- NULL
  set_val <- function (key, value) {
    assert_that(is.string(key))
    assign(key, value, cache)
  }
  get_val <- function (key) {
    assert_that(is.string(key))
    get(key, cache, inherits = FALSE)
  }
  key_exists <- function (key) {
    assert_that(is.string(key))
    exists(key, cache, inherits = FALSE)
  }
  .list <- function () {
    ls(cache)
  }
  .remove <- function () {
    cache <<- new.env(hash = TRUE, emptyenv())
  }
  .remove()
  structure(
    list(
      set = set_val, 
      get = get_val,
      exists = key_exists,
      ls = .list,
      rm = .remove
    ),
    class = "cache"
  )
}

#' @S3method print cache
print.cache <- function (object) {
  lo <- length(object$ls())
  showme <- sprintf("A datacache containing %s object%s.",
                    lo, if (lo == 1) "" else "s")
  cat(showme, sep='\n')
}

## register environment for caching taxon nodes
.taxcache <- new_cache()

## valid taxonomic rank designations used by NCBI
.ranks <- c("root","superkingdom","kingdom","subkingdom","superphylum",
            "phylum","subphylum","superclass","class","subclass","infraclass",
            "superorder","order","suborder","parvorder","infraorder",
            "superfamily","family","subfamily","tribe","subtribe","genus",
            "subgenus","species group","species subgroup","species","subspecies",
            "varietas","forma","no rank")

#' Return the taxonomic rank designations used by NCBI
#' 
#' @keywords internal
#' @export
.ncbi_taxon_ranks <- function() {
  .ranks
}

## register SQL table layout
geneid_db.sql <- '
CREATE TABLE genes (
  tax_id     INT UNSIGNED NOT NULL DEFAULT 0
);'

taxon_db.sql <- '
CREATE TABLE nodes (
tax_id        CHAR(10) NOT NULL UNIQUE,
parent_id     CHAR(10) NOT NULL,
rank          VARCHAR(50) DEFAULT NULL,
embl_code     CHAR(2) DEFAULT NULL,              
division_id   CHAR(2) NOT NULL,
PRIMARY KEY (tax_id)
);

CREATE TABLE names (
tax_id        CHAR(10) NOT NULL,
tax_name      VARCHAR(200) NOT NULL,
unique_name   VARCHAR(100) DEFAULT NULL,
class         VARCHAR(50) NOT NULL DEFAULT \'\',
FOREIGN KEY (tax_id) REFERENCES nodes (tax_id)
);

CREATE INDEX Fnodes ON nodes (tax_id);
CREATE INDEX Dnames on names (tax_id);
'
