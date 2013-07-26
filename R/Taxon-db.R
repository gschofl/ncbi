#' @importFrom rmisc db_create db_connect db_disconnect db_query db_count 
NULL
#' @importFrom rmisc db_bulk_insert trim compact check_timestamp
NULL
#' @importFrom rmisc is.empty "%|na|%" "%has_tables%" rBind
NULL
#' @importFrom RCurl basicTextGatherer curlPerform curlOptions CFILE close
NULL
#' @importFrom RSQLite dbSendQuery dbListTables dbListFields
NULL
#' @importFrom assertthat assert_that
NULL
#' @importClassesFrom RSQLite dbObjectId  SQLiteObject  SQLiteConnection
NULL
#' @importClassesFrom DBI DBIObject DBIConnection

.valid_TaxonDBConnection <- function (object) {
  errors <- character()
  if (!all(c("nodes", "names") %in% dbListTables(object))) {
    errors <- c(errors, "Table missing from 'TaxonDB'\n")
  }
  else {
    if (!all(c("tax_id", "parent_id", "rank", "embl_code", "division_id")
             %in% dbListFields(object, "nodes"))) {
      errors <- c(errors, "Field missing from table 'nodes'\n")
    }
    if (!all(c("tax_id", "tax_name", "unique_name", "class")
             %in% dbListFields(object, "names"))) {
      errors <- c(errors, "Field missing from table 'names'\n")
    }
  }
  
  if (length(errors) == 0L)
    TRUE
  else
    errors
}

#' Database Connections
#' 
#' \sQuote{\bold{TaxonDBConnection}}: A connection to an SQLite database
#' containing the NCBI taxonomy organised in two tables:
#' 
#' \bold{nodes} with fields:
#' 
#' \itemize{
#'    \item tax_id        CHAR(10)    Primary key
#'    \item parent_id     CHAR(10)
#'    \item rank          VARCHAR(50)
#'    \item embl_code     CHAR(2)
#'    \item division_id   CHAR(2)
#' }
#' 
#' \bold{names} with fields:
#' 
#' \itemize{
#'    \item tax_id        CHAR(10)     Primary key
#'    \item tax_name      VARCHAR(200)    
#'    \item unique_name   VARCHAR(100)
#'    \item class         VARCHAR(50)
#' }
#' 
#' \sQuote{\bold{GeneidDBConnection}}: A connection to an SQLite database
#' linking NCBI Gene IDs to Taxids.
#' 
#' 
#' @seealso
#'  The constructors \code{\link{taxonDBConnect}}, \code{\link{geneidDBConnect}}.  
#'
#' @rdname Connection-classes
#' @export
#' @classHierarchy
#' @classMethods 
new_TaxonDBConnection <- setClass('TaxonDBConnection',
                                  contains = 'SQLiteConnection',
                                  validity = .valid_TaxonDBConnection)


#' Create a connection to a local NCBI Taxonomy or Gene ID database
#' 
#' @param db_path Path to the taxonomy or gene ID database. If \code{NULL} the
#' databases are looked for in the \code{extdata} directory of the
#' installed \code{ncbi} package. This is the place where they are installed
#' by default by \code{\link{createTaxonDB}}.
#'
#' @return A \code{\linkS4class{TaxonDBConnection}} or a
#' \code{\linkS4class{GeneidDBConnection}}, respectively
#'    
#' @seealso
#' \code{\link{taxonDB}}, \code{\link{taxonByGeneID}}, 
#'
#' @importFrom assertthat is.dir
#' @rdname taxonDBConnect
#' @export
taxonDBConnect <- function (db_path = NULL) {
  if (is.null(db_path)) {
    db_path <- file.path(path.package("ncbi"), "extdata")
  }
  errmsg <- paste0("Cannot find a local installation of the the NCBI ",
                   "Taxonomy database in the directory ", sQuote(db_path),
                   "\nSpecify the exact path to the database or run the ",
                   "command 'createTaxonDB()'")
  if (is.dir(db_path)) {
    taxon_db <- normalizePath(dir(db_path, pattern="taxon.db", full.names=TRUE),
                              mustWork=FALSE)
    if (all_empty(taxon_db))
      stop(errmsg, call.=FALSE)
  }
  else {
    taxon_db <- normalizePath(db_path, mustWork=TRUE)
  }
  conn <- db_connect(taxon_db)
  new_TaxonDBConnection(conn)
}


.valid_GeneidDBConnection <- function (object) {
  errors <- character()
  if (!"genes" %in% dbListTables(object)) {
    errors <- c(errors, "Table missing from 'GeneidDB'\n")
  }
  else if (!"tax_id" %in% dbListFields(object, "genes")) {
    errors <- c(errors, "Field missing from table 'genes'\n")
  }
  
  if (length(errors) == 0L)
    TRUE
  else
    errors
}

#' @rdname Connection-classes
#' @export
new_GeneidDBConnection <- setClass('GeneidDBConnection',
                                  contains = 'SQLiteConnection',
                                  validity = .valid_GeneidDBConnection)


#' @rdname taxonDBConnect 
#' @export
geneidDBConnect <- function (db_path = NULL) {
  if (is.null(db_path)) {
    db_path <- file.path(path.package("ncbi"), "extdata")
  }
  errmsg <- paste0("Cannot find a local installation of the the GI_to_TaxId ",
                   "database in the directory ", sQuote(db_path),
                   "\nSpecify the exact path to the database or run the ",
                   "command 'createTaxonDB(with_geneid = TRUE)'")
  if (is.dir(db_path)) {
    geneid_db <- normalizePath(dir(db_path, pattern="geneid.db", full.names=TRUE),
                              mustWork=FALSE)
    if (all_empty(geneid_db))
      stop(errmsg, call.=FALSE)
  }
  else {
    geneid_db <- normalizePath(db_path, mustWork=TRUE)
  }
  conn <- db_connect(geneid_db)
  new_GeneidDBConnection(conn)
}


#' Create a local install of the NCBI Taxonomy database.
#' 
#' @details
#' From the commandline:
#' \code{R -q -e "require(ncbi); createTaxonDB('/path/to/db');createGeneidDB('/path/to/db')"}
#
#' @param db_path Parent directory for SQLite database files.
#' 
#' @seealso
#' \code{\link{taxonDBConnext}}, \code{\link{geneidDBConnext}}
#'
#' @rdname TaxonDB
#' @export
createTaxonDB <- function(db_path = file.path(path.package("ncbi"), "extdata")) {
  make_taxondb(db_path, update=FALSE)
}


#' @rdname TaxonDB
#' @export
updateTaxonDB <- function(db_path = file.path(path.package("ncbi"), "extdata")) {
  make_taxondb(db_path, update=TRUE)
}


#' @rdname TaxonDB
#' @export
createGeneidDB <- function(db_path = file.path(path.package("ncbi"), "extdata")) {
  make_geneiddb(db_path, update=FALSE)
}


make_taxondb <- function (db_path = file.path(path.package("ncbi"), "extdata"),
                          update = FALSE)
{ 
  db_name <- normalizePath(file.path(db_path, "taxon.db"), mustWork=FALSE)  
  zip <- "taxdmp.zip"
  zip_path <- normalizePath(file.path(db_path, zip), mustWork=FALSE)
  if (update) {
    status <- db_fetch(db_path, zip, check_timestamp=TRUE)
    if (is.null(status))
      return( db_name )
  } else {
    db_fetch(db_path, zip, check_timestamp=FALSE)
  }
  con <- db_create(db_name, taxon_db.sql)
  on.exit(db_disconnect(con))
  assert_that(con %has_tables% c("nodes", "names"))
  db_load(con, db_path, "taxon")
}


make_geneiddb <- function (db_path = file.path(path.package("ncbi"), "extdata"),
                           update = FALSE)
{
  db_name <- normalizePath(file.path(db_path, "geneid.db"), mustWork=FALSE)  
  if (update) {
    zip <- c("gi_taxid_nucl_diff.zip","gi_taxid_prot_diff.zip")
    return( db_name )
    # TODO IMPLEMENT
  } else {
    zip <- c("gi_taxid_nucl.zip","gi_taxid_prot.zip")
    zip_path <- normalizePath(file.path(db_path, zip), mustWork=FALSE)
    
    db_fetch(db_path, zip, check_timestamp=TRUE)
    
    dmp <- c("gi_taxid_nucl.dmp","gi_taxid_prot.dmp","gi_index.dmp")
    dmp_path <- normalizePath(file.path(db_path, dmp))
    unlink(dmp_path[file.exists(dmp_path)]) ## unlink existing dump files
    message("Extracting gi_taxid_nucl.zip ...")
    system(paste("gunzip -c", zip_path[1], ">", dmp_path[1]))
    message("Extracting gi_taxid_prot.zip ...")
    system(paste("gunzip -c", zip_path[2], ">", dmp_path[2]))
    message("Generating 'gi_index' ...")
    index_gi_taxid(dmp_path[1], dmp_path[2], dmp_path[3])
  }
  con <- db_create(db_name, geneid_db.sql)
  on.exit(db_disconnect(con))
  success <- db_load(con, db_path, "geneid")
  if (success) {
    message("Successfully loaded ", db_count(con, "genes"), " rows into 'genes' table")
  } else {
    message("Error: ", success)
  }
  
  return( db_name )
}


db_fetch <- function(db_path, zip, check_timestamp = FALSE, verbose = FALSE) {
  
  if (!file.exists(db_path)) {
    dir.create(db_path, recursive=TRUE)
  }
  
  datafiles <- normalizePath(file.path(db_path, zip), mustWork=FALSE)
  for (file in datafiles)
    status <- db_update( file, check_timestamp, verbose )
  
  return( status )
}


db_update <- function (file, check = FALSE, verbose = FALSE) {
  ncbi_base_url <- 'ftp://ftp.ncbi.nih.gov/pub/taxonomy'
  url <- paste0(ncbi_base_url, "/", basename(file))
  
  ## check_timestamp returns TRUE if the remote source is more recent
  ## than the local file or the local file does not exist.
  if (check && !check_timestamp(url, file, TRUE)) {
    ## if check and remote source is older than the local file
    ## do nothing
      return(NULL)
  }
  else {
    opts <- curlOptions(noprogress=FALSE)
    f <- CFILE(file, mode="wb")
    on.exit(RCurl::close(f))
    status <- curlPerform(url = url, .opts = opts, writedata = f@ref,
                          verbose = verbose)
    if (status != 0) {
      stop("Curl error code: ", status)
    }
    return(status)
  }
}


db_load <- function(con, db_path, type = "taxon") {
  if (type == "taxon")
  {
    zipfile <- normalizePath(file.path(db_path, "taxdmp.zip"))
    files <- c("nodes.dmp", "names.dmp")
    unzip(zipfile, files, exdir=db_path)
    dmp <- normalizePath(file.path(db_path, c("nodes.dmp", "names.dmp")), mustWork=TRUE)
    if ( db_bulk_insert(con, "nodes", 
                        df=as.data.frame(readNodes(dmp[1]), stringsAsFactors=FALSE)) )
    {
      message("Inserted ", db_count(con, "nodes"),
              " rows into ", sQuote("nodes"), " table.")
    }
    if ( db_bulk_insert(con, "names", 
                        df=as.data.frame(readNames(dmp[2]), stringsAsFactors=FALSE)) )
    {
      message("Inserted ", db_count(con, "names"),
              " rows into ", sQuote("names"), " table.")
    }
    unlink(dmp)
    return( TRUE )
  }
  else if (type == "geneid")
  {
    fn <- normalizePath(file.path(db_path, "gi_index.dmp"))
    message("Loading 'gi_index'. This will take a while ...")
    db <- list(geneidDBConnection = con)
    
    if ( length(getTaxidByGeneID(db, 1)) != 0 )
      dbSendQuery(con, "DELETE FROM genes")
    
    rc <- try({
      conId <- con@Id
      sep <- ","
      eol <- "\n"
      skip <- 0L
      .Call("RS_SQLite_importFile", conId, "genes", fn, sep, eol, skip, 
            PACKAGE = RSQLite:::.SQLitePkgName)
    })
    return( rc )
  } else {
    return( FALSE )
  }
}


##
dbGetTaxon <- function(db, taxid, cache = TRUE) {
  node <- dbGetNode(db, taxid)
  new_Taxon_full(
    shared = db,
    TaxId = node$tax_id %||% NA_character_,
    ScientificName = node$tax_name %||% NA_character_,
    Rank = node$rank %||% NA_character_,
    ParentTaxId = node$parent_id %||% NA_character_,
    OtherName = dbGetOtherName(db, taxid),
    Authority = dbGetAuthority(db, taxid),
    TypeMaterial = dbGetTypeMaterial(db, taxid),
    Lineage = dbGetLineage(db, taxid, cache)
  )
}

##
dbGetTaxonByGeneID <- function(db, geneid, cache=TRUE) {
  taxid <- as.character(getTaxidByGeneID(db, geneid))
  if (length(taxid) == 0 || taxid == 0)
    new_Taxon_full(shared = db)
  else
    dbGetTaxon(db, taxid, cache)
}

##
dbGetTaxonMinimal <- function(db, taxid) {
  conn <- db$taxonDBConnection
  taxid <- taxid %|na|% 0
  sql <- paste0("SELECT tax_id, tax_name, rank FROM nodes JOIN names USING ( tax_id ) ",
                "WHERE tax_id = ", taxid, " AND class = 'scientific name'") 
  data <- db_query(conn, sql)
  new_Taxon_minimal(
    shared = db,
    TaxId = data$tax_id %||% NA_character_,
    ScientificName = data$tax_name %||% NA_character_,
    Rank = data$rank %||% NA_character_)
}

##
dbGetTaxonMinimalByGeneID <- function(db, geneid) {
  taxid <- getTaxidByGeneID(db, geneid)
  if (length(taxid) == 0 || taxid == 0)
    new_Taxon_minimal(shared = db)
  else
    dbGetTaxonMinimal(db, taxid)
}

##
getTaxidByGeneID <- function(db, geneid) {
  conn <- db$geneidDBConnection 
  geneid <- geneid %|na|% 0
  sql <- paste0("SELECT tax_id FROM genes WHERE rowid = ", geneid)
  db_query(conn, sql, 1)
}

##
dbGetParentTaxId <- function(db, taxid) {
  conn <- db$taxonDBConnection 
  taxid <- taxid %|na|% 0
  sql <- paste0("SELECT parent_id FROM nodes WHERE tax_id = ", taxid)
  db_query(conn, sql, 1) %||% NA_character_
}

##
dbGetScientificName <- function(db, taxid) {
  conn <- db$taxonDBConnection 
  taxid <- taxid %|na|% 0
  sql <- paste0("SELECT tax_name, class FROM names WHERE tax_id = ", taxid,
                " AND class = 'scientific name'")
  db_query(conn, sql, 1) %||% NA_character_
}

##
dbGetOtherName <- function(db, taxid) {
  conn <- db$taxonDBConnection 
  taxid <- taxid %|na|% 0
  sql <- paste0("SELECT tax_name, class FROM names WHERE tax_id = ", taxid,
                " AND class != 'scientific name' AND class != 'type material'",
                " AND class != 'authority'")
  data <- db_query(conn, sql)
  setNames(data$tax_name, nm=camelise(data$class)) %||% NA_character_
}

##
dbGetTypeMaterial <- function(db, taxid) {
  conn <- db$taxonDBConnection 
  taxid <- taxid %|na|% 0
  sql <- paste0("SELECT tax_name FROM names WHERE tax_id = ", taxid,
                " AND class = 'type material'")
  db_query(conn, sql, 1) %||% NA_character_
}

##
dbGetAuthority <- function(db, taxid) {
  conn <- db$taxonDBConnection 
  taxid <- taxid %|na|% 0
  sql <- paste0("SELECT tax_name FROM names WHERE tax_id = ", taxid,
                " AND class = 'authority'")
  db_query(conn, sql, 1) %||% NA_character_
}

##
dbGetRank <- function(db, taxid) {
  conn <- db$taxonDBConnection 
  taxid <- taxid %|na|% 0
  sql <- paste0("SELECT rank FROM nodes WHERE tax_id = ", taxid)
  db_query(conn, sql, 1) %||% NA_character_
}


##' @return TaxId, ParentId, ScientificName, Rank
dbGetNode <- function(db, taxid) {
  conn <- db$taxonDBConnection 
  taxid <- taxid %|na|% 0
  sql <- paste0("SELECT tax_id, parent_id, tax_name, rank FROM nodes JOIN names",
                " USING ( tax_id ) WHERE tax_id = ", taxid,
                " AND class = 'scientific name'")
  db_query(conn, sql)
}

##
dbGetLineage <- function (db, taxid, cache = TRUE) {
  if (cache) {
    #print(taxid)
    .get_cached_lineage(db, taxid)
  } else {
    .get_lineage(db, taxid)
  }
}


.get_cached_lineage <- function (db, id) {
  if (!.taxcache$exists(id)) {
    lin <- .lineage_to_cache(db, id)
  } else {
    lin <- rbind(.lineage_from_cache(id),
                 dbGetNode(db, id)[, c('tax_id', 'tax_name', 'rank')])
  }
  Lineage( lin[-1L, ], shared = db )
}


.lineage_to_cache <- function(db, id) {
  
  verbose <- getOption("verbose")
  ## pid : parental taxid
  ##  id : taxid
  ## cid : child taxid
  lin_list   <- vector('list', 50L)
  lin_fields <- c('tax_id', 'tax_name', 'rank')
  i          <- 1

  ## fetch the current node, split off the pid, and store in 'lin_list'
  node <- dbGetNode(db, id)
  lin_list[[i]] <- node[, lin_fields]
  pid <- node$parent_id
  
  ## if the supplied taxid isn't in the database, the node will be
  ## empty, and we don't cache and return the empty node.
  if  (length(pid) > 0) {
    while (id != pid) {
      cid <- id
      id <- pid
      
      ## fetch the node and store it in 'lin_list'
      node <- dbGetNode(db, id)    
      i <- i + 1
      lin_list[[i]] <- node[, lin_fields]
      if (verbose)
        cat("Caching lineage:", cid, "->", id, "\n")
      
      ## cache the current node
      .taxcache$set(cid, node[, lin_fields])
      
      ## if the current parental id is cached drop into the cache and get the
      ## rest of the lineage.
      if (.taxcache$exists(pid)) {
        return( rbind(.lineage_from_cache(pid), rBind(rev(compact(lin_list)))) )
      }
      
      ##  split off a new pid
      pid <- node$parent_id
    }
    
    ## finally assign root node to root id
    .taxcache$set(id, node[, lin_fields])
    rBind(rev(compact(lin_list)))
  }
  else
    lin_list[[1]] 
}


.lineage_from_cache <- function (id) {
  verbose <- getOption("verbose")
  if (verbose)
    cat("Getting cached lineage:", id, "\n")
  
  lin_list <- vector('list', 50)
  i        <- 1
  node     <- .taxcache$get(id)
  pid      <- node$tax_id
  while (id != pid) {
    lin_list[[i]] <- node
    node <- .taxcache$get(pid)
    id <- pid
    pid <- node$tax_id
    i <- i + 1
  }
  lin_list <- rev(compact(lin_list))
  
  if (length(lin_list) > 0)
    rBind( lin_list )
  else
    lin_list
}


.get_lineage <- function(db, id) {
  Lineage( (function (db, id) {
    node <- dbGetNode(db, id)
    pid <- node$parent_id
    lin <- cbind(tax_id = node$tax_id,
                 tax_name = node$tax_name,
                 rank = node$rank)
    if (length(pid) > 0 && pid != id)
      lin <- rbind(Recall(db, pid), lin)
    lin
  })(db, id)[-1, , drop = FALSE], shared = db)
}


camelise <- function(s) {
  if (is.null(s))
    return( s )
  cap <- function(s) paste0(toupper(substring(s, 1, 1)),
                            substring(s, 2), collapse="")
  gsub("[[:punct:]]", "", sapply(strsplit(s, " "), cap))
}

