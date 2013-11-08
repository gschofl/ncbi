#' @include utils.R
NULL
#' @importFrom rmisc db_create db_connect db_disconnect db_query db_count 
NULL
#' @importFrom RSQLite dbListTables dbListFields dbSendQuery
NULL
#' @importFrom RCurl curlOptions CFILE close curlPerform
NULL
#' @importClassesFrom RSQLite dbObjectId SQLiteObject SQLiteConnection
NULL
#' @importClassesFrom DBI DBIObject DBIConnection
NULL

.valid_TaxonDBConnection <- function(object) {
  errors <- character()
  if (!all(c("nodes", "names") %in% dbListTables(object))) {
    errors <- c(errors, "Table missing from 'TaxonDB'\n")
  } else {
    if (!all(c("tax_id", "parent_id", "rank", "embl_code", "division_id")
             %in% dbListFields(object, "nodes"))) {
      errors <- c(errors, "Field missing from table 'nodes'\n")
    }
    if (!all(c("tax_id", "tax_name", "unique_name", "class")
             %in% dbListFields(object, "names"))) {
      errors <- c(errors, "Field missing from table 'names'\n")
    }
  }
  
  if (length(errors) == 0L) { TRUE } else { errors }
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
#' @keywords internal classes
#' @export
#' @classHierarchy
#' @classMethods 
new_TaxonDBConnection <- setClass('TaxonDBConnection',
                                  contains='SQLiteConnection',
                                  validity=.valid_TaxonDBConnection)


#' Create connections to a local NCBI Taxonomy or GeneID database
#' 
#' These functions are not typically invoked by the user. The path
#' to the taxonomy and GeneID databases is specified rather by
#' setting the global option \code{ncbi.taxonomy.path}.
#' 
#' Run the code{\link{createTaxonDB}} and ode{\link{createGeneidDB}} to
#' install the taxonomy and geneid databases locally.
#' 
#' @usage taxonDBConnect(db_path=getOption("ncbi.taxonomy.path")
#' @param db_path Path to the directory containing the local taxonomy and
#' GeneID databases. This path cshould be configured by setting
#' the option \code{ncbi.taxonomy.path}.
#'
#' @return A \code{\linkS4class{TaxonDBConnection}} or a
#' \code{\linkS4class{GeneidDBConnection}}, respectively.
#'    
#' @seealso
#' \code{\link{taxonDB}}, \code{\link{taxonByGeneID}}, 
#'
#' @rdname taxonDBConnect
#' @keywords internal
#' @export
taxonDBConnect <- memoise(function(db_path=getOption("ncbi.taxonomy.path")) {
  db_path <- ncbi.taxonomy.path(db_path)
  errmsg <- paste0("Cannot find a local installation of the the NCBI ",
                   "Taxonomy database 'taxon.db' in the directory ", sQuote(db_path),
                   "\nSpecify the correct path of the database directory or run the ",
                   "command 'createTaxonDB()'")
  taxon_db <- normalizePath(dir(db_path, pattern="taxon.db", full.names=TRUE),
                            mustWork=FALSE)
  if (length(taxon_db) == 0) {
    stop(errmsg, call.=FALSE)
  }
  conn <- db_connect(taxon_db)
  new_TaxonDBConnection(conn)
})


.valid_GeneidDBConnection <- function(object) {
  errors <- character()
  if (!"genes" %in% dbListTables(object)) {
    errors <- c(errors, "Table missing from 'GeneidDB'\n")
  } else if (!"tax_id" %in% dbListFields(object, "genes")) {
    errors <- c(errors, "Field missing from table 'genes'\n")
  }
  
  if (length(errors) == 0L) { TRUE } else { errors }
}

#' @rdname Connection-classes
#' @keywords internal classes
#' @export
new_GeneidDBConnection <- 
  setClass('GeneidDBConnection',
           contains='SQLiteConnection',
           validity=.valid_GeneidDBConnection
  )


#' @usage geneidDBConnect(db_path=getOption("ncbi.taxonomy.path")
#' @rdname taxonDBConnect
#' @keywords internal
#' @export
geneidDBConnect <- memoise(function(db_path=getOption("ncbi.taxonomy.path")) {
  db_path <- ncbi.taxonomy.path(db_path)
  errmsg <- paste0("Cannot find a local installation of the the GI_to_TaxId ",
                   "database 'geneid.db' in the directory ", sQuote(db_path),
                   "\nSpecify the correct path of the database directory or run the ",
                   "command 'createGeneidDB()'")
  geneid_db <- normalizePath(dir(db_path, pattern="geneid.db", full.names=TRUE),
                             mustWork=FALSE)
  if (length(geneid_db) == 0) {
    stop(errmsg, call.=FALSE)
  }
  conn <- db_connect(geneid_db)
  new_GeneidDBConnection(conn)
})


#' Create a local install of the NCBI Taxonomy database.
#' 
#' @details
#' From the commandline run:
#' \code{R -q -e "require(ncbi);createTaxonDB();createGeneidDB()"}
#' 
#' This will install two SQLite databases "taxon.db" and "geneid.db"
#' in "$HOME/local/db/taxonomy/". To override the default installation
#' directory set the option \code{ncbi.taxonomy.path} in your .Rprofile.
#
#' @param db_path Parent directory for SQLite database files.
#' 
#' @seealso
#' \code{\link{taxonDBConnect}}, \code{\link{geneidDBConnect}}
#'
#' @rdname TaxonDB
#' @export
createTaxonDB <- function(db_path=getOption("ncbi.taxonomy.path")) {
  make_taxondb(ncbi.taxonomy.path(db_path), update=FALSE)
}


#' @rdname TaxonDB
#' @export
updateTaxonDB <- function(db_path=getOption("ncbi.taxonomy.path")) {
  make_taxondb(ncbi.taxonomy.path(db_path), update=TRUE)
}


#' @rdname TaxonDB
#' @export
createGeneidDB <- function(db_path=getOption("ncbi.taxonomy.path")) {
  make_geneiddb(ncbi.taxonomy.path(db_path), update=FALSE)
}


#' @rdname TaxonDB
#' @export
updateGeneidDB <- function(db_path=getOption("ncbi.taxonomy.path")) {
  make_geneiddb(ncbi.taxonomy.path(db_path), update=TRUE)
}



#' @importFrom rmisc "%has_tables%"
make_taxondb <- function(db_path, update=FALSE) {
  url <- 'ftp://ftp.ncbi.nih.gov/pub/taxonomy'
  zipped <- "taxdump.tar.gz"
  db_name <- normalizePath(file.path(db_path, "taxon.db"), mustWork=FALSE)
  
  if (update) {
    status <- fetch_files(path=db_path, url, files=zipped, check=TRUE)
    if (is.null(status)) {
      return(db_name)
    }
  } else {
    fetch_files(db_path, url, zipped, check=FALSE)
  }
  con <- db_create(db_name, taxon_db.sql)
  on.exit(db_disconnect(con))
  assert_that(con %has_tables% c("nodes", "names"))
  db_load(con, db_path, "taxon")
}


#' @importFrom rmisc has_command
make_geneiddb <- function(db_path, update=FALSE) {
  assert_that(has_command("gunzip"))
  url <- 'ftp://ftp.ncbi.nih.gov/pub/taxonomy'
  db_name <- normalizePath(file.path(db_path, "geneid.db"), mustWork=FALSE)
  
  if (update) {
    zipped <- c("gi_taxid_nucl_diff.zip", "gi_taxid_prot_diff.zip")
    .NotYetImplemented()
    # TODO IMPLEMENT
  } else {
    zipped <- c("gi_taxid_nucl.dmp.gz", "gi_taxid_prot.dmp.gz")
    zipped_path <- normalizePath(file.path(db_path, zipped), mustWork=FALSE)
    fetch_files(db_path, url, zipped, check=TRUE)
    
    # No dmp extension for the gi_index file, so that it doesn't get
    # clobbered when I use the same taxonomy dumps with Krona.
    dmp <- c("gi_taxid_nucl.dmp", "gi_taxid_prot.dmp", "gi_index")
    dmp_path <- normalizePath(file.path(db_path, dmp))
    
    unlink(dmp_path[file.exists(dmp_path)]) ## unlink existing dmp files
    message("Extracting gi_taxid_nucl.dmp.gz ...")
    system(paste("gunzip -c", zipped_path[1], ">", dmp_path[1]))
    message("Extracting gi_taxid_prot.dmp.gz ...")
    system(paste("gunzip -c", zipped_path[2], ">", dmp_path[2]))
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
  
  db_name
}


#' Fetch files from a remote location conditional on a difference in
#' timestamp or content-length.
#' 
#' @param path Local file path.
#' @param url Remote base URL.
#' @param files File names.
#' @param check if \code{TRUE}, fetch the file only if the
#' remote file is newer than the local version.
#' @param verbose be verbose
#' @keywords internal
#' @export
fetch_files <- function(path, url, files, check=FALSE, verbose=FALSE) {
  if (!file.exists(path)) {
    dir.create(path, recursive=TRUE)
  }
  files <- normalizePath(file.path(path, files), mustWork=FALSE)
  for (file in files) {
    status <- fetch_file(url, file, check, verbose)
  }
  invisible(status)
}


#' @importFrom rmisc file_compare strip_ext
fetch_file <- function(url, file, check=FALSE, verbose=FALSE) {
  assert_that(is.string(url))
  assert_that(is.string(file))
  url <- paste0(strip_ext(url, "/$"), "/", basename(file))
  ## check_timestamp returns TRUE if the remote source is more recent
  ## than the local file or the local file does not exist.
  if (check && !file_compare(file, url, time=check, .message=TRUE)) {
    ## if the remote source is older than the local file do nothing
    return(NULL)
  } else {
    opts <- curlOptions(noprogress=FALSE)
    f <- CFILE(file, mode="wb")
    on.exit(RCurl::close(f))
    status <- curlPerform(url=url, .opts=opts, writedata=f@ref,
                          verbose=verbose)
    if (status != 0) {
      stop("Curl error code: ", status)
    }
    return(status)
  }
}

#' @importFrom rmisc db_bulk_insert
db_load <- function(con, db_path, type="taxon") {
  if (type == "taxon") {
    assert_that(has_command("gunzip"), has_command("gzip"))
    zipfile <- normalizePath(file.path(db_path, "taxdump.tar.gz"))
    dmpfiles <- c("nodes.dmp", "names.dmp")
    system(paste("gunzip -f", zipfile))
    tarfile <- strip_ext(zipfile, level=1)
    untar(tarfile, files=dmpfiles, exdir=normalizePath(db_path))
    system(paste0("gzip ", tarfile))
    dmp <- normalizePath(file.path(db_path, dmpfiles), mustWork=TRUE)
    if (db_bulk_insert(con, "nodes", 
                       as.data.frame(readNodes(dmp[1]), stringsAsFactors=FALSE))) {
      message("Inserted ", db_count(con, "nodes"),
              " rows into ", sQuote("nodes"), " table.")
    }
    if (db_bulk_insert(con, "names", 
                       as.data.frame(readNames(dmp[2]), stringsAsFactors=FALSE))) {
      message("Inserted ", db_count(con, "names"),
              " rows into ", sQuote("names"), " table.")
    }
    unlink(dmp)
    return(TRUE)
  }
  else if (type == "geneid") {
    fn <- normalizePath(file.path(db_path, "gi_index"))
    message("Loading 'gi_index'. This will take a while ...")
    db <- list(geneidDBConnection=con)
    if (length(getTaxidByGeneID(db, 1)) != 0) {
      dbSendQuery(con, "DELETE FROM genes")
    }
    rc <- try({
      conId <- con@Id
      sep <- ","
      eol <- "\n"
      skip <- 0L
      .Call("RS_SQLite_importFile", conId, "genes", fn, sep, eol, skip, 
            PACKAGE=RSQLite:::.SQLitePkgName)
    })
    return(rc)
  }
  else {
    return(FALSE)
  }
}


##
dbGetTaxon <- memoise(function(db, taxid) {
  node <- dbGetNode(db, taxid)
  new_Taxon_full(
    shared=db,
    TaxId=node$tax_id %||% NA_character_,
    ScientificName=node$tax_name %||% NA_character_,
    Rank=node$rank %||% NA_character_,
    ParentTaxId=node$parent_id %||% NA_character_,
    OtherName=dbGetOtherName(db, taxid),
    Authority=dbGetAuthority(db, taxid),
    TypeMaterial=dbGetTypeMaterial(db, taxid),
    Lineage=dbGetLineage(db, taxid)
  )
})

##
dbGetTaxonByGeneID <- memoise(function(db, geneid) {
  taxid <- as.character(getTaxidByGeneID(db, geneid))
  if (length(taxid) == 0 || taxid == 0) {
    new_Taxon_full(shared=db)
  } else {
    dbGetTaxon(db, taxid)
  }
})


#' @importFrom memoise memoise
dbGetTaxonMinimal <- memoise(function(db, taxid) {
  conn <- db$taxonDBConnection
  taxid <- taxid %|na|% 0
  sql <- paste0("SELECT tax_id, tax_name, rank FROM nodes JOIN names USING ( tax_id ) ",
                "WHERE tax_id=", taxid, " AND class='scientific name'") 
  data <- db_query(conn, sql)
  new_Taxon_minimal(
    shared=db,
    TaxId=data$tax_id %||% NA_character_,
    ScientificName=data$tax_name %||% NA_character_,
    Rank=data$rank %||% NA_character_)
})

##
dbGetTaxonMinimalByGeneID <- memoise(function(db, geneid) {
  taxid <- getTaxidByGeneID(db, geneid)
  if (length(taxid) == 0 || taxid == 0) {
    new_Taxon_minimal(shared=db)
  } else {
    dbGetTaxonMinimal(db, taxid)
  }
})

##
getTaxidByGeneID <- function(db, geneid) {
  conn <- db$geneidDBConnection 
  geneid <- geneid %|na|% 0
  sql <- paste0("SELECT tax_id FROM genes WHERE rowid=", geneid)
  db_query(conn, sql, 1)
}

##
dbGetParentTaxId <- function(db, taxid) {
  conn <- db$taxonDBConnection 
  taxid <- taxid %|na|% 0
  sql <- paste0("SELECT parent_id FROM nodes WHERE tax_id=", taxid)
  db_query(conn, sql, 1) %||% NA_character_
}

##
dbGetScientificName <- function(db, taxid) {
  conn <- db$taxonDBConnection 
  taxid <- taxid %|na|% 0
  sql <- paste0("SELECT tax_name, class FROM names WHERE tax_id=", taxid,
                " AND class='scientific name'")
  db_query(conn, sql, 1) %||% NA_character_
}

##
dbGetOtherName <- function(db, taxid) {
  conn <- db$taxonDBConnection 
  taxid <- taxid %|na|% 0
  sql <- paste0("SELECT tax_name, class FROM names WHERE tax_id=", taxid,
                " AND class != 'scientific name' AND class != 'type material'",
                " AND class != 'authority'")
  data <- db_query(conn, sql)
  setNames(data$tax_name, camelise(data$class)) %||% NA_character_
}

##
dbGetTypeMaterial <- function(db, taxid) {
  conn <- db$taxonDBConnection 
  taxid <- taxid %|na|% 0
  sql <- paste0("SELECT tax_name FROM names WHERE tax_id=", taxid,
                " AND class='type material'")
  db_query(conn, sql, 1) %||% NA_character_
}

##
dbGetAuthority <- function(db, taxid) {
  conn <- db$taxonDBConnection 
  taxid <- taxid %|na|% 0
  sql <- paste0("SELECT tax_name FROM names WHERE tax_id=", taxid,
                " AND class='authority'")
  db_query(conn, sql, 1) %||% NA_character_
}

##
dbGetRank <- function(db, taxid) {
  conn <- db$taxonDBConnection 
  taxid <- taxid %|na|% 0
  sql <- paste0("SELECT rank FROM nodes WHERE tax_id=", taxid)
  db_query(conn, sql, 1) %||% NA_character_
}


##' @return TaxId, ParentId, ScientificName, Rank
dbGetNode <- memoise(function(db, taxid) {
  conn <- db$taxonDBConnection 
  taxid <- taxid %|na|% 0
  sql <- paste0("SELECT tax_id, parent_id, tax_name, rank FROM nodes JOIN names",
                " USING ( tax_id ) WHERE tax_id=", taxid,
                " AND class='scientific name'")
  db_query(conn, sql)
})

##
dbGetLineage <- memoise(function(db, taxid) {
  if (!.taxcache$exists(taxid)) {
    lin <- .lineage_to_cache(db, taxid)
  } else {
    lin <- rbind(.lineage_from_cache(taxid),
                 dbGetNode(db, taxid)[, c('tax_id', 'tax_name', 'rank')])
  }
  Lineage( lin[-1L, ], shared=db )
})


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
  if (length(pid) > 0) {
    while (id != pid) {
      cid <- id
      id <- pid
      
      ## fetch the node and store it in 'lin_list'
      node <- dbGetNode(db, id)    
      i <- i + 1
      lin_list[[i]] <- node[, lin_fields]
      if (verbose) {
        cat("Caching lineage:", cid, "->", id, "\n")
      }
      
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
  } else {
    lin_list[[1]]
  }
}


.lineage_from_cache <- function(id) {
  verbose <- getOption("verbose")
  if (verbose) {
    cat("Getting cached lineage:", id, "\n")
  }
  lin_list <- vector('list', 50)
  i        <- 1
  node     <- .taxcache$get(id)
  pid      <- node$tax_id
  while (id != pid) {
    lin_list[[i]] <- node
    node <- .taxcache$get(pid)
    id   <- pid
    pid  <- node$tax_id
    i    <- i + 1
  }
  lin_list <- rev(compact(lin_list))
  
  if (length(lin_list) > 0) {
    rBind( lin_list )
  } else {
    lin_list
  }
}


camelise <- function(s) {
  cap <- function(s) {
    paste0(toupper(substring(s, 1, 1)), substring(s, 2), collapse="")
  }
  if (is.null(s)) {
    return(s)
  }
  gsub("[[:punct:]]", "", sapply(strsplit(s, "[ _]+"), cap))
}

