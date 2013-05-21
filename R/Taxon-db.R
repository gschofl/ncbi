#' @importFrom rmisc db_create db_connect db_disconnect db_query db_count
NULL
#' @importFrom rmisc db_bulk_insert trim compact is_empty "%has_tables%"
NULL
#' @importFrom RCurl basicTextGatherer curlPerform curlOptions CFILE close
NULL
#' @importFrom RSQLite dbSendQuery
NULL
#' @importFrom assertthat assert_that

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

#' Create a local install of the NCBI Taxonomy database.
#' 
#' @details
#' From the commandline:
#' \code{R -q -e "require(ncbi);createTaxonDB('/path/to/db')"}
#
#' @param dbPath Source directory for SQLite database files.
#' @param with_geneid Include mappings of Gene ID to Taxonomy ID (very large table!)
#' @rdname TaxonDB
#' @export
createTaxonDB <- function(dbPath = file.path(path.package("ncbi"), "extdata"),
                          with_geneid = TRUE) {
  make_taxondb(dbPath, update=FALSE)
  if (with_geneid)
    make_geneiddb(dbPath, update=FALSE)
  return(TRUE)
}


#' @rdname TaxonDB
#' @export
updateTaxonDB <- function(dbPath = file.path(path.package("ncbi"), "extdata"),
                          with_geneid = TRUE) {
  make_taxondb(dbPath, update=TRUE)
  if (with_geneid)
    make_geneiddb(dbPath, update=TRUE)
  return(TRUE)
}


make_taxondb <- function (dbPath = file.path(path.package("ncbi"), "extdata"),
                          update = FALSE)
{ 
  dbName <- normalizePath(file.path(dbPath, "taxon.db"), mustWork=FALSE)  
  zip <- "taxdmp.zip"
  zipPath <- normalizePath(file.path(dbPath, zip), mustWork=FALSE)
  if (update) {
    status <- db_fetch(dbPath, zip, check_timestamp=TRUE)
    if (is.null(status))
      return( dbName )
  } else {
    db_fetch(dbPath, zip, check_timestamp=FALSE)
  }
  con <- db_create(dbName, taxon_db.sql)
  on.exit(db_disconnect(con))
  assert_that(con %has_tables% c("nodes", "names"))
  db_load(con, dbPath, "taxon")
}


make_geneiddb <- function (dbPath = file.path(path.package("ncbi"), "extdata"),
                           update = FALSE)
{
  dbName <- normalizePath(file.path(dbPath, "geneid.db"), mustWork=FALSE)  
  if (update) {
    zip <- c("gi_taxid_nucl_diff.zip","gi_taxid_prot_diff.zip")
    return( dbName )
    # TODO IMPLEMENT
  } else {
    zip <- c("gi_taxid_nucl.zip","gi_taxid_prot.zip")
    zipPath <- normalizePath(file.path(dbPath, zip), mustWork=FALSE)
    
    db_fetch(dbPath, zip, check_timestamp=TRUE)
    
    dmp <- c("gi_taxid_nucl.dmp","gi_taxid_prot.dmp","gi_index.dmp")
    dmpPath <- normalizePath(file.path(dbPath, dmp))
    unlink(dmpPath[file.exists(dmpPath)]) ## unlink existing dump files
    message("Extracting gi_taxid_nucl.zip ...")
    system(paste("gunzip -c", zipPath[1], ">", dmpPath[1]))
    message("Extracting gi_taxid_prot.zip ...")
    system(paste("gunzip -c", zipPath[2], ">", dmpPath[2]))
    message("Generating 'gi_index' ...")
    index_gi_taxid(dmpPath[1], dmpPath[2], dmpPath[3])
  }
  con <- db_create(dbName, geneid_db.sql)
  on.exit(db_disconnect(con))
  success <- db_load(con, dbPath, "geneid")
  if (success) {
    message("Successfully loaded ", db_count(con, "genes"), " rows into 'genes' table")
  } else {
    message("Error: ", success)
  }
  
  return( dbName )
}


db_fetch <- function(dbPath, zip, check_timestamp = FALSE, verbose = FALSE) {
  
  if (!file.exists(dbPath)) {
    dir.create(dbPath, recursive=TRUE)
  }
  
  datafiles <- normalizePath(file.path(dbPath, zip), mustWork=FALSE)
  for (file in datafiles)
    status <- db_update( file, check_timestamp, verbose )
  
  return( status )
}


db_update <- function (file, check_timestamp = FALSE, verbose = FALSE) {
  ncbi_data_url <- 'ftp://ftp.ncbi.nih.gov/pub/taxonomy'
  url <- paste0(ncbi_data_url, "/", basename(file))
  
  if (check_timestamp && file.exists(file)) {
    local_time <- file.info(file)$ctime
    h <- basicTextGatherer()
    curlPerform(url=url, nobody=TRUE, filetime=TRUE,
                headerfunction=h$update, verbose=FALSE)
    pt <- "Last-Modified: "
    remote_time <- as.POSIXct(
      strptime(base::sub(pt, "", grep(pt, strsplit(h$value(), "\r\n")[[1]], value=TRUE)),
               format="%a, %d %b %Y %H:%M:%S GMT", tz="GMT")
    )
    if (remote_time < local_time) {
      message("Local file is more recent than remote file.")
      return(NULL)
    }
  }

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


db_load <- function(con, dbPath, type = "taxon") {
  if (type == "taxon")
  {
    zipfile <- normalizePath(file.path(dbPath, "taxdmp.zip"))
    files <- c("nodes.dmp", "names.dmp")
    unzip(zipfile, files, exdir=dbPath)
    dmp <- normalizePath(file.path(dbPath, c("nodes.dmp", "names.dmp")), mustWork=TRUE)
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
    fn <- normalizePath(file.path(dbPath, "gi_index.dmp"))
    message("Loading 'gi_index'. This will take a while ...")
    if ( length(getTaxidByGeneID(con, 1)) != 0 )
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


dbGetTaxon <- function(con, taxId) {
  node <- dbGetNode(con, taxId)
  new("Taxon_full",
      TaxId = node[["tax_id"]] %||% NA_character_,
      ScientificName = node[["tax_name"]] %||% NA_character_,
      Rank = node[["rank"]] %||% NA_character_,
      ParentTaxId = node[["parent_id"]] %||% NA_character_,
      OtherName = dbGetOtherName(con, taxId),
      Authority = dbGetAuthority(con, taxId),
      TypeMaterial = dbGetTypeMaterial(con, taxId),
      Lineage = dbGetLineage(con, taxId))
}


dbGetTaxonByGeneID <- function(con1, con2, geneid) {
  taxid <- getTaxidByGeneID(con1, geneid)
  if (length(taxid) == 0 || taxid == 0)
    return( new("Taxon_full") )
  else
    dbGetTaxon(con2, taxid)
}


dbGetTaxonMinimal <- function(con, taxId) {
  sql <- paste0("SELECT tax_id, tax_name, rank FROM nodes JOIN names USING ( tax_id ) ",
                "WHERE tax_id = ", taxId, " AND class = 'scientific name'") 
  data <- db_query(con, sql)
  new("Taxon_minimal",
      TaxId = data[["tax_id"]] %||% NA_character_,
      ScientificName = data[["tax_name"]] %||% NA_character_,
      Rank = data[["rank"]] %||% NA_character_)
}


dbGetTaxonMinimalByGeneID <- function(con1, con2, geneid) {
  taxid <- getTaxidByGeneID(con1, geneid)
  if (length(taxid) == 0 || taxid == 0)
    return( new("Taxon_full") )
  else
    dbGetTaxonMinimal(con2, taxid)
}


getTaxidByGeneID <- function(con, geneid) {
  sql <- paste0("SELECT tax_id FROM genes WHERE rowid = ", geneid)
  db_query(con, sql, 1)
}


dbGetParentTaxId <- function(con, taxId) {
  sql <- paste0("SELECT parent_id FROM nodes WHERE tax_id = ", taxId)
  db_query(con, sql, 1) %||% NA_character_
}


dbGetScientificName <- function(con, taxId) {
  sql <- paste0("SELECT tax_name, class FROM names WHERE tax_id = ", taxId,
                " AND class = 'scientific name'")
  db_query(con, sql, 1) %||% NA_character_
}


dbGetOtherName <- function(con, taxId) {
  sql <- paste0("SELECT tax_name, class FROM names WHERE tax_id = ", taxId,
                " AND class != 'scientific name' AND class != 'type material'",
                " AND class != 'authority'")
  data <- db_query(con, sql)
  setNames(data[["tax_name"]], nm=camelise(data[["class"]])) %||% NA_character_
}


dbGetTypeMaterial <- function(con, taxId) {
  sql <- paste0("SELECT tax_name FROM names WHERE tax_id = ", taxId,
                " AND class = 'type material'")
  db_query(con, sql, 1) %||% NA_character_
}


dbGetAuthority <- function(con, taxId) {
  sql <- paste0("SELECT tax_name FROM names WHERE tax_id = ", taxId,
                " AND class = 'authority'")
  db_query(con, sql, 1) %||% NA_character_
}


dbGetRank <- function(con, taxId) {
  sql <- paste0("SELECT rank FROM nodes WHERE tax_id = ", taxId)
  db_query(con, sql, 1) %||% NA_character_
}


#' @return TaxId, ParentId, ScientificName, Rank
dbGetNode <- function(con, taxId) {
  sql <- paste0("SELECT tax_id, parent_id, tax_name, rank FROM nodes JOIN names",
                " USING ( tax_id ) WHERE tax_id = ", taxId,
                " AND class = 'scientific name'")
  db_query(con, sql)
}


dbGetLineage <- function(con, taxId) {
  Lineage( (function (con, taxId) {
    node <- dbGetNode(con, taxId)
    parentId <- node[["parent_id"]]
    lineage <- cbind(tax_id=node[["tax_id"]],
                     tax_name=node[["tax_name"]],
                     rank=node[["rank"]])
    if (length(parentId) > 0 && parentId != taxId)
      lineage <- rbind(Recall(con, parentId), lineage)
    lineage
  })(con, taxId)[-1,] )
}


camelise <- function(s) {
  if (is.null(s))
    return( s )
  cap <- function(s) paste0(toupper(substring(s, 1, 1)),
                            substring(s, 2), collapse="")
  gsub("[[:punct:]]", "", sapply(strsplit(s, " "), cap))
}

