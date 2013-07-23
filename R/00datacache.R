## register environment for caching taxon nodes
## not exported
.lineage.cache <- new.env(hash=TRUE, parent=emptyenv())

## register valid rank designations on NCBI
## not exported
.ranks  <- c("root","superkingdom","kingdom","subkingdom","superphylum",
             "phylum","subphylum","superclass","class","subclass","infraclass",
             "superorder","order","suborder","parvorder","infraorder",
             "superfamily","family","subfamily","tribe","subtribe","genus",
             "subgenus","species group","species subgroup","species","subspecies",
             "varietas","forma","no rank")

## register SQL tables
## not exported
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
