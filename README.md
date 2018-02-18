


# ncbi

`ncbi` allows to search, download and parse data records from various NCBI
databases. 

This package builds on the interface to the NCBI Entrez facilities provided
by the package [rentrez](https://github.com/gschofl/rentrez) but attempts to
parse the retrieved XML or text records into native R objects.

Sequences in fasta format are returned as `AAStringSet` or `DNAStringSet`
objects. GenBank flat files are returned as `gbRecord`
objects (Package [biofiles](https://github.com/gschofl/biofiles)). Taxon
records are returned as `taxon` or `taxonList` objects, PubMed records are returned as `pubmed` objects.

Currently implemented are functions for the sequence databases _protein_,
_nuccore_, _nucgss_, and _nucest_, as well as for _pubmed_ and _taxonomy_.

### Installation

This package is currently only available via github. It depends on a number
of utility functions provided in my [rmisc](https://github.com/gschofl/rmisc) package. Use Hadley Wickham's [devtools](https://github.com/hadley/devtools)
to install:

`ncbi` depends on functionality provided by packages
currently not available from CRAN:
[rentrez](https://github.com/gschofl/rentrez),
[biofiles](https://github.com/gschofl/biofiles), and
[rmisc](https://github.com/gschofl/rmisc).

Use Hadley Wickham's [devtools](https://github.com/hadley/devtools) to 
install:



```r
require(devtools)
install_github("rmisc", "gschofl")
install_github("rentrez", "gschofl")
install_github("biofiles", "gschofl")
install_github("ncbi", "gschofl")
```




### Important functions

#### `protein`, `nucleotide`, `EST`, and `GSS`


#### `parseTSeqSet`, and `parseGenBank`


#### `pubmed`, `parsePubmed`, and `browsePubmed`


#### `taxon` and `parseTaxon`


### Examples

#### Retrieving protein sequences for _Chlamydia psittaci_ 6BC:

Let's first retrieve the accession numbers for all proteins for _C. psittaci_
6BC. As a default `retmax` is set to 100. To get all accession numbers we set
`retmax = NULL` and `rettype='acc'`.


```r
psit_acc <- protein("Chlamydia psittaci 6BC[ORGN]", rettype = "acc", retmax = NULL)
psit_acc

 ##    [1] "Q46203.1"       "F0T4D1.1"       "Q46225.1"       "F0T376.1"      
 ##    [5] "YP_005663703.1" "YP_005663701.1" "YP_005663699.1" "YP_005663704.1"
 ##    [9] "YP_005663702.1" "YP_005663700.1" "YP_005663698.1" "YP_005663696.1"
 ##   [13] "YP_005663694.1" "YP_005663692.1" "YP_005663690.1" "YP_005663688.1"
....
```




Download the first 10 proteins. The default retrieval type is `fasta` and gets
parsed into an `AAStringSet` or `DNAStringSet`. Parsing can be turned of
by setting the attribute `parse = FALSE`. 


```r
psit_seq <- protein(psit_acc[1:10])
psit_seq

 ##    A AAStringSet instance of length 10
 ##       width seq                                         names               
 ##   [1]   402 MKKLLKSALLFAATGSALSL...TGEARLINERAAHMNAQFRF Q46203.1 RecName:...
 ##   [2]   433 MIKGRRTKLHTFLYDCFLIF...KVAFDRTWESFKRYIPCVKI F0T4D1.1 RecName:...
....
```






```r
protein(psit_acc[1:10], parse = FALSE)

 ##  <?xml version="1.0"?>
 ##  <!DOCTYPE TSeqSet PUBLIC "-//NCBI//NCBI TSeq/EN" "http://www.ncbi.nlm.nih.gov/dtd/NCBI_TSeq.dtd">
 ##  <TSeqSet>
 ##    <TSeq>
 ##      <TSeq_seqtype value="protein"/>
 ##      <TSeq_gi>75428263</TSeq_gi>
 ##      <TSeq_accver>Q46203.1</TSeq_accver>
 ##      <TSeq_taxid>331636</TSeq_taxid>
....
```




Look at the metadata that comes with the sequences


```r
require(Biostrings)
names(elementMetadata(psit_seq))

 ##  [1] "gi"      "accver"  "sid"     "local"   "taxid"   "orgname" "defline"
 ##  [8] "length"
```






```r
elementMetadata(psit_seq)[["defline"]]

 ##   [1] "RecName: Full=Major outer membrane porin; Short=MOMP; Flags: Precursor"                                                                                                                                                                                  
 ##   [2] "RecName: Full=3-deoxy-D-manno-octulosonic acid transferase; Short=Kdo transferase; AltName: Full=Kdo(2)-lipid IV(A) 3-deoxy-D-manno-octulosonic acid transferase; AltName: Full=Kdo(3)-lipid IV(A) 3-deoxy-D-manno-octulosonic acid transferase; AltNam>"
 ##   [3] "RecName: Full=2-dehydro-3-deoxyphosphooctonate aldolase; AltName: Full=3-deoxy-D-manno-octulosonic acid 8-phosphate synthase; AltName: Full=KDO-8-phosphate synthase; Short=KDO 8-P synthase; Short=KDOPS; AltName: Full=Phospho-2-dehydro-3-deoxyocton>"
 ##   [4] "RecName: Full=Small cysteine-rich outer membrane protein omcA; Short=Small-CRP; AltName: Full=9 kDa cysteine-rich lipoprotein; Short=9KD-CRP; Flags: Precursor"                                                                                          
 ##   [5] "replicative DNA helicase [Chlamydophila psittaci 6BC]"                                                                                                                                                                                                   
 ##   [6] "hypothetical protein G5O_p0004 [Chlamydophila psittaci 6BC]"                                                                                                                                                                                             
 ##   [7] "hypothetical protein G5O_p0002 [Chlamydophila psittaci 6BC]"                                                                                                                                                                                             
 ##   [8] "site-specific recombinase, phage integrase family [Chlamydophila psittaci 6BC]"                                                                                                                                                                          
 ##   [9] "hypothetical protein G5O_p0005 [Chlamydophila psittaci 6BC]"                                                                                                                                                                                             
 ##  [10] "sporulation initiation inhibitor protein soj [Chlamydophila psittaci 6BC]"
....
```




Fetch the full Genbank entry for a protein


```r
gi5 <- elementMetadata(psit_seq)[["gi"]][5]
p <- protein(gi5, "gp")

 ##  Parsing features into "file7a619c5daf2.db"
r
p

 ##  'gbRecord' database 'file7a619c5daf2.db' with 10 features
 ##  LOCUS       YP_005663703 458 aa AA linear BCT 27-SEP-2012
 ##  DEFINITION  replicative DNA helicase [Chlamydophila psittaci 6BC].
 ##  ACCESSION   YP_005663703
 ##  VERSION     YP_005663703.1 GI:384451104
 ##  DBLINK      Project: 159845
 ##  DBSOURCE    REFSEQ: accession NC_017288.1
 ##  KEYWORDS    .
 ##  SOURCE      Chlamydophila psittaci 6BC (Chlamydia psittaci 6BC)
 ##    ORGANISM  Chlamydophila psittaci 6BC
 ##              Bacteria; Chlamydiae; Chlamydiales; Chlamydiaceae;
 ##               Chlamydia/Chlamydophila group; Chlamydia.
 ##  REFERENCE   Not implemented yet
 ##  COMMENT     PROVISIONAL REFSEQ: This record has not yet been subject
 ##               to final NCBI review. The reference sequence is identical
 ##               to AEB56030. Method: conceptual translation.
 ##  ORIGIN      mikqseeekdlldveffvlgqavnylehahvvvrrlsehhfksenhknifllirdilrdrd
 ##               ...
 ##               gqieqdadvilflhrkdyysqeatkglseiivgknrhgsvfsttlrfnsctgkftiqkeaw
```






```r
require(biofiles)
features(p)

 ##  'gbFeatureList' with 10 features:
 ##  
 ##  Feature:         Location/Qualifiers:
 ##   source          1..458
 ##                   /organism="Chlamydophila psittaci 6BC"
 ##                   /strain="6BC"
 ##                   /culture_collection="ATCC:VR-125"
 ##                   /db_xref="taxon:331636"
 ##                   /plasmid="pCps6BC"
 ##  
....
```




#### Retrieving taxon information for Chlamydiaceae:

Which taxa within the family _Chlamydiaceae_ are on NCBI


```r
tx <- taxon("Chlamydiaceae[subtree]", "uilist", retmax = NULL)
length(tx)

 ##  [1] 254
r
head(tx, 10)

 ##   [1] 1283074 1282969 1282882 1282662 1282442 1260223 1260222 1238237
 ##   [9] 1238236 1238235
```




Download all taxon records


```r
txlist <- taxon("Chlamydiaceae[subtree]", retmax = NULL)
txlist

 ##  A 'taxonList' instance of length 254
 ##  Chlamydia psittaci FalTex (1283074; no rank) 
 ##  Chlamydia psittaci 6BC/83 (1282969; no rank) 
 ##  Chlamydia psittaci CT1 (1282882; no rank) 
 ##  Chlamydia psittaci str. Borg (1282662; no rank) 
 ##  Chlamydia psittaci RTH (1282442; no rank) 
 ##  Chlamydia trachomatis IU888 (1260223; no rank) 
 ##  Chlamydia trachomatis IU824 (1260222; no rank) 
 ##  Chlamydia psittaci 10_1398_11 (1238237; no rank) 
 ##  Chlamydia psittaci 10_743_SC13 (1238236; no rank) 
....
```




The scientific names of all those recognized as species


```r
sciName(txlist)[taxRank(txlist) == "species"]

 ##   [1] "Chlamydia sp. 10DC88"                 
 ##   [2] "Chlamydia sp. 12DC97"                 
 ##   [3] "Chlamydia sp. 12DC96"                 
 ##   [4] "Chlamydia sp. PV_7344/2"              
 ##   [5] "Chlamydia sp. PV_7341/13"             
 ##   [6] "Chlamydia sp. PV_58394/2012"          
 ##   [7] "Chlamydia sp. PV_48558/2010"          
 ##   [8] "Chlamydia sp. PV_3954/22"             
 ##   [9] "Chlamydia sp. PV_3515/3"              
 ##  [10] "Chlamydia sp. PV_2863/2"              
....
```




#### Retrieving data from Pubmed:

First we search PubMed for all publications with _Chlamydia psittaci_ in the
title from 2010 to 2012. We use the `esearch` function from [rentrez](https://github.com/gschofl/rentrez) for greater control over the search.


```r
require(rentrez)
pmid <- esearch("Chlamydia psittaci[TITL] and ", "pubmed", mindate = 2010, maxdate = 2012)
pmid

 ##  ESearch query using the 'pubmed' database.
 ##  Query term: 'Chlamydia psittaci[TITL] AND 2010[EDAT] : 2012[EDAT]'
 ##  Total number of hits: 24
 ##  Number of hits retrieved: 24
 ##   [1] "23265868" "23227890" "23209198" "23189195" "23098816" "22957128"
 ##   [7] "22689815" "22506068" "22472082" "22382892" "22302240" "22299031"
 ##  [13] "22296995" "21921110" "21885218" "21846923" "21791668" "21761223"
 ##  [19] "21622741" "21393457" "21173126" "21152037" "20807089" "22802266"
```




Now we fetch the first 5 of these records from PubMed. We can extract the 
PMIDs from the `esearch` object using the function `idList` and pass them to
`pubmed` or, more conveniently, simply pass on the subsetted `esearch object.


```r
ids <- idList(pmid)
ids[1:5]

 ##  [1] "23265868" "23227890" "23209198" "23189195" "23098816"
r
p <- pubmed(pmid[1:5])
p

 ##  A 'pubmed' instance of length 5
 ##  [1] Pmid: 23265868	References: 1
 ##  -------------------------------------------------------------------
 ##  Ostermann C, Schroedl W, Schubert E, Sachse K and Reinhold P
 ##  (2012). "Dose-dependent effects of Chlamydia psittaci infection on
 ##  pulmonary gas exchange, innate immunity and acute-phase reaction
 ##  in a bovine respiratory model." _Veterinary journal (London,
 ##  England : 1997)_. ISSN 1532-2971, <URL:
 ##  https://doi.org/10.1016/j.tvjl.2012.10.035>.
 ##  
 ##  [2] Pmid: 23227890	References: 1
 ##  -------------------------------------------------------------------
 ##  Gartrell B, French N, Howe L, Nelson N, Houston M, Burrows E,
 ##  Russell J and Anderson S (2012). "First detection of Chlamydia
 ##  psittaci from a wild native passerine bird in New Zealand." _New
 ##  Zealand veterinary journal_. ISSN 0048-0169, <URL:
 ##  https://doi.org/10.1080/00480169.2012.740656>.
 ##  
....
```




Which journals where they published in?


```r
journal(p)

 ##  [[1]]
 ##  [1] "Veterinary journal (London, England : 1997)"
 ##  attr(,"abbrev")
 ##  [1] "Vet. J."
 ##  attr(,"issn")
 ##  [1] "1532-2971"
 ##  
 ##  [[2]]
 ##  [1] "New Zealand veterinary journal"
 ##  attr(,"abbrev")
 ##  [1] "N Z Vet J"
 ##  attr(,"issn")
....
```




Which are the authors?


```r
author(p)

 ##  [[1]]
 ##  [1] "Carola Ostermann" "Wieland Schroedl" "Evelyn Schubert" 
 ##  [4] "Konrad Sachse"    "Petra Reinhold"  
 ##  
 ##  [[2]]
 ##  [1] "Bd Gartrell" "Np French"   "L Howe"      "Nj Nelson"   "M Houston"  
 ##  [6] "Ea Burrows"  "Jc Russell"  "Sh Anderson"
 ##  
 ##  [[3]]
 ##  [1] "Sarah Van Lent"         "Jurgen R Piet"         
 ##  [3] "Delphine Beeckman"      "Arie van der Ende"     
 ##  [5] "Filip Van Nieuwerburgh" "Patrik Bavoil"         
....
```




And finally which are the DOIs?


```r
doi(p)

 ##   [1] doi:10.1016/j.tvjl.2012.10.035
 ##   [2] doi:10.1080/00480169.2012.740656
 ##   [3] doi:10.1128/JB.01828-12
 ##   [4] doi:10.1371/journal.pone.0050327
 ##   [5] doi:10.1016/j.vetmic.2012.09.026
```








