#ncbi

`ncbi` allows to search, download and parse data records from various NCBI
databases. 

This R package builds on the interface to the NCBI Entrez facilities provided
by the package `rentrez` but attempts to parse the retrieved XML or text
records into native R objects.

Sequences in fasta format are returned as `AAStringSet` or `DNAStringSet`
objects (Package `Biostrings`). GenBank flat files are returned as `gbRecord`
objects (Package `biofiles`). Taxon records are returned as `taxon` or 
`taxonList` objects (Package `ncbi`), Pubmed records are returned as `bibentry`s.

Currently implemented are functions for the sequence databases _protein_,
_nuccore_, _nucgss_, and _nucest_, as well as for _pubmed_ and _taxonomy_.

##Install

`ncbi` depends on functionality provided by packages
currently not available from CRAN: `rentrez`, `biofiles`, and `rmisc`

Use Hadley Wickham's [devtools](https://github.com/hadley/devtools) to 
install:

```r     
require(devtools)
install_github("rmisc", "gschofl")
install_github("rentrez", "gschofl", "no_parsing")
install_github("biofiles", "gschofl")
install_github("ncbi", "gschofl")
```

##Examples

###Retrieving protein sequences for _Chlamydia psittaci_ 6BC:

Let's first retrieve the accession numbers for all proteins for _C. psittaci_
6BC. As a default `retmax` is set to 100. To get all accession numbers we set
`retmax = NULL`

```r  
psit_acc <- protein("Chlamydia psittaci 6BC[ORGN]", rettype="acc", retmax=NULL)
length(psit_acc)
    #  [1] 3500
psit_acc[1:10]
    #  [1] "Q46203.1"       "P59691.2"       "F0T376.1"      
    #  [4] "Q46225.1"       "YP_005663704.1" "YP_005663702.1"
    #  [7] "YP_005663700.1" "YP_005663698.1" "YP_005663703.1"
    # [10] "YP_005663701.1"
```

Download the first 10 proteins

```r  
psit_seq <- protein(psit_acc[1:10])
psit_seq
    #    A AAStringSet instance of length 10
    #      width seq                                                 names               
    #  [1]   402 MKKLLKSALLFAATGSALSLQALP...KWSITGEARLINERAAHMNAQFRF Q46203.1 RecName:...
    #  [2]   505 MAWENVRVRVAPSPTGDPHVGTAY...PKKIQTTIDKVLKEEDFENKIFEF P59691.2 RecName:...
    #  [3]    87 MKKAVLLATVFCGVVGLTSCCRIV...SECNPGVQGPQAKGCTSLDGRCKQ F0T376.1 RecName:...
    #  [4]   269 MFSDKMILIAGPCVIEEEETTLEI...TFEALLPIWNQLYQCVRSFEMAAV Q46225.1 RecName:...
    #  [5]   335 MRKGNSLHRNLDTIFGNEVYSNNL...SSKMIYAYDKSTIADNASKKVSLI YP_005663704.1 si...
    #  [6]   347 MVKSENQIIKSSLHLENQKFGRKP...FYLNKSKFQQISTNKTIQSTTNKN YP_005663702.1 hy...
    #  [7]   259 MKTLAFCSFKGGTGKTTLSLNIGS...DILNLTKEIENKLFSDEKLVQETL YP_005663700.1 sp...
    #  [8]   307 MSGLSHHHKSRLFLTVLEAANVWL...STQMLSAYDTGMTDNLTSQLPLIF YP_005663698.1 si...
    #  [9]   458 MIKQSEEEKDLLDVEFFVLGQAVN...GSVFSTTLRFNSCTGKFTIQKEAW YP_005663703.1 re...
    # [10]   264 MGNSGFYLNDTQNCVFADNIKLGQ...GDSILGTTATSNISFLEVKQQTNG YP_005663701.1 hy...
```

Look at the metadata that comes with the sequences


```r
require(Biostrings)
names(elementMetadata(psit_seq))
    # [1] "gi"      "accver"  "sid"     "local"   "taxid"   "orgname" "defline" "length"
> elementMetadata(psit_seq)[["defline"]]
    #  [1] "RecName: Full=Major outer membrane porin; Short=MOMP; Flags: Precursor"                                                                                                                                                                                  
    #  [2] "RecName: Full=Glutamate--tRNA ligase; AltName: Full=Glutamyl-tRNA synthetase; Short=GluRS"                                                                                                                                                               
    #  [3] "RecName: Full=Small cysteine-rich outer membrane protein omcA; Short=Small-CRP; AltName: Full=9 kDa cysteine-rich lipoprotein; Short=9KD-CRP; Flags: Precursor"                                                                                          
    #  [4] "RecName: Full=2-dehydro-3-deoxyphosphooctonate aldolase; AltName: Full=3-deoxy-D-manno-octulosonic acid 8-phosphate synthase; AltName: Full=KDO-8-phosphate synthase; Short=KDO 8-P synthase; Short=KDOPS; AltName: Full=Phospho-2-dehydro-3-deoxyocton>"
    #  [5] "site-specific recombinase, phage integrase family [Chlamydophila psittaci 6BC]"                                                                                                                                                                          
    #  [6] "hypothetical protein G5O_p0005 [Chlamydophila psittaci 6BC]"                                                                                                                                                                                             
    #  [7] "sporulation initiation inhibitor protein soj [Chlamydophila psittaci 6BC]"                                                                                                                                                                               
    #  [8] "site-specific recombinase, phage integrase family [Chlamydophila psittaci 6BC]"                                                                                                                                                                          
    #  [9] "replicative DNA helicase [Chlamydophila psittaci 6BC]"                                                                                                                                                                                                   
    # [10] "hypothetical protein G5O_p0004 [Chlamydophila psittaci 6BC]"        
```

Download the full Genbank entry for a protein


```r
gi5 <- elementMetadata(psit_seq)[["gi"]][5]
p <- protein(gi5, "gp")
    # Parsing features into “file63845f2468ac.db”
p
    # ‘gbRecord’ database ‘file63845f2468ac.db’ with 8 features
    # LOCUS       YP_005663704 335 aa AA linear BCT 27-SEP-2012
    # DEFINITION  site-specific recombinase, phage integrase family
    #              [Chlamydophila psittaci 6BC].
    # ACCESSION   YP_005663704
    # VERSION     YP_005663704.1 GI:384451105
    # DBLINK      Project: 159845
    # DBSOURCE    REFSEQ: accession NC_017288.1
    # KEYWORDS    .
    # SOURCE      Chlamydophila psittaci 6BC (Chlamydia psittaci 6BC)
    #   ORGANISM  Chlamydophila psittaci 6BC
    #             Bacteria; Chlamydiae; Chlamydiales; Chlamydiaceae;
    #              Chlamydia/Chlamydophila group; Chlamydia.
    # REFERENCE   Not implemented yet
    # COMMENT     PROVISIONAL REFSEQ: This record has not yet been subject
    #              to final NCBI review. The reference sequence is identical
    #              to AEB56031. Method: conceptual translation.
    # ORIGIN      mrkgnslhrnldtifgnevysnnlthqeflsvrnqhlweklrdiplyeavhtwlssltnh
    #              ...
    #              hipfkvtphvlratavteykkmgcsdsdimkitghssskmiyaydkstiadnaskkvsli

require(biofiles)
features(p)
    # 'gbFeatureList' with 8 features:
    # 
    # Feature:         Location/Qualifiers:
    #  source          1..335
    #                  /organism="Chlamydophila psittaci 6BC"
    #                  /strain="6BC"
    #                  /culture_collection="ATCC:VR-125"
    #                  /db_xref="taxon:331636"
    #                  /plasmid="pCps6BC"
    # 
    # ...
    # Feature:         Location/Qualifiers:
    #  CDS             1..335
    #                  /locus_tag="G5O_p0007"
    #                  /coded_by="NC_017288.1:6479..7486"
    #                  /transl_table="11"
    #                  /db_xref="GeneID:12242274"
```

###Retrieving taxon information for Chlamydiaceae:

Which taxa within the family _Chlamydiaceae_ are on NCBI

```r
tx <- taxon("Chlamydiaceae[subtree]", "uilist", retmax=NULL)
length(tx)
    # [1] 233
head(tx, 10)
    # [1] 1238237 1238236 1238235 1238234 1238233 1234369 1234368 1234367
    # [9] 1221877 1218358
```

Download all taxon records

```r
txlist <- taxon("Chlamydiaceae[subtree]", retmax=NULL)
txlist
    # A ‘taxonList’ instance of length 233
    # Chlamydia psittaci 10_1398_11 (1238237; no rank) 
    # Chlamydia psittaci 10_743_SC13 (1238236; no rank) 
    # Chlamydia psittaci 10_881_SC42 (1238235; no rank) 
    # Chlamydia psittaci 10_1957 (1238234; no rank) 
    # Chlamydia psittaci 10_2393 (1238233; no rank) 
    # Chlamydia pecorum P787 (1234369; no rank) 
    # Chlamydia pecorum W73 (1234368; no rank) 
    # Chlamydia pecorum PV3056/3 (1234367; no rank) 
    # Chlamydia psittaci 01DC12 (1221877; no rank) 
    # Chlamydia psittaci WC (1218358; no rank)
      .
      .
      .
```

The scientific names of all those recognized as species

```r
sciName(txlist)[taxRank(txlist) == 'species']
    #  [1] "Chlamydia sp. 09G3706"                 "Chlamydia sp. 09G3705"                
    #  [3] "Chlamydia sp. 08-1274/23"              "Chlamydia sp. 08-1274/21"             
    #  [5] "Chlamydia sp. 08-1274/19"              "Chlamydia sp. 08-1274/13"             
    #  [7] "Chlamydia sp. 08-1274/3"               "Chlamydia sp. 10-1957/2"              
    #  [9] "Chlamydia sp. 10-2393/6"               "Chlamydia sp. 09-424/4"               
    # [11] "Chlamydia sp. 09-130/5"                "Chlamydia sp. 09-130/4"               
    # [13] "Chlamydophila sp. 09-589/S46"          "Chlamydophila sp. 09-489/LP23"        
    # [15] "Chlamydiaceae bacterium 10-1398/6"     "Chlamydiaceae bacterium 10-1398/11"   
    # [17] "Chlamydophila sp. S384"                "Chlamydophila sp. C312-513"           
    # [19] "Chlamydophila sp. 08-1274_Flock9"      "Chlamydophila sp. 08-1274_Flock3"     
    # [21] "Chlamydophila sp. 08-1274_Flock23"     "Chlamydophila sp. 08-1274_Flock22"    
    # [23] "Chlamydophila sp. 08-1274_Flock21"     "Chlamydophila sp. 08-1274_Flock19"    
    # [25] "Chlamydophila sp. 08-1274_Flock13"     "Candidatus Clavochlamydia salmonicola"
    # [27] "uncultured Chlamydophila sp."          "Chlamydophila sp. 6617-T5"            
    # [29] "Chlamydophila sp. 6620-T4"             "Chlamydophila sp. 6688-T2"            
    # [31] "uncultured Chlamydiaceae bacterium"    "Chlamydophila sp. Rostinovo-70"       
    # [33] "Chlamydia sp. CH301104"                "cf. Chlamydia sp. RLUH-1"             
    # [35] "uncultured Chlamydia sp."              "uncultured Chlamydia suis CTRAUS5"    
    # [37] "Chlamydophila sp. CPSCROC"             "Chlamydophila sp. CPSSNAKE"           
    # [39] "Chlamydophila sp. PEENT"               "Chlamydiaceae gen. sp. EQVAG"         
    # [41] "Chlamydia cf. pneumoniae CPXT1"        "Chlamydia pecorum"                    
    # [43] "Chlamydia muridarum"                   "Chlamydia suis"                       
    # [45] "Chlamydia pneumoniae"                  "Chlamydophila caviae"                 
    # [47] "Chlamydophila felis"                   "Chlamydophila abortus"                
    # [49] "Chlamydia psittaci"                    "Chlamydia sp."                        
    # [51] "Chlamydia trachomatis"          
```

###Retrieving data from Pubmed:

Search PubMed for all publications with _Chlamydia psittaci_ in the title
in 2012

```r
require(rentrez)
pmid <- esearch("Chlamydia psittaci[TITL]", "pubmed", mindate=2012, maxdate=2012)
pmid
    # ESearch query using the ‘pubmed’ database.
    # Query term: ‘Chlamydia psittaci[TITL] AND 2012[EDAT] : 2012[EDAT]’
    # Total number of hits: 9
    # Number of hits retrieved: 9
    # [1] "23098816" "22957128" "22689815" "22506068" "22472082" "22382892" "22302240"
    # [8] "22299031" "22296995"
```

Now fetch the first 5 of these records from pubmed

```r
p <- pubmed(idList(pmid)[1:5])
p
    # Yin L, Kalmar I, Lagae S, Vandendriessche S, Vanderhaeghen W,
    # Butaye P, Cox E and Vanrompay D (2012). “Emerging Chlamydia
    # psittaci infections in the chicken industry and pathology of
    # Chlamydia psittaci genotype B and D strains in specific pathogen
    # free chickens.” _Veterinary microbiology_. ISSN 1873-2542, <URL:
    # http://dx.doi.org/10.1016/j.vetmic.2012.09.026>.
    # 
    # Blomqvist M, Christerson L, Waldenström J, Lindberg P, Helander B,
    # Gunnarsson G, Herrmann B and Olsen B (2012). “Chlamydia psittaci in
    # birds of prey, Sweden.” _Infection ecology & epidemiology_, *2*.
    # ISSN 2000-8686, <URL: http://dx.doi.org/10.3402/iee.v2i0.8435>.
    # 
    # Braukmann M, Sachse K, Jacobsen I, Westermann M, Menge C, Saluz H
    # and Berndt A (2012). “Distinct intensity of host-pathogen
    # interactions in Chlamydia psittaci- and Chlamydia abortus-infected
    # chicken embryos.” _Infection and immunity_, *80*(9), pp. 2976-88.
    # ISSN 1098-5522, <URL: http://dx.doi.org/10.1128/IAI.00437-12>.
    # 
    # Voigt A, Schöfl G and Saluz H (2012). “The Chlamydia psittaci
    # genome: a comparative analysis of intracellular pathogens.” _PloS
    # one_, *7*(4), pp. e35097. ISSN 1932-6203, <URL:
    # http://dx.doi.org/10.1371/journal.pone.0035097>.
    # 
    # Collina F, De Chiara A, De Renzo A, De Rosa G, Botti G and Franco R
    # (2012). “Chlamydia psittaci in ocular adnexa MALT lymphoma: a
    # possible role in lymphomagenesis and a different geographical
    # distribution.” _Infectious agents and cancer_, *7*, pp. 8. ISSN
    # 1750-9378, <URL: http://dx.doi.org/10.1186/1750-9378-7-8>.
```

Which journals where they published in

```r
journal(p)
    # [[1]]
    # [1] "Veterinary microbiology"
    # attr(,"abbrev")
    # [1] "Vet. Microbiol."
    # attr(,"issn")
    # [1] "1873-2542"
    # 
    # [[2]]
    # [1] "Infection ecology & epidemiology"
    # attr(,"abbrev")
    # [1] "Infect Ecol Epidemiol"
    # attr(,"issn")
    # [1] "2000-8686"
    # 
    # [[3]]
    # [1] "Infection and immunity"
    # attr(,"abbrev")
    # [1] "Infect. Immun."
    # attr(,"issn")
    # [1] "1098-5522"
    # 
    # [[4]]
    # [1] "PloS one"
    # attr(,"abbrev")
    # [1] "PLoS ONE"
    # attr(,"issn")
    # [1] "1932-6203"
    # 
    # [[5]]
    # [1] "Infectious agents and cancer"
    # attr(,"abbrev")
    # [1] "Infect. Agents Cancer"
    # attr(,"issn")
    # [1] "1750-9378"
```
