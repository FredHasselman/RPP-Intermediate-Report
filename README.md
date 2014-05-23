RPP Frequentist Committee Analyses
===
     
All R scripts in this hub require:
 * Source file `scicuRe_source.R` sourceable directly from GitHub (see below)    
 * Data files available in this repository (see below)    
    
Data Version Control Strategy:    
    
Latest Spreadsheet             -> `RPPdata_master.dat` on GitHub    
Cast into `RPPdata_master.dat` -> `RPPdata_cast.dat` on GitHub    
  
Subsequent files add `_XYZ` to prefix `RPPdata_cast`


R code below takes care of sourcing the appropriate functions and downloading the approriate data:
```
# SETUP -------------------------------------------------------------------
require(RCurl)
require(devtools)

# SOURCE FROM GITHUB -------------------------------------------------

# [sciCure](http://fredhasselman.github.io/scicuRe/)
#
# Use this code to source it directly from GitHub:
source_url("https://raw.githubusercontent.com/FredHasselman/scicuRe/master/scicuRe_source.R")


# LOAD DATA FROM GITHUB ---------------------------------------------------

## Latest RPP Spreadsheet was converted to tab delimeted file and uploaded to GitHub

# FINdata <- read.xlsx2("RPP master data 18-05-2106.xlsx",sheetName="Statisitics",startRow=2,endRow=32,stringsAsFactors=FALSE)
# write.table(FINdata,file="RPPdata_master.dat",sep="\t",row.names=T,fileEncoding="UTF-8",quote=F)

urltxt    <- getURL("https://raw.githubusercontent.com/FredHasselman/RPP/master/RPPdata_master.dat")
RPPdata_master <- read.delim(textConnection(urltxt),stringsAsFactors=F)
closeAllConnections()

## Complete data after running RPPmasterdata.R

urltxt   <- getURL("https://raw.githubusercontent.com/FredHasselman/RPP/master/RPPdata_cast.dat")
RPPdata_cast  <- read.delim(textConnection(urltxt),stringsAsFactors=F)
closeAllConnections()
```
