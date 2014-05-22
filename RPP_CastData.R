# SETUP -------------------------------------------------------------------

# Libraries
require(xlsx)
require(MBESS)
require(RCurl)

# SOURCE GITHUB FUNCTIONS -------------------------------------------------

# [sciCure](http://fredhasselman.github.io/scicuRe/)
#
# Use this code to source it directly from GitHub:

source_https <- function(url, ...) {
  require(RCurl)
  # parse and evaluate each .R script
  sapply(c(url, ...), function(u) {
    eval(parse(text = getURL(u, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))), envir = .GlobalEnv)
  })
}
# Source the scicuRe_source.R toolbox!
source_https("https://raw.github.com/FredHasselman/scicuRe/master/scicuRe_source.R")
# The `source_https()` function was found [here](http://tonybreyal.wordpress.com/2011/11/24/source_https-sourcing-an-r-script-from-github/)


# # OTHER FUNCTIONS AND VARIABLES -----------------------------------------

# Search patterns 
real_pat <- paste("(\\d*(","(\\,|\\.)?","\\d+){0,2})",sep="[[:blank:]]?")
Fpat <- paste0("(?<type>F)[[:print:]]?[(](?<df1>",real_pat,")[,](?<df2>",real_pat,")[)]=(?<stat>",real_pat,")[[:print:]]*")
tpat <- paste0("(?<type>t)[(](?<df1>",real_pat,")[)]=(?<stat>[-]?",real_pat,")[[:print:]]*")
Xpat <- paste0("(?<type>X\\^2|χ2)[(](?<df1>",real_pat,")([,]N=(?<N>",real_pat,"))?[)]=(?<stat>",real_pat,")[[:print:]]*")


# LOAD MASTER DATA FROM GITHUB ---------------------------------------------------

## Latest RPP Spreadsheet was converted to tab delimeted file and uploaded to GitHub

# FINdata <- read.xlsx2("RPP master data 18-05-2106.xlsx",sheetName="Statisitics",startRow=2,endRow=32,stringsAsFactors=FALSE)
# write.table(FINdata,file="RPPdata_master.dat",sep="\t",row.names=T,fileEncoding="UTF-8",quote=F)

urltxt         <- getURL("https://raw.githubusercontent.com/FredHasselman/RPP/master/RPPdata_master.dat")
RPPdata_master <- read.delim(textConnection(urltxt),stringsAsFactors=F)
closeAllConnections()

# RE-CAST -----------------------------------------------------------------

# Cast info from RPPdata_master into new form
RPPdata_cast <- data.frame(
  WARNING = rep("MISSING TEST INFO: prediction:directed/undirected, samples:dependent/independent/equalN/equalVar, factor:Nlevels/between/within, effect:interaction/main/planned contrast/random/fixed, posthoc: yes/no/correction",nrow(RPPdata_master)),
  name=RPPdata_master[["ARTICLE.TITLE"]],
  OSFid=RPPdata_master[["OSF.ID"]],
  stat.ori.string=gsub("[[:blank:]]+","",RPPdata_master[["Test.statistic"]],perl=T),
  stat.ori.type=rep("unknown",nrow(RPPdata_master)),
  stat.ori.N=RPPdata_master[["N"]],
  stat.ori.N.recalc=rep(0,nrow(RPPdata_master)),
  stat.ori.df1=rep(0,nrow(RPPdata_master)),
  stat.ori.df2=rep(NA,nrow(RPPdata_master)),
  stat.ori.ncp.p=gsub("([[:blank:]<=>])+","",RPPdata_master[["p.value"]],perl=T),
  alpha.ori=rep(0.05,nrow(RPPdata_master)),
  stat.ori.ncp=rep(0,nrow(RPPdata_master)),
  stat.ori.ncp.ciL=rep(0,nrow(RPPdata_master)),
  stat.ori.ncp.ciU=rep(0,nrow(RPPdata_master)),
  stat.ori.ncp.ci.type=rep("unknown",nrow(RPPdata_master)),
  stat.ori.crit=rep(0,nrow(RPPdata_master)),
  stat.ori.ncp.p.recalc=rep(0,nrow(RPPdata_master)),
  prediction.ori=rep("not equal",nrow(RPPdata_master)),
  stat.ori.crit.p=rep(0,nrow(RPPdata_master)),
  stat.ori.H0=rep(NA,nrow(RPPdata_master)),
  stat.ori.H1=rep(NA,nrow(RPPdata_master)),
  stat.ori.decideNP=rep("Accept/Reject",nrow(RPPdata_master)),
  ES.ori.type=RPPdata_master[["ES.metric"]],
  ES.ori.value=RPPdata_master[["ES.value"]],
  ES.ori.value.recalc=rep(NA,nrow(RPPdata_master)),
  ES.ori.d=rep(0,nrow(RPPdata_master)),
  ES.ori.d.ciL=rep(0,nrow(RPPdata_master)),
  ES.ori.d.ciU=rep(0,nrow(RPPdata_master)),
  ES.ori.r=rep(0,nrow(RPPdata_master)),
  ES.ori.r.ciL=rep(0,nrow(RPPdata_master)),
  ES.ori.r.ciU=rep(0,nrow(RPPdata_master)),
  aprioriPOW.ori=rep("unknown",nrow(RPPdata_master)),
  posthocPOW.ori=rep(0,nrow(RPPdata_master)),
  aprioriSEV.ori=rep("unknown",nrow(RPPdata_master)),
  posthocSEV.ori=rep(0,nrow(RPPdata_master)),
  stat.rep.string=gsub("[[:blank:]]+","",RPPdata_master[["Replication..Statistic"]]),
  stat.rep.type=rep("unknown",nrow(RPPdata_master)),
  stat.rep.N=RPPdata_master[["N.1"]],
  stat.rep.N.recalc=rep(0,nrow(RPPdata_master)),
  stat.rep.df1=rep(0,nrow(RPPdata_master)),
  stat.rep.df2=rep(NA,nrow(RPPdata_master)),
  stat.rep.ncp.p=gsub("([[:blank:]<=>])+","",RPPdata_master[["p.value.1"]],perl=T),
  alpha.rep=rep(0.05,nrow(RPPdata_master)),
  stat.rep.ncp=rep(0,nrow(RPPdata_master)),
  stat.rep.ncp.ciL=rep(0,nrow(RPPdata_master)),
  stat.rep.ncp.ciU=rep(0,nrow(RPPdata_master)),
  stat.rep.ncp.ci.type=rep("unknown",nrow(RPPdata_master)),
  stat.rep.crit=rep(0,nrow(RPPdata_master)),
  stat.rep.ncp.p.recalc=rep(0,nrow(RPPdata_master)),
  prediction.rep=rep("not equal",nrow(RPPdata_master)),
  stat.rep.crit.p=rep(0,nrow(RPPdata_master)),
  stat.rep.H0=rep(NA,nrow(RPPdata_master)),
  stat.rep.H1=rep(NA,nrow(RPPdata_master)),
  stat.rep.decideNP=rep("Accept/Reject",nrow(RPPdata_master)),
  ES.rep.type=RPPdata_master[["ES.metric.1"]],
  ES.rep.value=RPPdata_master[["ES.value.1"]],
  ES.rep.value.recalc=rep(0,nrow(RPPdata_master)),
  ES.rep.d=rep(0,nrow(RPPdata_master)),
  ES.rep.d.ciL=rep(0,nrow(RPPdata_master)),
  ES.rep.d.ciU=rep(0,nrow(RPPdata_master)),
  ES.rep.r=rep(0,nrow(RPPdata_master)),
  ES.rep.r.ciL=rep(0,nrow(RPPdata_master)),
  ES.rep.r.ciU=rep(0,nrow(RPPdata_master)),
  aprioriPOW.rep=RPPdata_master[["Power"]],
  posthocPOW.rep=rep(0,nrow(RPPdata_master)),
  aprioriSEV.rep=rep("unknown",nrow(RPPdata_master)),
  posthocSEV.rep=rep(0,nrow(RPPdata_master)),
  stringsAsFactors=F
  )


# EXTRACT INFO BASED ON COPIED STAT STRINGS -------------------------------

# Original ----------------------------------------------------------------
idF <- grep(Fpat,RPPdata_cast$stat.ori.string,ignore.case = T, perl=T)
idt <- grep(tpat,RPPdata_cast$stat.ori.string,ignore.case = T, perl=T)
idX <- grep(Xpat,RPPdata_cast$stat.ori.string,ignore.case = T, perl=T)
RPPdata_cast["stat.ori.string"][idX, ] <- gsub("([xX]\\^2|χ2)","X^2",RPPdata_cast["stat.ori.string"][idX, ],perl=T)

for(s in 1:nrow(RPPdata_master)){
  
  if(s%in%idF){pat <- Fpat}
  if(s%in%idt){pat <- tpat}
  if(s%in%idX){pat <- Xpat}
  
  matchOri <- gregexpr(pat,RPPdata_cast["stat.ori.string"][s, ],ignore.case=T,perl=T)[[1]]
  
  for(field in attributes(matchOri)$capture.names){
    if(field!=""){
      fieldID <- attributes(matchOri)$capture.names==field
      if(attributes(matchOri)$capture.start[fieldID]!=-1){
        switch(field,
               type =  RPPdata_cast["stat.ori.type"][s, ] <- substr(RPPdata_cast["stat.ori.string"][s, ], start=attributes(matchOri)$capture.start[fieldID],stop=(attributes(matchOri)$capture.start[fieldID]+attributes(matchOri)$capture.length[fieldID]-1)),
               stat =  RPPdata_cast["stat.ori.ncp"][s, ] <- substr(RPPdata_cast["stat.ori.string"][s, ], start=attributes(matchOri)$capture.start[fieldID],stop=(attributes(matchOri)$capture.start[fieldID]+attributes(matchOri)$capture.length[fieldID]-1)),
               df1 =  RPPdata_cast["stat.ori.df1"][s, ] <- substr(RPPdata_cast["stat.ori.string"][s, ], start=attributes(matchOri)$capture.start[fieldID],stop=(attributes(matchOri)$capture.start[fieldID]+attributes(matchOri)$capture.length[fieldID]-1)),
               df2 =  RPPdata_cast["stat.ori.df2"][s, ] <- substr(RPPdata_cast["stat.ori.string"][s, ], start=attributes(matchOri)$capture.start[fieldID],stop=(attributes(matchOri)$capture.start[fieldID]+attributes(matchOri)$capture.length[fieldID]-1)),
               N =  RPPdata_cast["stat.ori.N"][s, ] <- substr(RPPdata_cast["stat.ori.string"][s, ], start=attributes(matchOri)$capture.start[fieldID],stop=(attributes(matchOri)$capture.start[fieldID]+attributes(matchOri)$capture.length[fieldID]-1))
        )
      }
    }
  }
  
}

idF <- grep(Fpat,RPPdata_cast[["stat.rep.string"]],ignore.case = T, perl=T)
idt <- grep(tpat,RPPdata_cast[["stat.rep.string"]],ignore.case = T, perl=T)
idX <- grep(Xpat,RPPdata_cast[["stat.rep.string"]],ignore.case = T,perl=T)
RPPdata_cast["stat.rep.string"][idX, ] <- gsub("([xX]\\^2|χ2)","X^2",RPPdata_cast["stat.rep.string"][idX, ],perl=T)


# Replication -------------------------------------------------------------

for(s in 1:nrow(RPPdata_master)){
  
  if(s%in%idF){pat <- Fpat}
  if(s%in%idt){pat <- tpat}
  if(s%in%idX){pat <- Xpat}
  
  # Replication
  matchRep <- gregexpr(pat,RPPdata_cast["stat.rep.string"][s, ],ignore.case=T,perl=T)[[1]]
  
  for(field in attributes(matchRep)$capture.names){
    if(field!=""){
      fieldID <- attributes(matchRep)$capture.names==field
      if(attributes(matchRep)$capture.start[fieldID]!=-1){
        switch(field,
               type =  RPPdata_cast["stat.rep.type"][s, ] <- substr(RPPdata_cast["stat.rep.string"][s, ], start=attributes(matchRep)$capture.start[fieldID],stop=(attributes(matchRep)$capture.start[fieldID]+attributes(matchRep)$capture.length[fieldID]-1)),
               stat =  RPPdata_cast["stat.rep.ncp"][s, ] <- substr(RPPdata_cast["stat.rep.string"][s, ], start=attributes(matchRep)$capture.start[fieldID],stop=(attributes(matchRep)$capture.start[fieldID]+attributes(matchRep)$capture.length[fieldID]-1)),
               df1 =  RPPdata_cast["stat.rep.df1"][s, ] <- substr(RPPdata_cast["stat.rep.string"][s, ], start=attributes(matchRep)$capture.start[fieldID],stop=(attributes(matchRep)$capture.start[fieldID]+attributes(matchRep)$capture.length[fieldID]-1)),
               df2 =  RPPdata_cast["stat.rep.df2"][s, ] <- substr(RPPdata_cast["stat.rep.string"][s, ], start=attributes(matchRep)$capture.start[fieldID],stop=(attributes(matchRep)$capture.start[fieldID]+attributes(matchRep)$capture.length[fieldID]-1)),
               N =  RPPdata_cast["stat.rep.N"][s, ] <- substr(RPPdata_cast["stat.rep.string"][s, ], start=attributes(matchRep)$capture.start[fieldID],stop=(attributes(matchRep)$capture.start[fieldID]+attributes(matchRep)$capture.length[fieldID]-1))
        )
      }
    }
  }
}


# RE-CALCULATE -------------------------------------------------------------

# Get basic info for original and replication studies.
#
# Rule for F and Chi^2 distributions: 
#  - Statistically significant effect   -> CI[alpha/2,alpha/2]
#  - Statistically insignificant effect -> CI[0,alpha]
# Warnings are raised for unknown test statistics and data set to NA

# Original
infer.ori <- ldply(seq_along(RPPdata_cast[,1]), function(s){
    cat(s,"\n")
    decideNP(stat.type=RPPdata_cast$stat.ori.type[[s]], 
             stat.ncp=as.numeric(RPPdata_cast$stat.ori.ncp[[s]]), 
             stat.df=c(as.numeric(RPPdata_cast$stat.ori.df1[[s]]),as.numeric(RPPdata_cast$stat.ori.df2[[s]])),
             stat.N=as.numeric(RPPdata_cast$stat.ori.N[[s]]), 
             alpha=0.05, CL=0.95, prediction="not equal")  
})

# Replication
infer.rep <- ldply(seq_along(RPPdata_cast[,1]), function(s){
    cat(s,"\n")
    decideNP(stat.type=RPPdata_cast$stat.rep.type[[s]], 
             stat.ncp=as.numeric(RPPdata_cast$stat.rep.ncp[[s]]), 
             stat.df=c(as.numeric(RPPdata_cast$stat.rep.df1[[s]]),as.numeric(RPPdata_cast$stat.rep.df2[[s]])),
             stat.N=as.numeric(RPPdata_cast$stat.rep.N[[s]]), 
             alpha=0.05, CL=0.95, prediction="not equal")
})

RPPdata_cast[ ,which(colnames(RPPdata_cast)=="stat.ori.ncp"):which(colnames(RPPdata_cast)=="stat.ori.decideNP")] <- infer.ori
RPPdata_cast[ ,which(colnames(RPPdata_cast)=="stat.rep.ncp"):which(colnames(RPPdata_cast)=="stat.rep.decideNP")] <- infer.rep

# Use test-stat CI sto calculate effectsize CIs
# Warnings are due to division during conversion and are set to NA

ESconv.ori <- ldply(seq_along(RPPdata_cast[,1]), function(s){
  cat(s,"\n")
  convertES(RPPdata_cast[["stat.ori.type"]][s],infer.ori[s,],stat.df=c(as.numeric(RPPdata_cast[["stat.ori.df1"]][s]),as.numeric(RPPdata_cast[["stat.ori.df1"]][s])))
  })

ESconv.rep <- ldply(seq_along(RPPdata_cast[,1]), function(s){
  cat(s,"\n")
  convertES(RPPdata_cast[["stat.rep.type"]][s],infer.rep[s,],stat.df=c(as.numeric(RPPdata_cast[["stat.rep.df1"]][s]),as.numeric(RPPdata_cast[["stat.rep.df1"]][s]))) 
  })

RPPdata_cast[ ,which(colnames(RPPdata_cast)=="ES.ori.d"):which(colnames(RPPdata_cast)=="ES.ori.r.ciU")] <- ESconv.ori
RPPdata_cast[ ,which(colnames(RPPdata_cast)=="ES.rep.d"):which(colnames(RPPdata_cast)=="ES.rep.r.ciU")] <- ESconv.rep

# And POW
for(s in seq_along(RPPdata_cast[,1])){
  cat(s,"\n")
  RPPdata_cast[s,"posthocPOW.ori"]<-posthocPOWer(RPPdata_cast[["stat.ori.type"]][s],infer.rep[s,],stat.df=c(as.numeric(RPPdata_cast[["stat.ori.df1"]][s]),as.numeric(RPPdata_cast[["stat.ori.df1"]][s])))
}

for(s in seq_along(RPPdata_cast[,1])){
  cat(s,"\n")
  RPPdata_cast[s,"posthocPOW.rep"]<-posthocPOWer(RPPdata_cast[["stat.rep.type"]][s],infer.rep[s,],stat.df=c(as.numeric(RPPdata_cast[["stat.rep.df1"]][s]),as.numeric(RPPdata_cast[["stat.rep.df1"]][s])))
}

# SAVE and EXPORT ---------------------------------------------------------

write.table(RPPdata_cast,file="RPPdata_cast.dat",sep="\t",row.names=T,fileEncoding="UTF-8",quote=F)
save(RPPdata_cast,file="RPPdata_cast.Rdata")

 
# Data are here
urltxt   <- getURL("https://raw.githubusercontent.com/FredHasselman/RPP/master/RPPdata_cast.dat")
RPPdata_cast  <- read.delim(textConnection(urltxt),stringsAsFactors=F)
closeAllConnections()
