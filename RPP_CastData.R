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

# Other functions

# Search patterns 
real_pat <- paste("(\\d*(","(\\,|\\.)?","\\d+){0,2})",sep="[[:blank:]]?")
Fpat <- paste0("(?<type>F)[[:print:]]?[(](?<df1>",real_pat,")[,](?<df2>",real_pat,")[)]=(?<stat>",real_pat,")[[:print:]]*")
tpat <- paste0("(?<type>t)[(](?<df1>",real_pat,")[)]=(?<stat>[-]?",real_pat,")[[:print:]]*")
Xpat <- paste0("(?<type>X\\^2|χ2)[(](?<df1>",real_pat,")([,]N=(?<N>",real_pat,"))?[)]=(?<stat>",real_pat,")[[:print:]]*")

# Monster function to read the data... all best I could do for now
recalcSTAT <- function(type,ncp,df1,df2,N,CL=rep(.95,length(type))){
  recalcData <- data.frame(type=type,ncp=ncp,df1=df1,df2=df2,ncp.CIL=rep(0,length(type)),ncp.CIU=rep(0,length(type)),p.recalc=rep(0,length(type)),N.recalc=rep(0,length(type)),posthocPOWER=rep(0,length(type)),ES.r.recalc=rep(0,length(type)),ES.r.CIL=rep(0,length(type)),ES.r.CIU=rep(0,length(type)),ES.d.recalc=rep(0,length(type)),ES.d.CIL=rep(0,length(type)),ES.d.CIU=rep(0,length(type)))
  
  for(stat in seq_along(type)){
    if(type[stat] == "F"){
      CI <- conf.limits.ncf(F.value = ncp[stat], conf.level = CL[stat], df.1 = df1[stat], df.2 = df2[stat]) 
      recalcData["ncp.CIL"][stat, ] <- CI$Lower.Limit
      recalcData["ncp.CIU"][stat, ] <- CI$Upper.Limit 
      recalcData["p.recalc"][stat, ] <- (1 - pf(ncp[stat], df1=df1[stat], df2=df2[stat]))
      recalcData["N.recalc"][stat, ] <- df1[stat]+df2[stat]+1
      recalcData["posthocPOWER"][stat, ] <- php.F(recalcData["p.recalc"][stat, ],df1[stat],df2[stat]) 
      recalcData["ES.r.recalc"][stat, ]  <- f_r(f=ncp[stat],df1=df1[stat],df2=df2[stat])
      recalcData["ES.r.CIL"][stat, ]     <- f_r(f=CI$Lower.Limit,df1=df1[stat],df2=df2[stat])
      recalcData["ES.r.CIU"][stat, ]     <- f_r(f=CI$Upper.Limit,df1=df1[stat],df2=df2[stat])
      recalcData["ES.d.recalc"][stat, ]  <- f_d(f=ncp[stat],df1=df1[stat],df2=df2[stat])
      recalcData["ES.d.CIL"][stat, ]     <- f_d(f=CI$Lower.Limit,df1=df1[stat],df2=df2[stat])
      recalcData["ES.d.CIU"][stat, ]     <- f_d(f=CI$Upper.Limit,df1=df1[stat],df2=df2[stat])
    }
    if(type[stat] == "t"){
      CI <- conf.limits.nct(t.value = ncp[stat], conf.level = CL[stat], df = df1[stat])
      recalcData["ncp.CIL"][stat, ] <- CI$Lower.Limit
      recalcData["ncp.CIU"][stat, ] <- CI$Upper.Limit 
      recalcData["p.recalc"][stat, ] <- (1 - pt(abs(ncp[stat]), df=df1[stat]))
      recalcData["N.recalc"][stat, ] <- df1[stat]+1 
      recalcData["posthocPOWER"][stat, ] <- php.t(P=recalcData["p.recalc"][stat, ],df=df1[stat])
      recalcData["ES.r.recalc"][stat, ]  <- t_r(t=ncp[stat],df=df1[stat])
      recalcData["ES.r.CIL"][stat, ]     <- t_r(t=CI$Lower.Limit,df=df1[stat])
      recalcData["ES.r.CIU"][stat, ]     <- t_r(t=CI$Upper.Limit,df=df1[stat])
      recalcData["ES.d.recalc"][stat, ]  <- t_d(t=ncp[stat],df=df1[stat])
      recalcData["ES.d.CIL"][stat, ]     <- t_d(t=CI$Lower.Limit,df=df1[stat])
      recalcData["ES.d.CIU"][stat, ]     <- t_d(t=CI$Upper.Limit,df=df1[stat])
    }
    if(type[stat] == "X^2"){
      ifelse(df1[stat]==1,{
         CI <- conf.limits.nc.chisq(Chi.Square = ncp[stat], alpha.lower=0, alpha.upper=(1-CL), conf.level = NULL, df = df1[stat])},{
           CI <- conf.limits.nc.chisq(Chi.Square = ncp[stat], conf.level = CL[stat], df = df1[stat])
         })
      recalcData["ncp.CIL"][stat, ]      <- CI$Lower.Limit
      recalcData["ncp.CIU"][stat, ]      <- CI$Upper.Limit 
      recalcData["p.recalc"][stat, ]     <- (1 - pchisq(ncp[stat], df=df1[stat]))
      recalcData["N.recalc"][stat, ]     <- N[stat]
      recalcData["posthocPOWER"][stat, ] <- NA
      recalcData["ES.r.recalc"][stat, ]  <- X_r(X=ncp[stat],df=df1[stat],N=N[stat])
      recalcData["ES.r.CIL"][stat, ]     <- X_r(X=CI$Lower.Limit,df=df1[stat],N=N[stat])
      recalcData["ES.r.CIU"][stat, ]     <- X_r(X=CI$Upper.Limit,df=df1[stat],N=N[stat])
      recalcData["ES.d.recalc"][stat, ]  <- X_d(X=ncp[stat],df=df1[stat],N=N[stat])
      recalcData["ES.d.CIL"][stat, ]     <- X_d(X=CI$Lower.Limit,df=df1[stat],N=N[stat])
      recalcData["ES.d.CIU"][stat, ]     <- X_d(X=CI$Upper.Limit,df=df1[stat],N=N[stat])
    }
    if(type[stat] == "unknown"){
      recalcData["ncp.CIL"][stat, ]  <- NA
      recalcData["ncp.CIU"][stat, ]  <- NA 
      recalcData["p.recalc"][stat, ] <- NA
      recalcData["N.recalc"][stat, ] <- NA
      recalcData["posthocPOWER"][stat, ] <- NA
      recalcData["ES.r.recalc"][stat, ]  <- NA
      recalcData["ES.r.CIL"][stat, ]     <- NA
      recalcData["ES.r.CIU"][stat, ]     <- NA
      recalcData["ES.d.recalc"][stat, ]  <- NA
      recalcData["ES.d.CIL"][stat, ]     <- NA
      recalcData["ES.d.CIU"][stat, ]     <- NA
    }
    
  }
  return(recalcData)
}

# GET DATA - RECALCULATE --------------------------------------------------
# setwd() to file location ( code = day-month-time)
FINstudies <- read.xlsx2("RPP master data 15-05-2140.xlsx",sheetName="Statisitics",startRow=2,endRow=32,stringsAsFactors=FALSE)

RPPdata <- data.frame(
  WARNING = rep("MISSING TEST INFO: prediction:directed/undirected, samples:dependent/independent/equalN/equalVar, factor:Nlevels/between/within, effect:interaction/main/planned contrast/random/fixed, posthoc: yes/no/correction",nrow(FINstudies)),
  name=FINstudies[["ARTICLE.TITLE"]],
  OSFid=FINstudies[["OSF.ID"]],
  stat.ori.string=gsub("[[:blank:]]+","",FINstudies[["Test.statistic"]],perl=T),
  stat.ori.type=rep("unknown",nrow(FINstudies)),
  stat.ori.ncp=rep(0,nrow(FINstudies)),
  stat.ori.ncp.CIL=rep(0,nrow(FINstudies)),
  stat.ori.ncp.CIU=rep(0,nrow(FINstudies)),
  stat.ori.p=gsub("([[:blank:]<=>])+","",FINstudies[["p.value"]],perl=T),
  stat.ori.p.recalc=rep(0,nrow(FINstudies)),
  stat.ori.N=FINstudies[["N"]],
  stat.ori.N.recalc=rep(0,nrow(FINstudies)),
  stat.ori.df1=rep(0,nrow(FINstudies)),
  stat.ori.df2=rep(NA,nrow(FINstudies)),
  prediction.ori=rep("not equal",nrow(FINstudies)),
  alpha.ori=rep(0.05,nrow(FINstudies)),
  ES.ori.type=FINstudies[["ES.metric"]],
  ES.ori.value=FINstudies[["ES.value"]],
  ES.ori.value.recalc=rep(0,nrow(FINstudies)),
  ES.ori.r=rep(0,nrow(FINstudies)),
  ES.ori.r.CIL=rep(0,nrow(FINstudies)),
  ES.ori.r.CIU=rep(0,nrow(FINstudies)),
  ES.ori.d=rep(0,nrow(FINstudies)),
  ES.ori.d.CIL=rep(0,nrow(FINstudies)),
  ES.ori.d.CIU=rep(0,nrow(FINstudies)),
  aprioriPOW.ori=rep("unknown",nrow(FINstudies)),
  posthocPOW.ori=rep(0,nrow(FINstudies)),
  stat.rep.string=gsub("[[:blank:]]+","",FINstudies[["Replication..Statistic"]]),
  stat.rep.type=rep("unknown",nrow(FINstudies)),
  stat.rep.ncp=rep(0,nrow(FINstudies)),
  stat.rep.df1=rep(0,nrow(FINstudies)),
  stat.rep.df2=rep(NA,nrow(FINstudies)),
  stat.rep.ncp.CIL=rep(0,nrow(FINstudies)),
  stat.rep.ncp.CIU=rep(0,nrow(FINstudies)),
  stat.rep.p=gsub("([[:blank:]<=>])+","",FINstudies[["p.value.1"]],perl=T),
  stat.rep.p.recalc=rep(0,nrow(FINstudies)),
  stat.rep.N=FINstudies[["N.1"]],
  stat.rep.N.recalc=rep(0,nrow(FINstudies)),
  prediction.rep=rep("not equal",nrow(FINstudies)),
  alpha.rep=rep(0.05,nrow(FINstudies)),
  ES.rep.type=FINstudies[["ES.metric.1"]],
  ES.rep.value=FINstudies[["ES.value.1"]],
  ES.rep.value.recalc=rep(0,nrow(FINstudies)),
  ES.rep.r=rep(0,nrow(FINstudies)),
  ES.rep.r.CIL=rep(0,nrow(FINstudies)),
  ES.rep.r.CIU=rep(0,nrow(FINstudies)),
  ES.rep.d=rep(0,nrow(FINstudies)),
  ES.rep.d.CIL=rep(0,nrow(FINstudies)),
  ES.rep.d.CIU=rep(0,nrow(FINstudies)),
  aprioriPOW.rep=FINstudies[["Power"]],
  posthocPOW.rep=rep(0,nrow(FINstudies)),
  stringsAsFactors=F
  )


# Original ----------------------------------------------------------------

idF <- grep(Fpat,RPPdata$stat.ori.string,ignore.case = T, perl=T)
idt <- grep(tpat,RPPdata$stat.ori.string,ignore.case = T, perl=T)
idX <- grep(Xpat,RPPdata$stat.ori.string,ignore.case = T, perl=T)
RPPdata["stat.ori.string"][idX, ] <- gsub("([xX]\\^2|χ2)","X^2",RPPdata["stat.ori.string"][idX, ],perl=T)

for(s in 1:nrow(FINstudies)){
  
  if(s%in%idF){pat <- Fpat}
  if(s%in%idt){pat <- tpat}
  if(s%in%idX){pat <- Xpat}
  
  matchOri <- gregexpr(pat,RPPdata["stat.ori.string"][s, ],ignore.case=T,perl=T)[[1]]
  
  for(field in attributes(matchOri)$capture.names){
    if(field!=""){
      fieldID <- attributes(matchOri)$capture.names==field
      if(attributes(matchOri)$capture.start[fieldID]!=-1){
        switch(field,
               type =  RPPdata["stat.ori.type"][s, ] <- substr(RPPdata["stat.ori.string"][s, ], start=attributes(matchOri)$capture.start[fieldID],stop=(attributes(matchOri)$capture.start[fieldID]+attributes(matchOri)$capture.length[fieldID]-1)),
               stat =  RPPdata["stat.ori.ncp"][s, ] <- substr(RPPdata["stat.ori.string"][s, ], start=attributes(matchOri)$capture.start[fieldID],stop=(attributes(matchOri)$capture.start[fieldID]+attributes(matchOri)$capture.length[fieldID]-1)),
               df1 =  RPPdata["stat.ori.df1"][s, ] <- substr(RPPdata["stat.ori.string"][s, ], start=attributes(matchOri)$capture.start[fieldID],stop=(attributes(matchOri)$capture.start[fieldID]+attributes(matchOri)$capture.length[fieldID]-1)),
               df2 =  RPPdata["stat.ori.df2"][s, ] <- substr(RPPdata["stat.ori.string"][s, ], start=attributes(matchOri)$capture.start[fieldID],stop=(attributes(matchOri)$capture.start[fieldID]+attributes(matchOri)$capture.length[fieldID]-1)),
               N =  RPPdata["stat.ori.N"][s, ] <- substr(RPPdata["stat.ori.string"][s, ], start=attributes(matchOri)$capture.start[fieldID],stop=(attributes(matchOri)$capture.start[fieldID]+attributes(matchOri)$capture.length[fieldID]-1))
        )
      }
    }
  }
  
}

idF <- grep(Fpat,RPPdata[["stat.rep.string"]],ignore.case = T, perl=T)
idt <- grep(tpat,RPPdata[["stat.rep.string"]],ignore.case = T, perl=T)
idX <- grep(Xpat,RPPdata[["stat.rep.string"]],ignore.case = T,perl=T)
RPPdata["stat.rep.string"][idX, ] <- gsub("([xX]\\^2|χ2)","X^2",RPPdata["stat.rep.string"][idX, ],perl=T)


# Replication -------------------------------------------------------------

for(s in 1:nrow(FINstudies)){
  
  if(s%in%idF){pat <- Fpat}
  if(s%in%idt){pat <- tpat}
  if(s%in%idX){pat <- Xpat}
  
  # Replication
  matchRep <- gregexpr(pat,RPPdata["stat.rep.string"][s, ],ignore.case=T,perl=T)[[1]]
  
  for(field in attributes(matchRep)$capture.names){
    if(field!=""){
      fieldID <- attributes(matchRep)$capture.names==field
      if(attributes(matchRep)$capture.start[fieldID]!=-1){
        switch(field,
               type =  RPPdata["stat.rep.type"][s, ] <- substr(RPPdata["stat.rep.string"][s, ], start=attributes(matchRep)$capture.start[fieldID],stop=(attributes(matchRep)$capture.start[fieldID]+attributes(matchRep)$capture.length[fieldID]-1)),
               stat =  RPPdata["stat.rep.ncp"][s, ] <- substr(RPPdata["stat.rep.string"][s, ], start=attributes(matchRep)$capture.start[fieldID],stop=(attributes(matchRep)$capture.start[fieldID]+attributes(matchRep)$capture.length[fieldID]-1)),
               df1 =  RPPdata["stat.rep.df1"][s, ] <- substr(RPPdata["stat.rep.string"][s, ], start=attributes(matchRep)$capture.start[fieldID],stop=(attributes(matchRep)$capture.start[fieldID]+attributes(matchRep)$capture.length[fieldID]-1)),
               df2 =  RPPdata["stat.rep.df2"][s, ] <- substr(RPPdata["stat.rep.string"][s, ], start=attributes(matchRep)$capture.start[fieldID],stop=(attributes(matchRep)$capture.start[fieldID]+attributes(matchRep)$capture.length[fieldID]-1)),
               N =  RPPdata["stat.rep.N"][s, ] <- substr(RPPdata["stat.rep.string"][s, ], start=attributes(matchRep)$capture.start[fieldID],stop=(attributes(matchRep)$capture.start[fieldID]+attributes(matchRep)$capture.length[fieldID]-1))
        )
      }
    }
  }
}


# Recalculate -------------------------------------------------------------

recalc.ori <- recalcSTAT(type=RPPdata$stat.ori.type,ncp=as.numeric(RPPdata$stat.ori.ncp),df1=as.numeric(RPPdata$stat.ori.df1),df2=as.numeric(RPPdata$stat.ori.df2),N=as.numeric(RPPdata$stat.ori.N))  

RPPdata$stat.ori.ncp.CIL <- recalc.ori$ncp.CIL
RPPdata$stat.ori.ncp.CIU <- recalc.ori$ncp.CIU
RPPdata$stat.ori.p.recalc <- recalc.ori$p.recalc
RPPdata$stat.ori.N.recalc <- recalc.ori$N.recalc
RPPdata$posthocPOW.ori <- recalc.ori$posthocPOWER
RPPdata$ES.ori.r <- recalc.ori$ES.r.recalc
RPPdata$ES.ori.r.CIL <- recalc.ori$ES.r.CIL
RPPdata$ES.ori.r.CIU <- recalc.ori$ES.r.CIU
RPPdata$ES.ori.d <- recalc.ori$ES.d.recalc
RPPdata$ES.ori.d.CIL <- recalc.ori$ES.d.CIL
RPPdata$ES.ori.d.CIU <- recalc.ori$ES.d.CIU
  

recalc.rep <- recalcSTAT(type=RPPdata$stat.rep.type,ncp=as.numeric(RPPdata$stat.rep.ncp),df1=as.numeric(RPPdata$stat.rep.df1),df2=as.numeric(RPPdata$stat.rep.df2),as.numeric(RPPdata$stat.rep.N))  

RPPdata$stat.rep.ncp.CIL <- recalc.rep$ncp.CIL
RPPdata$stat.rep.ncp.CIU <- recalc.rep$ncp.CIU
RPPdata$stat.rep.p.recalc <- recalc.rep$p.recalc
RPPdata$stat.rep.N.recalc <- recalc.rep$N.recalc
RPPdata$posthocPOW.rep <- recalc.rep$posthocPOWER
RPPdata$ES.rep.r <- recalc.rep$ES.r.recalc
RPPdata$ES.rep.r.CIL <- recalc.rep$ES.r.CIL
RPPdata$ES.rep.r.CIU <- recalc.rep$ES.r.CIU
RPPdata$ES.rep.d <- recalc.rep$ES.d.recalc
RPPdata$ES.rep.d.CIL <- recalc.rep$ES.d.CIL
RPPdata$ES.rep.d.CIU <- recalc.rep$ES.d.CIU


# SAVE and EXPORT ---------------------------------------------------------

RPPdata$ori.sig  <- factor(ifelse(as.vector(RPPdata$stat.ori.p.recalc)<.05,1,0),levels=c(0,1),labels=c("Accept H0","Reject H0"))
RPPdata$rep.sig  <- factor(ifelse(as.vector(RPPdata$stat.rep.p.recalc)<.05,1,0),levels=c(0,1),labels=c("Accept H0","Reject H0"))

write.table(RPPdata,file="RPPdata_15_05_2140.txt",sep="\t",row.names=F,fileEncoding="UTF-8",quote=F)
save(RPPdata,file="RPPdata_15_05_2140.Rdata")

# Data ar here
urltxt   <- getURL("https://raw.githubusercontent.com/FredHasselman/RPP/master/RPPdata.dat")
RPPdata  <- read.delim(textConnection(urltxt),stringsAsFactors=F)
closeAllConnections()
