# SETUP -------------------------------------------------------------------

require(MBESS)
require(plyr)
require(ggplot2)
require(RCurl)
require(devtools)

# SOURCE FROM GITHUB -------------------------------------------------

# [sciCure](http://fredhasselman.github.io/scicuRe/)
#
# Use this code to source it directly from GitHub:
source_url("https://raw.githubusercontent.com/FredHasselman/scicuRe/master/scicuRe_source.R")

# LOAD DATA FROM GITHUB ---------------------------------------------------

# Complete pairs ori-rep
urltxt   <- getURL("https://raw.githubusercontent.com/FredHasselman/RPP/master/RPPcompletepairs.dat")
RPPclean <- read.delim(textConnection(urltxt),stringsAsFactors=F)
closeAllConnections()

# From the replication report:
# This result was significant in a two-sided t-test, t(7) = 2.892, p = .023, d = 1.023. Conducting this analysis on the
# replication dataset yields very similar results with a recall rate of 70%, 95% confidence interval
# [58%, 82%], t(14) = 3.708, p = .002, d = .957. A fixed-effects meta-analysis produces a
# combined estimate of 71%, 95% CI [62%, 79%], p < .0001

SEV <- sev.data(RPPclean,study=2)

SEV.ori <- SEV[[1]]
SEV.rep <- SEV[[2]]

pdf("RPP_Severity_Example_sigsig.pdf",onefile=T,width=14,height=10,pointsize=10)
p<-plotReplication(SEV.ori, SEV.rep, d.axis="r",pl.connect=T,studyname=RPPclean$name[[5]])
plot(p)
dev.off()

infer <- matrix(nrow=length(RPPclean[,1]), ncol=8, dimnames=list(seq_along(RPPclean[,1]),c("ori.x","ori.sev","rep.x","rep.sev","ori.xrep","ori.sevrep","rep.xori","rep.sevori")))
SEV.oril <- list()
SEV.repl <- list()
NP <- list()
cnt=0
skip=c(21,23,24)
for(s in seq_along(RPPclean[,1])){
  if(s%in%skip){
    cat("skipped:",s,"\n")
    } else {
      cnt=cnt+1
      SEV <- sev.data(RPPclean,study=s)
      SEV.oril[[cnt]] <- SEV[[1]]
      SEV.repl[[cnt]] <- SEV[[2]]
      infer[cnt, ] <- c(SEV[[1]]$severity[1,1],SEV[[1]]$severity[1,4],SEV[[2]]$severity[1,1],SEV[[2]]$severity[1,4],SEV[[1]]$severity[6,1],SEV[[1]]$severity[6,4],SEV[[2]]$severity[6,1],SEV[[2]]$severity[6,4])
      NP[[cnt]] <- c(SEV[[1]]$inference,SEV[[2]]$inference)
      
    cat(s,"\n")
    }
}

d<- (NP[[1]])
plot(infer[,4],RPPclean$stat.rep.p.recalc,type="p",xlab="Severity on replication",ylab="p-value on replication")
points(infer[,4][RPPclean$stat.rep.p.recalc<=.05],RPPclean$stat.rep.p.recalc[RPPclean$stat.rep.p.recalc<=.05],col=2)

plot(infer[,2],infer[,4],type="p",xlab="Severity on original",ylab="Severity on replication")
points(infer[,2][RPPclean$stat.rep.p.recalc<=.05],infer[,4][RPPclean$stat.rep.p.recalc<=.05],col=2)

plot(infer[,2],RPPclean$ES.ori.r,type="p",xlab="Severity on original",ylab="Effect size on replication")
points(infer[,2][RPPclean$stat.rep.p.recalc<=.05],RPPclean$ES.ori.r[RPPclean$stat.rep.p.recalc<=.05],col=2)


plot(infer[,2],RPPclean$stat.rep.p.recalc,type="p",xlab="Severity on original",ylab="p-value on replication")
points(infer[,2][RPPclean$stat.rep.p.recalc<=.05],RPPclean$stat.rep.p.recalc[RPPclean$stat.rep.p.recalc<=.05],col=2)

# Plot to PDF -------------------------------------------------------------
# Skip chisquare
skip=c(21,23,24)
pdf("RPP_Severity_Figures_28studies.pdf",paper="a4r",width=0,height=0)
for(s in seq_along(RPPclean[,1])){
  if(s%in%skip){
    cat("skipped:",s,"\n")
    } else {
    p<-plotReplication(SEV.oril[[s]],SEV.repl[[s]],d.axis="stat",studyname=paste(s,RPPclean$name[[s]]),pl.connect=T)
    cat(s,"\n")
    }
  plot(p)
}
dev.off()
