require(MBESS)
require(plyr)
require(ggplot2)

load(file="RPPclean.Rdata")
write.table(RPPdata,file="RPPdata.dat",sep="\t",row.names=F) 
write.table(RPPclean,file="RPPcompletepairs.dat",sep="\t",row.names=F) 

RPPdata   <- read.table("RPPdata.dat",sep="\t",header=TRUE)
RPPclean  <- read.table("RPPcompletepairs.dat",sep="\t",header=TRUE)

# save(RPPclean,file="sevRPPdata.Rdata")

# From the replication report:
# This result was significant in a two-sided t-test, t(7) = 2.892, p = .023, d = 1.023. Conducting this analysis on the
# replication dataset yields very similar results with a recall rate of 70%, 95% confidence interval
# [58%, 82%], t(14) = 3.708, p = .002, d = .957. A fixed-effects meta-analysis produces a
# combined estimate of 71%, 95% CI [62%, 79%], p < .0001

Ori.n  <- 8
Ori.df <- 7
Ori.t  <- 2.892
Ori.p  <- 0.23

Rep.n  <- 15
Rep.df <- 14
Rep.t  <- 3.708
Rep.p  <- .002



SEV.ori <- sev.info(stat.type="t",stat.ncp=Ori.t,stat.df=Ori.df,prediction="not equal",mus=list(SEV.comp.stat=Rep.t),compare=list(SEV.comp.df=Rep.df))
SEV.rep <- sev.info(stat.type="t",stat.ncp=Rep.t,stat.df=Rep.df,prediction="not equal",mus=list(SEV.comp.stat=Ori.t),compare=list(SEV.comp.df=Ori.df))

SEV.ori[[2]]
SEV.rep[[2]]

plotReplication(SEV.ori,SEV.rep,d.axis="stat")


SEV.ori <- sev.info(stat.type=RPPdata$stat.ori.type[[s]],stat.ncp=as.numeric(RPPdata$stat.ori.ncp[[s]]),stat.df=c(as.numeric(RPPdata$stat.ori.df1[[s]]),as.numeric(RPPdata$stat.ori.df2[[s]])), as.numeric(RPPdata$stat.ori.N[[s]]), prediction=RPPdata$prediction.ori[[s]],mus=list(SEV.ori.rep=as.numeric(RPPdata$stat.ori.ncp[[s]])))

SEV.rep <- sev.info(stat.type=RPPdata$stat.rep.type[[s]],stat.ncp=as.numeric(RPPdata$stat.rep.ncp[[s]]),stat.df=c(as.numeric(RPPdata$stat.rep.df1[[s]]),as.numeric(RPPdata$stat.rep.df2[[s]])), as.numeric(RPPdata$stat.rep.N[[s]]), prediction=RPPdata$prediction.rep[[s]],mus=list(SEV.ori.rep=as.numeric(RPPdata$stat.ori.ncp[[s]])))

plotReplication(SEV.ori,SEV.rep,d.axis="stat")

RPPclean <- subset(RPPdata,stat.ori.type!="unknown") 
RPPclean <- subset(RPPclean,stat.rep.type!="unknown") 


SEV.oril <- llply(seq_along(RPPclean[,1]),function(s){ 
  return(with(RPPclean, 
              sev.info(stat.type=stat.ori.type[[s]],
                       stat.ncp=as.numeric(stat.ori.ncp[[s]]),
                       stat.df=c(as.numeric(stat.ori.df1[[s]]), as.numeric(stat.ori.df2[[s]])),
                       stat.N=as.numeric(stat.ori.N[[s]]), prediction=prediction.ori[[s]],
                       mus=list(SEV.ori.rep=as.numeric(stat.rep.ncp[[s]])),
                       compare=list(SEV.comp.df=c(as.numeric(stat.rep.df1[[s]]),as.numeric(stat.rep.df2[[s]])))   )))
  })

SEV.repl <- llply(seq_along(RPPclean[,1]),function(s){ 
  return(with(RPPclean,
              sev.info(stat.type=stat.rep.type[[s]],
                       stat.ncp=as.numeric(stat.rep.ncp[[s]]),
                       stat.df=c(as.numeric(stat.rep.df1[[s]]), as.numeric(stat.rep.df2[[s]])), 
                       stat.N=as.numeric(stat.rep.N[[s]]), 
                       prediction=prediction.rep[[s]],
                       mus=list(SEV.ori.rep=as.numeric(stat.ori.ncp[[s]])),
                       compare=list(SEV.comp.df=c(as.numeric(stat.ori.df1[[s]]),as.numeric(stat.ori.df2[[s]])))   )))
})

skip=c(21,22,23,24)
pdf("RPP_Severity_Figures_28studies.pdf",paper="a4r",width=0,height=0)
for(s in seq_along(RPPclean[,1])){
  if(s%in%skip){
    cat("skipped:",s,"\n")
    } else {
    p<-plotReplication(SEV.oril[[s]],SEV.repl[[s]],d.axis="stat",pl.connect=T)
    cat(s,"\n")
    }
  plot(p)
}
dev.off()
dev.new()
stat.type=RPPclean$stat.rep.type[[s]]
                       stat.ncp=as.numeric(RPPclean$stat.rep.ncp[[s]]) 
                       stat.df=c(as.numeric(RPPclean$stat.rep.df1[[s]]), as.numeric(RPPclean$stat.rep.df2[[s]])) 
                       stat.N=as.numeric(RPPclean$stat.rep.N[[s]]) 
                       prediction=RPPclean$prediction.rep[[s]]
                       mus=list(SEV.ori.rep=as.numeric(RPPclean$stat.ori.ncp[[s]]))
                       compare=list(SEV.comp.df=c(as.numeric(RPPclean$stat.ori.df1[[s]]),as.numeric(RPPclean$stat.ori.df2[[s]])))   



SEV.oril[[s]]$inference
SEV.repl[[s]]$severity

mdply(SEV.oril, function(ori) data.frame(ori.x='/;ori$severity[1,1],ori.y=ori$severity[1,4]))
,rep.x=rep$severity[1,1],rep.y=rep$severity[1,4]))

#volcano plot
b3<-ggplot(xy, aes(x = xvar)) +
  stat_density(aes(ymax = ..density..,  ymin = -..density..,
               fill = zvar, color = zvar),
               geom = "ribbon", position = "identity") +
  facet_grid(. ~ zvar) +
  coord_flip() +
  theme(legend.position = "none")
