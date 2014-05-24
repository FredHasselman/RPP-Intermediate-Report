# SETUP -------------------------------------------------------------------
require(MBESS)
require(plyr)
require(reshape2)
require(ggplot2)
require(gplots)
require(RCurl)
require(metafor)
require(klaR)
require(MASS)
require(MAc)
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
RPPmaster <- read.delim(textConnection(urltxt),stringsAsFactors=F)
closeAllConnections()

## Recast data after running RPP_CastData.R
urltxt   <- getURL("https://raw.githubusercontent.com/FredHasselman/RPP/master/RPPdata_cast.dat")
RPPdata_cast  <- read.delim(textConnection(urltxt),stringsAsFactors=F)
closeAllConnections()

# LOTS OF PLOTS -----------------------------------------------------------

# Construct some variables and run analyses
x1 <- subset(RPPdata_cast,stat.rep.H1)
x0 <- subset(RPPdata_cast,stat.rep.H0)


# Change p to 1-p
stat.ori.ncp.p.recalc <- stat.ori.ncp.p.recalc
stat.rep.ncp.p.recalc <- stat.rep.ncp.p.recalc
stat.ori.ncp.p.recalc[which(RPPdata_cast$stat.ori.crit>0)] <- 1-stat.ori.ncp.p.recalc[which(RPPdata_cast$stat.ori.crit>0)]
stat.rep.ncp.p.recalc[which(RPPdata_cast$stat.rep.crit>0)] <- 1-stat.rep.ncp.p.recalc[which(RPPdata_cast$stat.rep.crit>0)]

ok  <-complete.cases(RPPdata_cast$ES.ori.r, RPPdata_cast$ES.rep.r)
okp <-complete.cases(stat.ori.ncp.p.recalc, stat.rep.ncp.p.recalc)

hro=hist(RPPdata_cast$ES.ori.r[ok])
hrr=hist(RPPdata_cast$ES.rep.r[ok])
hpo=hist(stat.ori.ncp.p.recalc[okp],breaks=c(0,0.1))
hpr=hist(stat.rep.ncp.p.recalc[okp])

#Summary stats on p-values
mean(stat.ori.ncp.p.recalc[okp])
sd(stat.ori.ncp.p.recalc[okp])
mean(stat.rep.ncp.p.recalc[okp])
sd(stat.rep.ncp.p.recalc[okp])

#Summary stats on effect sizes
mean(RPPdata_cast$ES.ori.r[ok])
sd(RPPdata_cast$ES.ori.r[ok])
mean(RPPdata_cast$ES.rep.r[ok])
sd(RPPdata_cast$ES.rep.r[ok])


# WRITE TO PDF FILE -------------------------------------------------------

pdf("RPP_Figures_30.pdf",paper="a4r",width=0,height=0)

par(mfrow=c(2,1),pty="m")
t1<-table(RPPdata_cast$stat.ori.decideNP[okp],RPPdata_cast$stat.rep.decideNP[okp],dnn=list("Original","Replication"))
textplot(capture.output(t1),cex=1.1,halign="center")
t2<-table(RPPdata_cast$journal[okp],RPPdata_cast$stat.rep.decideNP[okp],dnn=list("Journal","Replication"))
textplot(capture.output(t2),cex=1.1,halign="center")
par(mfrow=c(1,1))

par(mfrow=c(1,2),pty="s")
plot(RPPdata_cast$stat.ori.ncp.p[okp], stat.ori.ncp.p.recalc[okp], xlim=c(0,0.06), ylim=c(0,0.06), xlab="Reported p-value [original study]",ylab="Re-calculated p-value [original study]",main="Recalculating p-values")
plot(RPPdata_cast$stat.rep.ncp.p[okp],stat.rep.ncp.p.recalc[okp],xlab="Reported p-value [replication study]",ylab="Re-calculated p-value [replication study]", main="Recalculating p-values")
par(mfrow=c(1,1))

#p-values Histogram
plot(hpo, col=rgb(0,0,1,1/4),xlim=c(0,1), ylim=c(0,30), xlab="p-value", main="Histograms of original versus replication p-values")
plot(hpr, col=rgb(1,0,0,1/4),xlim=c(0,1), add=T)
legend(0.3,20,c("Original","Replication"),lty=c(1,1),lwd=c(2.5,2.5),col=c("blue","red"),cex=.9)

#p-values densities
plot(density(stat.ori.ncp.p.recalc[okp]), col="blue", main="Density plots of original versus replication p-values", xlim=c(0,1),xlab="p-value")
lines(density(stat.rep.ncp.p.recalc[okp]), col="red")
legend(0.3,17,c("Original","Replication"),lty=c(1,1),lwd=c(2.5,2.5),col=c("blue","red"))

#Effect sizes histogram
plot(hro, col=rgb(0,0,1,1/4),xlim=c(0,1), ylim=c(0,10), xlab="Effect Size", main="Histograms of original versus replication effect sizes")
plot(hrr, col=rgb(1,0,0,1/4),xlim=c(0,1), add=T)
legend(0.3,9,c("Original","Replication"),lty=c(1,1),lwd=c(2.5,2.5),col=c("blue","red"))

#Effect sizes densities
plot(density(RPPdata_cast$ES.ori.r[okp]), col="blue", main="Density plots of original versus replication effect sizes", xlab="Effect size")
lines(density(RPPdata_cast$ES.rep.r[okp]), col="red")
legend(0.7,2,c("Original","Replication"),lty=c(1,1),lwd=c(2.5,2.5),col=c("blue","red"),cex=.8)

#Effect sizes scatterplot
cr<-cor(RPPdata_cast$ES.ori.r[okp],RPPdata_cast$ES.rep.r[okp])
plot(RPPdata_cast$ES.ori.r[okp],RPPdata_cast$ES.rep.r[okp], xlim=c(0,1), ylim=c(0,1), xlab="Original Effect Size", ylab="Replication Effect Size", col=ifelse(RPPdata_cast$ES.ori.r[okp]>RPPdata_cast$ES.rep.r[okp], "red","darkgreen"), pch=16,  main="Scatterplot of original versus replication p-values")
abline(0,1, col="blue")
text(0.2,0.8,paste("r =",round(cr,digits=2)))

ggplot(RPPdata_cast[ok,]) + 
  geom_point(aes(x=stat.ori.ncp.p.recalc, y=stat.rep.ncp.p.recalc),size=5) +
  scale_y_continuous(limits=c(0,1),breaks=c(0,.05,.5,1)) + scale_x_continuous(limits=c(0,1),breaks=c(0,.05,.5,1)) +
  geom_hline(yintercept=.05,colour="red",linetype=2) + geom_vline(xintercept=.05,colour="red",linetype=2)  +
  labs(title= paste("Original vs. Replication p-value \ncorrelation =",round(cor(stat.ori.ncp.p.recalc,stat.rep.ncp.p.recalc,use="complete.obs"),digits=2)), colour="Inference (Replication)") + 
  xlab("P-value original") + ylab("P-value replication") + 
  theme_bw(base_size = 16, base_family = "") + coord_fixed()

ggplot(RPPdata_cast[ok,]) + 
  geom_point(aes(x=stat.ori.ncp.p.recalc, y=stat.rep.ncp.p.recalc),size=5) +
  scale_y_continuous(limits=c(0,1),breaks=c(0,.05,.5,1)) + scale_x_continuous(limits=c(0,.06),breaks=c(0,.05,.5,1)) +
  geom_hline(yintercept=.05,colour="red",linetype=2) + geom_vline(xintercept=.05,colour="red",linetype=2)  +
  labs(title= paste("Original vs. Replication p-value \ncorrelation =",round(cor(x0$ES.rep.r,x0$ES.ori.r,use="complete.obs"), digits=4),"\nCorrelation Reject H0 = ",round(cor(x1$ES.rep.r,x1$ES.ori.r,use="complete.obs"), digits=4)), fill="Inference (Replication)",shape="Inference (Replication)") + 
  geom_smooth(aes(y=stat.rep.ncp.p.recalc,x=stat.ori.ncp.p.recalc,fill=stat.rep.decideNP),colour="black",method="glm",fullrange=FALSE,alpha=.2) + 
  xlab("P-value original") + ylab("P-value replication") + 
  theme_bw(base_size = 16, base_family = "")

ggplot(RPPdata_cast[ok,]) +
  geom_point(aes(x=stat.ori.ncp.p.recalc, y=stat.rep.ncp.p.recalc,colour=journal),size=8) + 
  geom_point(aes(x=stat.ori.ncp.p.recalc, y=stat.rep.ncp.p.recalc,shape=stat.rep.decideNP),size=5) +
  scale_y_continuous(limits=c(0,1),breaks=c(0,.05,.5,1)) + scale_x_continuous(limits=c(0,.06),breaks=c(0,.05,.5,1)) +
  geom_hline(yintercept=.05,colour="red",linetype=2) + geom_vline(xintercept=.05,colour="red",linetype=2)  +
  labs(title= paste("Original vs. Replication p-value \nCorrelation Accept H0 = ",round(cor(x0$stat.ori.ncp.p.recalc,x0$stat.rep.ncp.p.recalc,use="complete.obs"), digits=4),"\nCorrelation Reject H0 = ",round(cor(x1$stat.ori.ncp.p.recalc,x1$stat.rep.ncp.p.recalc,use="complete.obs"), digits=4)), fill="Inference (Replication)",shape="Inference (Replication)",colour="Journal") + 
  geom_smooth(aes(y=stat.rep.ncp.p.recalc,x=stat.ori.ncp.p.recalc,fill=stat.rep.decideNP),colour="black",method="glm",fullrange=FALSE,alpha=.2) + 
  xlab("P-value original") + ylab("P-value replication") + 
  theme_bw(base_size = 16, base_family = "")


ggplot(RPPdata_cast[ok,]) + 
  geom_point(aes(x=ES.ori.r, y=ES.rep.r,shape=stat.rep.decideNP),size=5) +
  scale_y_continuous(limits=c(0,1),breaks=c(0,.1,.3,.5,1)) + scale_x_continuous(limits=c(0,1),breaks=c(0,.1,.3,.5,1)) +
  geom_segment(x=.1,xend=.1,y=.1,yend=1.5,colour="red",linetype=2) + geom_segment(x=.1,xend=1.5,y=.1,yend=.1,colour="red",linetype=2) +
  geom_segment(x=.3,xend=.3,y=.3,yend=1.5,colour="orange",linetype=2) + geom_segment(x=.3,xend=1.5,y=.3,yend=.3,colour="orange",linetype=2) +
  geom_segment(x=.5,xend=.5,y=.5,yend=1.5,colour="green",linetype=2) + geom_segment(x=.5,xend=1.5,y=.5,yend=.5,colour="green",linetype=2) +
  geom_smooth(aes(y=ES.rep.r,x=ES.ori.r),colour="black",method="glm",fullrange=FALSE,alpha=.2) + 
  labs(title= paste("Original vs. Replication Effect Size (r)\nCorrelation = ",round(cor(RPPdata_cast$ES.rep.r,RPPdata_cast$ES.ori.r,use="complete.obs"),digits=4)), colour="Inference (Replication)",shape="Inference (Replication)") + 
  xlab("Effect size (r) original") + ylab("Effect size (r) replication") + 
  theme_bw(base_size = 16, base_family = "") + coord_fixed()

ggplot(RPPdata_cast[ok,]) + 
  geom_point(aes(x=ES.ori.r, y=ES.rep.r,colour=journal),size=8) + 
  geom_point(aes(x=ES.ori.r, y=ES.rep.r,shape=stat.rep.decideNP),size=5) +
  scale_y_continuous(limits=c(0,1),breaks=c(0,.1,.3,.5,1)) + scale_x_continuous(limits=c(0,1),breaks=c(0,.1,.3,.5,1)) +
  geom_segment(x=.1,xend=.1,y=.1,yend=1.5,colour="red",linetype=2) + geom_segment(x=.1,xend=1.5,y=.1,yend=.1,colour="red",linetype=2) +
  geom_segment(x=.3,xend=.3,y=.3,yend=1.5,colour="orange",linetype=2) + geom_segment(x=.3,xend=1.5,y=.3,yend=.3,colour="orange",linetype=2) +
  geom_segment(x=.5,xend=.5,y=.5,yend=1.5,colour="green",linetype=2) + geom_segment(x=.5,xend=1.5,y=.5,yend=.5,colour="green",linetype=2) +
  geom_smooth(aes(y=ES.rep.r,x=ES.ori.r),colour="black",method="glm",fullrange=FALSE,alpha=.2) + 
  labs(title= paste("Original vs. Replication Effect Size (r) by Journal\nCorrelation = ",round(cor(RPPdata_cast$ES.rep.r,RPPdata_cast$ES.ori.r,use="complete.obs"),digits=4)), colour="Journal",shape="Inference (Replication)") + 
  xlab("Effect size (r) original") + ylab("Effect size (r) replication") + 
  theme_bw(base_size = 16, base_family = "") + coord_fixed()

ggplot(RPPdata_cast[ok,]) + 
  geom_point(aes(x=ES.ori.r, y=ES.rep.r,shape=stat.rep.decideNP),size=5)  +
  scale_y_continuous(limits=c(0,1),breaks=c(0,.1,.3,.5,1)) + scale_x_continuous(limits=c(0,1),breaks=c(0,.1,.3,.5,1)) +
  geom_segment(x=.1,xend=.1,y=.1,yend=1.5,colour="red",linetype=2) + geom_segment(x=.1,xend=1.5,y=.1,yend=.1,colour="red",linetype=2) +
  geom_segment(x=.3,xend=.3,y=.3,yend=1.5,colour="orange",linetype=2) + geom_segment(x=.3,xend=1.5,y=.3,yend=.3,colour="orange",linetype=2) +
  geom_segment(x=.5,xend=.5,y=.5,yend=1.5,colour="green",linetype=2) + geom_segment(x=.5,xend=1.5,y=.5,yend=.5,colour="green",linetype=2) +
  geom_smooth(aes(y=ES.rep.r,x=ES.ori.r,group=stat.rep.decideNP,fill=stat.rep.decideNP),method="glm",fullrange=FALSE,alpha=.2,colour="black") + 
  labs(title= paste("Original vs. Replication Effect Size (r)\nCorrelation Accept H0 = ",round(cor(x0$ES.rep.r,x0$ES.ori.r,use="complete.obs"), digits=4),"\nCorrelation Reject H0 = ",round(cor(x1$ES.rep.r,x1$ES.ori.r,use="complete.obs"), digits=4)), fill="Inference (Replication)",shape="Inference (Replication)") + 
  xlab("Effect size (r) original") + ylab("Effect size (r) replication") + 
  theme_bw(base_size = 16, base_family = "") + coord_fixed()

ggplot(RPPdata_cast[ok,]) + 
  geom_point(aes(x=ES.ori.r, y=ES.rep.r,colour=journal),size=8) + 
  geom_point(aes(x=ES.ori.r, y=ES.rep.r,shape=stat.rep.decideNP),size=5)  +
  scale_y_continuous(limits=c(0,1),breaks=c(0,.1,.3,.5,1)) + scale_x_continuous(limits=c(0,1),breaks=c(0,.1,.3,.5,1)) +
  geom_segment(x=.1,xend=.1,y=.1,yend=1.5,colour="red",linetype=2) + geom_segment(x=.1,xend=1.5,y=.1,yend=.1,colour="red",linetype=2) +
  geom_segment(x=.3,xend=.3,y=.3,yend=1.5,colour="orange",linetype=2) + geom_segment(x=.3,xend=1.5,y=.3,yend=.3,colour="orange",linetype=2) +
  geom_segment(x=.5,xend=.5,y=.5,yend=1.5,colour="green",linetype=2) + geom_segment(x=.5,xend=1.5,y=.5,yend=.5,colour="green",linetype=2) +
  geom_smooth(aes(y=ES.rep.r,x=ES.ori.r,group=stat.rep.decideNP,fill=stat.rep.decideNP),colour="black",method="glm",fullrange=FALSE,alpha=.2) + 
  labs(title= paste("Original vs. Replication Effect Size (r) by Journal\nCorrelation Accept H0 = ",round(cor(x0$ES.rep.r,x0$ES.ori.r,use="complete.obs"), digits=4),"\nCorrelation Reject H0 = ",round(cor(x1$ES.rep.r,x1$ES.ori.r,use="complete.obs"), digits=4)), colour="Journal",fill="Inference (Replication)",shape="Inference (Replication)") + 
  xlab("Effect size (r) original") + ylab("Effect size (r) replication") + 
  theme_bw(base_size = 16, base_family = "") + coord_fixed()

mytheme <- theme_bw(base_size = 16, base_family = "") + theme(legend.position = "bottom") 
ggplot(RPPdata_cast[ok,]) + 
  geom_errorbar(aes(x=ES.ori.r,y=ES.rep.r,ymin=ES.rep.r.ciL,ymax=ES.rep.r.ciU,color=ES.rep.r)) +
  geom_errorbarh(aes(x=ES.ori.r,y=ES.rep.r, xmin=ES.ori.r.ciL,xmax=ES.ori.r.ciU,color=ES.ori.r)) +
  geom_point(aes(x=ES.ori.r, y=ES.rep.r,shape=stat.rep.decideNP,colour=ES.rep.r),size=8) +
  geom_point(aes(x=ES.ori.r, y=ES.rep.r,shape=stat.rep.decideNP),size=6)  + 
  geom_point(aes(x=ES.ori.r, y=ES.rep.r,shape=stat.rep.decideNP,colour=ES.ori.r),size=3)  + 
  scale_y_continuous(limits=c(0,1),breaks=c(0,.1,.3,.5,1)) + 
  scale_x_continuous(limits=c(0,1),breaks=c(0,.1,.3,.5,1)) +
  scale_shape_discrete(guide=FALSE) + # guide_legend(title.position="top",title.hjust=.5))+
  scale_colour_continuous(low="red",high="green",limits=c(0, 1),breaks=c(0,0.3,0.5,1),na.value="blue",guide=guide_colourbar(direction="horizontal",title.position="bottom",barwidth = 20, barheight = 1,,title.hjust=.5)) +
  labs(title= "Original vs. Replication Effect Size (r) with 95% CI\n(Triangles represent p < .05 on Replication)", fill="Inference (Replication)",shape="Inference (Replication)", colour="Effect Size\n(Inner = Original, Outer = Replication)") + 
  xlab("Effect size (r) original") + ylab("Effect size (r) replication") + mytheme + coord_fixed()

par(mfrow=c(1,2),pty="s")
# Classify!
fit <- qda(RPPdata_cast$stat.rep.H1[ok]~RPPdata_cast$ES.ori.r[ok] + stat.ori.ncp.p.recalc[ok], data=RPPdata_cast[ok,], prior=c(.5,.5),CV=T)
ct1  <- table(RPPdata_cast$stat.rep.H1[ok], fit$class, dnn=list("Inference (Replication)","Classified as:"))
# total percent correct
pc <- sum(diag(prop.table(ct1)))
drawparti(RPPdata_cast$stat.rep.H1[ok], RPPdata_cast$ES.ori.r[ok], stat.ori.ncp.p.recalc[ok], data=RPPdata_cast, na.action="na.omit",method="qda",xlab="Original Effect Size r",ylab="Original p-value",legend.err = F,image.colors = terrain.colors(n=2,alpha=.5),col.correct = "darkgreen",col.mean = "grey40",col.wrong = "darkred")
title(main = "Quadratic Discriminant Analysis:\nReject H0 on Replication",sub=paste("Correctly classified:",round(pc*100,digits=2),"%"))

fit <- qda(RPPdata_cast$stat.rep.H1[ok]~RPPdata_cast$ES.ori.r[ok] + RPPdata_cast$ES.rep.r[ok], data=RPPdata_cast[ok,], prior=c(.5,.5),CV=T)
ct2 <- table(RPPdata_cast$stat.rep.H1[ok], fit$class, dnn=list("Inference (Replication)","Classified as:"))

# total percent correct
pc<-sum(diag(prop.table(ct2)))
drawparti(RPPdata_cast$stat.rep.H1[ok], RPPdata_cast$ES.ori.r[ok], RPPdata_cast$ES.rep.r[ok], data=RPPdata_cast, na.action="na.omit",method="qda",xlab="Original Effect Size r",ylab="Replication Effect Size r",legend.err = F,image.colors = terrain.colors(n=2,alpha=.5),col.correct = "darkgreen",col.mean = "grey40",col.wrong = "darkred")
title(main = "Quadratic Discriminant Analysis:\nReject H0 on Replication",sub=paste("Correctly classified:",round(pc*100,digits=2),"%"))
par(mfrow=c(1,1))

par(mfrow=c(2,1),pty="m")
textplot(capture.output(ct1),cex=1.1)
textplot(capture.output(ct2),cex=1.1)
par(mfrow=c(1,1))

# Exploratory meta-analysis

id=c(which(ok),which(ok))
n =c(RPPdata_cast$stat.ori.N[ok],RPPdata_cast$stat.rep.N[ok])
r =c(RPPdata_cast$ES.ori.r[ok],RPPdata_cast$ES.rep.r[ok])
mod1 <- rep(c("Original","Replication"),each=length(which(ok)))
dat <- data.frame(id,n,r,mod1)
metadat <- agg(id = id, r = r, n=n,mod = mod1, data=dat) 
metadat$var.r <- var_r(metadat$r, metadat$n) # MAc function to derive variance
metadat$z <- r_to_z(metadat$r)  # MAc function to convert to Fisher's z (z')
metadat$var.z <- var_z(metadat$n)  # MAc function for variance of z'
metadat$mod1 <- melt(t(cbind(RPPdata_cast$stat.ori.decideNP[ok],RPPdata_cast$stat.rep.decideNP[ok])))$value

#sma <- mareg(r ~ mod*mod1, var = var.r, method = "REML",  data = metadat)
sma <- rma(yi=r, vi=var.r, mods= ~ mod*mod1, data=metadat)
text<-capture.output(summary(sma))
textplot(text)

plotcon(es = r, var = var.r, mod = mod1, data = metadat, method= "random", modname= "Inference" )
plotcon(es = r, var = var.r, mod = mod, data = metadat, method= "random", modname= "Study" )

forest(sma, xlim=c(-2,2),slab=paste("Study",metadat$id,levels(metadat$mod)),ilab=cbind(metadat$mod,levels(metadat$mod1)),ilab.xpos=c(-10,-1), order=order(metadat$r),rows=c(1:28,29:56), cex=.75,main="Forest plot RE model:\nES (r) = Study * N-P decision",xlab="Effect Size r")
 text(c(-1.8,-1), 58, c("Study", "N-P decision"))
 text(1.5,     58, "r [95% CI]")

funnel(sma,addtau2=T,level=c(90, 95, 99), shade=c("white", "gray", "darkgray"),refline=0, yaxis="seinv", main="Funnel plot RE model:\nES (r) = Study * N-P decision", xlab="Areas indicate ES pseudo CIs of [.90 .95 .99]")

# Influence plots
plot(influence(sma))  

dev.off()
