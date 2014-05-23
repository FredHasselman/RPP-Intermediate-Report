# SETUP -------------------------------------------------------------------
require(MBESS)
require(plyr)
require(ggplot2)
require(gplots)
require(RCurl)
require(metafor)
require(xlsx)

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

## Complete pairs of ori-rep for plotting comparisons
##
## Data were cleaned if test info was missing for an "ori-rep" pair
# RPPclean <- subset(RPPdata,stat.ori.type!="unknown") 
# RPPclean <- subset(RPPclean,stat.rep.type!="unknown") 

urltxt   <- getURL("https://raw.githubusercontent.com/FredHasselman/RPP/master/RPPdata_cast_pairwise.dat")
RPPclean <- read.delim(textConnection(urltxt),stringsAsFactors=F)
closeAllConnections()

# LOTS OF PLOTS -----------------------------------------------------------

x1 <- subset(RPPdata_cast,stat.rep.H1)
x0 <- subset(RPPdata_cast,stat.rep.H0)


# Change p to 1-p
RPPdata_cast$stat.ori.ncp.p.recalc[which(RPPdata_cast$stat.ori.crit>0)] <- 1-RPPdata_cast$stat.ori.ncp.p.recalc[which(RPPdata_cast$stat.ori.crit>0)]

RPPdata_cast$stat.rep.ncp.p.recalc[which(RPPdata_cast$stat.rep.crit>0)] <- 1-RPPdata_cast$stat.rep.ncp.p.recalc[which(RPPdata_cast$stat.rep.crit>0)]

ok<-complete.cases(RPPdata_cast$ES.ori.r, RPPdata_cast$ES.rep.r)

pdf("RPP_Figures_30studies.pdf",paper="a4r",width=0,height=0)

par(mfrow=c(1,2),pty="s")
plot(RPPdata_cast$stat.ori.ncp.p, RPPdata_cast$stat.ori.ncp.p.recalc, xlim=c(0,0.06), ylim=c(0,0.06), xlab="Reported p-value [original study]",ylab="Re-calculated p-value [original study]",main="Recalculating p-values")
plot(RPPdata_cast$stat.rep.ncp.p,RPPdata_cast$stat.rep.ncp.p.recalc,xlab="Reported p-value [replication study]",ylab="Re-calculated p-value [replication study]", main="Recalculating p-values")
par(mfrow=c(1,1))

ggplot(RPPdata_cast[ok,]) + geom_point(aes(x=stat.ori.ncp.p.recalc, y=stat.rep.ncp.p.recalc),size=5) +
  scale_y_continuous(limits=c(0,1),breaks=c(0,.05,.5,1)) + scale_x_continuous(limits=c(0,1),breaks=c(0,.05,.5,1)) +
  geom_hline(yintercept=.05,colour="red",linetype=2) + geom_vline(xintercept=.05,colour="red",linetype=2)  +
  labs(title= paste("Original vs. Replication p-value \ncorrelation =",round(cor(RPPdata_cast$stat.ori.ncp.p.recalc,RPPdata_cast$stat.rep.ncp.p.recalc,use="complete.obs"),digits=2)), colour="Inference (Replication)") + 
  xlab("P-value original") + ylab("P-value replication") + 
  theme_bw(base_size = 16, base_family = "")+ coord_fixed()

ggplot(RPPdata_cast[ok,]) + geom_point(aes(x=stat.ori.ncp.p.recalc, y=stat.rep.ncp.p.recalc),size=5) +
  scale_y_continuous(limits=c(0,1),breaks=c(0,.05,.5,1)) + scale_x_continuous(limits=c(0,.06),breaks=c(0,.05,.5,1)) +
  geom_hline(yintercept=.05,colour="red",linetype=2) + geom_vline(xintercept=.05,colour="red",linetype=2)  +
  labs(title= paste("Original vs. Replication p-value \ncorrelation =",round(cor(RPPdata_cast$stat.ori.ncp.p.recalc,RPPdata_cast$stat.rep.ncp.p.recalc,use="complete.obs"),digits=2)), colour="Inference (Replication)") + 
  geom_smooth(aes(y=stat.rep.ncp.p.recalc,x=stat.ori.ncp.p.recalc),colour="black",method="glm",fullrange=FALSE,alpha=.2) + 
  xlab("P-value original") + ylab("P-value replication") + 
  theme_bw(base_size = 16, base_family = "")

ggplot(RPPdata_cast[ok,]) + geom_point(aes(x=ES.ori.r, y=ES.rep.r),size=5)  +
  scale_y_continuous(limits=c(0,1),breaks=c(0,.1,.3,.5,1)) + scale_x_continuous(limits=c(0,1),breaks=c(0,.1,.3,.5,1)) +
  geom_segment(x=.1,xend=.1,y=.1,yend=1.5,colour="red",linetype=2) + geom_segment(x=.1,xend=1.5,y=.1,yend=.1,colour="red",linetype=2) +
  geom_segment(x=.3,xend=.3,y=.3,yend=1.5,colour="orange",linetype=2) + geom_segment(x=.3,xend=1.5,y=.3,yend=.3,colour="orange",linetype=2) +
  geom_segment(x=.5,xend=.5,y=.5,yend=1.5,colour="green",linetype=2) + geom_segment(x=.5,xend=1.5,y=.5,yend=.5,colour="green",linetype=2) +
  geom_smooth(aes(y=ES.rep.r,x=ES.ori.r),colour="black",method="glm",fullrange=FALSE,alpha=.2) + 
  labs(title= paste("Original vs. Replication Effect Size (r)\nCorrelation = ",round(cor(RPPdata_cast$ES.rep.r,RPPdata_cast$ES.ori.r,use="complete.obs"),digits=4)), colour="Inference (Replication)") + 
  xlab("Effect size (r) original") + ylab("Effect size (r) replication") + 
  theme_bw(base_size = 16, base_family = "")+ coord_fixed()

ggplot(RPPdata_cast[ok,]) + geom_point(aes(x=ES.ori.r, y=ES.rep.r,shape=stat.rep.decideNP),size=5)  +
  scale_y_continuous(limits=c(0,1),breaks=c(0,.1,.3,.5,1)) + scale_x_continuous(limits=c(0,1),breaks=c(0,.1,.3,.5,1)) +
  geom_segment(x=.1,xend=.1,y=.1,yend=1.5,colour="red",linetype=2) + geom_segment(x=.1,xend=1.5,y=.1,yend=.1,colour="red",linetype=2) +
  geom_segment(x=.3,xend=.3,y=.3,yend=1.5,colour="orange",linetype=2) + geom_segment(x=.3,xend=1.5,y=.3,yend=.3,colour="orange",linetype=2) +
  geom_segment(x=.5,xend=.5,y=.5,yend=1.5,colour="green",linetype=2) + geom_segment(x=.5,xend=1.5,y=.5,yend=.5,colour="green",linetype=2) +
  geom_smooth(aes(y=ES.rep.r,x=ES.ori.r,group=stat.rep.decideNP,fill=stat.rep.decideNP),method="glm",fullrange=FALSE,alpha=.2) + 
  labs(title= paste("Original vs. Replication Effect Size (r)\nCorrelation Accept H0 = ",round(cor(x0$ES.rep.r,x0$ES.ori.r,use="complete.obs"), digits=4),"\nCorrelation Reject H0 = ",round(cor(x1$ES.rep.r,x1$ES.ori.r,use="complete.obs"), digits=4)), fill="Inference (Replication)",shape="Inference (Replication)") + 
  xlab("Effect size (r) original") + ylab("Effect size (r) replication") + 
  theme_bw(base_size = 16, base_family = "") + coord_fixed()

ggplot(RPPdata_cast[ok,]) + geom_point(aes(x=ES.ori.r, y=ES.rep.r,shape=stat.rep.decideNP),size=5)  +
  scale_y_continuous(limits=c(0,1),breaks=c(0,.1,.3,.5,1)) + scale_x_continuous(limits=c(0,1),breaks=c(0,.1,.3,.5,1)) +
  geom_segment(x=.1,xend=.1,y=.1,yend=1.5,colour="red",linetype=2) + geom_segment(x=.1,xend=1.5,y=.1,yend=.1,colour="red",linetype=2) +
  geom_segment(x=.3,xend=.3,y=.3,yend=1.5,colour="orange",linetype=2) + geom_segment(x=.3,xend=1.5,y=.3,yend=.3,colour="orange",linetype=2) +
  geom_segment(x=.5,xend=.5,y=.5,yend=1.5,colour="green",linetype=2) + geom_segment(x=.5,xend=1.5,y=.5,yend=.5,colour="green",linetype=2) +
  geom_smooth(aes(y=ES.rep.r,x=ES.ori.r,group=stat.rep.decideNP,fill=stat.rep.decideNP),method="glm",fullrange=FALSE,alpha=.2) + 
  labs(title= paste("Original vs. Replication Effect Size (r)\nCorrelation Accept H0 = ",round(cor(x0$ES.rep.r,x0$ES.ori.r,use="complete.obs"), digits=4),"\nCorrelation Reject H0 = ",round(cor(x1$ES.rep.r,x1$ES.ori.r,use="complete.obs"), digits=4)), fill="Inference (Replication)",shape="Inference (Replication)") + 
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
  labs(title= "Original vs. Replication Effect Size (r) with 95% CI\n(Circles represent p <. 05 on Replication)", fill="Inference (Replication)",shape="Inference (Replication)", colour="Effect Size\n(Inner = Original, Outer = Replication)") + 
  xlab("Effect size (r) original") + ylab("Effect size (r) replication") + mytheme + coord_fixed()

# Classify!


fit <- qda(RPPdata_cast$stat.rep.H1[ok]~RPPdata_cast$ES.ori.r[ok] + RPPdata_cast$stat.ori.ncp.p.recalc[ok], data=RPPdata_cast[ok,], prior=c(.5,.5),CV=T)
ct  <- table(RPPdata_cast$stat.rep.H1[ok], fit$class)
textplot(ct)
# total percent correct
pc <- sum(diag(prop.table(ct)))
drawparti(RPPdata_cast$stat.rep.H1[ok], RPPdata_cast$ES.ori.r[ok], RPPdata_cast$stat.ori.ncp.p.recalc[ok], data=RPPdata_cast, na.action="na.omit",method="qda",xlab="Original Effect Size r",ylab="Original p-value",legend.err = F,image.colors = terrain.colors(n=2,alpha=.5),col.correct = "darkgreen",col.mean = "grey40",col.wrong = "darkred")
title(main = "Quadratic Discriminant Analysis:\nReject H0 on Replication",sub=paste("Correctly classified:",round(pc*100,digits=2),"%"))

fit <- qda(RPPdata_cast$stat.rep.H1[ok]~RPPdata_cast$ES.ori.r[ok] + RPPdata_cast$ES.rep.r[ok], data=RPPdata_cast[ok,], prior=c(.5,.5),CV=T)
ct <- table(RPPdata_cast$stat.rep.H1[ok], fit$class)
# total percent correct
pc <- sum(diag(prop.table(ct)))
drawparti(RPPdata_cast$stat.rep.H1[ok], RPPdata_cast$ES.ori.r[ok], RPPdata_cast$ES.rep.r[ok], data=RPPdata_cast, na.action="na.omit",method="qda",xlab="Original Effect Size r",ylab="Replication Effect Size r",legend.err = F,image.colors = terrain.colors(n=2,alpha=.5),col.correct = "darkgreen",col.mean = "grey40",col.wrong = "darkred")
title(main = "Quadratic Discriminant Analysis:\nReject H0 on Replication",sub=paste("Correctly classified:",round(pc*100,digits=2),"%"))


# Exploratory meta-analysis
require(klaR)
require(MASS)
require(MAc)

id=c(which(ok),which(ok))
n =c(RPPdata_cast$stat.ori.N[ok],RPPdata_cast$stat.rep.N[ok])
r =c(RPPdata_cast$ES.ori.r[ok],RPPdata_cast$ES.rep.r[ok])
mod1 <- rep(c("Original","Replication"),each=length(which(ok)))
dat <- data.frame(id,n,r,mod1,mod2)
metadat <- agg(id = id, r = r, n=n,mod = mod1, data=dat) 
metadat$var.r <- var_r(metadat$r, metadat$n) # MAc function to derive variance
metadat$z <- r_to_z(metadat$r)  # MAc function to convert to Fisher's z (z')
metadat$var.z <- var_z(metadat$n)  # MAc function for variance of z'
metadat$mod1 <- factor(melt(t(cbind(RPPdata_cast$stat.ori.decideNP[ok],RPPdata_cast$stat.rep.decideNP[ok])))$value)

#sma <- mareg(r ~ mod*mod1, var = var.r, method = "REML",  data = metadat)
sma <- rma(yi=r, vi=var.r, mods= ~ mod*mod1, data=metadat)
text<-capture.output(summary(sma))
textplot(text)

plotcon(es = r, var = var.r, mod = mod1, data = metadat, method= "random", modname= "Inference" )
plotcon(es = r, var = var.r, mod = mod, data = metadat, method= "random", modname= "Study" )

forest(sma, xlim=c(-2,2),slab=paste("Study",metadat$id,levels(metadat$mod)),ilab=cbind(metadat$mod,levels(metadat$mod1)),ilab.xpos=c(-10,-1), order=order(rev(metadat$mod),metadat$r),rows=c(1:28,29:56), cex=.75,main="Forest plot RE model:\nES (r) = Study * N-P decision",xlab="Effect Size r")
 text(c(-1.8,-1), 58, c("Study", "N-P decision"))
 text(1.5,     58, "r [95% CI]")

funnel(sma,addtau2=T,level=c(90, 95, 99), shade=c("white", "gray", "darkgray"),refline=0, yaxis="seinv", main="Funnel plot RE model:\nES (r) = Study * N-P decision", xlab="Areas indicate ES pseudo CIs of [.90 .95 .99]")


# Influence plots
plot(influence(sma))  

dev.off()
