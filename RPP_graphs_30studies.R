# Use with RPPdata
load(file="RPPdata_15_05_2140.Rdata")

x1 <- subset(RPPdata,as.numeric(rep.sig)==2)
x0 <- subset(RPPdata,as.numeric(rep.sig)==1)

pdf("RPP_Figures_30studies.pdf",paper="a4r",width=0,height=0)

par(mfrow=c(1,2),pty="s")
plot(RPPdata$stat.ori.p, RPPdata$stat.ori.p.recalc, xlim=c(0,0.06), ylim=c(0,0.06), xlab="Reported p-value [original study]",ylab="Re-calculated p-value [original study]")
plot(RPPdata$stat.rep.p,RPPdata$stat.rep.p.recalc,xlab="Reported p-value [replication study]",ylab="Re-calculated p-value [replication study]")
par(mfrow=c(1,1))

ggplot(RPPdata) + geom_point(aes(x=stat.ori.p.recalc, y=stat.rep.p.recalc),size=5) +
  scale_y_continuous(limits=c(0,1),breaks=c(0,.05,.5,1)) + scale_x_continuous(limits=c(0,1),breaks=c(0,.05,.5,1)) +
  geom_hline(yintercept=.05,colour="red",linetype=2) + geom_vline(xintercept=.05,colour="red",linetype=2)  +
  labs(title= paste("Original vs. Replication p-value \ncorrelation =",round(cor(RPPdata$stat.ori.p.recalc,RPPdata$stat.rep.p.recalc,use="complete.obs"),digits=2)), colour="Inference (Replication)") + 
  xlab("P-value original") + ylab("P-value replication") + 
  theme_bw(base_size = 16, base_family = "")+ coord_fixed()

ggplot(RPPdata) + geom_point(aes(x=stat.ori.p.recalc, y=stat.rep.p.recalc),size=5) +
  scale_y_continuous(limits=c(0,1),breaks=c(0,.05,.5,1)) + scale_x_continuous(limits=c(0,.06),breaks=c(0,.05,.5,1)) +
  geom_hline(yintercept=.05,colour="red",linetype=2) + geom_vline(xintercept=.05,colour="red",linetype=2)  +
  labs(title= paste("Original vs. Replication p-value \ncorrelation =",round(cor(RPPdata$stat.ori.p.recalc,RPPdata$stat.rep.p.recalc,use="complete.obs"),digits=2)), colour="Inference (Replication)") + 
  geom_smooth(aes(y=stat.rep.p.recalc,x=stat.ori.p.recalc),colour="black",method="glm",fullrange=FALSE,alpha=.2) + 
  xlab("P-value original") + ylab("P-value replication") + 
  theme_bw(base_size = 16, base_family = "")

ggplot(RPPdata) + geom_point(aes(x=ES.ori.r, y=ES.rep.r),size=5)  +
  scale_y_continuous(limits=c(0,1),breaks=c(0,.1,.3,.5,1)) + scale_x_continuous(limits=c(0,1),breaks=c(0,.1,.3,.5,1)) +
  geom_segment(x=.1,xend=.1,y=.1,yend=1.5,colour="red",linetype=2) + geom_segment(x=.1,xend=1.5,y=.1,yend=.1,colour="red",linetype=2) +
  geom_segment(x=.3,xend=.3,y=.3,yend=1.5,colour="orange",linetype=2) + geom_segment(x=.3,xend=1.5,y=.3,yend=.3,colour="orange",linetype=2) +
  geom_segment(x=.5,xend=.5,y=.5,yend=1.5,colour="green",linetype=2) + geom_segment(x=.5,xend=1.5,y=.5,yend=.5,colour="green",linetype=2) +
  geom_smooth(aes(y=ES.rep.r,x=ES.ori.r),colour="black",method="glm",fullrange=FALSE,alpha=.2) + 
  labs(title= paste("Original vs. Replication Effect Size (r)\nCorrelation = ",round(cor(RPPdata$ES.rep.r,RPPdata$ES.ori.r,use="complete.obs"),digits=4)), colour="Inference (Replication)") + 
  xlab("Effect size (r) original") + ylab("Effect size (r) replication") + 
  theme_bw(base_size = 16, base_family = "")+ coord_fixed()

ggplot(RPPdata) + geom_point(aes(x=ES.ori.r, y=ES.rep.r,shape=rep.sig),size=5)  +
  scale_y_continuous(limits=c(0,1),breaks=c(0,.1,.3,.5,1)) + scale_x_continuous(limits=c(0,1),breaks=c(0,.1,.3,.5,1)) +
  geom_segment(x=.1,xend=.1,y=.1,yend=1.5,colour="red",linetype=2) + geom_segment(x=.1,xend=1.5,y=.1,yend=.1,colour="red",linetype=2) +
  geom_segment(x=.3,xend=.3,y=.3,yend=1.5,colour="orange",linetype=2) + geom_segment(x=.3,xend=1.5,y=.3,yend=.3,colour="orange",linetype=2) +
  geom_segment(x=.5,xend=.5,y=.5,yend=1.5,colour="green",linetype=2) + geom_segment(x=.5,xend=1.5,y=.5,yend=.5,colour="green",linetype=2) +
  geom_smooth(aes(y=ES.rep.r,x=ES.ori.r,group=rep.sig,fill=rep.sig),method="glm",fullrange=FALSE,alpha=.2) + 
  labs(title= paste("Original vs. Replication Effect Size (r)\nCorrelation Accept H0 = ",round(cor(x0$ES.rep.r,x0$ES.ori.r,use="complete.obs"), digits=4),"\nCorrelation Reject H0 = ",round(cor(x1$ES.rep.r,x1$ES.ori.r,use="complete.obs"), digits=4)), fill="Inference (Replication)",shape="Inference (Replication)") + 
  xlab("Effect size (r) original") + ylab("Effect size (r) replication") + 
  theme_bw(base_size = 16, base_family = "") + coord_fixed()

dev.off()
