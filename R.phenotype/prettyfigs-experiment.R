### make 6 panel pretty figs from experiment
rm(list=ls())
library(reshape)
#quartz(width=5,height=9)
png('output.phenotype/prettyfigs-experiment.png',width=5,height=9,units="in",res=700)
par(mfrow=c(3,2),mai=c(.4,.3,.3,.2))

### max plant height of survivors ###
dat <- read.csv("data.phenotype/tall.short_fieldexp_100617_removebiomassoutliers.csv",header=TRUE)
dat <- dat[!is.na(dat$Survival.Yes),] # missing plots are removed.
tmp <- dat[dat$max.Height>0,]
s <- tmp[,c("Origin.Zone","Transplant.Zone","max.Height")]
s <- melt(s)
xbar <- cast(s,Origin.Zone~Transplant.Zone,mean,na.rm=T)
s.se <- cast(s,Origin.Zone~Transplant.Zone,sd,na.rm=T)
n <- cast(s,Origin.Zone~Transplant.Zone,length)
s.se[,2:3] <- s.se[,2:3]/sqrt(n[,2:3])
print(xbar)
plot(x=c(0.75,1.2),y=xbar[1,2:3],xlim=c(0.6,1.4),ylim=c(25,55),
     type="n",xaxt="n",xlab="")
mtext("A. Maximum height (cm)",cex=.9,line=.5)
# short origin
arrows(x0=c(0.75,1.2),y0=as.numeric(xbar[1,2:3]-s.se[1,2:3]),x1=c(0.75,1.2),y1=as.numeric(xbar[1,2:3]+s.se[1,2:3]),col="red",length=.08,angle=90,code=3)
segments(.75,xbar[1,2],1.2,xbar[1,3],col="red")
points(x=c(0.75,1.2),y=xbar[1,2:3],pch=c(21,19),col="red",cex=2,bg=c("white","red"))
# tall origin
arrows(x0=c(0.75,1.2),y0=as.numeric(xbar[2,2:3]-s.se[1,2:3]),x1=c(0.75,1.2),y1=as.numeric(xbar[2,2:3]+s.se[1,2:3]),col="black",length=.08,angle=90,code=3)
segments(.75,xbar[2,2],1.2,xbar[2,3],col="black")
points(x=c(0.75,1.2),y=xbar[2,2:3],pch=c(19,21),col="black",cex=2,bg=c("black","white"))
mtext(c("Short","Tall"),at=c(0.75,1.2),side=1,line=0.75)

### above to belowground ###
dat <- read.csv("data.phenotype//tall.short_fieldexp_100617_removebiomassoutliers.csv",header=TRUE)
dat <- dat[!is.na(dat$Survival.Yes),] # missing plots are removed.
dat <- dat[!is.na(dat$Above.below),]
s <- dat[,c("Origin.Zone","Transplant.Zone","Above.below")]
s <- melt(s)
xbar <- cast(s,Origin.Zone~Transplant.Zone,mean,na.rm=T)
s.se <- cast(s,Origin.Zone~Transplant.Zone,sd,na.rm=T)
n <- cast(s,Origin.Zone~Transplant.Zone,length)
s.se[,2:3] <- s.se[,2:3]/sqrt(n[,2:3])
print(xbar)
plot(x=c(0.75,1.2),y=xbar[1,2:3],xlim=c(0.6,1.4),ylim=c(0.6,1.2),
     type="n",xaxt="n",xlab="")
mtext("B. Above-to-Belowground Ratio",cex=.9,line=.5)
# short origin
arrows(x0=c(0.75,1.2),y0=as.numeric(xbar[1,2:3]-s.se[1,2:3]),x1=c(0.75,1.2),y1=as.numeric(xbar[1,2:3]+s.se[1,2:3]),col="red",length=.08,angle=90,code=3)
segments(.75,xbar[1,2],1.2,xbar[1,3],col="red")
points(x=c(0.75,1.2),y=xbar[1,2:3],pch=c(21,19),col="red",cex=2,bg=c("white","red"))
# tall origin
arrows(x0=c(0.75,1.2),y0=as.numeric(xbar[2,2:3]-s.se[1,2:3]),x1=c(0.75,1.2),y1=as.numeric(xbar[2,2:3]+s.se[1,2:3]),col="black",length=.08,angle=90,code=3)
segments(.75,xbar[2,2],1.2,xbar[2,3],col="black")
points(x=c(0.75,1.2),y=xbar[2,2:3],pch=c(19,21),col="black",cex=2,bg=c("black","white"))
mtext(c("Short","Tall"),at=c(0.75,1.2),side=1,line=0.75)
segments(-1,1,3,1,lty=2)

### stem density ###
dat <- read.csv("data.phenotype//tall.short_fieldexp_100617_removebiomassoutliers.csv",header=TRUE)
dat <- dat[!is.na(dat$Survival.Yes),] # missing plots are removed.
dat <- dat[!is.na(dat$num.live.stems),]
s <- dat[,c("Origin.Zone","Transplant.Zone","num.live.stems")]
s <- melt(s)
xbar <- cast(s,Origin.Zone~Transplant.Zone,mean,na.rm=T)
s.se <- cast(s,Origin.Zone~Transplant.Zone,sd,na.rm=T)
n <- cast(s,Origin.Zone~Transplant.Zone,length)
s.se[,2:3] <- s.se[,2:3]/sqrt(n[,2:3])
print(xbar)
plot(x=c(0.75,1.2),y=xbar[1,2:3],xlim=c(0.6,1.4),ylim=c(0,14),
     type="n",xaxt="n",xlab="")
mtext("C. Stem density",cex=.9,line=.5)
# short origin
arrows(x0=c(0.75,1.2),y0=as.numeric(xbar[1,2:3]-s.se[1,2:3]),x1=c(0.75,1.2),y1=as.numeric(xbar[1,2:3]+s.se[1,2:3]),col="red",length=.08,angle=90,code=3)
segments(.75,xbar[1,2],1.2,xbar[1,3],col="red")
points(x=c(0.75,1.2),y=xbar[1,2:3],pch=c(21,19),col="red",cex=2,bg=c("white","red"))
# tall origin
arrows(x0=c(0.75,1.2),y0=as.numeric(xbar[2,2:3]-s.se[1,2:3]),x1=c(0.75,1.2),y1=as.numeric(xbar[2,2:3]+s.se[1,2:3]),col="black",length=.08,angle=90,code=3)
segments(.75,xbar[2,2],1.2,xbar[2,3],col="black")
points(x=c(0.75,1.2),y=xbar[2,2:3],pch=c(19,21),col="black",cex=2,bg=c("black","white"))
mtext(c("Short","Tall"),at=c(0.75,1.2),side=1,line=0.75)

## Total biomass ###
dat <- read.csv("data.phenotype//tall.short_fieldexp_100617_removebiomassoutliers.csv",header=TRUE)
dat <- dat[!is.na(dat$Survival.Yes),] # missing plots are removed.
dat <- dat[!is.na(dat$Total.biomass),]
s <- dat[,c("Origin.Zone","Transplant.Zone","Total.biomass")]
s <- melt(s)
xbar <- cast(s,Origin.Zone~Transplant.Zone,mean,na.rm=T)
s.se <- cast(s,Origin.Zone~Transplant.Zone,sd,na.rm=T)
n <- cast(s,Origin.Zone~Transplant.Zone,length)
s.se[,2:3] <- s.se[,2:3]/sqrt(n[,2:3])
print(xbar)
plot(x=c(0.75,1.2),y=xbar[1,2:3],xlim=c(0.6,1.4),ylim=c(0,14),
     type="n",xaxt="n",xlab="")
mtext("D. Total biomass (g)",cex=.9,line=.5)
# short origin
arrows(x0=c(0.75,1.2),y0=as.numeric(xbar[1,2:3]-s.se[1,2:3]),x1=c(0.75,1.2),y1=as.numeric(xbar[1,2:3]+s.se[1,2:3]),col="red",length=.08,angle=90,code=3)
segments(.75,xbar[1,2],1.2,xbar[1,3],col="red")
points(x=c(0.75,1.2),y=xbar[1,2:3],pch=c(21,19),col="red",cex=2,bg=c("white","red"))
# tall origin
arrows(x0=c(0.75,1.2),y0=as.numeric(xbar[2,2:3]-s.se[1,2:3]),x1=c(0.75,1.2),y1=as.numeric(xbar[2,2:3]+s.se[1,2:3]),col="black",length=.08,angle=90,code=3)
segments(.75,xbar[2,2],1.2,xbar[2,3],col="black")
points(x=c(0.75,1.2),y=xbar[2,2:3],pch=c(19,21),col="black",cex=2,bg=c("black","white"))
mtext(c("Short","Tall"),at=c(0.75,1.2),side=1,line=0.75)

### Number of seeded plants ###
dat <- read.csv("data.phenotype//tall.short_fieldexp_100617_removebiomassoutliers.csv",header=TRUE)
dat <- dat[!is.na(dat$Survival.Yes),] # missing plots are removed.
dat <- dat[!is.na(dat$Flowering.Yes),]
s <- dat[,c("Origin.Zone","Transplant.Zone","Flowering.Yes")]
s <- melt(s)
xbar <- cast(s,Origin.Zone~Transplant.Zone,mean,na.rm=T)
print(xbar)
plot(x=c(0.75,1.2),y=xbar[1,2:3],xlim=c(0.6,1.4),ylim=c(0,.2),
     type="n",xaxt="n",xlab="")
mtext("E. Flowering plants frequency",cex=.9,line=.5)
# short origin
segments(.75,xbar[1,2],1.2,xbar[1,3],col="red")
points(x=c(0.75,1.2),y=xbar[1,2:3],pch=c(21,19),col="red",cex=2,bg=c("white","red"))
# tall origin
segments(.75,xbar[2,2],1.2,xbar[2,3],col="black")
points(x=c(0.75,1.2),y=xbar[2,2:3],pch=c(19,21),col="black",cex=2,bg=c("black","white"))
mtext(c("Short","Tall"),at=c(0.75,1.2),side=1,line=0.75)
dev.off()
