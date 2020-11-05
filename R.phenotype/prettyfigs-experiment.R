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

### survival
library(survival)
survival <- read.csv("data.phenotype/Tall.Short_field_survivalanalysis_011618.csv")


survival$origin.by.transplant <- factor(paste(survival$Origin.Zone,".to.",survival$Transplant.Zone,sep=""))
(fit2 <- survfit(Surv(as.numeric(survival$Week.to.death), survival$Status_Dec_survive.0) ~ origin.by.transplant, data=survival))

plot(x=fit2$time,y=fit2$surv,xlab="Weeks",ylab="",ylim=c(0,1),xlim=c(0,30),type="n")
mtext("F. Survivorship",cex=.9,line=.5)
# groups
f <- c(rep(names(fit2$strata)[1],as.numeric(fit2$strata)[1]),
       rep(names(fit2$strata)[2],as.numeric(fit2$strata)[2]),
       rep(names(fit2$strata)[3],as.numeric(fit2$strata)[3]),
       rep(names(fit2$strata)[4],as.numeric(fit2$strata)[4]))
# short.to.short
time <- fit2$time[f=="origin.by.transplant=Short.to.Short"]
lx <- fit2$surv[f=="origin.by.transplant=Short.to.Short"]
se <- fit2$std.err[f=="origin.by.transplant=Short.to.Short"]
points(x=c(0,time),y=c(1,lx),col="pink",cex=1,pch=19,type="b")
points(x=c(0,time),y=c(1,fit2$lx)+c(0,se),type="l",col="pink",lty="dotted")
points(x=c(0,time),y=c(1,lx)-c(0,se),type="l",col="pink",lty="dotted")

# short.to.tall
time <- fit2$time[f=="origin.by.transplant=Short.to.Tall"]
lx <- fit2$surv[f=="origin.by.transplant=Short.to.Tall"]
se <- fit2$std.err[f=="origin.by.transplant=Short.to.Tall"]
points(x=c(0,time),y=c(1,lx),col="red",cex=1,pch=19,type="b")
points(x=c(0,time),y=c(1,lx)+c(0,se),type="l",col="red",lty="dotted")
points(x=c(0,time),y=c(1,lx)-c(0,se),type="l",col="red",lty="dotted")

# tall to short
time <- fit2$time[f=="origin.by.transplant=Tall.to.Short"]
lx <- fit2$surv[f=="origin.by.transplant=Tall.to.Short"]
se <- fit2$std.err[f=="origin.by.transplant=Tall.to.Short"]
points(x=c(0,time),y=c(1,lx),col="grey",cex=1,pch=19,type="b")
points(x=c(0,time),y=c(1,lx)+c(0,se),type="l",col="grey",lty="dotted")
points(x=c(0,time),y=c(1,lx)-c(0,se),type="l",col="grey",lty="dotted")

# tall to short
time <- fit2$time[f=="origin.by.transplant=Tall.to.Tall"]
lx <- fit2$surv[f=="origin.by.transplant=Tall.to.Tall"]
se <- fit2$std.err[f=="origin.by.transplant=Tall.to.Tall"]
points(x=c(0,time),y=c(1,lx),col="black",cex=1,pch=19,type="b")
points(x=c(0,time),y=c(1,lx)+c(0,se),type="l",col="black",lty="dotted")
points(x=c(0,time),y=c(1,lx)-c(0,se),type="l",col="black",lty="dotted")

text(x=c(20,20),y=c(0.77,0.14),c("Short-zone","Tall-zone"),col=c("grey","black"),cex=1.5)
dev.off()
