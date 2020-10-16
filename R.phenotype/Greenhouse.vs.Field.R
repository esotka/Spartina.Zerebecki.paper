### greenhouse final heights vs field collected heights

### plant height - CG
rm(list=ls())
library(reshape)
dat <- read.csv('data.phenotype/CG_final.csv')
dat.surv <- dat[dat$Survival==1 & dat$Julian.Day=="428",]
dat.surv$Julian.Day <- as.factor(dat.surv$Julian.Day)
xbar.CG <- tapply(dat.surv$Max.Height,dat.surv$Tall.Short,mean)
sd.CG <- tapply(dat.surv$Max.Height,dat.surv$Tall.Short,sd)
n.CG <- table(dat.surv$Tall.Short)
se.CG <- sd.CG/sqrt(n.CG)

### plant height - field survey
dat2 <- read.csv('data.phenotype/tall.short_fieldsurvey_2015.csv')
dat2$Zone <- as.factor(dat2$Zone)
xbar.FS <- tapply(dat2$AVERAGE.HT,dat2$Zone,mean)
sd.FS <- tapply(dat2$AVERAGE.HT,dat2$Zone,sd)
n.FS <- table(dat2$Zone)
se.FS <- sd.FS/sqrt(n.FS)

out <- data.frame(
  xbar=c(xbar.CG,xbar.FS),
  n= c(as.numeric(n.CG),as.numeric(n.FS)),
  se=c(as.numeric(se.CG),as.numeric(se.FS)))
rownames(out) <- c("CG-short","CG-tall","FS-short","FS-tall")
print(out)

pdf("output.phenotype/Greenhouse.vs.Field.pdf",height=4,width=3)
x <- c(0.75,0.75,1.2,1.2)
plot(x,y=out$xbar,xlim=c(0.6,1.4),ylim=c(0,100),pch=19,
     type="p",xaxt="n",xlab="",ylab="Stem height (cm)",col=c("red","black","red","black"))
arrows(x0=x,y0=out$xbar-out$se,x1=x,y1=out$xbar+out$se,length=.08,angle=90,code=3,col=c("red","black","red","black"))
mtext(c("Greenhouse","Field"),side=1,line=.5,at=c(0.75,1.2))
dev.off()
