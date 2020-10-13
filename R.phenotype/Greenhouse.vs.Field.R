### greenhouse final heights vs field collected heights

### plant height - CG
rm(list=ls())
library(readxl)
library(reshape)
dat <- read_xlsx('data.phenotype/CG_tall.short_repeatmeasure110116_final.xlsx',sheet=5)
dat$`Max Height` <- as.numeric(dat$`Max Height`)
dat$`Julian Day` <- as.factor(dat$`Julian Day`)
dat$`Tall/Short` <- as.factor(dat$`Tall/Short`)
dat <- dat[!is.na(dat$`Max Height`),]  # remove missing data
dat <- dat[dat$`Julian Day`=="428",]
xbar.CG <- tapply(dat$`Max Height`,dat$`Tall/Short`,mean)
sd.CG <- tapply(dat$`Max Height`,dat$`Tall/Short`,sd)
n.CG <- table(dat$`Tall/Short`)
se.CG <- sd.CG/sqrt(n.CG)

### plant height - field survey
dat2 <- read_xlsx('data.phenotype/tall.short_fieldsurvey_2015.xlsx',sheet=2)
dat2$Zone <- as.factor(dat2$Zone)
xbar.FS <- tapply(dat2$`AVERAGE HT`,dat2$Zone,mean)
sd.FS <- tapply(dat2$`AVERAGE HT`,dat2$Zone,sd)
n.FS <- table(dat2$Zone)
se.FS <- sd.FS/sqrt(n.FS)

out <- data.frame(
  xbar=c(xbar.CG,xbar.FS),
  n= c(as.numeric(n.CG),as.numeric(n.FS)),
  se=c(as.numeric(se.CG),as.numeric(se.FS)))
rownames(out) <- c("CG-short","CG-tall","FS-short","FS-tall")


pdf("output.phenotype/Greenhouse.vs.Field.pdf",height=4,width=3)
x <- c(0.75,0.75,1.2,1.2)
plot(x,y=out$xbar,xlim=c(0.6,1.4),ylim=c(0,100),pch=19,
     type="p",xaxt="n",xlab="",ylab="Stem height (cm)",col=c("red","black","red","black"))
arrows(x0=x,y0=out$xbar-out$se,x1=x,y1=out$xbar+out$se,length=.08,angle=90,code=3,col=c("red","black","red","black"))
mtext(c("Greenhouse","Field"),side=1,line=.5,at=c(0.75,1.2))
dev.off()
