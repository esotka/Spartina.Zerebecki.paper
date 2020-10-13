### plant height - repeated measures
rm(list=ls())
library(readxl)
library(reshape)
dat <- read_xlsx('data.phenotype/CG_tall.short_repeatmeasure110116_final.xlsx',sheet=5)
dat$`Max Height` <- as.numeric(dat$`Max Height`)
dat$`Julian Day` <- as.factor(dat$`Julian Day`)
dat$`Tall/Short` <- as.factor(dat$`Tall/Short`)
dat <- dat[!is.na(dat$`Max Height`),]
tmp <- data.frame(dat[,c("Max Height","Tall/Short","Julian Day")])
tmp <- melt(tmp)
xbar <- cast(tmp,Julian.Day~Tall.Short,mean,na.rm=T)
stdev <- cast(tmp,Julian.Day~Tall.Short,sd,na.rm=T)
n <- cast(tmp,Julian.Day~Tall.Short)
se <- stdev[,2:3]/sqrt(n[,2:3])
pdf('output.phenotype/CG plot.pdf',width=5,height=4)
xbar$Julian.Day <- as.numeric(as.character(xbar$Julian.Day))
plot(x=xbar$Julian.Day,xbar$Tall,type="b",pch=19,ylim=c(0,45),ylab="Maximum Stem Height (cm)",xlab="Days in Greenhouse")
points(x=xbar$Julian.Day,xbar$Short,type="b",pch=19,col="red")
arrows(x0 = xbar$Julian.Day,y0=xbar$Tall+se$Tall,
       x1 = xbar$Julian.Day,y1=xbar$Tall-se$Tall,
       col="black",length=.08,angle=90,code=3)
arrows(x0 = xbar$Julian.Day,y0=xbar$Short+se$Short,
       x1 = xbar$Julian.Day,y1=xbar$Short-se$Short,
       col="red",length=.08,angle=90,code=3)
dev.off()
