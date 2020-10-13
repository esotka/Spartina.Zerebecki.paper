# survival
dat <- dat[!is.na(dat$Survival.Yes),] # missing plots are removed.
s <- dat[,c("Origin.Zone","Transplant.Zone","Survival.Yes")]
s <- melt(s)
sd <- cast(s,Origin.Zone~Transplant.Zone,sum)
n <- cast(s,Origin.Zone~Transplant.Zone,length)
n[,3] <- n[,3]+30## add in death of BI
surv <- data.frame(short.garden=sd$`Short`/n$`Short`,tall.garden=sd$`Tall`/n$`Tall`)
print(sd)
plot(x=c(0.75,1.2),y=surv[1,],xlim=c(0.6,1.4),ylim=c(0,1),
     type="b",xaxt="n",xlab="",pch=19,col="red",cex=2,
     main="A. Survivorship (8 mths)")
points(x=c(0.75,1.2),y=surv[2,],type="b",col="black",pch=19,cex=2)
legend(0.57,0.22,c("Short origin","Tall origin"),bty = "n",col=c("red","black"),pt.cex=3,cex=1.3,pch=20)
mtext(c("Short","Tall"),at=c(0.75,1.2),side=1,line=0.75)
mtext("Transplant zone",at=0.95,side=1,line=2.2)
