### Analysis of survivorship - common garden
library(coxme)
library(survival)
rm(list=ls())
CG <- read.csv("data.phenotype/CG_survival_final.csv")


coxfit1 <- coxme(Surv(as.numeric(Day.to.death),Status_end_survive.0) ~  Tall.Short + (1|Site), data=CG)
coxfit2 <- coxme(Surv(as.numeric(Day.to.death),Status_end_survive.0) ~   (1|Site), data=CG)
print(anova(coxfit1,coxfit2))

fit1 <- survfit(Surv(Day.to.death,Status_end_survive.0) ~  Tall.Short , data=CG)


plot(x=fit1$time,y=fit1$surv,xlab="Weeks",ylab="",ylim=c(0,1),type="n",xlim=c(0,500))
# short origin
n = as.numeric(fit1$strata[1])
points(x=c(0,fit1$time[1:n]),y=c(1,fit1$surv[1:n]),col="grey",cex=1,pch=19,type="b")
points(x=c(0,fit1$time[1:n]),y=c(1,fit1$surv[1:n])+c(0,fit1$std.err[1:n]),type="l",col="grey",lty="dotted")
points(x=c(0,fit1$time[1:n]),y=c(1,fit1$surv[1:n])-c(0,fit1$std.err[1:n]),type="l",col="grey",lty="dotted")
# tall origin
n2 = as.numeric(fit1$strata[2])
points(x=c(0,fit1$time[n+1:n2]),y=c(1,fit1$surv[n+1:n2]),col="black",cex=1,pch=19,type="b")
points(x=c(0,fit1$time[n+1:n2]),y=c(1,fit1$surv[n+1:n2])+c(0,fit1$std.err[n+1:n2]),type="l",col="black",lty="dotted")
points(x=c(0,fit1$time[n+1:n2]),y=c(1,fit1$surv[n+1:n2])-c(0,fit1$std.err[n+1:n2]),type="l",col="black",lty="dotted")

