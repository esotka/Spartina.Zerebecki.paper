### aster analysis of field experiment
### analysis of number of seeds + survival
### some model code borrowed from Samis et al 2016 Evolution 

library(aster)
rm(list=ls())
dat <- read.csv('data.phenotype/tall.short_flowertotals_102020.csv')
vars <- c("Surivial1","NumberFlower1", "Total.Seeds.Zero_noFlower")
dat <- dat[complete.cases(dat[,colnames(dat)%in%vars]),]

redat <- reshape (dat, varying = list(vars),direction = "long", timevar = "varb", times = as.factor(vars), v.names = "resp")

redat <- data.frame(redat, root = 1)

pred <- c(0,1,2)
fam <- c(1,2,3)
sapply(fam.default(), as.character)[fam]

fit <- grep("Total.Seeds.Zero_noFlower",as.character(redat$varb))
fit <- is.element(seq(along = redat$varb),fit)
redat <- data.frame(redat,fit=as.integer(fit))

out0 <- aster(resp ~ varb + fit, pred, fam, varb, id, root, data = redat)
out1 <- aster(resp ~ varb + fit:(Transplant.Zone), pred, fam, varb, id, root, data = redat)
out2 <- aster(resp ~ varb + fit:(Origin.Zone), pred, fam, varb, id, root, data = redat)
out3 <- aster(resp ~ varb + fit:(Transplant.Zone + Origin.Zone), pred, fam, varb, id, root, data = redat)
out4 <- aster(resp ~ varb + fit:(Transplant.Zone * Origin.Zone), pred, fam, varb, id, root, data = redat)
#summary(out1, show.graph = TRUE)

# likelihood ratio tests - ANOVA table
anova(out1,out3) # Origin Zone effect
anova(out2,out3) # Transplant Zone effect
anova(out3,out4) ## additve vs interactive model

### version with Blocked (Transplant site)
out0 <- aster(resp ~ varb + fit, pred, fam, varb, id, root, data = redat)
out0b <- aster(resp ~ varb + Origin.Site + fit, pred, fam, varb, id, root, data = redat)
out0c <- aster(resp ~ varb + Transplant.Site + fit, pred, fam, varb, id, root, data = redat)
out1b <- aster(resp ~ varb + Origin.Site + Transplant.Site + fit:(Transplant.Zone), pred, fam, varb, id, root, data = redat)
out2b <- aster(resp ~ varb + Origin.Site + Transplant.Site + fit:(Origin.Zone), pred, fam, varb, id, root, data = redat)
out3b <- aster(resp ~ varb + Origin.Site + Transplant.Site + fit:(Transplant.Zone + Origin.Zone), pred, fam, varb, id, root, data = redat)
out4b <- aster(resp ~ varb + Origin.Site + Transplant.Site + fit:(Transplant.Zone * Origin.Zone), pred, fam, varb, id, root, data = redat)

anova(out0,out0b) # Origin Site effect
anova(out0,out0c) # Transplant Site effect
anova(out1b,out3b) # Origin Zone effect
anova(out2b,out3b) # Transplant Zone effect
anova(out3b,out4b) ## additve vs interactive model



### Aster Models with Random effects (reaster()), with Transplant.Site (BI, FB, FJ) as random effect
rout1 <- reaster(resp ~ varb + fit:(Transplant.Zone), list(Transplant.Site = ~0 + fit:Transplant.Site),pred, fam, varb, id, root, data = redat)
rout2 <- reaster(resp ~ varb + fit:(Origin.Zone),list(Transplant.Site = ~0 + fit:Transplant.Site), pred, fam, varb, id, root, data = redat)
rout3 <- aster(resp ~ varb + fit:(Transplant.Zone + Origin.Zone), pred, fam, varb, id, root, data = redat)
rout4 <- reaster(resp ~ varb + fit:(Transplant.Zone * Origin.Zone), list(Transplant.Site = ~0 + fit:Transplant.Site), pred, fam, varb, id, root, data = redat)

# likelihood ratio tests - ANOVA table

#anova(rout1,rout3) # Origin Zone effect = didnt' resolve
#anova(rout2,rout3) # Transplant Zone effect = didn't resolve
anova(rout3,rout4) ## additve vs interactive model (compare with interaction of non-random Aster model)

### For graphical display, "we calculated unconditional expected values of lifetime fruit production and its standard error from fixed effects models (C. Geyer, pers. comm.)"

### Lifetime fitness between Source zones at Short Transplant Zone
sh <- redat[redat$Transplant.Zone=="Short",]
out.sh0 <- aster(resp ~ varb + fit, pred, fam, varb, id, root, data = sh)
out.sh1 <- aster(resp ~ varb + fit:(Origin.Zone), pred, fam, varb, id, root, data = sh)
anova(out.sh0,out.sh1)
p1 <- predict(out.sh1)
sh.xbar <- tapply(p1,sh$Origin.Zone,mean)
sh.sd <-tapply(p1,sh$Origin.Zone,sd)
sh.n <- tapply(p1,sh$Origin.Zone,length)
sh.se <- sh.sd/sqrt(sh.n)
### Lifetime fitness between Source zones at Tall Transplant Zone
ta <- redat[redat$Transplant.Zone=="Tall",]
out.ta0 <- aster(resp ~ varb + fit, pred, fam, varb, id, root, data = ta)
out.ta1 <- aster(resp ~ varb + fit:(Origin.Zone), pred, fam, varb, id, root, data = ta)
p1 <- predict(out.ta1)
ta.xbar <- tapply(p1,ta$Origin.Zone,mean)
ta.sd <-tapply(p1,ta$Origin.Zone,sd)
ta.n <- tapply(p1,ta$Origin.Zone,length)
ta.se <- ta.sd/sqrt(ta.n)

out <- data.frame(Transplant.Zone=c("Short","Short","Tall","Tall"),
           Origin.Zone=c(names(sh.xbar),names(ta.xbar)),
           xbar=c(sh.xbar,ta.xbar),n=c(sh.n,ta.n),se=c(sh.se,ta.se))

anova(out.sh0,out.sh1)
anova(out.ta0,out.ta1)



png('output.phenotype/aster model.png',width=4,height=5,units="in",res=700)
plot(x=c(0.75,1.2),y=out$xbar[1:2],xlim=c(0.6,1.4),ylim=c(0,10),
     type="n",xaxt="n",xlab="",ylab="Composite fitness")
# short origin
segments(x0 = .75,y0 = out$xbar[1],x1 = 1.2,y1 = out$xbar[3],col="red")
points(x=c(0.75,1.2),y=out$xbar[c(1,3)],pch=c(21,19),col="red",cex=2,bg=c("white","red"))
# tall origin
segments(.75,out$xbar[2],1.2,out$xbar[4],col="black")
points(x=c(0.75,1.2),y=out$xbar[c(2,4)],pch=c(19,21),col="black",cex=2,bg=c("black","white"))
mtext(c("Short","Tall"),at=c(0.75,1.2),side=1,line=0.75)
mtext("Transplant zone",at=1,side=1,line=2)
text(1.1,5,"Short origin",col="red")
text(0.9,1,"Tall origin",col="black")
dev.off()

