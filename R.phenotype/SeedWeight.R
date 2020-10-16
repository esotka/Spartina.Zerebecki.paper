## analysis of field-collected seed weight.  
library(lattice)
rm(list=ls())
dat <- read.csv('data.phenotype/tall_short_seedweight.csv')
m <- lm(Individual.Seed.Weight..mg.~Site*Form,data=dat)
print(anova(m))
# BI
print("BI")
m <- lm(Individual.Seed.Weight..mg.~Form,data=dat[dat$Site=="Bowens Island",])
print(anova(m))
# FB
print("FB")
m <- lm(Individual.Seed.Weight..mg.~Form,data=dat[dat$Site=="Folly Beach",])
print(anova(m))
# FJ
print("FJ")
m <- lm(Individual.Seed.Weight..mg.~Form,data=dat[dat$Site=="Fort Johnson",])
print(anova(m))

pdf('output.phenotype/SeedWeight.pdf')
print(bwplot(Individual.Seed.Weight..mg.~Site_Form,data=dat))
dev.off()

#### correlation of seed weight and plant height (common garden experiment)
library(readxl)
library(reshape)
### common garden data
dat <- read.csv('data.phenotype/CG_final.csv')
dat.surv <- dat[dat$Survival==1 & dat$Julian.Day=="428",]
tmp <- data.frame(dat.surv[,c("Max.Height","Tall.Short","Julian.Day","Site")])
tmp <- melt(tmp)
xbar.ht <- cast(tmp,~Site+Tall.Short,mean,na.rm=T)

### seed weight
dat <- read.csv('data.phenotype/tall_short_seedweight.csv')
xbar.seed <- tapply(dat$Individual.Seed.Weight..mg.,dat$Site_Form,mean,na.rm=T)

xbar.both <- data.frame(xbar.ht=as.numeric(xbar.ht[-1]),xbar.seed)
print("correlation of seed weight and plant height (common garden experiment)")
print(summary(lm(xbar.both$xbar.ht~xbar.both$xbar.seed)))

