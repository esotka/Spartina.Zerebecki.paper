## analysis of field-collected seed weight.  

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
dat <- read_xlsx('data.phenotype/CG_tall.short_repeatmeasure110116_final.xlsx',sheet=5)
dat$`Max Height` <- as.numeric(dat$`Max Height`)
dat$`Julian Day` <- as.factor(dat$`Julian Day`)
dat$`Tall/Short` <- as.factor(dat$`Tall/Short`)
dat <- dat[!is.na(dat$`Max Height`),]
tmp <- data.frame(dat[,c("Max Height","Tall/Short","Site","Julian Day")])
tmp <- melt(tmp)
xbar.ht <- cast(tmp[tmp$Julian.Day=="428",],~Site+Tall.Short,mean,na.rm=T)

### seed weight
dat <- read.csv('data.phenotype/tall_short_seedweight.csv')
xbar.seed <- tapply(dat$Individual.Seed.Weight..mg.,dat$Site_Form,mean,na.rm=T)

xbar.both <- data.frame(xbar.ht=as.numeric(xbar.ht[-1]),xbar.seed)
print("correlation of seed weight and plant height (common garden experiment)")
print(summary(lm(xbar.both$xbar.ht~xbar.both$xbar.seed)))

