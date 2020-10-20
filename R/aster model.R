### aster analysis of field experiment
### analysis of number of seeds + survival

library(aster)
rm(list=ls())
#dat <- read.csv("data.phenotype/tall.short_flowertotals_060818_corrected.csv",header=TRUE)
dat <-read.csv("data.phenotype/tall.short_flowertotals_091520_RemoveMsplots.csv")
meta <- paste(dat$Field.Site,dat$Transect,dat$Plot..,dat$Genotype.ID)
dat2 <- read.csv("data.phenotype/tall.short_fieldexp_100617_removebiomassoutliers.csv",header=TRUE)
meta2 <- paste(dat2$Field.Site,dat2$Transect,dat2$Plot..,dat2$Genotype.ID)
dat$Total.Seeds.with.zeros <- dat2$Total.Seeds.with.zeros[match(meta,meta2)]
#dat <- dat[!is.na(dat$Surivial),] # pull missing plots
dat$Total.Seeds.with.zeros[is.na(dat$Total.Seeds.with.zeros)] <- 0
dat$Transect  <- as.factor(dat$Transect)
dat$Plot..  <-as.factor(dat$Plot..)
dat$Total.Seeds.with.zeros <- as.integer(dat$Total.Seeds.with.zeros)
dat$Flower1[dat$Total.Seeds.with.zeros==83] <- 1
#all <- dat[,c("Origin.Zone","Transplant.Zone","Transplant.Site","Origin.Site","Plot..","Transect","Surivial","Flower.yes.1","Total.Seeds.with.zeros")]
vars <- c("Surivial1","Flower1", "Total.Seeds.with.zeros")


redat <- reshape (dat, varying = list(vars),direction = "long", timevar = "varb", times = as.factor(vars), v.names = "resp")

redat <- data.frame(redat, root = 1)

pred <- c(0,1,2)
fam <- c(1,1,2)
sapply(fam.default(), as.character)[fam]

aout1 <- aster(resp ~ varb ,pred, fam, varb, id, root,data=redat)

print(summary(aout1, show.graph = TRUE))

aout2 <- aster(resp ~ varb + Transplant.Zone,pred=pred, fam=fam, varvar=varb, idvar=id, root=root, data = redat)
print(summary(aout2, show.graph = TRUE))

aout3 <- aster(resp ~ varb + Origin.Zone,pred, fam, varb, id, root, data = redat)
print(summary(aout3, show.graph = TRUE))

aout4 <- aster(resp ~ varb + Transplant.Zone + Origin.Zone,
               +     pred, fam, varb, id, root, data = redat)
print(summary(aout4, show.graph = TRUE))

aout5 <- aster(resp ~ varb + Transplant.Zone * Origin.Zone,
               +  pred, fam, varb, id, root, data = redat)
print(summary(aout5, show.graph = TRUE))

print(anova(aout1, aout2, aout4, aout5))
print(anova(aout1, aout3, aout4, aout5))
## build with transplant zone (it solely is the significant)
## build with origin zone (can only nest), additive effects (so presence of transplant zone is important)

### testing from R code book
## way to predict the values......

## what if only use survival adn flowering
vars <- c("Surivial1","Flower1")
pred <- c(0,1)
fam <- c(1,1)

anova(aout1, aout2, aout4, aout5)
anova(aout1, aout3, aout4, aout5)
## same that transplant is significant effects on fitness...
