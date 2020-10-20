### Field experiment - ANOVAs
library(lmerTest)
library(reshape)
rm(list=ls())
dat <- read.csv("data.phenotype/tall.short_fieldexp_100617_removebiomassoutliers.csv",header=TRUE)
dat <- dat[dat$Survival.Yes==1,] # missing plots are removed.

y <- c("max.Height","Average.Height","num.live.stems","Total.biomass","Aboveground.Biomass..g.","Belowground.Biomass..g.","Above.below")
for(i in 1:length(y))
{
print(paste("response =",y[i]))
tmp <- dat[,c("Origin.Zone","Transplant.Zone","Transplant.Site","Origin.Site","Plot..","Transect")]
tmp$y <- dat[,colnames(dat)==as.character(y[i])]
tmp <- tmp[!is.na(tmp$y),]
tmp$y <- as.numeric(as.character(tmp$y))
m <- lmer(y~Transplant.Zone*Origin.Zone + (1|Plot../Transect) + (1|Transplant.Site) +  (1|Origin.Site),data=tmp)
print(anova(m))
}

### Field experiment - survival

library(coxme)
library(survival)
dat <- read.csv('data.phenotype/Tall.Short_field_survivalanalysis_011618.csv')
coxfit <- coxph(Surv(Week.to.death,Status_Dec_survive.0) ~  Origin.Zone * Transplant.Zone, data=dat)
print(anova(coxfit))

### Field experiment - flowering
library(car)
flower <- read.csv("data.phenotype/tall.short_flowertotals_060818_corrected.csv")
sflower <- subset(flower, Surivial == "1")
model <- glm(Flower.yes.1~ Transplant.Zone * Origin.Zone,family=binomial(link="logit"), data=sflower) 
print(Anova(model))


