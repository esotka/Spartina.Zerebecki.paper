### Field experiment - published ANOVAs
library(lmerTest)
library(reshape)
rm(list=ls())
dat <- read.csv("data.phenotype/tall.short_fieldexp_100617_removebiomassoutliers.csv",header=TRUE)
dat <- dat[dat$Survival.Yes==1,] # missing plots are removed.

stats1 <- c()

sink("output.phenotype/field.experiment.analysis.SitesAsFixedEffects.txt")
y <- c("max.Height","Average.Height","num.live.stems","Total.biomass","Aboveground.Biomass..g.","Belowground.Biomass..g.","Above.below")
for(i in 1:length(y))
{
print(paste("response =",y[i]))
tmp <- dat[,c("Origin.Zone","Transplant.Zone","Transplant.Site","Origin.Site","Plot..","Transect")]
tmp$y <- dat[,colnames(dat)==as.character(y[i])]
tmp <- tmp[!is.na(tmp$y),]
tmp$y <- as.numeric(as.character(tmp$y))
m <- lm(y~Transplant.Site + Origin.Site+ Transplant.Zone*Origin.Zone,data=tmp)
print(anova(m))
stats1 <- rbind(stats1,data.frame(y=y[i],x=c("Transplant Site","Origin Site", "Transplant.Zone","Origin.Zone","Interaction"),p=round(anova(m)$'Pr(>F)'[1:5],3)))
}
sink()

### Field experiment - ANOVAs using siteXzone as "home vs away"
dat$Origin.Zone.Site <- factor(paste(dat$Origin.Zone,dat$Origin.Site,sep="."))
dat$Transplant.Zone.Site <- factor(paste(dat$Transplant.Zone,dat$Transplant.Site,sep="."))
stats2 <- c()
for(i in 1:length(y))
{
  print(paste("response =",y[i]))
  tmp <- dat[,c("Origin.Zone.Site","Transplant.Zone.Site","Transplant.Site","Origin.Site","Plot..","Transect")]
  tmp$y <- dat[,colnames(dat)==as.character(y[i])]
  tmp <- tmp[!is.na(tmp$y),]
  tmp$y <- as.numeric(as.character(tmp$y))
  tmp$Origin.Zone.Site <- factor(tmp$Origin.Zone.Site)
  tmp$Transplant.Zone.Site <- factor(tmp$Transplant.Zone.Site)
  m <- lm(y~Transplant.Zone.Site*Origin.Zone.Site ,data=tmp)
  print(anova(m))
  stats2 <- rbind(stats2,data.frame(y=y[i],x=c("Transplant.Zone.Site","Origin.Zone.Site","Interaction"),p=round(anova(m)$'Pr(>F)'[1:3],3)))
}

print(cbind(stats1,stats2[,2:3]))

### Field experiment - survival

library(coxme)
library(survival)
dat <- read.csv('data.phenotype/Tall.Short_field_survivalanalysis_011618.csv')
coxfit <- coxph(Surv(Week.to.death,Status_Dec_survive.0) ~  Origin.Zone * Transplant.Zone, data=dat)
print(anova(coxfit))

### Field experiment - flowering
library(car)
flower <- read.csv("data.phenotype/tall.short_flowertotals_102020.csv")
sflower <- subset(flower, Surivial1 == "1")
model <- glm(Flower1~ Transplant.Zone * Origin.Zone,family=binomial(link="logit"), data=sflower) 
print(Anova(model))



