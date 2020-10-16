### RM ANOVA
### Max plant height in greenhouse over time
rm(list=ls())
library(lmerTest)
dat <- read.csv('data.phenotype/CG_final.csv')
dat.surv <- dat[dat$Survival==1,]
model1 <- lmer(Max.Height ~ Tall.Short * Date+ (1|Site) + (1|Greenhouse.Placement), data=dat.surv)
print(anova(model1))
uniquedates <- sort(unique(dat.surv$Julian.Day))
### per timepoint
for (i in 1:length(uniquedates))
{
  tmp <- dat.surv[dat.surv$Julian.Day==uniquedates[i],]
  model.tmp <- lmer(Max.Height ~ Tall.Short + (1|Site) , data=tmp)
  print(paste("day=",uniquedates[i]))
  print(anova(model.tmp))
}

### Stem density in greenhouse over time
model2 <- lmer(Density ~ Tall.Short * Date+ (1|Site) + (1|Greenhouse.Placement), data=dat.surv)
print(anova(model2))

### Leaf number in greenhouse over time
model3 <- lmer(Leaf.Number ~ Tall.Short * Date+ (1|Site) + (1|Greenhouse.Placement), data=dat.surv)
print(anova(model3))
### per timepoint
for (i in 1:length(uniquedates))
{
  tmp <- dat.surv[dat.surv$Julian.Day==uniquedates[i],]
  if(!i==2){
  model.tmp <- lmer(Leaf.Number ~ Tall.Short + (1|Site) , data=tmp)
  print(paste("day=",uniquedates[i]))
  print(anova(model.tmp))
}}

### Average heights of rest of stems
dat2 <- read.csv('data.phenotype/CG_avgheights_final.csv')
modelAH <- lmer(Avg.height.of.additional.stems ~ Tall.Short + (1|Site), data=dat2)
anova(modelAH)


