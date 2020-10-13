library(lme4)
library(lmerTest)

library(multcomp)
library(survival)
library(survminer)
library(coxme)
library(car)
###Field experiment

survival <- read.csv("data.phenotype/Tall.Short_field_survivalanalysis_011618.csv")
str(survival)


## transplant zone effect...
levels(survival$Transplant.Zone)

(fit1 <- survfit(Surv(as.numeric(survival$Week.to.death), survival$Status_Dec_survive.0) ~ Transplant.Zone, data=survival))
plot(fit1, xlab="Weeks", ylab="Cumulative mortality", main="Kaplan meier curves Transplant Zone")
legend("bottomleft", c("Tall Zone", "Short Zone"))
require(survminer)
ggsurvplot(fit1,size = 1,                 # change line size
           palette = 
             c("gray", "black"),# custom color palettes
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,
           xlab="Time (weeks)",
           ylab= expression(Proportion~of~italic("Spartina")~survived),
           font.main = 18,
           font.x =  16,
           font.y = 16,
  
           legend.labs = 
             c("Short Transplant Zone", "Tall Transplant Zone"),
           legend = "right")

## plot with all 4 combinations
(fit1 <- survfit(Surv(as.numeric(survival$Week.to.death), survival$Status_Dec_survive.0) ~  Origin.Zone + Transplant.Zone, data=survival))
plot(fit1, xlab="Weeks", ylab="Cumulative mortality", main="Kaplan meier curves Transplant Zone")
legend("bottomleft", c("Tall Zone", "Short Zone"))
require(survminer)
ggsurvplot(fit1,size = 1,                 # change line size
           palette = 
             c("black", "gray", "black", "gray"),# custom color palettes
           linetype = c("solid", "solid", "dashed", "dashed"),
           conf.int = TRUE,          # Add confidence interval
           pval = FALSE,
           xlab="Time (weeks)",
           ylab= expression(Proportion~of~italic("Spartina")~survived),
           font.main = 18,
           font.x =  18,
           font.y = 18,
           legend.labs = 
             c("Short Transplant - Short Origin Zone", "Tall Transplant - Short Origin Zone","Short Transplant - Tall Origin Zone", "Tall Transplant - Tall Origin Zone"),
           legend = "right")
?ggsurvplot

### make panels for each site
FB <- subset(survival, Transplant.Site == "Folly Beach")
FJ <- subset(survival, Transplant.Site == "Fort Johnson")
BI <- subset(survival, Transplant.Site == "Bowen's Island")

coxfitFB <- coxph(Surv(as.numeric(Week.to.death),Status_Dec_survive.0) ~  Origin.Zone * Transplant.Zone 
                  , data=FB)
Anova(coxfitFB)
summary(coxfitFB)

coxfitFJ <- coxph(Surv(as.numeric(Week.to.death),Status_Dec_survive.0) ~  Origin.Zone * Transplant.Zone 
                  , data=FJ)

Anova(coxfitFJ)
summary(coxfitFJ)

coxfitBI <- coxph(Surv(as.numeric(Week.to.death),Status_Dec_survive.0) ~   Origin.Zone * Transplant.Zone 
                  , data=BI)
Anova(coxfitBI)
summary(coxfitBI)
### there is problems with BI model - coefficients are INF - remove origin zone - then ok...


##graphs for each site

(fit1 <- survfit(Surv(as.numeric(FJ$Week.to.death), FJ$Status_Dec_survive.0) ~ Transplant.Zone, data=FJ))
plot(fit1, xlab="Weeks", ylab="Cumulative mortality", main="Fort Johnson")
legend("bottomleft", c("Tall Zone", "Short Zone"))
ggsurvplot(fit1,size = 1,                 # change line size
           palette = 
             c("grey", "black"),# custom color palettes
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,
           xlab="Time (weeks)",
           ylab= expression(Proportion~of~italic("Spartina")~survived),
           legend.labs = 
             c("Short Transplant Zone", "Tall Transplant Zone"),
           legend = "right")

(fit1 <- survfit(Surv(as.numeric(BI$Week.to.death), BI$Status_Dec_survive.0) ~ Transplant.Zone, data=BI))
plot(fit1, xlab="Weeks", ylab="Cumulative mortality", main="Bowen Island")
legend("bottomleft", c("Tall Zone", "Short Zone"))
ggsurvplot(fit1,size = 1,                 # change line size
           palette = 
             c("gray", "black"),# custom color palettes
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,
           xlab="Time (weeks)",
           ylab= expression(Proportion~of~italic("Spartina")~survived),
           legend.labs = 
             c("Short Transplant Zone", "Tall Transplant Zone"),
           legend = "right")

(fit1 <- survfit(Surv(as.numeric(FB$Week.to.death), FB$Status_Dec_survive.0) ~ Transplant.Zone, data=FB))
plot(fit1, xlab="Weeks", ylab="Cumulative mortality", main="Folly Beach")
legend("bottomleft", c("Tall Zone", "Short Zone"))
ggsurvplot(fit1,size = 1,                 # change line size
           palette = 
             c("gray", "black"),# custom color palettes
           conf.int = TRUE,          # Add confidence interval
           pval = TRUE,
           xlab="Time (weeks)",
           ylab= expression(Proportion~of~italic("Spartina")~survived),
           legend.labs = 
             c("Short Transplant Zone", "Tall Transplant Zone"),
           legend = "right")
