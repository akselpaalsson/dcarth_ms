
#load packages
library(lmerTest)
library(lme4)
library(emmeans)
library(DHARMa)
#setwd("/Users/apalsson/Desktop/dcarth_ms/")
data <- read.csv("dcarth_ms_dryad_data_1123.csv")
data$site_plot <- paste(data$site, data$plot, sep="_")

m1 <- glmer(survival1_w1~ altitude*site_altitude + (1|site/site_plot) + (1|population/mother), binomial, data=data, na.action=na.exclude)
m2 <- glmer(survival1_w1~ altitude+site_altitude + (1|site/site_plot) + (1|population/mother), binomial, data=data, na.action=na.exclude)
anova(m1,m2)
m1_residuals <- simulateResiduals(m1, n=1000)
plot(m1_residuals)
plotResiduals(m1_residuals, data$site_altitude, quantreg = T)
plotResiduals(m1_residuals, data$altitude, quantreg = T)

em_cont1<-emmeans(m1, specs = pairwise ~ altitude*site_altitude, type="response", adjust="none")
###############################################################################################################################################################################
data <- subset(data,survival1_w1=="1")

m1 <- glmer(survival2_s1~ altitude*site_altitude + (1|site/site_plot) + (1|population/mother), binomial, data=data, na.action=na.exclude)
m2 <- glmer(survival2_s1~ altitude+site_altitude + (1|site/site_plot) + (1|population/mother), binomial, data=data, na.action=na.exclude)
anova(m1,m2)
m1_residuals <- simulateResiduals(m1, n=1000)
plot(m1_residuals)
plotResiduals(m1_residuals, data$site_altitude, quantreg = T)
plotResiduals(m1_residuals, data$altitude, quantreg = T)

em_cont1<-emmeans(m1, specs = pairwise ~ altitude*site_altitude, type="response", adjust="none")
####################################################################################################################################################################################
data <- subset(data,survival2_s1=="1")

m1 <- glmer(survival3_w2~ altitude*site_altitude + (1|site) + (1|population/mother), binomial, data=data, na.action=na.exclude)
m2 <- glmer(survival3_w2~ altitude+site_altitude + (1|site) + (1|population/mother), binomial, data=data, na.action=na.exclude)
anova(m1,m2)
m1_residuals <- simulateResiduals(m1, n=1000)
plot(m1_residuals)
plotResiduals(m1_residuals, data$site_altitude, quantreg = T)
plotResiduals(m1_residuals, data$altitude, quantreg = T)

em_cont1<-emmeans(m1, specs = pairwise ~ altitude*site_altitude, type="response", adjust="none")
####################################################################################################################################################################################
data <- subset(data,survival3_w2=="1")
m1 <- glmer(survival4_s2~ altitude*site_altitude + (1|site/site_plot) + (1|population/mother), binomial, data=data, na.action=na.exclude)
m2 <- glmer(survival4_s2~ altitude+site_altitude + (1|site/site_plot) + (1|population/mother), binomial, data=data, na.action=na.exclude)
anova(m1,m2)
m1_residuals <- simulateResiduals(m1, n=1000)
plot(m1_residuals)
plotResiduals(m1_residuals, data$site_altitude, quantreg = T)
plotResiduals(m1_residuals, data$altitude, quantreg = T)

em_cont1<-emmeans(m1, specs = pairwise ~ altitude*site_altitude, type="response", adjust="none")
################################################################################################################################################################
data <- subset(data,survival4_s2=="1")

#mother excluded, causes singular fit
m1 <- glmer(survival5_w3~ altitude*site_altitude + (1|site/site_plot) + (1|population), binomial, data=data, na.action=na.exclude)
m2 <- glmer(survival5_w3~ altitude+site_altitude + (1|site/site_plot) + (1|population), binomial, data=data, na.action=na.exclude)
anova(m1,m2)
m1_residuals <- simulateResiduals(m1, n=1000)
plot(m1_residuals)
plotResiduals(m1_residuals, data$site_altitude, quantreg = T)
plotResiduals(m1_residuals, data$altitude, quantreg = T)

em_cont1<-emmeans(m1, specs = pairwise ~ altitude*site_altitude, type="response", adjust="none")
################################################################################################################################################################
data <- subset(data,survival5_w3=="1")

#population and mother excluded, casues singular fit and lack of convergence
m1 <- glmer(survival6_s3~ altitude*site_altitude + (1|site/site_plot) , binomial, data=data, na.action=na.exclude)
m2 <- glmer(survival6_s3~ altitude+site_altitude + (1|site/site_plot) , binomial, data=data, na.action=na.exclude)
anova(m1,m2)
m1_residuals <- simulateResiduals(m1, n=1000)
plot(m1_residuals)
plotResiduals(m1_residuals, data$site_altitude, quantreg = T)
plotResiduals(m1_residuals, data$altitude, quantreg = T)

em_cont1<-emmeans(m1, specs = pairwise ~ altitude*site_altitude, type="response", adjust="none")
################################################################################################################################################################
data <- subset(data,survival6_s3=="1")
#population and mother excluded, casues singular fit
m1 <- glmer(survival_7_w4~ altitude*site_altitude + (1|site/site_plot), binomial, data=data, na.action=na.exclude)
m2 <- glmer(survival_7_w4~ altitude+site_altitude + (1|site/site_plot), binomial, data=data, na.action=na.exclude)
anova(m1,m2)
m1_residuals <- simulateResiduals(m1, n=1000)
plot(m1_residuals)
plotResiduals(m1_residuals, data$site_altitude, quantreg = T)
plotResiduals(m1_residuals, data$altitude, quantreg = T)
em_cont1<-emmeans(m1, specs = pairwise ~ altitude*site_altitude, type="response", adjust="none")

