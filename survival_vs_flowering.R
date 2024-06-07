library(DHARMa)
library(car)
library(dplyr)
library(emmeans);emm_options(rg.limit = 1000000)
library(lme4)
library(ggplot2)
#setwd("/Users/apalsson/Desktop/dcarth_ms/"
data <- read.csv("dcarth_ms_dryad_data_1123.csv")
data$site_plot <- paste(data$site, data$plot, sep="_")
data <- subset(data,survival1_w1=="1")
m1 <-glmer(survival2_s1 ~ flower1*altitude*site_altitude+ (1|site/site_plot) + (1|population), binomial, data=data, na.action=na.exclude)
m1_residuals <- simulateResiduals(m1, n=1000)
plot(m1_residuals)
plotResiduals(m1_residuals, data$flower1, quantreg = T)
plotResiduals(m1_residuals, data$site_altitude, quantreg = T)
plotResiduals(m1_residuals, data$altitude, quantreg = T)
summary(m1)
Anova(m1)
m1_emmeans <- emmeans(m1, ~ flower1 * site_altitude * altitude)
pairs_reversed <- contrast(m1_emmeans, method = "revpairwise", type = "response")
pairs_reversed
low_sites <- subset(data, site_altitude=="low_site")
high_sites <- subset(data, site_altitude=="high_site")
m1 <-glm(survival2_s1 ~ flower1*altitude , binomial, data=low_sites)
Anova(m1)
m1 <-glmer(survival2_s1 ~ flower1*altitude+ (1|site) , binomial, data=high_sites)
Anova(m1)
################################################################################################################################
data <- read.csv("dcarth_ms_dryad_data_1123.csv")
data$site_plot <- paste(data$site, data$plot, sep="_")
data <- subset(data,survival2_s1=="1")
m1 <-glmer(survival3_w2 ~ flower1*altitude*site_altitude+ (1|site) , binomial, data=data)
m1_residuals <- simulateResiduals(m1, n=1000)
plot(m1_residuals)
plotResiduals(m1_residuals, data$flower1, quantreg = T)
plotResiduals(m1_residuals, data$site_altitude, quantreg = T)
plotResiduals(m1_residuals, data$altitude, quantreg = T)
summary(m1) 
Anova(m1)
em_cont1<-emmeans(m1, specs = pairwise ~ flower1*altitude*site_altitude, type="response")
m1_emmeans <- emmeans(m1, ~ flower1 * site_altitude * altitude)
pairs_reversed <- contrast(m1_emmeans, method = "revpairwise", type = "response")

pairs_reversed_df <- as.data.frame(pairs_reversed)
desired_contrasts <- c("flower11 low_site low / flower10 low_site low",
                       "flower11 low_site high / flower10 low_site high",
                       "flower11 high_site low / flower10 high_site low",
                       "flower11 high_site high / flower10 high_site high")

subset_pairs_reversed_df <- subset(pairs_reversed_df, contrast %in% desired_contrasts)
low_sites <- subset(data, site_altitude=="low_site")
high_sites <- subset(data, site_altitude=="high_site")
m1 <-glm(survival3_w2 ~ flower1*altitude , binomial, data=low_sites)
Anova(m1)
m1 <-glmer(survival3_w2 ~ flower1*altitude+ (1|site) , binomial, data=high_sites)
Anova(m1)
res1<- as.data.frame(em_cont1[1])
###
survival2_s1_flower1_emmeans<-res1
df_surv2<-as.data.frame(survival2_s1_flower1_emmeans)
df_surv2$type <- paste(df_surv2$emmeans.site_altitude, df_surv2$emmeans.flower1, df_surv2$emmeans.altitude, sep="_")
df_surv2$type <- as.factor(df_surv2$type)
high_sites_df_surv2 <- subset(df_surv2, emmeans.site_altitude=="high_site") 
high_sites_df_surv2$type<-droplevels(high_sites_df_surv2$type)
levels(high_sites_df_surv2$type)
high_sites_df_surv2$type <- factor(high_sites_df_surv2$type, levels=c("high_site_1_low", "high_site_0_low", "high_site_1_high", "high_site_0_high"))
plotten <- ggplot(data = high_sites_df_surv2, aes(x=type, y= emmeans.prob, color=emmeans.altitude)) +
  geom_point(size = 3, position=position_dodge(width=0.8), shape=c(17,17,16,16))+
  geom_errorbar(aes(ymin = emmeans.asymp.LCL, ymax = emmeans.asymp.UCL),width = 0.2,position=position_dodge(width=0.8))+
  xlab("type")+
  ylab("survival probability w2 +-CI") + scale_color_manual(values=c("blue", "red"))+
  theme_bw()+theme_classic()+ ylim(0.7,1)+theme(legend.position='none')
plotten
################################################################################################################################
low_sites_df_surv2 <- subset(df_surv2, emmeans.site_altitude=="low_site") 
low_sites_df_surv2$type<-droplevels(low_sites_df_surv2$type)
levels(low_sites_df_surv2$type)
low_sites_df_surv2$type <- factor(low_sites_df_surv2$type, levels=c("low_site_1_low", "low_site_0_low", "low_site_1_high", "low_site_0_high"))

plotten <- ggplot(data = low_sites_df_surv2, aes(x=type, y= emmeans.prob, color=emmeans.altitude)) +
  geom_point(size = 3, position=position_dodge(width=0.8), shape=c(17,17,16,16))+
  geom_errorbar(aes(ymin = emmeans.asymp.LCL, ymax = emmeans.asymp.UCL),width = 0.2,position=position_dodge(width=0.8))+
  xlab("type")+
  ylab("survival probability w2 +-CI") + scale_color_manual(values=c("blue", "red"))+
  theme_bw()+theme_classic()+ ylim(0.7,1)+ theme(legend.position='none')
plotten
################################################################################################################################
data <- read.csv("dcarth_ms_dryad_data_1123.csv")
data$site_plot <- paste(data$site, data$plot, sep="_")
data <- subset(data,survival3_w2=="1")
m1 <-glm(survival4_s2 ~ flower2*altitude*site_altitude , binomial, data=data)
m1_residuals <- simulateResiduals(m1, n=1000)
plot(m1_residuals)
plotResiduals(m1_residuals, data$flower2, quantreg = T)
plotResiduals(m1_residuals, data$site_altitude, quantreg = T)
plotResiduals(m1_residuals, data$altitude, quantreg = T)
summary(m1) 
Anova(m1)
em_cont1<-emmeans(m1, specs = pairwise ~ flower2*altitude*site_altitude, type="response")
m1_emmeans <- emmeans(m1, ~ flower2* site_altitude * altitude)
pairs_reversed <- contrast(m1_emmeans, method = "revpairwise", type = "response")

pairs_reversed_df <- as.data.frame(pairs_reversed)
desired_contrasts <- c("flower11 low_site low / flower10 low_site low",
                       "flower11 low_site high / flower10 low_site high",
                       "flower11 high_site low / flower10 high_site low",
                       "flower11 high_site high / flower10 high_site high")

subset_pairs_reversed_df <- subset(pairs_reversed_df, contrast %in% desired_contrasts)
subset_pairs_reversed_df


################################################################################################################################
################################################################################################################################
data <- read.csv("dcarth_ms_dryad_data_1123.csv")
data$site_plot <- paste(data$site, data$plot, sep="_")
data <- subset(data,survival4_s2=="1")
m1 <-glmer(survival5_w3 ~ flower2*altitude*site_altitude+(1|site) , binomial, data=data)

m1_residuals <- simulateResiduals(m1, n=1000)
plot(m1_residuals)
plotResiduals(m1_residuals, data$flower2, quantreg = T)
plotResiduals(m1_residuals, data$site_altitude, quantreg = T)
plotResiduals(m1_residuals, data$altitude, quantreg = T)
summary(m1) 
Anova(m1)
em_cont1<-emmeans(m1, specs = pairwise ~ flower2*altitude*site_altitude, type="response")
m1_emmeans <- emmeans(m1, ~ flower2* site_altitude * altitude)
pairs_reversed <- contrast(m1_emmeans, method = "revpairwise", type = "response")

pairs_reversed_df <- as.data.frame(pairs_reversed)
desired_contrasts <- c("flower21 low_site low / flower20 low_site low",
                       "flower21 low_site high / flower20 low_site high",
                       "flower21 high_site low / flower20 high_site low",
                       "flower21 high_site high / flower20 high_site high")

subset_pairs_reversed_df <- subset(pairs_reversed_df, contrast %in% desired_contrasts)
subset_pairs_reversed_df

################################################################################################################################
data <- read.csv("dcarth_ms_dryad_data_1123.csv")
data$site_plot <- paste(data$site, data$plot, sep="_")
data <- subset(data,survival5_w3=="1")
m1 <-glmer(survival6_s3 ~ flower3*altitude*site_altitude+(1|site) , binomial, data=data)
#m1 <-glm(survival5_w3 ~ flower2*altitude*site_altitude , binomial, data=data)

m1_residuals <- simulateResiduals(m1, n=1000)
plot(m1_residuals)
plotResiduals(m1_residuals, data$flower3, quantreg = T)
plotResiduals(m1_residuals, data$site_altitude, quantreg = T)
plotResiduals(m1_residuals, data$altitude, quantreg = T)
summary(m1) 
Anova(m1)
em_cont1<-emmeans(m1, specs = pairwise ~ flower3*altitude*site_altitude, type="response")
m1_emmeans <- emmeans(m1, ~ flower3* site_altitude * altitude)
pairs_reversed <- contrast(m1_emmeans, method = "revpairwise", type = "response")

pairs_reversed_df <- as.data.frame(pairs_reversed)
desired_contrasts <- c("flower31 low_site low / flower30 low_site low",
                       "flower31 low_site high / flower30 low_site high",
                       "flower31 high_site low / flower30 high_site low",
                       "flower31 high_site high / flower30 high_site high")

subset_pairs_reversed_df <- subset(pairs_reversed_df, contrast %in% desired_contrasts)
subset_pairs_reversed_df
################################################################################################################################
data <- read.csv("dcarth_ms_dryad_data_1123.csv")
data$site_plot <- paste(data$site, data$plot, sep="_")
data <- subset(data,survival6_s3=="1")
m1 <-glmer(survival_7_w4 ~ flower3*altitude*site_altitude+(1|site) , binomial, data=data)
m1_residuals <- simulateResiduals(m1, n=1000)
plot(m1_residuals)
plotResiduals(m1_residuals, data$site_altitude, quantreg = T)
plotResiduals(m1_residuals, data$altitude, quantreg = T)
summary(m1) 
Anova(m1)
em_cont1<-emmeans(m1, specs = pairwise ~ flower3*altitude*site_altitude, type="response")
m1_emmeans <- emmeans(m1, ~ flower3* site_altitude * altitude)
pairs_reversed <- contrast(m1_emmeans, method = "revpairwise", type = "response")

pairs_reversed_df <- as.data.frame(pairs_reversed)
desired_contrasts <- c("flower31 low_site low / flower30 low_site low",
                       "flower31 low_site high / flower30 low_site high",
                       "flower31 high_site low / flower30 high_site low",
                       "flower31 high_site high / flower30 high_site high")

subset_pairs_reversed_df <- subset(pairs_reversed_df, contrast %in% desired_contrasts)
subset_pairs_reversed_df
