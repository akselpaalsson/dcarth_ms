library(tidyverse)
library(emmeans)
library(lmerTest)
library(lmerTest)
library(lme4)
#setwd("/Users/apalsson/Desktop/dcarth_ms/"
data <- read.csv("dcarth_ms_dryad_data_1123.csv")
data$site_plot <- paste(data$site, data$plot, sep="_")

data <- data[complete.cases(data$E15_biomass), ]
low_sites <- subset(data, site_altitude=="low_site")
high_sites <- subset(data, site_altitude=="high_site")
low_in_low <- subset(low_sites, altitude=="low") 
high_in_low <- subset(low_sites, altitude=="high") 
low_in_high <- subset(high_sites, altitude=="low") 
high_in_high <- subset(high_sites, altitude=="high") 

boxplot(high_in_low$E15_biomass)
boxplot(low_in_low$E15_biomass)
plot(high_in_low$E15_biomass, high_in_low$survival1_w1)
plot(low_in_low$E15_biomass, low_in_low$survival1_w1)
plot(high_in_high$E15_biomass, high_in_high$survival1_w1)
plot(low_in_high$E15_biomass, low_in_high$survival1_w1)

m0 <- glm(survival1_w1 ~ E15_biomass*altitude*site_altitude,binomial, data=data)
m1_residuals <- simulateResiduals(m0, n=1000)
par(mfrow = c(1, 2))
plotResiduals(m1_residuals, data$altitude, quantreg = T)
plotResiduals(m1_residuals, data$E15_biomass, quantreg = T)
Anova(m0)
test(emtrends(m0,  c("site_altitude" ,"altitude"), var="E15_biomass"))
################################################################################################################################################
data <- read.csv("dcarth_ms_dryad_data_1123.csv")
data <- subset(data,survival1_w1=="1")
data$site_plot <- paste(data$site, data$plot, sep="_")
data <- data[complete.cases(data$S16_biomass), ]
low_sites <- subset(data, site_altitude=="low_site")
high_sites <- subset(data, site_altitude=="high_site")
low_in_low <- subset(low_sites, altitude=="low") 
high_in_low <- subset(low_sites, altitude=="high") 
low_in_high <- subset(high_sites, altitude=="low") 
high_in_high <- subset(high_sites, altitude=="high") 


boxplot(high_in_low$S16_biomass)
boxplot(low_in_low$S16_biomass)
plot(high_in_low$S16_biomass, high_in_low$survival2_s1)
plot(low_in_low$S16_biomass, low_in_low$survival2_s1)
plot(high_in_high$S16_biomass, high_in_high$survival2_s1)
plot(low_in_high$S16_biomass, low_in_high$survival2_s1)

high_in_low <- subset(high_in_low, S16_biomass < 1000) 
low_in_low <- subset(low_in_low, S16_biomass < 3000) 
low_sites <- rbind(low_in_low,high_in_low)

low_in_high <- subset(high_sites, altitude=="low")
high_in_high <- subset(high_sites, altitude=="high")
high_sites <- subset(high_sites, S16_biomass < 5000)  
data <- rbind(low_sites,high_sites)
boxplot(high_in_low$S16_biomass)
boxplot(low_in_low$S16_biomass)
plot(high_in_low$S16_biomass, high_in_low$survival2_s1)
plot(low_in_low$S16_biomass, low_in_low$survival2_s1)
plot(high_in_high$S16_biomass, high_in_high$survival2_s1)
plot(low_in_high$S16_biomass, low_in_high$survival2_s1)
m0 <- glm(survival2_s1 ~ S16_biomass*altitude*site_altitude,binomial, data=data)
m1_residuals <- simulateResiduals(m0, n=1000)
par(mfrow = c(1, 2))
plotResiduals(m1_residuals, data$altitude, quantreg = T)
plotResiduals(m1_residuals, data$S16_biomass, quantreg = T)
Anova(m0)
test(emtrends(m0,  c("site_altitude" ,"altitude"), var="S16_biomass"))
###

m1 <- glm( cbind(survival2_s1, 1-survival2_s1) ~ S16_biomass*altitude , data=low_sites, family=binomial ) #ok!
emmeans(m1, ~S16_biomass*altitude, at=list(S16_biomass=1), type='response')

new.df_l <- data.frame( S16_biomass=seq(4.37, 2714.33, by=0.1) )
new.df_l$altitude <- "low"

new.df_h <- data.frame( S16_biomass=seq(4.37,2714.33, by=0.1) )
new.df_h$altitude <- "high"
new.df <- rbind(new.df_l, new.df_h)
yhat.df <- new.df %>% mutate(fit = predict(m1, newdata=new.df, type='response') ) 
yhat.df <- emmeans(m1, ~S16_biomass*altitude, at=list(S16_biomass=seq(4.37,2714.33,by=0.1)), type='response') %>%
  as.data.frame()
#######
low_sites$altitude <- factor(low_sites$altitude, levels = c("low", "high"))
yhat.df$altitude <- factor(yhat.df$altitude, levels = c("low", "high"))

yhat_high <- subset(yhat.df, altitude=="high")
yhat_low <- subset(yhat.df, altitude=="low")
yhat_high_sub<- subset(yhat_high, S16_biomass < 957.91)
yhat.df <- rbind(yhat_high_sub, yhat_low)


p<-ggplot(low_sites, aes(x=S16_biomass)) +
  geom_ribbon(data=yhat.df, aes(ymin=asymp.LCL, ymax=asymp.UCL, fill=altitude), alpha=.4) +
  geom_line( data=yhat.df, aes(y=prob, color=altitude)) +
  geom_point(aes(y=survival2_s1, shape=altitude, color=altitude)) +
  labs(y='Probability of survival2_s1', x='S16_biomass', title='survival2_s1_S16_biomass_low_sites')
p<-p + theme_bw() + theme(legend.position = "none",panel.border = element_blank(), panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
p<-p+scale_color_manual(breaks = c("low", "high"),
                        values=c("red", "blue"))
p
m1 <- glm( cbind(survival2_s1, 1-survival2_s1) ~ S16_biomass*altitude , data=high_sites, family=binomial ) #ok!
emmeans(m1, ~S16_biomass*altitude, at=list(S16_biomass=1), type='response')

new.df_l <- data.frame( S16_biomass=seq(8.39, 4563.31, by=0.1) )
new.df_l$altitude <- "low"
new.df_h <- data.frame( S16_biomass=seq(8.39,4563.31, by=0.1) )
new.df_h$altitude <- "high"
new.df <- rbind(new.df_l, new.df_h)
yhat.df <- new.df %>% mutate(fit = predict(m1, newdata=new.df, type='response') ) 
yhat.df <- emmeans(m1, ~S16_biomass*altitude, at=list(S16_biomass=seq(8.39,4563.31,by=0.1)), type='response') %>%
  as.data.frame()
#######
low_sites$altitude <- factor(low_sites$altitude, levels = c("low", "high"))
yhat.df$altitude <- factor(yhat.df$altitude, levels = c("low", "high"))

low_in_high <- subset(high_sites, altitude=="low") 
high_in_high <- subset(high_sites, altitude=="high") 
yhat_high <- subset(yhat.df, altitude=="high")
yhat_low <- subset(yhat.df, altitude=="low")
yhat_high_sub<- subset(yhat_high, S16_biomass < 3982.43)
yhat.df <- rbind(yhat_high_sub, yhat_low)

p<-ggplot(high_sites, aes(x=S16_biomass)) +
  geom_ribbon(data=yhat.df, aes(ymin=asymp.LCL, ymax=asymp.UCL, fill=altitude), alpha=.4) +
  geom_line( data=yhat.df, aes(y=prob, color=altitude)) +
  geom_point(aes(y=survival2_s1, shape=altitude, color=altitude)) +
  scale_shape_manual(values=c(17, 16))+
  labs(y='Probability of survival2_s1', x='S16_biomass', title='survival2_s1_S16_biomass_low_sites')
p<-p + theme_bw() + theme(legend.position = "none",panel.border = element_blank(), panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
p<-p+scale_color_manual(breaks = c("low", "high"),
                        values=c("red", "blue"))
p

################################################################################################################################################
data <- read.csv("dcarth_ms_dryad_data_1123.csv")
data <- subset(data,survival2_s1=="1")
data$site_plot <- paste(data$site, data$plot, sep="_")
data <- data[complete.cases(data$S16_biomass), ]
low_sites <- subset(data, site_altitude=="low_site")
high_sites <- subset(data, site_altitude=="high_site")
low_in_low <- subset(low_sites, altitude=="low") 
high_in_low <- subset(low_sites, altitude=="high") 
low_in_high <- subset(high_sites, altitude=="low") 
high_in_high <- subset(high_sites, altitude=="high") 

boxplot(high_in_low$S16_biomass)
boxplot(low_in_low$S16_biomass)
plot(high_in_low$S16_biomass, high_in_low$survival3_w2)
plot(low_in_low$S16_biomass, low_in_low$survival3_w2)
plot(high_in_high$S16_biomass, high_in_high$survival3_w2)
plot(low_in_high$S16_biomass, low_in_high$survival3_w2)

high_in_low <- subset(high_in_low, S16_biomass < 1000) 
low_in_low <- subset(low_in_low, S16_biomass < 3000) 
low_sites <- rbind(low_in_low,high_in_low)

low_in_high <- subset(high_sites, altitude=="low")
high_in_high <- subset(high_sites, altitude=="high")
high_sites <- subset(high_sites, S16_biomass < 5000)
data <- rbind(low_sites,high_sites)
boxplot(high_in_low$S16_biomass)
boxplot(low_in_low$S16_biomass)
plot(high_in_low$S16_biomass, high_in_low$survival3_w2)
plot(low_in_low$S16_biomass, low_in_low$survival3_w2)
plot(high_in_high$S16_biomass, high_in_high$survival3_w2)
plot(low_in_high$S16_biomass, low_in_high$survival3_w2)
m0 <- glm(survival3_w2 ~ S16_biomass*altitude*site_altitude,binomial, data=data)
m1_residuals <- simulateResiduals(m0, n=1000)
par(mfrow = c(1, 2))
plotResiduals(m1_residuals, data$altitude, quantreg = T)
plotResiduals(m1_residuals, data$S16_biomass, quantreg = T)
Anova(m0)
test(emtrends(m0,  c("site_altitude" ,"altitude"), var="S16_biomass"))
###

m1 <- glm( cbind(survival3_w2, 1-survival3_w2) ~ S16_biomass*altitude , data=low_sites, family=binomial ) #ok!
emmeans(m1, ~S16_biomass*altitude, at=list(S16_biomass=1), type='response')

new.df_l <- data.frame( S16_biomass=seq(4.37, 2714.33, by=0.1) )
new.df_l$altitude <- "low"

new.df_h <- data.frame( S16_biomass=seq(4.37,2714.33, by=0.1) )
new.df_h$altitude <- "high"
new.df <- rbind(new.df_l, new.df_h)
yhat.df <- new.df %>% mutate(fit = predict(m1, newdata=new.df, type='response') ) 
yhat.df <- emmeans(m1, ~S16_biomass*altitude, at=list(S16_biomass=seq(4.37,2714.33,by=0.1)), type='response') %>%
  as.data.frame()
#######
low_sites$altitude <- factor(low_sites$altitude, levels = c("low", "high"))
yhat.df$altitude <- factor(yhat.df$altitude, levels = c("low", "high"))

yhat_high <- subset(yhat.df, altitude=="high")
yhat_low <- subset(yhat.df, altitude=="low")
yhat_high_sub<- subset(yhat_high, S16_biomass < 957.91)
yhat.df <- rbind(yhat_high_sub, yhat_low)


p<-ggplot(low_sites, aes(x=S16_biomass)) +
  geom_ribbon(data=yhat.df, aes(ymin=asymp.LCL, ymax=asymp.UCL, fill=altitude), alpha=.4) +
  geom_line( data=yhat.df, aes(y=prob, color=altitude)) +
  geom_point(aes(y=survival3_w2, shape=altitude, color=altitude)) +
  labs(y='Probability of survival3_w2', x='S16_biomass', title='survival3_w2_S16_biomass_low_sites')
p<-p + theme_bw() + theme(legend.position = "none",panel.border = element_blank(), panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
p<-p+scale_color_manual(breaks = c("low", "high"),
                        values=c("red", "blue"))
p

m1 <- glm( cbind(survival3_w2, 1-survival3_w2) ~ S16_biomass*altitude , data=high_sites, family=binomial ) #ok!
emmeans(m1, ~S16_biomass*altitude, at=list(S16_biomass=1), type='response')

new.df_l <- data.frame( S16_biomass=seq(8.39, 4563.31, by=0.1) )
new.df_l$altitude <- "low"
new.df_h <- data.frame( S16_biomass=seq(8.39,4563.31, by=0.1) )
new.df_h$altitude <- "high"
new.df <- rbind(new.df_l, new.df_h)
yhat.df <- new.df %>% mutate(fit = predict(m1, newdata=new.df, type='response') ) 
yhat.df <- emmeans(m1, ~S16_biomass*altitude, at=list(S16_biomass=seq(8.39,4563.31,by=0.1)), type='response') %>%
  as.data.frame()
#######
low_sites$altitude <- factor(low_sites$altitude, levels = c("low", "high"))
yhat.df$altitude <- factor(yhat.df$altitude, levels = c("low", "high"))

low_in_high <- subset(high_sites, altitude=="low") 
high_in_high <- subset(high_sites, altitude=="high") 
yhat_high <- subset(yhat.df, altitude=="high")
yhat_low <- subset(yhat.df, altitude=="low")
yhat_high_sub<- subset(yhat_high, S16_biomass < 3982.43)
yhat.df <- rbind(yhat_high_sub, yhat_low)

p<-ggplot(high_sites, aes(x=S16_biomass)) +
  geom_ribbon(data=yhat.df, aes(ymin=asymp.LCL, ymax=asymp.UCL, fill=altitude), alpha=.4) +
  geom_line( data=yhat.df, aes(y=prob, color=altitude)) +
  #geom_line( data=hyhat.df, aes(y=prob), color='blue') +
  geom_point(aes(y=survival3_w2, shape=altitude, color=altitude)) +
  scale_shape_manual(values=c(17, 16))+
  labs(y='Probability of survival3_w2', x='S16_biomass', title='survival3_w2_S16_biomass_low_sites')
p<-p + theme_bw() + theme(legend.position = "none",panel.border = element_blank(), panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
p<-p+scale_color_manual(breaks = c("low", "high"),
                        values=c("red", "blue"))
p
###########################################################################################################################
data <- read.csv("dcarth_ms_dryad_data_1123.csv")
data$site_plot <- paste(data$site, data$plot, sep="_")
data <- subset(data,survival3_w2=="1")
data <- data[complete.cases(data$S17_biomass), ]
#findeln 1_66  is 12993.71 this is an incorrect measurement and should be removed. 
data <- subset(data, S17_biomass < 12000)
m0 <- glm(survival4_s2 ~ S17_biomass*altitude*site_altitude,binomial, data=data)
summary(m0)
Anova(m0)
m1_residuals <- simulateResiduals(m0, n=1000)
par(mfrow = c(1, 3)) 
plotResiduals(m1_residuals, data$site_altitude, quantreg = T)
plotResiduals(m1_residuals, data$altitude, quantreg = T)
plotResiduals(m1_residuals, data$S16_biomass, quantreg = T)
test(emtrends(m0 , c("site_altitude" ,"altitude"), var="S17_biomass"))

###########################################################################################################################
data <- read.csv("dcarth_ms_dryad_data_1123.csv")
data$site_plot <- paste(data$site, data$plot, sep="_")
data <- subset(data,survival5_w3=="1")
data <- data[complete.cases(data$S18_biomass), ]
m1 <- glm(survival6_s3 ~ S18_biomass*altitude*site_altitude,binomial, data=data)
summary(m1)
Anova(m1)
m1_residuals <- simulateResiduals(m1, n=1000)
par(mfrow = c(1, 3)) 
plotResiduals(m1_residuals, data$site_altitude, quantreg = T)
plotResiduals(m1_residuals, data$altitude, quantreg = T)
plotResiduals(m1_residuals, data$S16_biomass, quantreg = T)
test(emtrends(m1,  c("site_altitude" ,"altitude"), var="S18_biomass"))
###########################################################################################################################
data <- read.csv("dcarth_ms_dryad_data_1123.csv")
data$site_plot <- paste(data$site, data$plot, sep="_")
data <- subset(data,survival6_s3=="1")
data <- data[complete.cases(data$S18_biomass), ]
m1 <- glm(survival_7_w4 ~ S18_biomass*altitude*site_altitude,binomial, data=data)
Anova(m1)
m1_residuals <- simulateResiduals(m1, n=1000)
par(mfrow = c(1, 3))
plotResiduals(m1_residuals, data$site_altitude, quantreg = T)
plotResiduals(m1_residuals, data$altitude, quantreg = T)
plotResiduals(m1_residuals, data$S16_biomass, quantreg = T)
test(emtrends(m1,  c("site_altitude" ,"altitude"), var="S18_biomass"))



