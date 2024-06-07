#load packages
library(lme4)
library(ggplot2)
library(DHARMa)


#setwd("/Users/apalsson/Desktop/dcarth_ms/")
data <- read.csv("dcarth_ms_dryad_data_1123.csv")
data$site_plot <- paste(data$site, data$plot, sep="_")


m1 <- lmer(log(E15_biomass)~ altitude*site_altitude+ (1|site/site_plot) + (1|population/mother),data=data, na.action=na.exclude)
m2 <- lmer(log(E15_biomass) ~altitude+site_altitude+ (1|site/site_plot) + (1|population/mother),data=data, na.action=na.exclude)
anova(m1,m2)
summary(m1)
m1_residuals <- simulateResiduals(m1, n=1000)
plot(m1_residuals)
#diagnostics
# residual vs. fitted values plot
plot(fitted(m1), resid(m1))
#qqplot of residuals
qqnorm(resid(m1)); qqline(resid(m1))
#scale-location plot
plot(sqrt(abs(resid(m1))) ~ fitted(m1))
#cook's distance
cooks_dist<-cooks.distance(m1);plot(cooks_dist, pch = 20);abline(h = 4/(length(fixef(m1))-1), col = "red", lty = 2)

em_cont1<-emmeans(m1, specs = pairwise ~ altitude*site_altitude, type="linear", adjust="none")
res1<- as.data.frame(em_cont1[1])

res1$emmeans.site_altitude <- factor(res1$emmeans.site_altitude, levels = c("low_site", "high_site"))

plotten<-ggplot(res1,aes(x=emmeans.site_altitude, y=emmeans.emmean, fill = emmeans.altitude)) + # Change fill to color
  theme_bw() +
  geom_point(position=position_dodge2(width=0.1),shape=c(16,16, 16,16),size=2.5,colour= c( "blue", "red","blue","red" )) + 
  geom_errorbar(ymin=c((res1[3,3]-res1[3,4]), (res1[4,3]-res1[4,4]),(res1[1,3]-res1[1,4]),(res1[2,3]-res1[2,4])),
                ymax=c((res1[3,3]+res1[3,4]), (res1[4,3]+res1[4,4]),(res1[1,3]+res1[1,4]),(res1[2,3]+res1[2,4])),width=0.1,position=position_dodge2(width=-2))+
  #geom_line(aes(group=snp), position = position_dodge(width = 0.1)) 
  scale_y_continuous(limits=c(4.5,9.5))+
  labs(title="E15_biomass", y="biomass w1", x="Transplant elevation", caption=NULL)
plotten2<-plotten + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
plotten1<-plotten2+ theme(legend.position = "none",legend.title = element_text(size=12, color = "black"), 
                          legend.text = element_text(size=10, colour="black"),
                          legend.key=element_rect(fill='white')) + 
  guides(colour = guide_legend(override.aes = list(size=2, stroke=1.5))) 
plotten1



#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
data16 <- subset(data, survival1_w1=="1")

m1 <- lmer(log(S16_biomass)~ altitude*site_altitude+ (1|site/site_plot) + (1|population/mother),data=data16, na.action=na.exclude)
m2 <- lmer(log(S16_biomass) ~altitude+site_altitude+ (1|site/site_plot) + (1|population/mother),data=data16, na.action=na.exclude)
anova(m1,m2) 
summary(m1)
#diagnostics
# residual vs. fitted values plot
plot(fitted(m1), resid(m1))
#qq plot of residuals
qqnorm(resid(m1)); qqline(resid(m1))
#scale-location plot
plot(sqrt(abs(resid(m1))) ~ fitted(m1))
#cook's distance
cooks_dist<-cooks.distance(m1);plot(cooks_dist, pch = 20);abline(h = 4/(length(fixef(m1))-1), col = "red", lty = 2)

em_cont1<-emmeans(m1, specs = pairwise ~ altitude*site_altitude, type="linear", adjust="none")
res1<- as.data.frame(em_cont1[1])

res1$emmeans.site_altitude <- factor(res1$emmeans.site_altitude, levels = c("low_site", "high_site"))

plotten<-ggplot(res1,aes(x=emmeans.site_altitude, y=emmeans.emmean, fill = emmeans.altitude)) + # Change fill to color
  theme_bw() +
  geom_point(position=position_dodge2(width=0.1),shape=c(16,16, 16,16),size=2.5,colour= c( "blue", "red","blue","red" )) + 
  geom_errorbar(ymin=c((res1[3,3]-res1[3,4]), (res1[4,3]-res1[4,4]),(res1[1,3]-res1[1,4]),(res1[2,3]-res1[2,4])),
                ymax=c((res1[3,3]+res1[3,4]), (res1[4,3]+res1[4,4]),(res1[1,3]+res1[1,4]),(res1[2,3]+res1[2,4])),width=0.1,position=position_dodge2(width=-2))+
  #geom_line(aes(group=snp), position = position_dodge(width = 0.1)) 
  scale_y_continuous(limits=c(4.5,9.5))+
  labs(title="S16_biomass", y="biomass s1", x="Transplant elevation", caption=NULL)
plotten2<-plotten + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
plotten1<-plotten2+ theme(legend.position = "none",legend.title = element_text(size=12, color = "black"), 
                          legend.text = element_text(size=10, colour="black"),
                          legend.key=element_rect(fill='white')) + 
  guides(colour = guide_legend(override.aes = list(size=2, stroke=1.5))) 
plotten1


#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
datae16 <- subset(data, survival2_s1=="1")

m1 <- lmer(log(E16_biomass)~ altitude*site_altitude+ (1|site/site_plot) + (1|population/mother),data=datae16, na.action=na.exclude)
m2 <- lmer(log(E16_biomass) ~altitude+site_altitude+ (1|site/site_plot) + (1|population/mother),data=datae16, na.action=na.exclude)
anova(m1,m2) 
summary(m1)
#diagnostics
# residual vs. fitted values plot
plot(fitted(m1), resid(m1))
#qqplot of residuals
qqnorm(resid(m1)); qqline(resid(m1))
#scale-location plot
plot(sqrt(abs(resid(m1))) ~ fitted(m1))
#cook's distance
cooks_dist<-cooks.distance(m1);plot(cooks_dist, pch = 20);abline(h = 4/(length(fixef(m1))-1), col = "red", lty = 2)

em_cont1<-emmeans(m1, specs = pairwise ~ altitude*site_altitude, type="linear", adjust="none")
res1<- as.data.frame(em_cont1[1])
res1$emmeans.site_altitude <- factor(res1$emmeans.site_altitude, levels = c("low_site", "high_site"))

plotten<-ggplot(res1,aes(x=emmeans.site_altitude, y=emmeans.emmean, fill = emmeans.altitude)) + # Change fill to color
  theme_bw() +
  geom_point(position=position_dodge2(width=0.1),shape=c(16,16, 16,16),size=2.5,colour= c( "blue", "red","blue","red" )) + 
  geom_errorbar(ymin=c((res1[3,3]-res1[3,4]), (res1[4,3]-res1[4,4]),(res1[1,3]-res1[1,4]),(res1[2,3]-res1[2,4])),
                ymax=c((res1[3,3]+res1[3,4]), (res1[4,3]+res1[4,4]),(res1[1,3]+res1[1,4]),(res1[2,3]+res1[2,4])),width=0.1,position=position_dodge2(width=-2))+
  #geom_line(aes(group=snp), position = position_dodge(width = 0.1)) 
  scale_y_continuous(limits=c(4.5,9.5))+
  labs(title="E16_biomass", y="biomass w2", x="Transplant elevation", caption=NULL)
plotten2<-plotten + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
plotten1<-plotten2+ theme(legend.position = "none",legend.title = element_text(size=12, color = "black"), 
                          legend.text = element_text(size=10, colour="black"),
                          legend.key=element_rect(fill='white')) + 
  guides(colour = guide_legend(override.aes = list(size=2, stroke=1.5))) 
plotten1


#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
data17 <- subset(data, survival3_w2=="1")

m1 <- lmer(log(S17_biomass)~ altitude*site_altitude+ (1|site/site_plot) + (1|population/mother),data=data17, na.action=na.exclude)
m2 <- lmer(log(S17_biomass) ~altitude+site_altitude+ (1|site/site_plot) + (1|population/mother),data=data17, na.action=na.exclude)
anova(m1,m2) 
#diagnostics
# residual vs. fitted values plot
plot(fitted(m1), resid(m1))
#qqplot of residuals
qqnorm(resid(m1)); qqline(resid(m1))
#scale-location plot
plot(sqrt(abs(resid(m1))) ~ fitted(m1))
#cook's distance
cooks_dist<-cooks.distance(m1);plot(cooks_dist, pch = 20);abline(h = 4/(length(fixef(m1))-1), col = "red", lty = 2)

em_cont1<-emmeans(m1, specs = pairwise ~ altitude*site_altitude, type="linear", adjust="none")
res1<- as.data.frame(em_cont1[1])
res1$emmeans.site_altitude <- factor(res1$emmeans.site_altitude, levels = c("low_site", "high_site"))

plotten<-ggplot(res1,aes(x=emmeans.site_altitude, y=emmeans.emmean, fill = emmeans.altitude)) + # Change fill to color
  theme_bw() +
  geom_point(position=position_dodge2(width=0.1),shape=c(16,16, 16,16),size=2.5,colour= c( "blue", "red","blue","red" )) + 
  geom_errorbar(ymin=c((res1[3,3]-res1[3,4]), (res1[4,3]-res1[4,4]),(res1[1,3]-res1[1,4]),(res1[2,3]-res1[2,4])),
                ymax=c((res1[3,3]+res1[3,4]), (res1[4,3]+res1[4,4]),(res1[1,3]+res1[1,4]),(res1[2,3]+res1[2,4])),width=0.1,position=position_dodge2(width=-2))+
  #geom_line(aes(group=snp), position = position_dodge(width = 0.1)) 
  scale_y_continuous(limits=c(4.5,9.5))+
  labs(title="S17_biomass", y="biomass s2", x="Transplant elevation", caption=NULL)
plotten2<-plotten + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
plotten1<-plotten2+ theme(legend.position = "none",legend.title = element_text(size=12, color = "black"), 
                          legend.text = element_text(size=10, colour="black"),
                          legend.key=element_rect(fill='white')) + 
  guides(colour = guide_legend(override.aes = list(size=2, stroke=1.5))) 
plotten1

######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
data18 <- subset(data, survival5_w3=="1")

m1 <- lmer(log(S18_biomass)~ altitude*site_altitude+ (1|site/site_plot) + (1|population/mother),data=data18, na.action=na.exclude)
m2 <- lmer(log(S18_biomass) ~altitude+site_altitude+ (1|site/site_plot) + (1|population/mother),data=data18, na.action=na.exclude)
anova(m1,m2) 
summary(m1)
#diagnostics
# residual vs. fitted values plot
plot(fitted(m1), resid(m1))
#qqplot of residuals
qqnorm(resid(m1)); qqline(resid(m1))
#scale-location plot
plot(sqrt(abs(resid(m1))) ~ fitted(m1))
#cook's distance
cooks_dist<-cooks.distance(m1);plot(cooks_dist, pch = 20);abline(h = 4/(length(fixef(m1))-1), col = "red", lty = 2)

em_cont1<-emmeans(m1, specs = pairwise ~ altitude*site_altitude, type="linear", adjust="none")
res1<- as.data.frame(em_cont1[1])

res1$emmeans.site_altitude <- factor(res1$emmeans.site_altitude, levels = c("low_site", "high_site"))

plotten<-ggplot(res1,aes(x=emmeans.site_altitude, y=emmeans.emmean, fill = emmeans.altitude)) + # Change fill to color
  theme_bw() +
  geom_point(position=position_dodge2(width=0.1),shape=c(16,16, 16,16),size=2.5,colour= c( "blue", "red","blue","red" )) + 
  geom_errorbar(ymin=c((res1[3,3]-res1[3,4]), (res1[4,3]-res1[4,4]),(res1[1,3]-res1[1,4]),(res1[2,3]-res1[2,4])),
                ymax=c((res1[3,3]+res1[3,4]), (res1[4,3]+res1[4,4]),(res1[1,3]+res1[1,4]),(res1[2,3]+res1[2,4])),width=0.1,position=position_dodge2(width=-2))+
  #geom_line(aes(group=snp), position = position_dodge(width = 0.1)) 
  scale_y_continuous(limits=c(4.5,9.5))+
  labs(title="S18_biomass", y="biomass s3", x="Transplant elevation", caption=NULL)
plotten2<-plotten + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
plotten1<-plotten2+ theme(legend.position = "none",legend.title = element_text(size=12, color = "black"), 
                          legend.text = element_text(size=10, colour="black"),
                          legend.key=element_rect(fill='white')) + 
  guides(colour = guide_legend(override.aes = list(size=2, stroke=1.5))) 
plotten1

#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
datae18 <- subset(data, survival6_s3=="1")

m1 <- lmer(log(E18_biomass)~ altitude*site_altitude+ (1|site/site_plot) + (1|population/mother),data=datae18, na.action=na.exclude)
m2 <- lmer(log(E18_biomass) ~altitude+site_altitude+ (1|site/site_plot) + (1|population/mother),data=datae18, na.action=na.exclude)
anova(m1,m2) 

#diagnostics
# residual vs. fitted values plot
plot(fitted(m1), resid(m1))
#qqplot of residuals
qqnorm(resid(m1)); qqline(resid(m1))
#scale-location plot
plot(sqrt(abs(resid(m1))) ~ fitted(m1))
#cook's distance
cooks_dist<-cooks.distance(m1);plot(cooks_dist, pch = 20);abline(h = 4/(length(fixef(m1))-1), col = "red", lty = 2)

em_cont1<-emmeans(m1, specs = pairwise ~ altitude*site_altitude, type="linear", adjust="none")
res1<- as.data.frame(em_cont1[1])

res1$emmeans.site_altitude <- factor(res1$emmeans.site_altitude, levels = c("low_site", "high_site"))

plotten<-ggplot(res1,aes(x=emmeans.site_altitude, y=emmeans.emmean, fill = emmeans.altitude)) + # Change fill to color
  theme_bw() +
  geom_point(position=position_dodge2(width=0.1),shape=c(16,16, 16,16),size=2.5,colour= c( "blue", "red","blue","red" )) + 
  geom_errorbar(ymin=c((res1[3,3]-res1[3,4]), (res1[4,3]-res1[4,4]),(res1[1,3]-res1[1,4]),(res1[2,3]-res1[2,4])),
                ymax=c((res1[3,3]+res1[3,4]), (res1[4,3]+res1[4,4]),(res1[1,3]+res1[1,4]),(res1[2,3]+res1[2,4])),width=0.1,position=position_dodge2(width=-2))+
  #geom_line(aes(group=snp), position = position_dodge(width = 0.1)) 
  scale_y_continuous(limits=c(4.5,9.5))+
  labs(title="E18_biomass", y="biomass w4", x="Transplant elevation", caption=NULL)
plotten2<-plotten + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
plotten1<-plotten2+ theme(legend.position = "none",legend.title = element_text(size=12, color = "black"), 
                          legend.text = element_text(size=10, colour="black"),
                          legend.key=element_rect(fill='white')) + 
  guides(colour = guide_legend(override.aes = list(size=2, stroke=1.5))) 
plotten1




