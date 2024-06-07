#load packages
library(lmerTest)
library(lme4)
library(emmeans)
library(DHARMa)
library(ggplot2)
#setwd("/Users/apalsson/Desktop/dcarth_ms/")
data <- read.csv("dcarth_ms_dryad_data_1123.csv")
data$site_plot <- paste(data$site, data$plot, sep="_")
data <- subset(data,survival1_w1=="1")

m1 <- glmer(flower1~ altitude*site_altitude+ (1|site/site_plot) + (1|population/mother),binomial,data=data, na.action=na.exclude)
m2 <- glmer(flower1 ~altitude+site_altitude+ (1|site/site_plot) + (1|population/mother),binomial,data=data, na.action=na.exclude)
anova(m1,m2)  
par(mfrow = c(2, 2))
m1_residuals <- simulateResiduals(m1, n=1000)
plot(m1_residuals)
plotResiduals(m1_residuals, data$site_altitude, quantreg = T)
plotResiduals(m1_residuals, data$altitude, quantreg = T)

#results
em_cont1<-emmeans(m1, specs = pairwise ~ altitude*site_altitude, type="response", adjust="none")

#values for plot
res1<- as.data.frame(em_cont1[1])

#plot
res1$emmeans.site_altitude <- factor(res1$emmeans.site_altitude, levels = c("low_site", "high_site"))
plotten<-ggplot(res1,aes(x=emmeans.site_altitude, y=emmeans.prob, fill = emmeans.altitude)) + # Change fill to color
  theme_bw() +
  geom_point(position=position_dodge2(width=0.1),shape=c(16,16, 16,16),size=4,colour= c( "blue", "red","blue","red" )) + 
  geom_errorbar(ymin=c((res1[3,3]-res1[3,4]), (res1[4,3]-res1[4,4]),(res1[1,3]-res1[1,4]),(res1[2,3]-res1[2,4])),
                ymax=c((res1[3,3]+res1[3,4]), (res1[4,3]+res1[4,4]),(res1[1,3]+res1[1,4]),(res1[2,3]+res1[2,4])),width=0.1,position=position_dodge2(width=-2))+
  scale_y_continuous(limits=c(0,1))+
  labs(title="gxe_flower1", y="flower1", x="Transplant elevation", caption=NULL)
plotten2<-plotten + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
plotten1<-plotten2+ theme(legend.position = "none",legend.title = element_text(size=12, color = "black"), 
                          legend.text = element_text(size=10, colour="black"),
                          legend.key=element_rect(fill='white')) + 
  guides(colour = guide_legend(override.aes = list(size=2, stroke=1.5))) 
flower1_carth<-plotten1


######################################################################################################################################################
data <- subset(data,survival3_w2=="1")

m1 <- glmer(flower2~ altitude*site_altitude+ (1|site/site_plot) + (1|population/mother),binomial,data=data, na.action=na.exclude)
m2 <- glmer(flower2 ~altitude+site_altitude+ (1|site/site_plot) + (1|population/mother),binomial,data=data, na.action=na.exclude)
anova(m1,m2)  

m1_residuals <- simulateResiduals(m1, n=1000)
plot(m1_residuals)
plotResiduals(m1_residuals, data$site_altitude, quantreg = T)
plotResiduals(m1_residuals, data$altitude, quantreg = T)


#results
em_cont1<-emmeans(m1, specs = pairwise ~ altitude*site_altitude, type="response", adjust="none")

#values for plot
res1<- as.data.frame(em_cont1[1])

#plot
res1$emmeans.site_altitude <- factor(res1$emmeans.site_altitude, levels = c("low_site", "high_site"))

plotten<-ggplot(res1,aes(x=emmeans.site_altitude, y=emmeans.prob, fill = emmeans.altitude)) + # Change fill to color
  theme_bw() +
  geom_point(position=position_dodge2(width=0.1),shape=c(16,16, 16,16),size=4,colour= c( "blue", "red","blue","red" )) + 
  geom_errorbar(ymin=c((res1[3,3]-res1[3,4]), (res1[4,3]-res1[4,4]),(res1[1,3]-res1[1,4]),(res1[2,3]-res1[2,4])),
                ymax=c((res1[3,3]+res1[3,4]), (res1[4,3]+res1[4,4]),(res1[1,3]+res1[1,4]),(res1[2,3]+res1[2,4])),width=0.1,position=position_dodge2(width=-2))+
  scale_y_continuous(limits=c(0,1))+
  labs(title="gxe_flower2", y="flower2", x="Transplant elevation", caption=NULL)
plotten2<-plotten + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
plotten1<-plotten2+ theme(legend.position = "none",legend.title = element_text(size=12, color = "black"), 
                          legend.text = element_text(size=10, colour="black"),
                          legend.key=element_rect(fill='white')) + 
  guides(colour = guide_legend(override.aes = list(size=2, stroke=1.5))) 
flower2_carth<-plotten1

################################################################################################################################################################################################
data <- subset(data,survival5_w3=="1")

m1 <- glmer(flower3~ altitude*site_altitude+ (1|site/site_plot) + (1|population),binomial,data=data, na.action=na.exclude)
m2 <- glmer(flower3 ~altitude+site_altitude+ (1|site/site_plot) + (1|population),binomial,data=data, na.action=na.exclude)
anova(m1,m2)

m1_residuals <- simulateResiduals(m1, n=1000)
plot(m1_residuals)
plotResiduals(m1_residuals, data$site_altitude, quantreg = T)
plotResiduals(m1_residuals, data$altitude, quantreg = T)


#results
em_cont1<-emmeans(m1, specs = pairwise ~ altitude*site_altitude, type="response", adjust="none")

#values for plot
res1<- as.data.frame(em_cont1[1])

####plot 
res1$emmeans.site_altitude <- factor(res1$emmeans.site_altitude, levels = c("low_site", "high_site"))

plotten<-ggplot(res1,aes(x=emmeans.site_altitude, y=emmeans.prob, fill = emmeans.altitude)) + # Change fill to color
  theme_bw() +
  geom_point(position=position_dodge2(width=0.1),shape=c(16,16, 16,16),size=4,colour= c( "blue", "red","blue","red" )) + 
  geom_errorbar(ymin=c((res1[3,3]-res1[3,4]), (res1[4,3]-res1[4,4]),(res1[1,3]-res1[1,4]),(res1[2,3]-res1[2,4])),
                ymax=c((res1[3,3]+res1[3,4]), (res1[4,3]+res1[4,4]),(res1[1,3]+res1[1,4]),(res1[2,3]+res1[2,4])),width=0.1,position=position_dodge2(width=-2))+
  scale_y_continuous(limits=c(0,1))+
  labs(title="gxe_flower3", y="flower3", x="Transplant elevation", caption=NULL)
plotten2<-plotten + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
plotten1<-plotten2+ theme(legend.position = "none",
                          legend.title = element_text(size=12, color = "black"), 
                          legend.text = element_text(size=10, colour="black"),
                          legend.key=element_rect(fill='white')) + 
  guides(colour = guide_legend(override.aes = list(size=2, stroke=1.5))) 
flower3_carth<-plotten1
