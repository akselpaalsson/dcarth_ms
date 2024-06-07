library(lme4)
library(glmmTMB)
library(emmeans)
library(DHARMa)
library(ggplot2)
#setwd 
#setwd("/Users/apalsson/Desktop/dcarth_ms/")
data <- read.csv("dcarth_ms_dryad_data_1123.csv")
data$site_plot <- paste(data$site, data$plot, sep="_")
data <- subset(data, survival1_w1=="1")
m1 <- glmmTMB(seed1~altitude*site_altitude+(1|site/site_plot) + (1|population/mother),data=data,ziformula=~1,family=poisson)
m2 <- glmmTMB(seed1~altitude+site_altitude+(1|site/site_plot) + (1|population/mother),data=data,ziformula=~1,family=poisson)
anova(m1,m2)
m1_residuals <- simulateResiduals(m1, n=1000)
plot(m1_residuals)  
em_cont1<-emmeans(m1, specs = pairwise ~ altitude*site_altitude, type="response", adjust="none")
em_cont1[1]

res1<- as.data.frame(em_cont1[1])
res1$emmeans.site_altitude <- factor(res1$emmeans.site_altitude, levels = c("low_site", "high_site"))

new.dat <- data.frame(altitude = data$altitude,
                      site_altitude= data$site_altitude, 
                      site =data$site,site_plot = data$site_plot, 
                      population = data$population,mother =data$mother)
new.dat$prediction <- predict(m1, newdata = new.dat, 
                              type = "response", re.form = NULL)
new_low <- subset(new.dat, altitude=="low");new_high <- subset(new.dat, altitude=="high")
new_low_site <- subset(new.dat, site_altitude=="low_site");new_high_site <- subset(new.dat, site_altitude=="high_site")
new_low_in_low <- subset(new_low, site_altitude=="low_site");new_low_in_high <- subset(new_low, site_altitude=="high_site")
new_high_in_low <- subset(new_high, site_altitude=="low_site");new_high_in_high <- subset(new_high, site_altitude=="high_site")

mean_low_in_low<-mean(new_low_in_low$prediction);mean_low_in_high<-mean(new_low_in_high$prediction)
mean_high_in_low<-mean(new_high_in_low$prediction);mean_high_in_high<-mean(new_high_in_high$prediction)

ci_up_low_in_low<-mean(new_low_in_low$prediction) + 1.96* (sd(new_low_in_low$prediction)/sqrt(nrow(new_low_in_low)))
ci_low_low_in_low<-mean(new_low_in_low$prediction) - 1.96* (sd(new_low_in_low$prediction)/sqrt(nrow(new_low_in_low)))
ci_up_high_in_low<-mean(new_high_in_low$prediction) + 1.96* (sd(new_high_in_low$prediction)/sqrt(nrow(new_high_in_low)))
ci_low_high_in_low<-mean(new_high_in_low$prediction) - 1.96* (sd(new_high_in_low$prediction)/sqrt(nrow(new_high_in_low)))

ci_up_low_in_high<-mean(new_low_in_high$prediction) + 1.96* (sd(new_low_in_high$prediction)/sqrt(nrow(new_low_in_high)))
ci_low_low_in_high<-mean(new_low_in_high$prediction) - 1.96* (sd(new_low_in_high$prediction)/sqrt(nrow(new_low_in_high)))
ci_up_high_in_high<-mean(new_high_in_high$prediction) + 1.96* (sd(new_high_in_high$prediction)/sqrt(nrow(new_high_in_high)))
ci_low_high_in_high<-mean(new_high_in_high$prediction) - 1.96* (sd(new_high_in_high$prediction)/sqrt(nrow(new_high_in_high)))

low_for_plotting <- "low";high_for_plotting <- "high"
low_site_for_plotting <- "low_site";high_site_for_plotting <- "high_site"
low_in_low_for_plotting<-cbind(low_for_plotting,low_site_for_plotting,mean_low_in_low,ci_low_low_in_low, ci_up_low_in_low)
high_in_low_for_plotting<-cbind(high_for_plotting, low_site_for_plotting,mean_high_in_low,ci_low_high_in_low, ci_up_high_in_low)
low_in_high_for_plotting<-cbind(low_for_plotting, high_site_for_plotting,mean_low_in_high,ci_low_low_in_high, ci_up_low_in_high)
high_in_high_for_plotting<-cbind(high_for_plotting, high_site_for_plotting, mean_high_in_high,ci_low_high_in_high, ci_up_high_in_high)
s16_seed_count_plotting<-rbind(low_in_low_for_plotting, high_in_low_for_plotting, low_in_high_for_plotting, high_in_high_for_plotting)
colnames(s16_seed_count_plotting)[1] <- "altitude";colnames(s16_seed_count_plotting)[2] <- "site_altitude"
colnames(s16_seed_count_plotting)[3] <- "mean";colnames(s16_seed_count_plotting)[4] <- "low_ci"
colnames(s16_seed_count_plotting)[5] <- "up_ci"
##
s16_seed_count_plotting_1<- as.data.frame(s16_seed_count_plotting)

s16_seed_count_plotting<-as.data.frame(s16_seed_count_plotting)
s16_seed_count_plotting$altitude <- as.factor(s16_seed_count_plotting$altitude)
s16_seed_count_plotting$site_altitude <- as.factor(s16_seed_count_plotting$site_altitude)
s16_seed_count_plotting$altitude <- as.factor(s16_seed_count_plotting$altitude)
s16_seed_count_plotting$mean <- as.numeric(s16_seed_count_plotting$mean)
s16_seed_count_plotting$low_ci <- as.numeric(s16_seed_count_plotting$low_ci)
s16_seed_count_plotting$up_ci <- as.numeric(s16_seed_count_plotting$up_ci)
s16_seed_count_plotting$site_altitude <- factor(s16_seed_count_plotting$site_altitude, levels = c("low_site", "high_site"))
s16_seed_count_plotting$mean <- as.numeric(s16_seed_count_plotting$mean)
s16_seed_count_plotting$low_ci <- as.numeric(s16_seed_count_plotting$low_ci)
s16_seed_count_plotting$up_ci <- as.numeric(s16_seed_count_plotting$up_ci)
plotten<-ggplot(s16_seed_count_plotting,aes(x=site_altitude, y=mean, fill = altitude)) +
  theme_bw() +
  geom_point(position=position_dodge(width=0.1),shape=c(16,16, 16,16),size=4,colour= c( "red", "blue","red","blue" )) + 
  geom_errorbar(ymin=c(s16_seed_count_plotting[1,4], s16_seed_count_plotting[2,4],s16_seed_count_plotting[3,4],s16_seed_count_plotting[4,4]),
                ymax=c(s16_seed_count_plotting[1,5], s16_seed_count_plotting[2,5],s16_seed_count_plotting[3,5],s16_seed_count_plotting[4,5]),width=0.1,position=position_dodge(0.1))+
  #geom_line(aes(group=snp), position = position_dodge(width = 0.1)) 
  scale_y_continuous(limits=c(10,40))+
  labs(title="gxe_seed1", y="seed1 ", x="Transplant elevation", caption=NULL)
plotten2<-plotten + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
plotten1<-plotten2+ theme(legend.position = "none",legend.title = element_text(size=12, color = "black"), 
                          legend.text = element_text(size=10, colour="black"),
                          legend.key=element_rect(fill='white')) + 
  guides(colour = guide_legend(override.aes = list(size=2, stroke=1.5))) 
plotten1
######################################################################################################################################################
data <- read.csv("dcarth_ms_dryad_data_1123.csv")
data$site_plot <- paste(data$site, data$plot, sep="_")
data <- subset(data, survival3_w2=="1")

#fit most parsimonious model
m1 <- glmmTMB(seed2~altitude*site_altitude+(1|site) + (1|population/mother),data=data,ziformula=~1,family=poisson)
m2 <- glmmTMB(seed2~altitude+site_altitude+(1|site) + (1|population/mother),data=data,ziformula=~1,family=poisson)
#simulate and check model diagnostics
m1_residuals <- simulateResiduals(m1, n=1000)
plot(m1_residuals)  
#Test interaction 
anova(m1,m2)
#perform contrasts tests
em_cont1<-emmeans(m1, specs = pairwise ~ altitude*site_altitude, type="response", adjust="none")
em_cont1
#save results for plotting
res1<- as.data.frame(em_cont1[1])

res1$emmeans.site_altitude <- factor(res1$emmeans.site_altitude, levels = c("low_site", "high_site"))

new.dat <- data.frame(altitude = data$altitude,
                      site_altitude= data$site_altitude, 
                      site =data$site,site_plot = data$site_plot, 
                      population = data$population,mother =data$mother)
new.dat$prediction <- predict(m1, newdata = new.dat, 
                              type = "response", re.form = NULL)
new_low <- subset(new.dat, altitude=="low");new_high <- subset(new.dat, altitude=="high")
new_low_site <- subset(new.dat, site_altitude=="low_site");new_high_site <- subset(new.dat, site_altitude=="high_site")
new_low_in_low <- subset(new_low, site_altitude=="low_site");new_low_in_high <- subset(new_low, site_altitude=="high_site")
new_high_in_low <- subset(new_high, site_altitude=="low_site");new_high_in_high <- subset(new_high, site_altitude=="high_site")

mean_low_in_low<-mean(new_low_in_low$prediction);mean_low_in_high<-mean(new_low_in_high$prediction)
mean_high_in_low<-mean(new_high_in_low$prediction);mean_high_in_high<-mean(new_high_in_high$prediction)

ci_up_low_in_low<-mean(new_low_in_low$prediction) + 1.96* (sd(new_low_in_low$prediction)/sqrt(nrow(new_low_in_low)))
ci_low_low_in_low<-mean(new_low_in_low$prediction) - 1.96* (sd(new_low_in_low$prediction)/sqrt(nrow(new_low_in_low)))
ci_up_high_in_low<-mean(new_high_in_low$prediction) + 1.96* (sd(new_high_in_low$prediction)/sqrt(nrow(new_high_in_low)))
ci_low_high_in_low<-mean(new_high_in_low$prediction) - 1.96* (sd(new_high_in_low$prediction)/sqrt(nrow(new_high_in_low)))

ci_up_low_in_high<-mean(new_low_in_high$prediction) + 1.96* (sd(new_low_in_high$prediction)/sqrt(nrow(new_low_in_high)))
ci_low_low_in_high<-mean(new_low_in_high$prediction) - 1.96* (sd(new_low_in_high$prediction)/sqrt(nrow(new_low_in_high)))
ci_up_high_in_high<-mean(new_high_in_high$prediction) + 1.96* (sd(new_high_in_high$prediction)/sqrt(nrow(new_high_in_high)))
ci_low_high_in_high<-mean(new_high_in_high$prediction) - 1.96* (sd(new_high_in_high$prediction)/sqrt(nrow(new_high_in_high)))

low_for_plotting <- "low";high_for_plotting <- "high"
low_site_for_plotting <- "low_site";high_site_for_plotting <- "high_site"
low_in_low_for_plotting<-cbind(low_for_plotting,low_site_for_plotting,mean_low_in_low,ci_low_low_in_low, ci_up_low_in_low)
high_in_low_for_plotting<-cbind(high_for_plotting, low_site_for_plotting,mean_high_in_low,ci_low_high_in_low, ci_up_high_in_low)
low_in_high_for_plotting<-cbind(low_for_plotting, high_site_for_plotting,mean_low_in_high,ci_low_low_in_high, ci_up_low_in_high)
high_in_high_for_plotting<-cbind(high_for_plotting, high_site_for_plotting, mean_high_in_high,ci_low_high_in_high, ci_up_high_in_high)
s16_seed_count_plotting<-rbind(low_in_low_for_plotting, high_in_low_for_plotting, low_in_high_for_plotting, high_in_high_for_plotting)
colnames(s16_seed_count_plotting)[1] <- "altitude";colnames(s16_seed_count_plotting)[2] <- "site_altitude";colnames(s16_seed_count_plotting)[3] <- "mean"
colnames(s16_seed_count_plotting)[4] <- "low_ci";colnames(s16_seed_count_plotting)[5] <- "up_ci"
##
s16_seed_count_plotting<-as.data.frame(s16_seed_count_plotting);s16_seed_count_plotting$altitude <- as.factor(s16_seed_count_plotting$altitude)
s16_seed_count_plotting$site_altitude <- as.factor(s16_seed_count_plotting$site_altitude);s16_seed_count_plotting$altitude <- as.factor(s16_seed_count_plotting$altitude)
s16_seed_count_plotting$mean <- as.numeric(s16_seed_count_plotting$mean);s16_seed_count_plotting$low_ci <- as.numeric(s16_seed_count_plotting$low_ci)
s16_seed_count_plotting$up_ci <- as.numeric(s16_seed_count_plotting$up_ci)
s16_seed_count_plotting$site_altitude <- factor(s16_seed_count_plotting$site_altitude, levels = c("low_site", "high_site"))

s16_seed_count_plotting$mean <- as.numeric(s16_seed_count_plotting$mean);s16_seed_count_plotting$low_ci <- as.numeric(s16_seed_count_plotting$low_ci)
s16_seed_count_plotting$up_ci <- as.numeric(s16_seed_count_plotting$up_ci)
plotten<-ggplot(s16_seed_count_plotting,aes(x=site_altitude, y=mean, fill = altitude)) + 
  theme_bw() +
  geom_point(position=position_dodge(width=0.1),shape=c(16,16, 16,16),size=4,colour= c( "red", "blue","red","blue" )) + 
  geom_errorbar(ymin=c(s16_seed_count_plotting[1,4], s16_seed_count_plotting[2,4],s16_seed_count_plotting[3,4],s16_seed_count_plotting[4,4]),
                ymax=c(s16_seed_count_plotting[1,5], s16_seed_count_plotting[2,5],s16_seed_count_plotting[3,5],s16_seed_count_plotting[4,5]),width=0.1,position=position_dodge(0.1))+
  scale_y_continuous(limits=c(0,12))+
  labs(title="gxe_seed2", y="seed2 ", x="Transplant elevation", caption=NULL)
plotten2<-plotten + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
plotten1<-plotten2+ theme(legend.position="none",legend.title = element_text(size=12, color = "black"), 
                          legend.text = element_text(size=10, colour="black"),
                          legend.key=element_rect(fill='white')) + 
  guides(colour = guide_legend(override.aes = list(size=2, stroke=1.5))) 
plotten1

#######################################################################################################################################
data <- read.csv("dcarth_ms_dryad_data_1123.csv")
data$site_plot <- paste(data$site, data$plot, sep="_")
data <- subset(data, survival5_w3=="1")

m1 <- glmmTMB(seed3~altitude*site_altitude+(1|site/site_plot) + (1|population/mother),data=data,ziformula=~1,family=poisson)
m2 <- glmmTMB(seed3~altitude+site_altitude+(1|site/site_plot) + (1|population/mother),data=data,ziformula=~1,family=poisson)
m1_residuals <- simulateResiduals(m1, n=1000)
plot(m1_residuals) 
anova(m1,m2)
em_cont1<-emmeans(m1, specs = pairwise ~ altitude*site_altitude, type="response", adjust="none")
em_cont1[1]

res1<- as.data.frame(em_cont1[1])
res1$emmeans.site_altitude <- factor(res1$emmeans.site_altitude, levels = c("low_site", "high_site"))

new.dat <- data.frame(altitude = data$altitude,
                      site_altitude= data$site_altitude, 
                      site =data$site,site_plot = data$site_plot, 
                      population = data$population,mother =data$mother)
new.dat$prediction <- predict(m1, newdata = new.dat, 
                              type = "response", re.form = NULL)
new_low <- subset(new.dat, altitude=="low");new_high <- subset(new.dat, altitude=="high")
new_low_site <- subset(new.dat, site_altitude=="low_site");new_high_site <- subset(new.dat, site_altitude=="high_site")
new_low_in_low <- subset(new_low, site_altitude=="low_site");new_low_in_high <- subset(new_low, site_altitude=="high_site")
new_high_in_low <- subset(new_high, site_altitude=="low_site");new_high_in_high <- subset(new_high, site_altitude=="high_site")

mean_low_in_low<-mean(new_low_in_low$prediction);mean_low_in_high<-mean(new_low_in_high$prediction)
mean_high_in_low<-mean(new_high_in_low$prediction);mean_high_in_high<-mean(new_high_in_high$prediction)

ci_up_low_in_low<-mean(new_low_in_low$prediction) + 1.96* (sd(new_low_in_low$prediction)/sqrt(nrow(new_low_in_low)))
ci_low_low_in_low<-mean(new_low_in_low$prediction) - 1.96* (sd(new_low_in_low$prediction)/sqrt(nrow(new_low_in_low)))
ci_up_high_in_low<-mean(new_high_in_low$prediction) + 1.96* (sd(new_high_in_low$prediction)/sqrt(nrow(new_high_in_low)))
ci_low_high_in_low<-mean(new_high_in_low$prediction) - 1.96* (sd(new_high_in_low$prediction)/sqrt(nrow(new_high_in_low)))

ci_up_low_in_high<-mean(new_low_in_high$prediction) + 1.96* (sd(new_low_in_high$prediction)/sqrt(nrow(new_low_in_high)))
ci_low_low_in_high<-mean(new_low_in_high$prediction) - 1.96* (sd(new_low_in_high$prediction)/sqrt(nrow(new_low_in_high)))
ci_up_high_in_high<-mean(new_high_in_high$prediction) + 1.96* (sd(new_high_in_high$prediction)/sqrt(nrow(new_high_in_high)))
ci_low_high_in_high<-mean(new_high_in_high$prediction) - 1.96* (sd(new_high_in_high$prediction)/sqrt(nrow(new_high_in_high)))

low_for_plotting <- "low";high_for_plotting <- "high"
low_site_for_plotting <- "low_site";high_site_for_plotting <- "high_site"
low_in_low_for_plotting<-cbind(low_for_plotting,low_site_for_plotting,mean_low_in_low,ci_low_low_in_low, ci_up_low_in_low)
high_in_low_for_plotting<-cbind(high_for_plotting, low_site_for_plotting,mean_high_in_low,ci_low_high_in_low, ci_up_high_in_low)
low_in_high_for_plotting<-cbind(low_for_plotting, high_site_for_plotting,mean_low_in_high,ci_low_low_in_high, ci_up_low_in_high)
high_in_high_for_plotting<-cbind(high_for_plotting, high_site_for_plotting, mean_high_in_high,ci_low_high_in_high, ci_up_high_in_high)
s16_seed_count_plotting<-rbind(low_in_low_for_plotting, high_in_low_for_plotting, low_in_high_for_plotting, high_in_high_for_plotting)
colnames(s16_seed_count_plotting)[1] <- "altitude";colnames(s16_seed_count_plotting)[2] <- "site_altitude"
colnames(s16_seed_count_plotting)[3] <- "mean";colnames(s16_seed_count_plotting)[4] <- "low_ci"
colnames(s16_seed_count_plotting)[5] <- "up_ci"
##
s16_seed_count_plotting_1<- as.data.frame(s16_seed_count_plotting)

s16_seed_count_plotting<-as.data.frame(s16_seed_count_plotting)
s16_seed_count_plotting$altitude <- as.factor(s16_seed_count_plotting$altitude)
s16_seed_count_plotting$site_altitude <- as.factor(s16_seed_count_plotting$site_altitude)
s16_seed_count_plotting$altitude <- as.factor(s16_seed_count_plotting$altitude)
s16_seed_count_plotting$mean <- as.numeric(s16_seed_count_plotting$mean)
s16_seed_count_plotting$low_ci <- as.numeric(s16_seed_count_plotting$low_ci)
s16_seed_count_plotting$up_ci <- as.numeric(s16_seed_count_plotting$up_ci)
s16_seed_count_plotting$site_altitude <- factor(s16_seed_count_plotting$site_altitude, levels = c("low_site", "high_site"))
s16_seed_count_plotting$mean <- as.numeric(s16_seed_count_plotting$mean)
s16_seed_count_plotting$low_ci <- as.numeric(s16_seed_count_plotting$low_ci)
s16_seed_count_plotting$up_ci <- as.numeric(s16_seed_count_plotting$up_ci)
plotten<-ggplot(s16_seed_count_plotting,aes(x=site_altitude, y=mean, fill = altitude)) +
  theme_bw() +
  geom_point(position=position_dodge(width=0.1),shape=c(16,16, 16,16),size=4,colour= c( "red", "blue","red","blue" )) + 
  geom_errorbar(ymin=c(s16_seed_count_plotting[1,4], s16_seed_count_plotting[2,4],s16_seed_count_plotting[3,4],s16_seed_count_plotting[4,4]),
                ymax=c(s16_seed_count_plotting[1,5], s16_seed_count_plotting[2,5],s16_seed_count_plotting[3,5],s16_seed_count_plotting[4,5]),width=0.1,position=position_dodge(0.1))+
  scale_y_continuous(limits=c(0,80))+
  labs(title="gxe_seed3", y="seed3 ", x="Transplant elevation", caption=NULL)
plotten2<-plotten + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
plotten1<-plotten2+ theme(legend.position = "none",legend.title = element_text(size=12, color = "black"), 
                          legend.text = element_text(size=10, colour="black"),
                          legend.key=element_rect(fill='white')) + 
  guides(colour = guide_legend(override.aes = list(size=2, stroke=1.5))) 
plotten1



