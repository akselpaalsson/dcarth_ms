library(lme4)
#setwd("/Users/apalsson/Desktop/dcarth_ms/"
data <- read.csv("establishment_data.csv", header = TRUE, sep=",", stringsAsFactors = FALSE)  
data$mother <- paste(data$mother,data$population, sep="_")
data$plot <- paste(data$site, data$plot, sep="_");data$altitude<-as.factor(data$altitude)
data$mother<-as.factor(data$mother);data$population<-as.factor(data$population)
data$site_altitude<-as.factor(data$site_altitude);data$site<-as.factor(data$site)
data$established_new<-data$established_nr/data$actual_seed_number
response_matrix <- cbind(data$established_nr, data$actual_seed_number)

m1<-glmer(response_matrix~altitude*site_altitude+(1|site/plot)+(1|population/mother),family="binomial", data=data)
m1_residuals <- simulateResiduals(m1, n=1000)
plot(m1_residuals)
plotResiduals(m1_residuals, data$site_altitude, quantreg = T)
plotResiduals(m1_residuals, data$altitude, quantreg = T)
summary(m1)
emmeans(m1, c("altitude","site_altitude"), type="response")

low_sites <- subset(data, site_altitude=="low_altitude")
response_matrix <- cbind(low_sites$established_nr, low_sites$actual_seed_number)
m1<-glmer(response_matrix~altitude+(1|site),family="binomial", data=low_sites)
m2<-glmer(response_matrix~1+(1|site),family="binomial", data=low_sites)
anova(m1,m2)

high_sites <- subset(data, site_altitude=="high_altitude")
response_matrix <- cbind(high_sites$established_nr, high_sites$actual_seed_number)
m1<-glmer(response_matrix~altitude+(1|site),family="binomial", data=high_sites)
m2<-glmer(response_matrix~1+(1|site),family="binomial", data=high_sites)
anova(m1,m2)
