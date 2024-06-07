library(survival)
library(ggplot2)
library(coxme)
library(survminer)
library(ggplot2)
#setwd("/Users/apalsson/Desktop/dcarth_ms/")
data <- read.csv("dcarth_ms_dryad_data_1123.csv")
data$site_plot <- paste(data$site, data$plot, sep="_")
##################################################################################################################################################################
data <- data %>%
  mutate(death = ifelse(survival1_w1 == 0 | 
                          survival2_s1 == 0 | 
                          survival3_w2 == 0 | 
                          survival4_s2 == 0 | 
                          survival5_w3 == 0 | 
                          survival6_s3 == 0 | 
                          survival_7_w4 == 0, 1, 0))

data <- data %>%
  mutate(time = case_when(
    survival1_w1 == 0 ~ 1,
    survival2_s1 == 0 ~ 2,
    survival3_w2 == 0 ~ 3,
    survival4_s2 == 0 ~ 4,
    survival5_w3 == 0 ~ 5,
    survival6_s3 == 0 ~ 6,
    survival_7_w4 == 1 ~ 7,
    survival_7_w4 == 0 ~ 7,
    TRUE ~ NA_integer_
  ))

m1 <- coxme(Surv(time, death) ~altitude*site_altitude + (1|site/site_plot)+(1|population/mother), data=data)   
summary(m1)

low_sites <- subset(data, site_altitude=="low_site")
m3 <- survfit(Surv(time, death) ~altitude, data=low_sites)   
ggsurvplot(m3, data=low_sites, risk.table =FALSE ,  conf.int = TRUE, palette = c("#0000FF", "#FF0000"))
summary(m3)

m4 <- coxme(Surv(time, death) ~altitude + (1|site/site_plot)+(1|population/mother), data=low_sites)   
summary(m4)

high_sites <- subset(data, site_altitude=="high_site")
m5 <- survfit(Surv(time, death) ~altitude, data=high_sites)   
ggsurvplot(m5, data=high_sites, risk.table =FALSE ,  conf.int = TRUE, palette = c("#0000FF", "#FF0000"))

m6 <- coxme(Surv(time, death) ~altitude + (1|site/site_plot)+(1|population/mother), data=high_sites)   
summary(m6)

low <- subset(data, altitude=="low")
m7 <- coxme(Surv(time, death) ~site_altitude + (1|site/site_plot)+(1|population/mother), data=low)   
summary(m7)

high <- subset(data, altitude=="high")
m8 <- survfit(Surv(time, death) ~altitude, data=high_sites)   
ggsurvplot(m8, data=high_sites, risk.table =TRUE ,  conf.int = TRUE, palette = c("#0000FF", "#FF0000"))

m9 <- coxme(Surv(time, death) ~site_altitude + (1|site/site_plot)+(1|population/mother), data=high)   
summary(m9)
