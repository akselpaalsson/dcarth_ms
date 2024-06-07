library(dplyr)
library(ggplot2)
library(tibble)
#setwd("/Users/apalsson/Desktop/dcarth_ms/")
ltre_high<-read.csv("carth_ltre_high.csv");ltre_high <- ltre_high[,-1]
ltre_low<-read.csv("carth_ltre_low.csv");ltre_low <- ltre_low[,-1]

carth_bca_cis_all_highhigh<-read.csv("carth_bca_cis_all_highhigh.csv")
carth_bca_cis_all_highlow<-read.csv("carth_bca_cis_all_highlow.csv")
carth_bca_cis_all_lowhigh<-read.csv("carth_bca_cis_all_lowhigh.csv")
carth_bca_cis_all_lowlow<-read.csv("carth_bca_cis_all_lowlow.csv")

carth_booted_outputs_highhigh<-read.csv("carth_booted_outputs_highhigh.csv")
carth_booted_outputs_highlow<-read.csv("carth_booted_outputs_highlow.csv")
carth_booted_outputs_lowhigh<-read.csv("carth_booted_outputs_lowhigh.csv")
carth_booted_outputs_lowlow<-read.csv("carth_booted_outputs_lowlow.csv")


carth_bca_cis_all_highlow <- carth_bca_cis_all_highlow %>%
  mutate(id = ifelse(grepl("stable", id), gsub("elasticity_", "", id), id))

carth_bca_cis_all_highhigh <- carth_bca_cis_all_highhigh %>%
  mutate(id = ifelse(grepl("stable", id), gsub("elasticity_", "", id), id))

carth_bca_cis_all_lowhigh <- carth_bca_cis_all_lowhigh %>%
  mutate(id = ifelse(grepl("stable", id), gsub("elasticity_", "", id), id))

carth_bca_cis_all_lowlow <- carth_bca_cis_all_lowlow %>%
  mutate(id = ifelse(grepl("stable", id), gsub("elasticity_", "", id), id))

#create ltre plots
ltre_low <- as.matrix(ltre_low)
barplot(ltre_low, col="grey", ylim=c(-0.5,0.1),
        ylab="Contribution of foreign vital rate", xlab="Vital rate", main="LTRE low sites")

ltre_high <- as.matrix(ltre_high)
barplot(ltre_high, col="grey", ylim=c(-0.5,0.1),
        ylab="Contribution of foreign vital rate", xlab="Vital rate", main="LTRE high sites")


#create lambda plots
#high sites
head(carth_bca_cis_all_highhigh)
mean(carth_booted_outputs_highhigh$boot_rep_lambda)
mean(carth_booted_outputs_lowhigh$boot_rep_lambda)
conf_v1_highhigh <- c(0.76156317,0.86501600) 
head(carth_bca_cis_all_lowhigh)
conf_v1_lowhigh <- c(0.561653402,0.6993271)
plotv_highhigh<-as.vector(carth_booted_outputs_highhigh$boot_rep_lambda)
plotv_lowhigh<-as.vector(carth_booted_outputs_lowhigh$boot_rep_lambda)
#this works and plots hist of bootstrap results
hist(plotv_highhigh,breaks=40,col = "blue", xlab = "Lambda",xlim=c(0.35,1), ylim=c(0,2500) , main = "high sites")
hist(plotv_lowhigh,breaks=40, col = "red", xlab = "Lambda", main = "", add=T)
abline(v =conf_v1_lowhigh,lty=4)
abline(v =conf_v1_highhigh,lty=4)

##low_sites lambda
mean(carth_booted_outputs_highlow$boot_rep_lambda)
mean(carth_booted_outputs_lowlow$boot_rep_lambda)
head(carth_bca_cis_all_lowlow)
conf_v1_lowlow <- c(1.925217415,2.49458483)
head(carth_bca_cis_all_highlow)
conf_v1_highlow <- c(1.389659,1.728118292)
plotv_highlow<-as.vector(carth_booted_outputs_highlow$boot_rep_lambda)
plotv_lowlow<-as.vector(carth_booted_outputs_lowlow$boot_rep_lambda)
hist(plotv_highlow,breaks=40,col = "blue", xlab = "Lambda",xlim=c(1,2.8), ylim=c(0,2000) , main = "low sites")
hist(plotv_lowlow,breaks=40, col = "red", xlab = "Lambda", main = "", add=T)
abline(v =conf_v1_lowlow,lty=4);abline(v =conf_v1_highlow,lty=4)

############elasticity and stable stage plots 
mean_boot_rep_lambda<-mean(na.omit(carth_booted_outputs_highlow$boot_rep_lambda))             
mean_boot_rep_elasticity_seed1<-mean(na.omit(carth_booted_outputs_highlow$boot_rep_elasticity_seed1))      
mean_boot_rep_elasticity_seed2<-mean(na.omit(carth_booted_outputs_highlow$boot_rep_elasticity_seed2))  
mean_boot_rep_elasticity_seed3<-mean(na.omit(carth_booted_outputs_highlow$boot_rep_elasticity_seed3)) 
mean_boot_rep_elasticity_surv1_1st_winter<-mean(na.omit(carth_booted_outputs_highlow$boot_elasticity_surv1_w1_to_surv2_s1))
mean_boot_rep_elasticity_surv1_1st_season<-mean(na.omit(carth_booted_outputs_highlow$boot_elasticity_surv2_s1_to_surv3_w2))
mean_boot_rep_elasticity_surv1_2nd_winter<-mean(na.omit(carth_booted_outputs_highlow$boot_elasticity_surv3_w2_to_surv4_s2))
mean_boot_rep_elasticity_surv1_2nd_season<-mean(na.omit(carth_booted_outputs_highlow$boot_elasticity_surv4_s2_to_surv5_w3))
mean_boot_rep_elasticity_surv1_3rd_winter<-mean(na.omit(carth_booted_outputs_highlow$boot_elasticity_surv5_w3_to_surv6_s3))
mean_boot_rep_elasticity_surv1_3rd_season<-mean(na.omit(carth_booted_outputs_highlow$boot_elasticity_surv6_s3_to_surv7_w4))
mean_boot_rep_stable1<-mean(na.omit(carth_booted_outputs_highlow$boot_stable1))                    
mean_boot_rep_stable2<-mean(na.omit(carth_booted_outputs_highlow$boot_stable2))                   
mean_boot_rep_stable3<-mean(na.omit(carth_booted_outputs_highlow$boot_stable3))                   
mean_boot_rep_stable4<-mean(na.omit(carth_booted_outputs_highlow$boot_stable4))                    
mean_boot_rep_stable5<-mean(na.omit(carth_booted_outputs_highlow$boot_stable5))                   
mean_boot_rep_stable6<-mean(na.omit(carth_booted_outputs_highlow$boot_stable6))                  
mean_boot_rep_stable7<-mean(na.omit(carth_booted_outputs_highlow$boot_stable7))                    

mean_boot_rep_lambda<-as.data.frame(mean_boot_rep_lambda)
mean_boot_rep_lambda$id<-"lambda";colnames(mean_boot_rep_lambda)[1]<- "value"
mean_boot_rep_elasticity_seed1<-as.data.frame(mean_boot_rep_elasticity_seed1)
mean_boot_rep_elasticity_seed1$id <-"elasticity_seed1";colnames(mean_boot_rep_elasticity_seed1)[1]<- "value"
mean_boot_rep_elasticity_seed2<-as.data.frame(mean_boot_rep_elasticity_seed2)
mean_boot_rep_elasticity_seed2$id <-"elasticity_seed2";colnames(mean_boot_rep_elasticity_seed2)[1]<- "value"
mean_boot_rep_elasticity_seed3<-as.data.frame(mean_boot_rep_elasticity_seed3)
mean_boot_rep_elasticity_seed3$id <-"elasticity_seed3";colnames(mean_boot_rep_elasticity_seed3)[1]<- "value"
mean_boot_rep_elasticity_surv1_1st_winter<-as.data.frame(mean_boot_rep_elasticity_surv1_1st_winter)
mean_boot_rep_elasticity_surv1_1st_winter$id <-"elasticity_surv1_w1_to_surv2_s1";colnames(mean_boot_rep_elasticity_surv1_1st_winter)[1]<- "value"
mean_boot_rep_elasticity_surv1_1st_season<-as.data.frame(mean_boot_rep_elasticity_surv1_1st_season)
mean_boot_rep_elasticity_surv1_1st_season$id <-"elasticity_surv2_s1_to_surv3_w2";colnames(mean_boot_rep_elasticity_surv1_1st_season)[1]<- "value"
mean_boot_rep_elasticity_surv1_2nd_winter<-as.data.frame(mean_boot_rep_elasticity_surv1_2nd_winter)
mean_boot_rep_elasticity_surv1_2nd_winter$id <-"elasticity_surv3_w2_to_surv4_s2";colnames(mean_boot_rep_elasticity_surv1_2nd_winter)[1]<- "value"
mean_boot_rep_elasticity_surv1_2nd_season<-as.data.frame(mean_boot_rep_elasticity_surv1_2nd_season)
mean_boot_rep_elasticity_surv1_2nd_season$id <-"elasticity_surv4_s2_to_surv5_w3";colnames(mean_boot_rep_elasticity_surv1_2nd_season)[1]<- "value"
mean_boot_rep_elasticity_surv1_3rd_winter<-as.data.frame(mean_boot_rep_elasticity_surv1_3rd_winter)
mean_boot_rep_elasticity_surv1_3rd_winter$id <-"elasticity_surv5_w3_to_surv6_s3";colnames(mean_boot_rep_elasticity_surv1_3rd_winter)[1]<- "value"
mean_boot_rep_elasticity_surv1_3rd_season<-as.data.frame(mean_boot_rep_elasticity_surv1_3rd_season)
mean_boot_rep_elasticity_surv1_3rd_season$id <-"elasticity_surv6_s3_to_surv7_w4";colnames(mean_boot_rep_elasticity_surv1_3rd_season)[1]<- "value"
mean_boot_rep_stable1<-as.data.frame(mean_boot_rep_stable1)
mean_boot_rep_stable1$id <-"stable1";colnames(mean_boot_rep_stable1)[1]<- "value"
mean_boot_rep_stable2<-as.data.frame(mean_boot_rep_stable2)
mean_boot_rep_stable2$id <-"stable2";colnames(mean_boot_rep_stable2)[1]<- "value"
mean_boot_rep_stable3<-as.data.frame(mean_boot_rep_stable3)
mean_boot_rep_stable3$id <-"stable3";colnames(mean_boot_rep_stable3)[1]<- "value"
mean_boot_rep_stable4<-as.data.frame(mean_boot_rep_stable4)
mean_boot_rep_stable4$id <-"stable4";colnames(mean_boot_rep_stable4)[1]<- "value"
mean_boot_rep_stable5<-as.data.frame(mean_boot_rep_stable5)
mean_boot_rep_stable5$id <-"stable5";colnames(mean_boot_rep_stable5)[1]<- "value"
mean_boot_rep_stable6<-as.data.frame(mean_boot_rep_stable6)
mean_boot_rep_stable6$id <-"stable6";colnames(mean_boot_rep_stable6)[1]<- "value"
mean_boot_rep_stable7<-as.data.frame(mean_boot_rep_stable7)
mean_boot_rep_stable7$id <-"stable7";colnames(mean_boot_rep_stable7)[1]<- "value"

elas_binded_highlow<-as.data.frame(rbind(mean_boot_rep_lambda,mean_boot_rep_elasticity_seed1,mean_boot_rep_elasticity_seed2,
                                         mean_boot_rep_elasticity_seed3,
                                         mean_boot_rep_elasticity_surv1_1st_winter,
                                         mean_boot_rep_elasticity_surv1_1st_season,mean_boot_rep_elasticity_surv1_2nd_winter,
                                         mean_boot_rep_elasticity_surv1_2nd_season,mean_boot_rep_elasticity_surv1_3rd_winter,
                                         mean_boot_rep_elasticity_surv1_3rd_season,mean_boot_rep_stable1,mean_boot_rep_stable2,
                                         mean_boot_rep_stable3,mean_boot_rep_stable4,mean_boot_rep_stable5,mean_boot_rep_stable6,mean_boot_rep_stable7
))
elas_binded_highlow$site_altitude <- "low_sites"
elas_binded_highlow$altitude <- "high"


elasticities_highlow <- merge(elas_binded_highlow, carth_bca_cis_all_highlow, by="id")


mean_boot_rep_lambda<-mean(na.omit(carth_booted_outputs_lowlow$boot_rep_lambda))             
mean_boot_rep_elasticity_seed1<-mean(na.omit(carth_booted_outputs_lowlow$boot_rep_elasticity_seed1))      
mean_boot_rep_elasticity_seed2<-mean(na.omit(carth_booted_outputs_lowlow$boot_rep_elasticity_seed2))  
mean_boot_rep_elasticity_seed3<-mean(na.omit(carth_booted_outputs_lowlow$boot_rep_elasticity_seed3)) 
mean_boot_rep_elasticity_surv1_1st_winter<-mean(na.omit(carth_booted_outputs_lowlow$boot_elasticity_surv1_w1_to_surv2_s1))
mean_boot_rep_elasticity_surv1_1st_season<-mean(na.omit(carth_booted_outputs_lowlow$boot_elasticity_surv2_s1_to_surv3_w2))
mean_boot_rep_elasticity_surv1_2nd_winter<-mean(na.omit(carth_booted_outputs_lowlow$boot_elasticity_surv3_w2_to_surv4_s2))
mean_boot_rep_elasticity_surv1_2nd_season<-mean(na.omit(carth_booted_outputs_lowlow$boot_elasticity_surv4_s2_to_surv5_w3))
mean_boot_rep_elasticity_surv1_3rd_winter<-mean(na.omit(carth_booted_outputs_lowlow$boot_elasticity_surv5_w3_to_surv6_s3))
mean_boot_rep_elasticity_surv1_3rd_season<-mean(na.omit(carth_booted_outputs_lowlow$boot_elasticity_surv6_s3_to_surv7_w4))
mean_boot_rep_stable1<-mean(na.omit(carth_booted_outputs_lowlow$boot_stable1))                    
mean_boot_rep_stable2<-mean(na.omit(carth_booted_outputs_lowlow$boot_stable2))                   
mean_boot_rep_stable3<-mean(na.omit(carth_booted_outputs_lowlow$boot_stable3))                   
mean_boot_rep_stable4<-mean(na.omit(carth_booted_outputs_lowlow$boot_stable4))                    
mean_boot_rep_stable5<-mean(na.omit(carth_booted_outputs_lowlow$boot_stable5))                   
mean_boot_rep_stable6<-mean(na.omit(carth_booted_outputs_lowlow$boot_stable6))                  
mean_boot_rep_stable7<-mean(na.omit(carth_booted_outputs_lowlow$boot_stable7))                    

mean_boot_rep_lambda<-as.data.frame(mean_boot_rep_lambda)
mean_boot_rep_lambda$id<-"lambda";colnames(mean_boot_rep_lambda)[1]<- "value"
mean_boot_rep_elasticity_seed1<-as.data.frame(mean_boot_rep_elasticity_seed1)
mean_boot_rep_elasticity_seed1$id <-"elasticity_seed1";colnames(mean_boot_rep_elasticity_seed1)[1]<- "value"
mean_boot_rep_elasticity_seed2<-as.data.frame(mean_boot_rep_elasticity_seed2)
mean_boot_rep_elasticity_seed2$id <-"elasticity_seed2";colnames(mean_boot_rep_elasticity_seed2)[1]<- "value"
mean_boot_rep_elasticity_seed3<-as.data.frame(mean_boot_rep_elasticity_seed3)
mean_boot_rep_elasticity_seed3$id <-"elasticity_seed3";colnames(mean_boot_rep_elasticity_seed3)[1]<- "value"
mean_boot_rep_elasticity_surv1_1st_winter<-as.data.frame(mean_boot_rep_elasticity_surv1_1st_winter)
mean_boot_rep_elasticity_surv1_1st_winter$id <-"elasticity_surv1_w1_to_surv2_s1";colnames(mean_boot_rep_elasticity_surv1_1st_winter)[1]<- "value"
mean_boot_rep_elasticity_surv1_1st_season<-as.data.frame(mean_boot_rep_elasticity_surv1_1st_season)
mean_boot_rep_elasticity_surv1_1st_season$id <-"elasticity_surv2_s1_to_surv3_w2";colnames(mean_boot_rep_elasticity_surv1_1st_season)[1]<- "value"
mean_boot_rep_elasticity_surv1_2nd_winter<-as.data.frame(mean_boot_rep_elasticity_surv1_2nd_winter)
mean_boot_rep_elasticity_surv1_2nd_winter$id <-"elasticity_surv3_w2_to_surv4_s2";colnames(mean_boot_rep_elasticity_surv1_2nd_winter)[1]<- "value"
mean_boot_rep_elasticity_surv1_2nd_season<-as.data.frame(mean_boot_rep_elasticity_surv1_2nd_season)
mean_boot_rep_elasticity_surv1_2nd_season$id <-"elasticity_surv4_s2_to_surv5_w3";colnames(mean_boot_rep_elasticity_surv1_2nd_season)[1]<- "value"
mean_boot_rep_elasticity_surv1_3rd_winter<-as.data.frame(mean_boot_rep_elasticity_surv1_3rd_winter)
mean_boot_rep_elasticity_surv1_3rd_winter$id <-"elasticity_surv5_w3_to_surv6_s3";colnames(mean_boot_rep_elasticity_surv1_3rd_winter)[1]<- "value"
mean_boot_rep_elasticity_surv1_3rd_season<-as.data.frame(mean_boot_rep_elasticity_surv1_3rd_season)
mean_boot_rep_elasticity_surv1_3rd_season$id <-"elasticity_surv6_s3_to_surv7_w4";colnames(mean_boot_rep_elasticity_surv1_3rd_season)[1]<- "value"


mean_boot_rep_stable1<-as.data.frame(mean_boot_rep_stable1)
mean_boot_rep_stable1$id <-"stable1";colnames(mean_boot_rep_stable1)[1]<- "value"
mean_boot_rep_stable2<-as.data.frame(mean_boot_rep_stable2)
mean_boot_rep_stable2$id <-"stable2";colnames(mean_boot_rep_stable2)[1]<- "value"
mean_boot_rep_stable3<-as.data.frame(mean_boot_rep_stable3)
mean_boot_rep_stable3$id <-"stable3";colnames(mean_boot_rep_stable3)[1]<- "value"
mean_boot_rep_stable4<-as.data.frame(mean_boot_rep_stable4)
mean_boot_rep_stable4$id <-"stable4";colnames(mean_boot_rep_stable4)[1]<- "value"
mean_boot_rep_stable5<-as.data.frame(mean_boot_rep_stable5)
mean_boot_rep_stable5$id <-"stable5";colnames(mean_boot_rep_stable5)[1]<- "value"
mean_boot_rep_stable6<-as.data.frame(mean_boot_rep_stable6)
mean_boot_rep_stable6$id <-"stable6";colnames(mean_boot_rep_stable6)[1]<- "value"
mean_boot_rep_stable7<-as.data.frame(mean_boot_rep_stable7)
mean_boot_rep_stable7$id <-"stable7";colnames(mean_boot_rep_stable7)[1]<- "value"

elas_binded_lowlow<-as.data.frame(rbind(mean_boot_rep_lambda,mean_boot_rep_elasticity_seed1,mean_boot_rep_elasticity_seed2,
                                        mean_boot_rep_elasticity_seed3,
                                        mean_boot_rep_elasticity_surv1_1st_winter,
                                        mean_boot_rep_elasticity_surv1_1st_season,mean_boot_rep_elasticity_surv1_2nd_winter,
                                        mean_boot_rep_elasticity_surv1_2nd_season,mean_boot_rep_elasticity_surv1_3rd_winter,
                                        mean_boot_rep_elasticity_surv1_3rd_season,mean_boot_rep_stable1,mean_boot_rep_stable2,
                                        mean_boot_rep_stable3,mean_boot_rep_stable4,mean_boot_rep_stable5,mean_boot_rep_stable6,mean_boot_rep_stable7))
elas_binded_lowlow$site_altitude <- "low_sites"
elas_binded_lowlow$altitude <- "low"

elasticities_lowlow <- merge(elas_binded_lowlow, carth_bca_cis_all_lowlow, by="id")



elasticities_low_sites <- as.data.frame(rbind(elasticities_highlow,elasticities_lowlow))

mean_boot_rep_lambda<-mean(na.omit(carth_booted_outputs_highhigh$boot_rep_lambda))             
mean_boot_rep_elasticity_seed1<-mean(na.omit(carth_booted_outputs_highhigh$boot_rep_elasticity_seed1))      
mean_boot_rep_elasticity_seed2<-mean(na.omit(carth_booted_outputs_highhigh$boot_rep_elasticity_seed2))  
mean_boot_rep_elasticity_seed3<-mean(na.omit(carth_booted_outputs_highhigh$boot_rep_elasticity_seed3)) 
mean_boot_rep_elasticity_surv1_1st_winter<-mean(na.omit(carth_booted_outputs_highhigh$boot_elasticity_surv1_w1_to_surv2_s1))
mean_boot_rep_elasticity_surv1_1st_season<-mean(na.omit(carth_booted_outputs_highhigh$boot_elasticity_surv2_s1_to_surv3_w2))
mean_boot_rep_elasticity_surv1_2nd_winter<-mean(na.omit(carth_booted_outputs_highhigh$boot_elasticity_surv3_w2_to_surv4_s2))
mean_boot_rep_elasticity_surv1_2nd_season<-mean(na.omit(carth_booted_outputs_highhigh$boot_elasticity_surv4_s2_to_surv5_w3))
mean_boot_rep_elasticity_surv1_3rd_winter<-mean(na.omit(carth_booted_outputs_highhigh$boot_elasticity_surv5_w3_to_surv6_s3))
mean_boot_rep_elasticity_surv1_3rd_season<-mean(na.omit(carth_booted_outputs_highhigh$boot_elasticity_surv6_s3_to_surv7_w4))
mean_boot_rep_stable1<-mean(na.omit(carth_booted_outputs_highhigh$boot_stable1))                    
mean_boot_rep_stable2<-mean(na.omit(carth_booted_outputs_highhigh$boot_stable2))                   
mean_boot_rep_stable3<-mean(na.omit(carth_booted_outputs_highhigh$boot_stable3))                   
mean_boot_rep_stable4<-mean(na.omit(carth_booted_outputs_highhigh$boot_stable4))                    
mean_boot_rep_stable5<-mean(na.omit(carth_booted_outputs_highhigh$boot_stable5))                   
mean_boot_rep_stable6<-mean(na.omit(carth_booted_outputs_highhigh$boot_stable6))                  
mean_boot_rep_stable7<-mean(na.omit(carth_booted_outputs_highhigh$boot_stable7))         


mean_boot_rep_lambda<-as.data.frame(mean_boot_rep_lambda)
mean_boot_rep_lambda$id<-"lambda";colnames(mean_boot_rep_lambda)[1]<- "value"
mean_boot_rep_elasticity_seed1<-as.data.frame(mean_boot_rep_elasticity_seed1)
mean_boot_rep_elasticity_seed1$id <-"elasticity_seed1";colnames(mean_boot_rep_elasticity_seed1)[1]<- "value"
mean_boot_rep_elasticity_seed2<-as.data.frame(mean_boot_rep_elasticity_seed2)
mean_boot_rep_elasticity_seed2$id <-"elasticity_seed2";colnames(mean_boot_rep_elasticity_seed2)[1]<- "value"
mean_boot_rep_elasticity_seed3<-as.data.frame(mean_boot_rep_elasticity_seed3)
mean_boot_rep_elasticity_seed3$id <-"elasticity_seed3";colnames(mean_boot_rep_elasticity_seed3)[1]<- "value"
mean_boot_rep_elasticity_surv1_1st_winter<-as.data.frame(mean_boot_rep_elasticity_surv1_1st_winter)
mean_boot_rep_elasticity_surv1_1st_winter$id <-"elasticity_surv1_w1_to_surv2_s1";colnames(mean_boot_rep_elasticity_surv1_1st_winter)[1]<- "value"
mean_boot_rep_elasticity_surv1_1st_season<-as.data.frame(mean_boot_rep_elasticity_surv1_1st_season)
mean_boot_rep_elasticity_surv1_1st_season$id <-"elasticity_surv2_s1_to_surv3_w2";colnames(mean_boot_rep_elasticity_surv1_1st_season)[1]<- "value"
mean_boot_rep_elasticity_surv1_2nd_winter<-as.data.frame(mean_boot_rep_elasticity_surv1_2nd_winter)
mean_boot_rep_elasticity_surv1_2nd_winter$id <-"elasticity_surv3_w2_to_surv4_s2";colnames(mean_boot_rep_elasticity_surv1_2nd_winter)[1]<- "value"
mean_boot_rep_elasticity_surv1_2nd_season<-as.data.frame(mean_boot_rep_elasticity_surv1_2nd_season)
mean_boot_rep_elasticity_surv1_2nd_season$id <-"elasticity_surv4_s2_to_surv5_w3";colnames(mean_boot_rep_elasticity_surv1_2nd_season)[1]<- "value"
mean_boot_rep_elasticity_surv1_3rd_winter<-as.data.frame(mean_boot_rep_elasticity_surv1_3rd_winter)
mean_boot_rep_elasticity_surv1_3rd_winter$id <-"elasticity_surv5_w3_to_surv6_s3";colnames(mean_boot_rep_elasticity_surv1_3rd_winter)[1]<- "value"
mean_boot_rep_elasticity_surv1_3rd_season<-as.data.frame(mean_boot_rep_elasticity_surv1_3rd_season)
mean_boot_rep_elasticity_surv1_3rd_season$id <-"elasticity_surv6_s3_to_surv7_w4";colnames(mean_boot_rep_elasticity_surv1_3rd_season)[1]<- "value"


mean_boot_rep_stable1<-as.data.frame(mean_boot_rep_stable1)
mean_boot_rep_stable1$id <-"stable1";colnames(mean_boot_rep_stable1)[1]<- "value"
mean_boot_rep_stable2<-as.data.frame(mean_boot_rep_stable2)
mean_boot_rep_stable2$id <-"stable2";colnames(mean_boot_rep_stable2)[1]<- "value"
mean_boot_rep_stable3<-as.data.frame(mean_boot_rep_stable3)
mean_boot_rep_stable3$id <-"stable3";colnames(mean_boot_rep_stable3)[1]<- "value"
mean_boot_rep_stable4<-as.data.frame(mean_boot_rep_stable4)
mean_boot_rep_stable4$id <-"stable4";colnames(mean_boot_rep_stable4)[1]<- "value"
mean_boot_rep_stable5<-as.data.frame(mean_boot_rep_stable5)
mean_boot_rep_stable5$id <-"stable5";colnames(mean_boot_rep_stable5)[1]<- "value"
mean_boot_rep_stable6<-as.data.frame(mean_boot_rep_stable6)
mean_boot_rep_stable6$id <-"stable6";colnames(mean_boot_rep_stable6)[1]<- "value"
mean_boot_rep_stable7<-as.data.frame(mean_boot_rep_stable7)
mean_boot_rep_stable7$id <-"stable7";colnames(mean_boot_rep_stable7)[1]<- "value"





elas_binded_highhigh<-as.data.frame(rbind(mean_boot_rep_lambda,mean_boot_rep_elasticity_seed1,mean_boot_rep_elasticity_seed2,
                                          mean_boot_rep_elasticity_seed3,
                                          mean_boot_rep_elasticity_surv1_1st_winter,
                                          mean_boot_rep_elasticity_surv1_1st_season,mean_boot_rep_elasticity_surv1_2nd_winter,
                                          mean_boot_rep_elasticity_surv1_2nd_season,mean_boot_rep_elasticity_surv1_3rd_winter,
                                          mean_boot_rep_elasticity_surv1_3rd_season,mean_boot_rep_stable1,mean_boot_rep_stable2,
                                          mean_boot_rep_stable3,mean_boot_rep_stable4,mean_boot_rep_stable5,mean_boot_rep_stable6,mean_boot_rep_stable7))
elas_binded_highhigh$site_altitude <- "high_sites"
elas_binded_highhigh$altitude <- "high"

elasticities_highhigh <- merge(elas_binded_highhigh, carth_bca_cis_all_highhigh, by="id")



mean_boot_rep_lambda<-mean(na.omit(carth_booted_outputs_lowhigh$boot_rep_lambda))             
mean_boot_rep_elasticity_seed1<-mean(na.omit(carth_booted_outputs_lowhigh$boot_rep_elasticity_seed1))      
mean_boot_rep_elasticity_seed2<-mean(na.omit(carth_booted_outputs_lowhigh$boot_rep_elasticity_seed2))  
mean_boot_rep_elasticity_seed3<-mean(na.omit(carth_booted_outputs_lowhigh$boot_rep_elasticity_seed3)) 
mean_boot_rep_elasticity_surv1_1st_winter<-mean(na.omit(carth_booted_outputs_lowhigh$boot_elasticity_surv1_w1_to_surv2_s1))
mean_boot_rep_elasticity_surv1_1st_season<-mean(na.omit(carth_booted_outputs_lowhigh$boot_elasticity_surv2_s1_to_surv3_w2))
mean_boot_rep_elasticity_surv1_2nd_winter<-mean(na.omit(carth_booted_outputs_lowhigh$boot_elasticity_surv3_w2_to_surv4_s2))
mean_boot_rep_elasticity_surv1_2nd_season<-mean(na.omit(carth_booted_outputs_lowhigh$boot_elasticity_surv4_s2_to_surv5_w3))
mean_boot_rep_elasticity_surv1_3rd_winter<-mean(na.omit(carth_booted_outputs_lowhigh$boot_elasticity_surv5_w3_to_surv6_s3))
mean_boot_rep_elasticity_surv1_3rd_season<-mean(na.omit(carth_booted_outputs_lowhigh$boot_elasticity_surv6_s3_to_surv7_w4))
mean_boot_rep_stable1<-mean(na.omit(carth_booted_outputs_lowhigh$boot_stable1))                    
mean_boot_rep_stable2<-mean(na.omit(carth_booted_outputs_lowhigh$boot_stable2))                   
mean_boot_rep_stable3<-mean(na.omit(carth_booted_outputs_lowhigh$boot_stable3))                   
mean_boot_rep_stable4<-mean(na.omit(carth_booted_outputs_lowhigh$boot_stable4))                    
mean_boot_rep_stable5<-mean(na.omit(carth_booted_outputs_lowhigh$boot_stable5))                   
mean_boot_rep_stable6<-mean(na.omit(carth_booted_outputs_lowhigh$boot_stable6))                  
mean_boot_rep_stable7<-mean(na.omit(carth_booted_outputs_lowhigh$boot_stable7))          

mean_boot_rep_lambda<-as.data.frame(mean_boot_rep_lambda)
mean_boot_rep_lambda$id<-"lambda";colnames(mean_boot_rep_lambda)[1]<- "value"
mean_boot_rep_elasticity_seed1<-as.data.frame(mean_boot_rep_elasticity_seed1)
mean_boot_rep_elasticity_seed1$id <-"elasticity_seed1";colnames(mean_boot_rep_elasticity_seed1)[1]<- "value"
mean_boot_rep_elasticity_seed2<-as.data.frame(mean_boot_rep_elasticity_seed2)
mean_boot_rep_elasticity_seed2$id <-"elasticity_seed2";colnames(mean_boot_rep_elasticity_seed2)[1]<- "value"
mean_boot_rep_elasticity_seed3<-as.data.frame(mean_boot_rep_elasticity_seed3)
mean_boot_rep_elasticity_seed3$id <-"elasticity_seed3";colnames(mean_boot_rep_elasticity_seed3)[1]<- "value"


mean_boot_rep_elasticity_surv1_1st_winter<-as.data.frame(mean_boot_rep_elasticity_surv1_1st_winter)
mean_boot_rep_elasticity_surv1_1st_winter$id <-"elasticity_surv1_w1_to_surv2_s1";colnames(mean_boot_rep_elasticity_surv1_1st_winter)[1]<- "value"
mean_boot_rep_elasticity_surv1_1st_season<-as.data.frame(mean_boot_rep_elasticity_surv1_1st_season)
mean_boot_rep_elasticity_surv1_1st_season$id <-"elasticity_surv2_s1_to_surv3_w2";colnames(mean_boot_rep_elasticity_surv1_1st_season)[1]<- "value"
mean_boot_rep_elasticity_surv1_2nd_winter<-as.data.frame(mean_boot_rep_elasticity_surv1_2nd_winter)
mean_boot_rep_elasticity_surv1_2nd_winter$id <-"elasticity_surv3_w2_to_surv4_s2";colnames(mean_boot_rep_elasticity_surv1_2nd_winter)[1]<- "value"
mean_boot_rep_elasticity_surv1_2nd_season<-as.data.frame(mean_boot_rep_elasticity_surv1_2nd_season)
mean_boot_rep_elasticity_surv1_2nd_season$id <-"elasticity_surv4_s2_to_surv5_w3";colnames(mean_boot_rep_elasticity_surv1_2nd_season)[1]<- "value"
mean_boot_rep_elasticity_surv1_3rd_winter<-as.data.frame(mean_boot_rep_elasticity_surv1_3rd_winter)
mean_boot_rep_elasticity_surv1_3rd_winter$id <-"elasticity_surv5_w3_to_surv6_s3";colnames(mean_boot_rep_elasticity_surv1_3rd_winter)[1]<- "value"
mean_boot_rep_elasticity_surv1_3rd_season<-as.data.frame(mean_boot_rep_elasticity_surv1_3rd_season)
mean_boot_rep_elasticity_surv1_3rd_season$id <-"elasticity_surv6_s3_to_surv7_w4";colnames(mean_boot_rep_elasticity_surv1_3rd_season)[1]<- "value"


mean_boot_rep_stable1<-as.data.frame(mean_boot_rep_stable1)
mean_boot_rep_stable1$id <-"stable1";colnames(mean_boot_rep_stable1)[1]<- "value"
mean_boot_rep_stable2<-as.data.frame(mean_boot_rep_stable2)
mean_boot_rep_stable2$id <-"stable2";colnames(mean_boot_rep_stable2)[1]<- "value"
mean_boot_rep_stable3<-as.data.frame(mean_boot_rep_stable3)
mean_boot_rep_stable3$id <-"stable3";colnames(mean_boot_rep_stable3)[1]<- "value"
mean_boot_rep_stable4<-as.data.frame(mean_boot_rep_stable4)
mean_boot_rep_stable4$id <-"stable4";colnames(mean_boot_rep_stable4)[1]<- "value"
mean_boot_rep_stable5<-as.data.frame(mean_boot_rep_stable5)
mean_boot_rep_stable5$id <-"stable5";colnames(mean_boot_rep_stable5)[1]<- "value"
mean_boot_rep_stable6<-as.data.frame(mean_boot_rep_stable6)
mean_boot_rep_stable6$id <-"stable6";colnames(mean_boot_rep_stable6)[1]<- "value"
mean_boot_rep_stable7<-as.data.frame(mean_boot_rep_stable7)
mean_boot_rep_stable7$id <-"stable7";colnames(mean_boot_rep_stable7)[1]<- "value"

elas_binded_lowhigh<-as.data.frame(rbind(mean_boot_rep_lambda,mean_boot_rep_elasticity_seed1,mean_boot_rep_elasticity_seed2,
                                         mean_boot_rep_elasticity_seed3,
                                         mean_boot_rep_elasticity_surv1_1st_winter,
                                         mean_boot_rep_elasticity_surv1_1st_season,mean_boot_rep_elasticity_surv1_2nd_winter,
                                         mean_boot_rep_elasticity_surv1_2nd_season,mean_boot_rep_elasticity_surv1_3rd_winter,
                                         mean_boot_rep_elasticity_surv1_3rd_season,mean_boot_rep_stable1,mean_boot_rep_stable2,
                                         mean_boot_rep_stable3,mean_boot_rep_stable4,mean_boot_rep_stable5,mean_boot_rep_stable6,mean_boot_rep_stable7))
elas_binded_lowhigh$site_altitude <- "high_sites"
elas_binded_lowhigh$altitude <- "low"

elasticities_lowhigh <- merge(elas_binded_lowhigh, carth_bca_cis_all_lowhigh, by="id")



elasticities_high_sites <- as.data.frame(rbind(elasticities_highhigh,elasticities_lowhigh))

elasticities_low_sites <- elasticities_low_sites %>%
  mutate(type = ifelse(grepl("stable", id), "stable", "elasticity")) %>%
  filter(!grepl("lambda", id))

elasticities_high_sites <- elasticities_high_sites %>%
  mutate(type = ifelse(grepl("stable", id), "stable", "elasticity")) %>%
  filter(!grepl("lambda", id))


elasticities_only_low_sites <- subset(elasticities_low_sites, type=="elasticity")
head(elasticities_only_low_sites)
##

#mapping of id values to nr values for high altitude
nr_high <- c(
  "elasticity_surv1_w1_to_surv2_s1" = 1,
  "elasticity_seed1" = 3,
  "elasticity_surv2_s1_to_surv3_w2" = 5,
  "elasticity_surv3_w2_to_surv4_s2" = 7,
  "elasticity_seed2" = 9,
  "elasticity_surv4_s2_to_surv5_w3" = 11,
  "elasticity_surv5_w3_to_surv6_s3" = 13,
  "elasticity_seed3" = 15,
  "elasticity_surv6_s3_to_surv7_w4" = 17
)

#mapping of id values to nr values for low altitude
nr_low <- c(
  "elasticity_surv1_w1_to_surv2_s1" = 2,
  "elasticity_seed1" = 4,
  "elasticity_surv2_s1_to_surv3_w2" = 6,
  "elasticity_surv3_w2_to_surv4_s2" = 8,
  "elasticity_seed2" = 10,
  "elasticity_surv4_s2_to_surv5_w3" = 12,
  "elasticity_surv5_w3_to_surv6_s3" = 14,
  "elasticity_seed3" = 16,
  "elasticity_surv6_s3_to_surv7_w4" = 18
)

#add the 'nr' column based on the mappings and altitude for plotting
elasticities_only_low_sites <- elasticities_only_low_sites %>%
  mutate(nr = case_when(
    altitude == "high" & id %in% names(nr_high) ~ nr_high[id],
    altitude == "low" & id %in% names(nr_low) ~ nr_low[id],
    TRUE ~ NA_real_  #assign na in case no match
  ))

head(elasticities_only_low_sites)
p1 <- ggplot(elasticities_only_low_sites, aes(x=nr, y=value, fill=altitude)) + 
  geom_bar(stat="identity", position=position_dodge(0.05)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.2, position=position_dodge(0.05)) +
  scale_y_continuous(limits=c(0,0.5)) +
  scale_fill_manual("altitude", values = c("high" = "blue", "low" = "red"))
p1 + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

####################################################################################################################################
####################################################################################################################################
####################################################################################################################################

elasticities_only_high_sites <- subset(elasticities_high_sites, type=="elasticity")
head(elasticities_only_low_sites)

nr_high <- c(
  "elasticity_surv1_w1_to_surv2_s1" = 1,
  "elasticity_seed1" = 3,
  "elasticity_surv2_s1_to_surv3_w2" = 5,
  "elasticity_surv3_w2_to_surv4_s2" = 7,
  "elasticity_seed2" = 9,
  "elasticity_surv4_s2_to_surv5_w3" = 11,
  "elasticity_surv5_w3_to_surv6_s3" = 13,
  "elasticity_seed3" = 15,
  "elasticity_surv6_s3_to_surv7_w4" = 17
)

nr_low <- c(
  "elasticity_surv1_w1_to_surv2_s1" = 2,
  "elasticity_seed1" = 4,
  "elasticity_surv2_s1_to_surv3_w2" = 6,
  "elasticity_surv3_w2_to_surv4_s2" = 8,
  "elasticity_seed2" = 10,
  "elasticity_surv4_s2_to_surv5_w3" = 12,
  "elasticity_surv5_w3_to_surv6_s3" = 14,
  "elasticity_seed3" = 16,
  "elasticity_surv6_s3_to_surv7_w4" = 18
)

elasticities_only_high_sites <- elasticities_only_high_sites %>%
  mutate(nr = case_when(
    altitude == "high" & id %in% names(nr_high) ~ nr_high[id],
    altitude == "low" & id %in% names(nr_low) ~ nr_low[id],
    TRUE ~ NA_real_  
  ))


p1 <- ggplot(elasticities_only_high_sites, aes(x=nr, y=value, fill=altitude)) + 
  geom_bar(stat="identity", position=position_dodge(0.05)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.2, position=position_dodge(0.05)) +
  scale_y_continuous(limits=c(0,0.5)) +
  scale_fill_manual("altitude", values = c("high" = "blue", "low" = "red"))
p1 + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
###############

stable_only_low_sites <- subset(elasticities_low_sites, type=="stable")
head(stable_only_low_sites)

nr_high <- c(
  "stable1" = 1,
  "stable2" = 3,
  "stable3" = 5,
  "stable4" = 7,
  "stable5" = 9,
  "stable6" = 11,
  "stable7" = 13
)

nr_low <- c(
  "stable1" = 2,
  "stable2" = 4,
  "stable3" = 6,
  "stable4" = 8,
  "stable5" = 10,
  "stable6" = 12,
  "stable7" = 14
)

stable_only_low_sites <- stable_only_low_sites %>%
  mutate(nr = case_when(
    altitude == "high" & id %in% names(nr_high) ~ nr_high[id],
    altitude == "low" & id %in% names(nr_low) ~ nr_low[id],
    TRUE ~ NA_real_  
  ))


p1 <- ggplot(stable_only_low_sites, aes(x=nr, y=value, fill=altitude)) + 
  geom_bar(stat="identity", position=position_dodge(0.05)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.2, position=position_dodge(0.05)) +
  scale_y_continuous(limits=c(0,1)) +
  scale_fill_manual("altitude", values = c("high" = "blue", "low" = "red"))
p1 + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

###############
###############
stable_only_high_sites <- subset(elasticities_high_sites, type=="stable")
head(stable_only_high_sites)

nr_high <- c(
  "stable1" = 1,
  "stable2" = 3,
  "stable3" = 5,
  "stable4" = 7,
  "stable5" = 9,
  "stable6" = 11,
  "stable7" = 13
)

nr_low <- c(
  "stable1" = 2,
  "stable2" = 4,
  "stable3" = 6,
  "stable4" = 8,
  "stable5" = 10,
  "stable6" = 12,
  "stable7" = 14
)


stable_only_high_sites <- stable_only_high_sites %>%
  mutate(nr = case_when(
    altitude == "high" & id %in% names(nr_high) ~ nr_high[id],
    altitude == "low" & id %in% names(nr_low) ~ nr_low[id],
    TRUE ~ NA_real_  
  ))


p1 <- ggplot(stable_only_high_sites, aes(x=nr, y=value, fill=altitude)) + 
  geom_bar(stat="identity", position=position_dodge(0.05)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.2, position=position_dodge(0.05)) +
  scale_y_continuous(limits=c(0,1)) +
  scale_fill_manual("altitude", values = c("high" = "blue", "low" = "red"))
p1 + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

###############
###############
