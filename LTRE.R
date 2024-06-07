library(boot)
library(popbio)
set.seed(89)
#setwd("/Users/apalsson/Desktop/dcarth_ms/")
data <- read.csv("dcarth_ms_dryad_data_1123.csv")
high_site <- subset(data, site_altitude=="high_site")
low_site <- subset(data, site_altitude=="low_site")

foo <- function(data, indices){
  dt<-data[indices,]
  
  high <- subset(low_site, altitude=="high")
  
  survival1_w1_0 <- subset(high, survival1_w1=="0");survival1_w1_1 <- subset(high, survival1_w1=="1")
  nrow_comb_surv1<-nrow(survival1_w1_0) +nrow(survival1_w1_1);prop_w1_surv <- (nrow(survival1_w1_1)/nrow_comb_surv1)
  
  survival2_s1_0 <- subset(high, survival2_s1=="0");survival2_s1_1 <- subset(high, survival2_s1=="1")
  nrow_comb_surv2<-nrow(survival2_s1_1) +nrow(survival2_s1_0);prop_s1_surv <- (nrow(survival2_s1_1)/nrow_comb_surv2)
  
  survival3_w2_0 <- subset(survival2_s1_1, survival3_w2=="0");survival3_w2_1 <- subset(high, survival3_w2=="1")
  nrow_comb_surv3<-nrow(survival3_w2_1) +nrow(survival3_w2_0);prop_w2_surv <- (nrow(survival3_w2_1)/nrow_comb_surv3)
  
  survival4_s2_0 <- subset(survival3_w2_1, survival4_s2=="0");survival4_s2_1 <- subset(high, survival4_s2=="1")
  nrow_comb_surv4<-nrow(survival4_s2_1) +nrow(survival4_s2_0);prop_s2_surv <- (nrow(survival4_s2_1)/nrow_comb_surv4)
  
  survival5_w3_0 <- subset(survival4_s2_1, survival5_w3=="0");survival5_w3_1 <- subset(high, survival5_w3=="1")
  nrow_comb_surv5<-nrow(survival5_w3_1) +nrow(survival5_w3_0);prop_w3_surv <- (nrow(survival5_w3_1)/nrow_comb_surv5)
  
  survival6_s3_0 <- subset(survival5_w3_1, survival6_s3=="0");survival6_s3_1 <- subset(high, survival6_s3=="1")
  nrow_comb_surv6<-nrow(survival6_s3_1) +nrow(survival6_s3_0);prop_s3_surv <- (nrow(survival6_s3_1)/nrow_comb_surv6)
  
  survival7_w4_0 <- subset(survival6_s3_1, survival_7_w4=="0");survival7_w4_1 <- subset(high, survival_7_w4=="1")
  nrow_comb_surv7<-nrow(survival7_w4_1) +nrow(survival7_w4_0);prop_w4_surv <- (nrow(survival7_w4_1)/nrow_comb_surv7)
  
  survival1_w1_1 <- subset(high, survival1_w1=="1");flower1_1 <- subset(survival1_w1_1,flower1=="1")
  flower1 <- nrow(flower1_1)/nrow(survival1_w1_1);seed1_sum <- sum(survival1_w1_1$seed1)
  seed1 <- (flower1*mean(seed1_sum/(nrow(survival1_w1_1))) * 0.0981)
  
  survival3_w2_1 <- subset(high, survival3_w2=="1");flower2_1 <- subset(survival3_w2_1,flower2=="1")
  flower2 <- nrow(flower2_1)/nrow(survival3_w2_1);seed2_sum <- sum(survival3_w2_1$seed2)
  seed2 <- (flower2*mean(seed2_sum/(nrow(survival3_w2_1))) * 0.0981)
  
  survival5_w3_1 <- subset(high, survival5_w3=="1");flower3_1 <- subset(survival5_w3_1,flower3=="1")
  flower3 <- nrow(flower3_1)/nrow(survival5_w3_1);seed3_sum <- sum(survival5_w3_1$seed3)
  seed3 <- (flower2*mean(seed3_sum/(nrow(survival5_w3_1))) * 0.0981)
  
  matrix_high_in_low <- matrix(c(0,seed1,0,seed2,0,seed3,0,
                                 prop_w1_surv,0,0,0,0,0,0,0,prop_s1_surv,0,0,0,0,0,0,0,prop_w2_surv,0,0,0,0,0,0,0,
                                 prop_s2_surv,0,0,0,0,0,0,0,prop_w3_surv,0,0,0,0,0,0,0,prop_s3_surv,0), nr= 7, byrow= TRUE )
  
  low <- subset(low_site, altitude=="low")
  survival1_w1_0 <- subset(low, survival1_w1=="0");survival1_w1_1 <- subset(low, survival1_w1=="1")
  nrow_comb_surv1<-nrow(survival1_w1_0) +nrow(survival1_w1_1);prop_w1_surv <- (nrow(survival1_w1_1)/nrow_comb_surv1)
  
  survival2_s1_0 <- subset(low, survival2_s1=="0");survival2_s1_1 <- subset(low, survival2_s1=="1")
  nrow_comb_surv2<-nrow(survival2_s1_1) +nrow(survival2_s1_0);prop_s1_surv <- (nrow(survival2_s1_1)/nrow_comb_surv2)
  
  survival3_w2_0 <- subset(survival2_s1_1, survival3_w2=="0");survival3_w2_1 <- subset(low, survival3_w2=="1")
  nrow_comb_surv3<-nrow(survival3_w2_1) +nrow(survival3_w2_0);prop_w2_surv <- (nrow(survival3_w2_1)/nrow_comb_surv3)
  
  survival4_s2_0 <- subset(survival3_w2_1, survival4_s2=="0");survival4_s2_1 <- subset(low, survival4_s2=="1")
  nrow_comb_surv4<-nrow(survival4_s2_1) +nrow(survival4_s2_0);prop_s2_surv <- (nrow(survival4_s2_1)/nrow_comb_surv4)
  
  survival5_w3_0 <- subset(survival4_s2_1, survival5_w3=="0");survival5_w3_1 <- subset(low, survival5_w3=="1")
  nrow_comb_surv5<-nrow(survival5_w3_1) +nrow(survival5_w3_0);prop_w3_surv <- (nrow(survival5_w3_1)/nrow_comb_surv5)
  
  survival6_s3_0 <- subset(survival5_w3_1, survival6_s3=="0");survival6_s3_1 <- subset(low, survival6_s3=="1")
  nrow_comb_surv6<-nrow(survival6_s3_1) +nrow(survival6_s3_0);prop_s3_surv <- (nrow(survival6_s3_1)/nrow_comb_surv6)
  
  survival7_w4_0 <- subset(survival6_s3_1, survival_7_w4=="0");survival7_w4_1 <- subset(low, survival_7_w4=="1")
  nrow_comb_surv7<-nrow(survival7_w4_1) +nrow(survival7_w4_0);prop_w4_surv <- (nrow(survival7_w4_1)/nrow_comb_surv7)
  
  survival1_w1_1 <- subset(low, survival1_w1=="1");flower1_1 <- subset(survival1_w1_1,flower1=="1")
  flower1 <- nrow(flower1_1)/nrow(survival1_w1_1);seed1_sum <- sum(survival1_w1_1$seed1)
  seed1 <- (flower1*mean(seed1_sum/(nrow(survival1_w1_1))) * 0.1124)
  
  survival3_w2_1 <- subset(low, survival3_w2=="1");flower2_1 <- subset(survival3_w2_1,flower2=="1")
  flower2 <- nrow(flower2_1)/nrow(survival3_w2_1);seed2_sum <- sum(survival3_w2_1$seed2)
  seed2 <- (flower2*mean(seed2_sum/(nrow(survival3_w2_1))) * 0.1124)
  
  
  
  survival5_w3_1 <- subset(low, survival5_w3=="1");flower3_1 <- subset(survival5_w3_1,flower3=="1")
  flower3 <- nrow(flower3_1)/nrow(survival5_w3_1);seed3_sum <- sum(survival5_w3_1$seed3)
  seed3 <- (flower2*mean(seed3_sum/(nrow(survival5_w3_1))) * 0.1124)
  
  matrix_low_in_low <- matrix(c(0,seed1,0,seed2,0,seed3,0,
                                prop_w1_surv,0,0,0,0,0,0,0,prop_s1_surv,0,0,0,0,0,0,0,prop_w2_surv,0,0,0,0,0,0,0,
                                prop_s2_surv,0,0,0,0,0,0,0,prop_w3_surv,0,0,0,0,0,0,0,prop_s3_surv,0), nr= 7, byrow= TRUE )
  
  
  ltre_res_high<-LTRE( matrix_high_in_low,matrix_low_in_low)
  
  
  return(
    
    ltre_res_high
    
  )
  
}

results_A = boot(data = low_site, statistic = foo, R = 20000)
ltre_low <- results_A$t
mean_ltre_surv1_1st_winter<-mean(ltre_low[,2]);mean_ltre_seed1<-mean(ltre_low[,8])
mean_ltre_surv1_1st_season<-mean(ltre_low[,10]);mean_ltre_surv1_2nd_winter<-mean(ltre_low[,18])
mean_ltre_seed2<-mean(ltre_low[,22]);mean_ltre_surv1_2nd_season<-mean(ltre_low[,26])
mean_ltre_surv1_3rd_winter<-mean(ltre_low[,34]);mean_ltre_seed3<-mean(ltre_low[,36])
mean_ltre_surv1_3rd_season<-mean(ltre_low[42])

means_ltre_low<-as.data.frame(rbind(mean_ltre_surv1_1st_winter
                                    ,mean_ltre_seed1,mean_ltre_surv1_1st_season
                                    ,mean_ltre_surv1_2nd_winter,mean_ltre_seed2,mean_ltre_surv1_2nd_season,mean_ltre_surv1_3rd_winter
                                    ,mean_ltre_seed3,mean_ltre_surv1_3rd_season))
means_ltre_low<-t(means_ltre_low)
#high sites 
foo <- function(data, indices){
  dt<-data[indices,]
  
  high <- subset(high_site, altitude=="high")
  
  survival1_w1_0 <- subset(high, survival1_w1=="0");survival1_w1_1 <- subset(high, survival1_w1=="1")
  nrow_comb_surv1<-nrow(survival1_w1_0) +nrow(survival1_w1_1);prop_w1_surv <- (nrow(survival1_w1_1)/nrow_comb_surv1)
  
  survival2_s1_0 <- subset(high, survival2_s1=="0");survival2_s1_1 <- subset(high, survival2_s1=="1")
  nrow_comb_surv2<-nrow(survival2_s1_1) +nrow(survival2_s1_0);prop_s1_surv <- (nrow(survival2_s1_1)/nrow_comb_surv2)
  
  survival3_w2_0 <- subset(survival2_s1_1, survival3_w2=="0");survival3_w2_1 <- subset(high, survival3_w2=="1")
  nrow_comb_surv3<-nrow(survival3_w2_1) +nrow(survival3_w2_0);prop_w2_surv <- (nrow(survival3_w2_1)/nrow_comb_surv3)
  
  survival4_s2_0 <- subset(survival3_w2_1, survival4_s2=="0");survival4_s2_1 <- subset(high, survival4_s2=="1")
  nrow_comb_surv4<-nrow(survival4_s2_1) +nrow(survival4_s2_0);prop_s2_surv <- (nrow(survival4_s2_1)/nrow_comb_surv4)
  
  survival5_w3_0 <- subset(survival4_s2_1, survival5_w3=="0");survival5_w3_1 <- subset(high, survival5_w3=="1")
  nrow_comb_surv5<-nrow(survival5_w3_1) +nrow(survival5_w3_0);prop_w3_surv <- (nrow(survival5_w3_1)/nrow_comb_surv5)
  
  survival6_s3_0 <- subset(survival5_w3_1, survival6_s3=="0");survival6_s3_1 <- subset(high, survival6_s3=="1")
  nrow_comb_surv6<-nrow(survival6_s3_1) +nrow(survival6_s3_0);prop_s3_surv <- (nrow(survival6_s3_1)/nrow_comb_surv6)
  
  survival7_w4_0 <- subset(survival6_s3_1, survival_7_w4=="0");survival7_w4_1 <- subset(high, survival_7_w4=="1")
  nrow_comb_surv7<-nrow(survival7_w4_1) +nrow(survival7_w4_0);prop_w4_surv <- (nrow(survival7_w4_1)/nrow_comb_surv7)
  
  survival1_w1_1 <- subset(high, survival1_w1=="1");flower1_1 <- subset(survival1_w1_1,flower1=="1")
  flower1 <- nrow(flower1_1)/nrow(survival1_w1_1);seed1_sum <- sum(survival1_w1_1$seed1)
  seed1 <- (flower1*mean(seed1_sum/(nrow(survival1_w1_1))) * 0.0511)
  
  survival3_w2_1 <- subset(high, survival3_w2=="1");flower2_1 <- subset(survival3_w2_1,flower2=="1")
  flower2 <- nrow(flower2_1)/nrow(survival3_w2_1);seed2_sum <- sum(survival3_w2_1$seed2)
  seed2 <- (flower2*mean(seed2_sum/(nrow(survival3_w2_1))) * 0.0511)
  
  survival5_w3_1 <- subset(high, survival5_w3=="1");flower3_1 <- subset(survival5_w3_1,flower3=="1")
  flower3 <- nrow(flower3_1)/nrow(survival5_w3_1);seed3_sum <- sum(survival5_w3_1$seed3)
  seed3 <- (flower2*mean(seed3_sum/(nrow(survival5_w3_1))) * 0.0511)
  
  matrix_high_in_high <- matrix(c(0,seed1,0,seed2,0,seed3,0,
                                  prop_w1_surv,0,0,0,0,0,0,0,prop_s1_surv,0,0,0,0,0,0,0,prop_w2_surv,0,0,0,0,0,0,0,
                                  prop_s2_surv,0,0,0,0,0,0,0,prop_w3_surv,0,0,0,0,0,0,0,prop_s3_surv,0), nr= 7, byrow= TRUE )
  
  low <- subset(high_site, altitude=="low")
  survival1_w1_0 <- subset(low, survival1_w1=="0");survival1_w1_1 <- subset(low, survival1_w1=="1")
  nrow_comb_surv1<-nrow(survival1_w1_0) +nrow(survival1_w1_1);prop_w1_surv <- (nrow(survival1_w1_1)/nrow_comb_surv1)
  
  survival2_s1_0 <- subset(low, survival2_s1=="0");survival2_s1_1 <- subset(low, survival2_s1=="1")
  nrow_comb_surv2<-nrow(survival2_s1_1) +nrow(survival2_s1_0);prop_s1_surv <- (nrow(survival2_s1_1)/nrow_comb_surv2)
  
  survival3_w2_0 <- subset(survival2_s1_1, survival3_w2=="0");survival3_w2_1 <- subset(low, survival3_w2=="1")
  nrow_comb_surv3<-nrow(survival3_w2_1) +nrow(survival3_w2_0);prop_w2_surv <- (nrow(survival3_w2_1)/nrow_comb_surv3)
  
  survival4_s2_0 <- subset(survival3_w2_1, survival4_s2=="0");survival4_s2_1 <- subset(low, survival4_s2=="1")
  nrow_comb_surv4<-nrow(survival4_s2_1) +nrow(survival4_s2_0);prop_s2_surv <- (nrow(survival4_s2_1)/nrow_comb_surv4)
  
  survival5_w3_0 <- subset(survival4_s2_1, survival5_w3=="0");survival5_w3_1 <- subset(low, survival5_w3=="1")
  nrow_comb_surv5<-nrow(survival5_w3_1) +nrow(survival5_w3_0);prop_w3_surv <- (nrow(survival5_w3_1)/nrow_comb_surv5)
  
  survival6_s3_0 <- subset(survival5_w3_1, survival6_s3=="0");survival6_s3_1 <- subset(low, survival6_s3=="1")
  nrow_comb_surv6<-nrow(survival6_s3_1) +nrow(survival6_s3_0);prop_s3_surv <- (nrow(survival6_s3_1)/nrow_comb_surv6)
  
  survival7_w4_0 <- subset(survival6_s3_1, survival_7_w4=="0");survival7_w4_1 <- subset(low, survival_7_w4=="1")
  nrow_comb_surv7<-nrow(survival7_w4_1) +nrow(survival7_w4_0);prop_w4_surv <- (nrow(survival7_w4_1)/nrow_comb_surv7)
  
  survival1_w1_1 <- subset(low, survival1_w1=="1");flower1_1 <- subset(survival1_w1_1,flower1=="1")
  flower1 <- nrow(flower1_1)/nrow(survival1_w1_1);seed1_sum <- sum(survival1_w1_1$seed1)
  seed1 <- (flower1*mean(seed1_sum/(nrow(survival1_w1_1))) * 0.0329)
  
  survival3_w2_1 <- subset(low, survival3_w2=="1");flower2_1 <- subset(survival3_w2_1,flower2=="1")
  flower2 <- nrow(flower2_1)/nrow(survival3_w2_1);seed2_sum <- sum(survival3_w2_1$seed2)
  seed2 <- (flower2*mean(seed2_sum/(nrow(survival3_w2_1))) * 0.0329)
  
  survival5_w3_1 <- subset(low, survival5_w3=="1");flower3_1 <- subset(survival5_w3_1,flower3=="1")
  flower3 <- nrow(flower3_1)/nrow(survival5_w3_1);seed3_sum <- sum(survival5_w3_1$seed3)
  seed3 <- (flower2*mean(seed3_sum/(nrow(survival5_w3_1))) * 0.0329)
  
  matrix_low_in_high <- matrix(c(0,seed1,0,seed2,0,seed3,0,
                                 prop_w1_surv,0,0,0,0,0,0,0,prop_s1_surv,0,0,0,0,0,0,0,prop_w2_surv,0,0,0,0,0,0,0,
                                 prop_s2_surv,0,0,0,0,0,0,0,prop_w3_surv,0,0,0,0,0,0,0,prop_s3_surv,0), nr= 7, byrow= TRUE )
  
  
  ltre_res_high<-LTRE( matrix_low_in_high,matrix_high_in_high)
  
  
  return(
    
    ltre_res_high
    
  )
  
}
results_A = boot(data = high_site, statistic = foo, R = 20000)
ltre_high <- results_A$t
mean_ltre_surv1_1st_winter<-mean(ltre_high[,2]);mean_ltre_seed1<-mean(ltre_high[,8])
mean_ltre_surv1_1st_season<-mean(ltre_high[,10]);mean_ltre_surv1_2nd_winter<-mean(ltre_high[,18])
mean_ltre_seed2<-mean(ltre_high[,22]);mean_ltre_surv1_2nd_season<-mean(ltre_high[,26])
mean_ltre_surv1_3rd_winter<-mean(ltre_high[,34]);mean_ltre_seed3<-mean(ltre_high[,36])
mean_ltre_surv1_3rd_season<-mean(ltre_high[42])

means_ltre_high<-as.data.frame(rbind(mean_ltre_surv1_1st_winter
                                     ,mean_ltre_seed1,mean_ltre_surv1_1st_season
                                     ,mean_ltre_surv1_2nd_winter,mean_ltre_seed2,mean_ltre_surv1_2nd_season,mean_ltre_surv1_3rd_winter
                                     ,mean_ltre_seed3,mean_ltre_surv1_3rd_season))
means_ltre_high<-t(means_ltre_high)
#
write.csv(means_ltre_high, file="carth_ltre_high.csv")
write.csv(means_ltre_low, file="carth_ltre_low.csv")
