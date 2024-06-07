set.seed(89)
library(boot)
library(popbio)
#setwd("/Users/apalsson/Desktop/dcarth_ms/")
#define functions
matrix_model_for_pp <- function(data, indices, establishment) {
  dt <- data[indices, ]
  survival1_w1_0 <- subset(dt, survival1_w1 == "0");survival1_w1_1 <- subset(dt, survival1_w1 == "1")
  nrow_comb_surv1 <- nrow(survival1_w1_0) + nrow(survival1_w1_1);prop_w1_surv <- nrow(survival1_w1_1) / nrow_comb_surv1
  
  survival2_s1_0 <- subset(dt, survival2_s1 == "0");survival2_s1_1 <- subset(dt, survival2_s1 == "1")
  nrow_comb_surv2 <- nrow(survival2_s1_1) + nrow(survival2_s1_0);prop_s1_surv <- nrow(survival2_s1_1) / nrow_comb_surv2
  
  survival3_w2_0 <- subset(survival2_s1_1, survival3_w2 == "0");survival3_w2_1 <- subset(dt, survival3_w2 == "1")
  nrow_comb_surv3 <- nrow(survival3_w2_1) + nrow(survival3_w2_0);prop_w2_surv <- nrow(survival3_w2_1) / nrow_comb_surv3
  
  survival4_s2_0 <- subset(survival3_w2_1, survival4_s2 == "0");survival4_s2_1 <- subset(dt, survival4_s2 == "1")
  nrow_comb_surv4 <- nrow(survival4_s2_1) + nrow(survival4_s2_0);prop_s2_surv <- nrow(survival4_s2_1) / nrow_comb_surv4
  
  survival5_w3_0 <- subset(survival4_s2_1, survival5_w3 == "0");survival5_w3_1 <- subset(dt, survival5_w3 == "1")
  nrow_comb_surv5 <- nrow(survival5_w3_1) + nrow(survival5_w3_0);prop_w3_surv <- nrow(survival5_w3_1) / nrow_comb_surv5
  
  survival6_s3_0 <- subset(survival5_w3_1, survival6_s3 == "0");survival6_s3_1 <- subset(dt, survival6_s3 == "1")
  nrow_comb_surv6 <- nrow(survival6_s3_1) + nrow(survival6_s3_0);prop_s3_surv <- nrow(survival6_s3_1) / nrow_comb_surv6
  
  survival7_w4_0 <- subset(survival6_s3_1, survival_7_w4 == "0");survival7_w4_1 <- subset(dt, survival_7_w4 == "1")
  nrow_comb_surv7 <- nrow(survival7_w4_1) + nrow(survival7_w4_0);prop_w4_surv <- nrow(survival7_w4_1) / nrow_comb_surv7
  
  survival1_w1_1 <- subset(dt, survival1_w1 == "1");flower1_1 <- subset(survival1_w1_1, flower1 == "1")
  flower1 <- nrow(flower1_1) / nrow(survival1_w1_1);seed1_sum <- sum(survival1_w1_1$seed1)
  seed1 <- (flower1 * mean(seed1_sum / nrow(survival1_w1_1)) * establishment)
  
  survival3_w2_1 <- subset(dt, survival3_w2 == "1");flower2_1 <- subset(survival3_w2_1, flower2 == "1")
  flower2 <- nrow(flower2_1) / nrow(survival3_w2_1);seed2_sum <- sum(survival3_w2_1$seed2)
  seed2 <- (flower2 * mean(seed2_sum / nrow(survival3_w2_1)) * establishment)
  
  survival5_w3_1 <- subset(dt, survival5_w3 == "1");flower3_1 <- subset(survival5_w3_1, flower3 == "1")
  flower3 <- nrow(flower3_1) / nrow(survival5_w3_1);seed3_sum <- sum(survival5_w3_1$seed3)
  seed3 <- (flower2 * mean(seed3_sum / nrow(survival5_w3_1)) * establishment)
  
  A <- matrix(c(0, seed1, 0, seed2, 0, seed3, 0,
                prop_w1_surv, 0, 0, 0, 0, 0, 0, 0, prop_s1_surv, 0, 0, 0, 0, 0, 0, 0, prop_w2_surv, 0, 0, 0, 0, 0, 0, 0,
                prop_s2_surv, 0, 0, 0, 0, 0, 0, 0, prop_w3_surv, 0, 0, 0, 0, 0, 0, 0, prop_s3_surv, 0), nr = 7, byrow = TRUE)
  
  A_eigen <- eigen.analysis(A)
  elasticity_seed1 <- A_eigen$elasticities[1, 2];elasticity_seed2 <- A_eigen$elasticities[1, 4];elasticity_seed3 <- A_eigen$elasticities[1, 6]
  surv1_w1_to_surv2_s1 <- A_eigen$elasticities[2, 1];surv2_s1_to_surv3_w2 <- A_eigen$elasticities[3, 2]
  surv3_w2_to_surv4_s2 <- A_eigen$elasticities[4, 3];surv4_s2_to_surv5_w3 <- A_eigen$elasticities[5, 4]
  surv5_w3_to_surv6_s3 <- A_eigen$elasticities[6, 5];surv6_s3_to_surv7_w4 <- A_eigen$elasticities[7, 6]
  
  binded <- rbind( elasticity_seed1, elasticity_seed2, elasticity_seed3, surv1_w1_to_surv2_s1,
                   surv2_s1_to_surv3_w2, surv3_w2_to_surv4_s2, surv4_s2_to_surv5_w3, surv5_w3_to_surv6_s3,
                   surv6_s3_to_surv7_w4)
  return(binded)
}

create_booted_outputs_for_pp <- function(results_A) {
  boot_elasticity_seed1 <- as.data.frame(results_A$t[,1]);colnames(boot_elasticity_seed1)[1] <- "boot_rep_elasticity_seed1"
  boot_elasticity_seed2 <- as.data.frame(results_A$t[,2]);colnames(boot_elasticity_seed2)[1] <- "boot_rep_elasticity_seed2"
  boot_elasticity_seed3 <- as.data.frame(results_A$t[,3]);colnames(boot_elasticity_seed3)[1] <- "boot_rep_elasticity_seed3"
  boot_elasticity_surv1_w1_to_surv2_s1 <- as.data.frame(results_A$t[,4]);colnames(boot_elasticity_surv1_w1_to_surv2_s1)[1] <- "boot_elasticity_surv1_w1_to_surv2_s1"
  boot_elasticity_surv2_s1_to_surv3_w2 <- as.data.frame(results_A$t[,5]);colnames(boot_elasticity_surv2_s1_to_surv3_w2)[1] <- "boot_elasticity_surv2_s1_to_surv3_w2"
  boot_elasticity_surv3_w2_to_surv4_s2 <- as.data.frame(results_A$t[,6]);colnames(boot_elasticity_surv3_w2_to_surv4_s2)[1] <- "boot_elasticity_surv3_w2_to_surv4_s2"
  boot_elasticity_surv4_s2_to_surv5_w3 <- as.data.frame(results_A$t[,7]);colnames(boot_elasticity_surv4_s2_to_surv5_w3)[1] <- "boot_elasticity_surv4_s2_to_surv5_w3"
  boot_elasticity_surv5_w3_to_surv6_s3 <- as.data.frame(results_A$t[,8]);colnames(boot_elasticity_surv5_w3_to_surv6_s3)[1] <- "boot_elasticity_surv5_w3_to_surv6_s3"
  boot_elasticity_surv6_s3_to_surv7_w4 <- as.data.frame(results_A$t[,9]);colnames(boot_elasticity_surv6_s3_to_surv7_w4)[1] <- "boot_elasticity_surv6_s3_to_surv7_w4"
  
  
  #combine dfs
  booted_outputs <- cbind(boot_elasticity_seed1, boot_elasticity_seed2, boot_elasticity_seed3, boot_elasticity_surv1_w1_to_surv2_s1,
                          boot_elasticity_surv2_s1_to_surv3_w2, boot_elasticity_surv3_w2_to_surv4_s2, boot_elasticity_surv4_s2_to_surv5_w3,
                          boot_elasticity_surv5_w3_to_surv6_s3,boot_elasticity_surv6_s3_to_surv7_w4)
  
  mean_boot_rep_elasticity_seed1<-mean(na.omit(booted_outputs$boot_rep_elasticity_seed1))      
  mean_boot_rep_elasticity_seed2<-mean(na.omit(booted_outputs$boot_rep_elasticity_seed2))  
  mean_boot_rep_elasticity_seed3<-mean(na.omit(booted_outputs$boot_rep_elasticity_seed3)) 
  mean_boot_rep_elasticity_surv1_1st_winter<-mean(na.omit(booted_outputs$boot_elasticity_surv1_w1_to_surv2_s1))
  mean_boot_rep_elasticity_surv1_1st_season<-mean(na.omit(booted_outputs$boot_elasticity_surv2_s1_to_surv3_w2))
  mean_boot_rep_elasticity_surv1_2nd_winter<-mean(na.omit(booted_outputs$boot_elasticity_surv3_w2_to_surv4_s2))
  mean_boot_rep_elasticity_surv1_2nd_season<-mean(na.omit(booted_outputs$boot_elasticity_surv4_s2_to_surv5_w3))
  mean_boot_rep_elasticity_surv1_3rd_winter<-mean(na.omit(booted_outputs$boot_elasticity_surv5_w3_to_surv6_s3))
  mean_boot_rep_elasticity_surv1_3rd_season<-mean(na.omit(booted_outputs$boot_elasticity_surv6_s3_to_surv7_w4))
  
  
  mean_boot_rep_elasticity_seed1<-as.data.frame(mean_boot_rep_elasticity_seed1)
  mean_boot_rep_elasticity_seed1$id <-"elasticity_seed1";colnames(mean_boot_rep_elasticity_seed1)[1]<- "value"
  mean_boot_rep_elasticity_seed2<-as.data.frame(mean_boot_rep_elasticity_seed2)
  mean_boot_rep_elasticity_seed2$id <-"elasticity_seed2";colnames(mean_boot_rep_elasticity_seed2)[1]<- "value"
  mean_boot_rep_elasticity_seed3<-as.data.frame(mean_boot_rep_elasticity_seed3)
  mean_boot_rep_elasticity_seed3$id <-"elasticity_seed3";colnames(mean_boot_rep_elasticity_seed3)[1]<- "value"
  
  mean_boot_rep_elasticity_surv1_1st_winter<-as.data.frame(mean_boot_rep_elasticity_surv1_1st_winter)
  mean_boot_rep_elasticity_surv1_1st_winter$id <-"elasticity_surv1_w1_to_surv2_s1"
  colnames(mean_boot_rep_elasticity_surv1_1st_winter)[1]<- "value"
  mean_boot_rep_elasticity_surv1_1st_season<-as.data.frame(mean_boot_rep_elasticity_surv1_1st_season)
  mean_boot_rep_elasticity_surv1_1st_season$id <-"elasticity_surv2_s1_to_surv3_w2"
  colnames(mean_boot_rep_elasticity_surv1_1st_season)[1]<- "value"
  mean_boot_rep_elasticity_surv1_2nd_winter<-as.data.frame(mean_boot_rep_elasticity_surv1_2nd_winter)
  mean_boot_rep_elasticity_surv1_2nd_winter$id <-"elasticity_surv3_w2_to_surv4_s2"
  colnames(mean_boot_rep_elasticity_surv1_2nd_winter)[1]<- "value"
  mean_boot_rep_elasticity_surv1_2nd_season<-as.data.frame(mean_boot_rep_elasticity_surv1_2nd_season)
  mean_boot_rep_elasticity_surv1_2nd_season$id <-"elasticity_surv4_s2_to_surv5_w3"
  colnames(mean_boot_rep_elasticity_surv1_2nd_season)[1]<- "value"
  mean_boot_rep_elasticity_surv1_3rd_winter<-as.data.frame(mean_boot_rep_elasticity_surv1_3rd_winter)
  mean_boot_rep_elasticity_surv1_3rd_winter$id <-"elasticity_surv5_w3_to_surv6_s3"
  colnames(mean_boot_rep_elasticity_surv1_3rd_winter)[1]<- "value"
  mean_boot_rep_elasticity_surv1_3rd_season<-as.data.frame(mean_boot_rep_elasticity_surv1_3rd_season)
  mean_boot_rep_elasticity_surv1_3rd_season$id <-"elasticity_surv6_s3_to_surv7_w4"
  colnames(mean_boot_rep_elasticity_surv1_3rd_season)[1]<- "value"
  #
  
  elas_binded<-as.data.frame(rbind(mean_boot_rep_elasticity_seed1,mean_boot_rep_elasticity_seed2,
                                   mean_boot_rep_elasticity_seed3,
                                   mean_boot_rep_elasticity_surv1_1st_winter,
                                   mean_boot_rep_elasticity_surv1_1st_season,mean_boot_rep_elasticity_surv1_2nd_winter,
                                   mean_boot_rep_elasticity_surv1_2nd_season,mean_boot_rep_elasticity_surv1_3rd_winter,
                                   mean_boot_rep_elasticity_surv1_3rd_season))
  return(elas_binded)
}

data <- read.csv("dcarth_ms_dryad_data_1123.csv")
high_site <- subset(data, site_altitude=="high_site");low_site <- subset(data, site_altitude=="low_site")
highlow<- subset(low_site, altitude=="high");lowlow <- subset(low_site, altitude=="low")
highhigh <- subset(high_site, altitude=="high");lowhigh <- subset(high_site, altitude=="low")
gib_h <- subset(high_site, population=="Gibidumsee");gib_l <- subset(low_site, population=="Gibidumsee")
fal_h <- subset(high_site, population=="Faldumalp");fal_l <- subset(low_site, population=="Faldumalp")
sim_h <- subset(high_site, population=="Simplonpass");sim_l <- subset(low_site, population=="Simplonpass")
gre_h <- subset(high_site, population=="Grengiols");gre_l <- subset(low_site, population=="Grengiols")
unt_h <- subset(high_site, population=="Unterstalden");unt_l <- subset(low_site, population=="Unterstalden")
nie_h <- subset(high_site, population=="Niedergampel");nie_l <- subset(low_site, population=="Niedergampel")


#high populations growing in low sites
bootstrap_function_highlow <- function(data, indices) {
  establishment <- 0.0981 
  matrix_model_for_pp(data, indices, establishment)
}
results_A = boot(data = gib_l, statistic = bootstrap_function_highlow, R = 20000)
gib_l_pp_output <- create_booted_outputs_for_pp(results_A)

results_A = boot(data = fal_l, statistic = bootstrap_function_highlow, R = 20000)
fal_l_pp_output <- create_booted_outputs_for_pp(results_A)

results_A = boot(data = sim_l, statistic = bootstrap_function_highlow, R = 20000)
sim_l_pp_output <- create_booted_outputs_for_pp(results_A)


#low populations growing in low sites
bootstrap_function_lowlow <- function(data, indices) {
  establishment <- 0.1124 
  matrix_model_for_pp(data, indices, establishment)
}
results_A = boot(data = gre_l, statistic = bootstrap_function_lowlow, R = 20000)
gre_l_pp_output <- create_booted_outputs_for_pp(results_A)

results_A = boot(data = unt_l, statistic = bootstrap_function_lowlow, R = 20000)
unt_l_pp_output <- create_booted_outputs_for_pp(results_A)

results_A = boot(data = nie_l, statistic = bootstrap_function_lowlow, R = 20000)
nie_l_pp_output <- create_booted_outputs_for_pp(results_A)


#high populations growing in high sites
bootstrap_function_highhigh <- function(data, indices) {
  establishment <- 0.0511 
  matrix_model_for_pp(data, indices, establishment)
}
results_A = boot(data = gib_h, statistic = bootstrap_function_highhigh, R = 20000)
gib_h_pp_output <- create_booted_outputs_for_pp(results_A)

results_A = boot(data = sim_h, statistic = bootstrap_function_highhigh, R = 20000)
sim_h_pp_output <- create_booted_outputs_for_pp(results_A)

results_A = boot(data = fal_h, statistic = bootstrap_function_highhigh, R = 20000)
fal_h_pp_output <- create_booted_outputs_for_pp(results_A)

#low populations growing in high sites
bootstrap_function_lowhigh <- function(data, indices) {
  establishment <- 0.0329 
  matrix_model_for_pp(data, indices, establishment)
}
results_A = boot(data = gre_h, statistic = bootstrap_function_lowhigh, R = 20000)
gre_h_pp_output <- create_booted_outputs_for_pp(results_A)

results_A = boot(data = unt_h, statistic = bootstrap_function_lowhigh, R = 20000)
unt_h_pp_output <- create_booted_outputs_for_pp(results_A)

results_A = boot(data = nie_h, statistic = bootstrap_function_lowhigh, R = 20000)
nie_h_pp_output <- create_booted_outputs_for_pp(results_A)

head(nie_h_pp_output)
nie_h_pp_output$pop <- "nie";nie_h_pp_output$site <- "high_site"
nie_h_pp_output$altitude <- "low";nie_h_pp_output$stage <- NA
nie_h_pp_output$stage <- c("r1","r2", "r3","s1","s2","s3", "s4", "s5", "s6")
nie_h<-nie_h_pp_output

nie_l_pp_output$pop <- "nie";nie_l_pp_output$site <- "low_site"
nie_l_pp_output$altitude <- "low";nie_l_pp_output$stage <- NA
nie_l_pp_output$stage <- c("r1","r2", "r3","s1","s2","s3", "s4", "s5", "s6")
nie_l<-nie_l_pp_output

gre_h_pp_output$pop <- "gre";gre_h_pp_output$site <- "high_site"
gre_h_pp_output$altitude <- "low";gre_h_pp_output$stage <- NA
gre_h_pp_output$stage <- c("r1","r2", "r3","s1","s2","s3", "s4", "s5", "s6")
gre_h<-gre_h_pp_output

gre_l_pp_output$pop <- "gre";gre_l_pp_output$site <- "low_site"
gre_l_pp_output$altitude <- "low";gre_l_pp_output$stage <- NA
gre_l_pp_output$stage <- c("r1","r2", "r3","s1","s2","s3", "s4", "s5", "s6")
gre_l<-gre_l_pp_output

unt_h_pp_output$pop <- "unt";unt_h_pp_output$site <- "high_site"
unt_h_pp_output$altitude <- "low";unt_h_pp_output$stage <- NA
unt_h_pp_output$stage <- c("r1","r2", "r3","s1","s2","s3", "s4", "s5", "s6");unt_h<-unt_h_pp_output

unt_l_pp_output$pop <- "unt";unt_l_pp_output$site <- "low_site"
unt_l_pp_output$altitude <- "low";unt_l_pp_output$stage <- NA
unt_l_pp_output$stage <- c("r1","r2", "r3","s1","s2","s3", "s4", "s5", "s6");unt_l<-unt_l_pp_output

gib_h_pp_output$pop <- "gib";gib_h_pp_output$site <- "high_site"
gib_h_pp_output$altitude <- "high";gib_h_pp_output$stage <- NA
gib_h_pp_output$stage <- c("r1","r2", "r3","s1","s2","s3", "s4", "s5", "s6");gib_h<-gib_h_pp_output

gib_l_pp_output$pop <- "gib";gib_l_pp_output$site <- "low_site"
gib_l_pp_output$altitude <- "high";gib_l_pp_output$stage <- NA
gib_l_pp_output$stage <- c("r1","r2", "r3","s1","s2","s3", "s4", "s5", "s6");gib_l<-gib_l_pp_output


sim_h_pp_output$pop <- "sim";sim_h_pp_output$site <- "high_site"
sim_h_pp_output$altitude <- "high";sim_h_pp_output$stage <- NA
sim_h_pp_output$stage <- c("r1","r2", "r3","s1","s2","s3", "s4", "s5", "s6");sim_h<-sim_h_pp_output

sim_l_pp_output$pop <- "sim";sim_l_pp_output$site <- "low_site"
sim_l_pp_output$altitude <- "high";sim_l_pp_output$stage <- NA
sim_l_pp_output$stage <- c("r1","r2", "r3","s1","s2","s3", "s4", "s5", "s6");sim_l<-sim_l_pp_output

fal_h_pp_output$pop <- "fal";fal_h_pp_output$site <- "high_site"
fal_h_pp_output$altitude <- "high";fal_h_pp_output$stage <- NA
fal_h_pp_output$stage <- c("r1","r2", "r3","s1","s2","s3", "s4", "s5", "s6");fal_h<-fal_h_pp_output

fal_l_pp_output$pop <- "fal";fal_l_pp_output$site <- "low_site"
fal_l_pp_output$altitude <- "high";fal_l_pp_output$stage <- NA
fal_l_pp_output$stage <- c("r1","r2", "r3","s1","s2","s3", "s4", "s5", "s6");fal_l<-fal_l_pp_output

data <- rbind(gib_h,gib_l,fal_h,fal_l,sim_h,sim_l,gre_h,gre_l,unt_h,unt_l,nie_h,nie_l)
high_sites <- subset(data, site=="high_site");low_sites <- subset(data, site=="low_site")
high <- subset(data, altitude=="high");low <- subset(data, altitude=="low")
highhigh <- subset(high_sites, altitude=="high");lowhigh <- subset(high_sites, altitude=="low")
highlow <- subset(low_sites, altitude=="high");lowlow <- subset(low_sites, altitude=="low")

#trait shift (difference between trait at home and trait at foreign site)
highhigh$shift <- highhigh$value - highlow$value  
lowlow$shift <- lowlow$value - lowhigh$value      
#trait distance (difference between trait at home and trait of local genotype at foreign site)
highhigh$dist <- highhigh$value - lowlow$value 
lowlow$dist  <- lowlow$value - highhigh$value 
cor.test(highhigh$shift, highhigh$dist)
cor.test(lowlow$shift, lowlow$dist)

data <- as.data.frame(lowlow)
data$pop <- as.factor(data$pop)
data$site <- factor(data$site, levels = c("low_site", "high_site"))
label = rownames(data$stage)
plotten<-ggplot(data,aes(x=dist, y=shift, fill = altitude)) + 
  theme_bw() +
  geom_point(shape=c(1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,0,0,0,0,0,0,0,0,0) ,size=4,colour= c("red")) + geom_smooth(aes(group = altitude), method = "lm", colour="red")
guides(colour = guide_legend(override.aes = list(size=5, stroke=1.5))) 
low_plast<-plotten + theme_bw() + theme(text = element_text(size=14, face="bold"),panel.border = element_blank(), panel.grid.major = element_blank(),
                                        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size =0.9),axis.ticks = element_line(colour = "black", size = 0.6))
low_plast<-low_plast+ theme(legend.position  ="none")

data <- as.data.frame(highhigh)
data$pop <- as.factor(data$pop)
str(data)
data$site <- factor(data$site, levels = c("low_site", "high_site"))
label = rownames(data$stage)
plotten<-ggplot(data,aes(x=dist, y=shift, fill = pop)) + 
  theme_bw() +
  geom_point(shape=c(1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,0,0,0,0,0,0,0,0,0) ,size=4,colour= c("blue")) + geom_smooth(aes(group = altitude), method = "lm", colour="blue")
guides(colour = guide_legend(override.aes = list(size=5, stroke=1.5))) 

high_plast<-plotten + theme_bw() + theme(text = element_text(size=14, face="bold"),panel.border = element_blank(), panel.grid.major = element_blank(),
                                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size =0.9),axis.ticks = element_line(colour = "black", size = 0.6))
high_plast<-high_plast+ theme(legend.position  ="none")

#####
egg::ggarrange(low_plast,high_plast, 
               ncol=2, nrow=1, widths=c(1,1), heights=c( 1))