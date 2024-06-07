set.seed(89)
library(boot)
library(popbio)

#setwd("/Users/apalsson/Desktop/dcarth_ms/")
#define functions
matrix_model <- function(data, indices, establishment) {
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
  lambda_1 <- A_eigen$lambda1[1];elasticity_seed1 <- A_eigen$elasticities[1, 2]
  elasticity_seed2 <- A_eigen$elasticities[1, 4];elasticity_seed3 <- A_eigen$elasticities[1, 6]
  surv1_w1_to_surv2_s1 <- A_eigen$elasticities[2, 1];surv2_s1_to_surv3_w2 <- A_eigen$elasticities[3, 2]
  surv3_w2_to_surv4_s2 <- A_eigen$elasticities[4, 3];surv4_s2_to_surv5_w3 <- A_eigen$elasticities[5, 4]
  surv5_w3_to_surv6_s3 <- A_eigen$elasticities[6, 5];surv6_s3_to_surv7_w4 <- A_eigen$elasticities[7, 6]
  stable1 <- A_eigen$stable.stage[1];stable2 <- A_eigen$stable.stage[2];stable3 <- A_eigen$stable.stage[3]
  stable4 <- A_eigen$stable.stage[4];stable5 <- A_eigen$stable.stage[5];stable6 <- A_eigen$stable.stage[6]
  stable7 <- A_eigen$stable.stage[7]
  binded <- rbind(lambda_1, elasticity_seed1, elasticity_seed2, elasticity_seed3, surv1_w1_to_surv2_s1,
                  surv2_s1_to_surv3_w2, surv3_w2_to_surv4_s2, surv4_s2_to_surv5_w3, surv5_w3_to_surv6_s3,
                  surv6_s3_to_surv7_w4, stable1, stable2, stable3, stable4, stable5, stable6, stable7)
  return(binded)
}

create_booted_outputs <- function(results_A) {
  boot_lambdas <- as.data.frame(results_A$t[,1]);colnames(boot_lambdas)[1] <- "boot_rep_lambda"
  boot_elasticity_seed1 <- as.data.frame(results_A$t[,2]);colnames(boot_elasticity_seed1)[1] <- "boot_rep_elasticity_seed1"
  boot_elasticity_seed2 <- as.data.frame(results_A$t[,3]);colnames(boot_elasticity_seed2)[1] <- "boot_rep_elasticity_seed2"
  boot_elasticity_seed3 <- as.data.frame(results_A$t[,4]);colnames(boot_elasticity_seed3)[1] <- "boot_rep_elasticity_seed3"
  boot_elasticity_surv1_w1_to_surv2_s1 <- as.data.frame(results_A$t[,5]);colnames(boot_elasticity_surv1_w1_to_surv2_s1)[1] <- "boot_elasticity_surv1_w1_to_surv2_s1"
  boot_elasticity_surv2_s1_to_surv3_w2 <- as.data.frame(results_A$t[,6]);colnames(boot_elasticity_surv2_s1_to_surv3_w2)[1] <- "boot_elasticity_surv2_s1_to_surv3_w2"
  boot_elasticity_surv3_w2_to_surv4_s2 <- as.data.frame(results_A$t[,7]);colnames(boot_elasticity_surv3_w2_to_surv4_s2)[1] <- "boot_elasticity_surv3_w2_to_surv4_s2"
  boot_elasticity_surv4_s2_to_surv5_w3 <- as.data.frame(results_A$t[,8]);colnames(boot_elasticity_surv4_s2_to_surv5_w3)[1] <- "boot_elasticity_surv4_s2_to_surv5_w3"
  boot_elasticity_surv5_w3_to_surv6_s3 <- as.data.frame(results_A$t[,9]);colnames(boot_elasticity_surv5_w3_to_surv6_s3)[1] <- "boot_elasticity_surv5_w3_to_surv6_s3"
  boot_elasticity_surv6_s3_to_surv7_w4 <- as.data.frame(results_A$t[,10]);colnames(boot_elasticity_surv6_s3_to_surv7_w4)[1] <- "boot_elasticity_surv6_s3_to_surv7_w4"
  boot_stable1 <- as.data.frame(results_A$t[,11]);colnames(boot_stable1)[1] <- "boot_stable1"
  boot_stable2 <- as.data.frame(results_A$t[,12]);colnames(boot_stable2)[1] <- "boot_stable2"
  boot_stable3 <- as.data.frame(results_A$t[,13]);colnames(boot_stable3)[1] <- "boot_stable3"
  boot_stable4 <- as.data.frame(results_A$t[,14]);colnames(boot_stable4)[1] <- "boot_stable4"
  boot_stable5 <- as.data.frame(results_A$t[,15]);colnames(boot_stable5)[1] <- "boot_stable5"
  boot_stable6 <- as.data.frame(results_A$t[,16]); colnames(boot_stable6)[1] <- "boot_stable6"
  boot_stable7 <- as.data.frame(results_A$t[,17]);colnames(boot_stable7)[1] <- "boot_stable7"
  
  #combine dfs 
  booted_outputs_highlow <- cbind(
    boot_lambdas, boot_elasticity_seed1, boot_elasticity_seed2, boot_elasticity_seed3, boot_elasticity_surv1_w1_to_surv2_s1,
    boot_elasticity_surv2_s1_to_surv3_w2, boot_elasticity_surv3_w2_to_surv4_s2, boot_elasticity_surv4_s2_to_surv5_w3,
    boot_elasticity_surv5_w3_to_surv6_s3, boot_elasticity_surv6_s3_to_surv7_w4, boot_stable1, boot_stable2, boot_stable3,
    boot_stable4, boot_stable5, boot_stable6, boot_stable7
  )
  
  return(booted_outputs_highlow)
}


create_bca_cis <- function(results_A) {
  #create bca confidence intervals
  get_bca_ci <- function(results, index, id) {
    ci <- boot.ci(results, index = index, conf = 0.95, type = c("bca"))
    bcaci <- as.vector(ci$bca)[c(4, 5)]
    df <- as.data.frame(t(bcaci))
    df$id <- id
    colnames(df)[1] <- "lower"
    colnames(df)[2] <- "upper"
    return(df)
  }
  
  #calculate bca confidence intervals for each index
  lambda_bcaci <- get_bca_ci(results_A, 1, "lambda")
  elasticity_seed1_bcaci <- get_bca_ci(results_A, 2, "elasticity_seed1")
  elasticity_seed2_bcaci <- get_bca_ci(results_A, 3, "elasticity_seed2")
  elasticity_seed3_bcaci <- get_bca_ci(results_A, 4, "elasticity_seed3")
  elasticity_surv1_w1_to_surv2_s1_bcaci <- get_bca_ci(results_A, 5, "elasticity_surv1_w1_to_surv2_s1")
  elasticity_surv2_s1_to_surv3_w2_bcaci <- get_bca_ci(results_A, 6, "elasticity_surv2_s1_to_surv3_w2")
  elasticity_surv3_w2_to_surv4_s2_bcaci <- get_bca_ci(results_A, 7, "elasticity_surv3_w2_to_surv4_s2")
  elasticity_surv4_s2_to_surv5_w3_bcaci <- get_bca_ci(results_A, 8, "elasticity_surv4_s2_to_surv5_w3")
  elasticity_surv5_w3_to_surv6_s3_bcaci <- get_bca_ci(results_A, 9, "elasticity_surv5_w3_to_surv6_s3")
  
  #elasticity_surv6_s3_to_surv7_w4_bcaci has fixed values
  elasticity_surv6_s3_to_surv7_w4_bcaci <- data.frame(
    lower = 0,
    upper = 0,
    id = "elasticity_surv6_s3_to_surv7_w4"
  )
  
  elasticity_stable1_bcaci <- get_bca_ci(results_A, 11, "elasticity_stable1")
  elasticity_stable2_bcaci <- get_bca_ci(results_A, 12, "elasticity_stable2")
  elasticity_stable3_bcaci <- get_bca_ci(results_A, 13, "elasticity_stable3")
  elasticity_stable4_bcaci <- get_bca_ci(results_A, 14, "elasticity_stable4")
  elasticity_stable5_bcaci <- get_bca_ci(results_A, 15, "elasticity_stable5")
  elasticity_stable6_bcaci <- get_bca_ci(results_A, 16, "elasticity_stable6")
  elasticity_stable7_bcaci <- get_bca_ci(results_A, 17, "elasticity_stable7")
  
  #combine dfs
  bca_cis_all_highlow <- rbind(
    lambda_bcaci, elasticity_seed1_bcaci, elasticity_seed2_bcaci, elasticity_seed3_bcaci,
    elasticity_surv1_w1_to_surv2_s1_bcaci, elasticity_surv2_s1_to_surv3_w2_bcaci,
    elasticity_surv3_w2_to_surv4_s2_bcaci, elasticity_surv4_s2_to_surv5_w3_bcaci,
    elasticity_surv5_w3_to_surv6_s3_bcaci, elasticity_surv6_s3_to_surv7_w4_bcaci,
    elasticity_stable1_bcaci, elasticity_stable2_bcaci, elasticity_stable3_bcaci,
    elasticity_stable4_bcaci, elasticity_stable5_bcaci, elasticity_stable6_bcaci,
    elasticity_stable7_bcaci
  )
  
  return(bca_cis_all_highlow)
}


##load data and run functions 
data <- read.csv("dcarth_ms_dryad_data_1123.csv")
high_site <- subset(data, site_altitude=="high_site")
low_site <- subset(data, site_altitude=="low_site")
highlow<- subset(low_site, altitude=="high")
lowlow <- subset(low_site, altitude=="low")
highhigh <- subset(high_site, altitude=="high")
lowhigh <- subset(high_site, altitude=="low")

lowhigh$population<-as.factor(lowhigh$population)
highhigh$population<-as.factor(highhigh$population)
lowlow$population<-as.factor(lowlow$population)
highlow$population<-as.factor(highlow$population)

#high populations growing in low sites
bootstrap_function_highlow <- function(data, indices) {
  establishment <- 0.0981 
  matrix_model(data, indices, establishment)
}
results_A = boot(data = highlow, statistic = bootstrap_function_highlow, R = 20000, strata = highlow$population)


bca_cis_all_highlow <- create_bca_cis(results_A)
write.csv(bca_cis_all_highlow, file="carth_bca_cis_all_highlow.csv")
booted_outputs_highlow <- create_booted_outputs(results_A)
write.csv(booted_outputs_highlow, file="carth_booted_outputs_highlow.csv")

#low populations growing in low sites
bootstrap_function_lowlow <- function(data, indices) {
  establishment <- 0.1124
  matrix_model(data, indices, establishment)
}
results_A = boot(data = lowlow, statistic = bootstrap_function_lowlow, R = 20000, strata = lowlow$population)

bca_cis_all_highlow <- create_bca_cis(results_A)
write.csv(bca_cis_all_highlow, file="carth_bca_cis_all_lowlow.csv")
booted_outputs_highlow <- create_booted_outputs(results_A)
write.csv(booted_outputs_highlow, file="carth_booted_outputs_lowlow.csv")


#high populations growing in high sites
bootstrap_function_highhigh <- function(data, indices) {
  establishment <- 0.0511
  matrix_model(data, indices, establishment)
}
results_A = boot(data = highhigh, statistic = bootstrap_function_highhigh, R = 20000, strata = highhigh$population)

bca_cis_all_highhigh <- create_bca_cis(results_A)
write.csv(bca_cis_all_highhigh, file="carth_bca_cis_all_highhigh.csv")
booted_outputs_highhigh <- create_booted_outputs(results_A)
write.csv(booted_outputs_highhigh, file="carth_booted_outputs_highhigh.csv")


#low populations growing in high sites
bootstrap_function_lowhigh <- function(data, indices) {
  establishment <- 0.0329
  matrix_model(data, indices, establishment)
}
results_A = boot(data = lowhigh, statistic = bootstrap_function_lowhigh, R = 20000, strata = lowhigh$population)

bca_cis_all_lowhigh <- create_bca_cis(results_A)
write.csv(bca_cis_all_lowhigh, file="carth_bca_cis_all_lowhigh.csv")
booted_outputs_lowhigh <- create_booted_outputs(results_A)
write.csv(booted_outputs_lowhigh, file="carth_booted_outputs_lowhigh.csv")