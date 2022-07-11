
#reading packages
library(tidyverse)
library(fpp2)
library(readxl)
library(MHMM)
library(mHMMbayes)
library(alluvial)
library(RColorBrewer)

#setting directory and loading data
setwd("C:/Users/Lenovo PC/OneDrive/Desktop/Bayes paper/Kaggle Data")
ccfraud <-read_excel("r_computations.xlsx",sheet=1,col_names = TRUE)

# specifying general model properties
m <- 3
n_dep <- 1
q_emiss <- c(2)

#specifying starting values
#first assuming equal transition probabilities
Cstart_TM <- diag(1/3, m)
Cstart_TM[lower.tri(Cstart_TM) | upper.tri(Cstart_TM)] <- (1/3)
Cstart_EM <- list(matrix(c(0.999277544, 0.000722456,
                           0.998094028, 0.001905972,
                           0.892561983, 0.107438017), byrow = TRUE, nrow = m, ncol = q_emiss))

#running model
#following standard J=1000 and burn_in=200
out_HMM <- mHMM(s_data = ccfraud, 
                gen = list(m = m, n_dep = n_dep, q_emiss = q_emiss), 
                start_val = c(list(Cstart_TM), Cstart_EM),
                mcmc = list(J = 1000, burn_in = 200))
out_HMM
summary(out_HMM)

#obtaining the transition probabilities at the group and subject level
est_gamma_group <- obtain_gamma(out_HMM, level = "group")
est_gamma_group

#specifying starting values using probabilities

#getting transition prob
data = ccfraud$fraud
tm = matrix(c(rep(0,9)),3,3);tm
tpm = matrix(c(rep(0,9)),3,3);tpm
for(i in 1:length(data)-1)
tm[data[i],data[i+1]]=(tm[data[i],data[i+1]]+1)
tm
#manually get probabilities 

Cnewstart_TM <- matrix(c(0, 1, 0,
                         0.00205071665, 0.9977949283, 0,
                         0, 0, 0), byrow= TRUE, nrow = m, ncol = m)
Cstart_EM <- list(matrix(c(0.999277544, 0.000722456,
                           0.998094028, 0.001905972,
                           0.892561983, 0.107438017), byrow = TRUE, nrow = m, ncol = q_emiss))

#running adjusted model
#following standard J=1000 and burn_in=200
out_HMM_new <- mHMM(s_data = ccfraud, 
                gen = list(m = m, n_dep = n_dep, q_emiss = q_emiss), 
                start_val = c(list(Cnewstart_TM), Cstart_EM),
                mcmc = list(J = 1000, burn_in = 200))
out_HMM_new
summary(out_HMM_new)

#graphing posterior densities for fitted multilevel HMM
#non informative
ccF_col <- c(brewer.pal(3,"PuBuGn")[c(1,2)])
ccF_lab <- c("Fraud", "Not Fraud")
lab_ccf <- c("Low", "Medium", "High")

plot(out_HMM, component = "emiss", col = ccF_col)
plot(out_HMM, component = "emiss", dep = 1, col = ccF_col, 
     parameter = "emiss", dep_lab = c("Fraud or Not"), cat_lab = ccF_lab)

plot(out_HMM, component = "gamma", dep = 1, col = ccF_col, 
     parameter = "gamma", dep_lab = c("Fraud or Not"), cat_lab = lab_ccf)

#informative
plot(out_HMM_new, component = "emiss", col = ccF_col, 
     parameter = "emiss", dep_lab = c("Fraud or Not"), cat_lab = ccF_lab)

plot(out_HMM_new, component = "gamma", dep = 1, col = ccF_col, 
     parameter = "gamma", dep_lab = c("Fraud or Not"), cat_lab = lab_ccf)

#plotting transition probabilities
#non informative
#obtaining the transition probabilities at the group and subject level
est_g_group <- obtain_gamma(out_HMM, level = "group")
est_g_group

#plot obtained transition probabilities
plot(est_g_group, col = rep(c("green", "blue", "black"), each = m))

#informative
#obtaining the transition probabilities at the group and subject level
est_gam_group <- obtain_gamma(out_HMM_new, level = "group")
est_gam_group

#plot obtained transition probabilities
plot(est_gam_group, col = rep(c("green", "blue", "black"), each = m))



