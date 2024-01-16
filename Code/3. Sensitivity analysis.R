####################################################################################################
## project: = Health effects of a minimum wage hike: Evidence from South Korea experiments
## author(s): Jung Hyun Kim
## code started: February, 2023
## last update: January, 2024
####################################################################################################

####################################################################################################
## clear work space
####################################################################################################
rm(list=ls())
## set seed
set.seed(41489)
####################################################################################################
## load packages 
####################################################################################################
x <- c("haven", "plm", "tidyverse", "ggplot2", "ggpubr", "dplyr", "expss", "foreign", "nnet", "reshape2","plyr","zoo",
       "dplyr", "gtools","devtools", "xtable",  "lmtest","tidyr", "readr", "lmtest", "lfe", "srvyr", "gtsummary", "WeightIt")

install.packages(x) 
lapply(x, library, character.only = TRUE)

#Please set your own directory.
setwd("~/Documents/CognitiveDecline/KLoSA_STATA_2022v3")
#download the data.
rm(list=ls())

####################################################################################################
## Sensitivity analysis 1. Alternative definition of control group. 
####################################################################################################

####################################################################################################
## 110%
####################################################################################################

library(haven)
KLoSA_main <- read_dta("KLoSA_main.dta")
KLoSA_main$wave <- as.numeric(KLoSA_main$wave)
KLoSA_main <- KLoSA_main[!is.na(KLoSA_main$wwgt_c),]

KLoSA_detail <- read_dta("KLoSA_detail.dta")
KLoSA_detail$wave <- as.numeric(KLoSA_detail$wave)

KLoSA_main <- left_join(KLoSA_main, KLoSA_detail,by = c("pid", "wave"))

# We consider two waves, 6 and 7.
KLoSA_main <- KLoSA_main[KLoSA_main$wave == 6 |KLoSA_main$wave == 7,]
KLoSA_detail <- NULL

# Remove individuals who weren't working at the baseline.
notworking_wave6 <- KLoSA_main[!is.na(KLoSA_main$wd_com032) & KLoSA_main$wave == 6,] 
notworking_wave6 <- subset(notworking_wave6, select = c("pid"))
KLoSA_main  <- left_join(notworking_wave6, KLoSA_main) 

notworking_wave6 <- NULL

# We select individuals worked as a paid employee at baseline.
employee <- KLoSA_main[KLoSA_main$wemp == 1 & KLoSA_main$wave == 6,] #1. employee #2. self-employed #3. unpaid family-related labor 
employee  <- subset(employee, select = c("pid"))
KLoSA_main  <- left_join(employee, KLoSA_main) 

employee <- NULL

# We select individuals with main health outcome.
KLoSA_main <- KLoSA_main[!is.na(KLoSA_main$wmmse),]


#We calculate hourly wage from monthly wage and weekly working hours. 

KLoSA_main$ww <- KLoSA_main$wd_com052/(52/12) #weekly wage
KLoSA_main$hw <- KLoSA_main$ww/KLoSA_main$wd_com032 #hourly wage
KLoSA_main[!is.na(KLoSA_main$hw) & KLoSA_main$hw < 0,] <- NA #those without hourly wage information and negative hourly wage are considered as missing.

minimum_wage = 0.753 # minimum wage at year 2018

# Exclude individuals whose minimum wage at baseline was bigger than 150% of it.

attrition <- KLoSA_main[KLoSA_main$wave == 6 & KLoSA_main$hw > 0,]
attrition <- attrition[attrition$hw <= minimum_wage * 1.5 & attrition$wave == 6 ,]

attrition <- subset(attrition, select = c("pid"))
attrition <- semi_join(KLoSA_main, attrition) 

# Exclude unemployed individuals after the minimum wage increase.
# In the sensitivity analysis, we relax this condition. 

attrition <- attrition[!is.na(attrition$hw),]
KLoSA_main <- attrition # without unemployed individuals 

# Include individuals with observations available in both waves.

KLoSA_main <- 
  KLoSA_main %>% 
  dplyr::mutate(wave = as.integer(wave)) %>% 
  group_by(pid) %>% 
  dplyr::mutate(grp = cumsum(c(1, diff(wave) != 1))) %>% 
  filter(n() >= 2)# without unemployed individuals 

# Intervention group assignment
# 1) Hourly wage at baseline is smaller than the minimum wage.
intervention_group <- KLoSA_main[KLoSA_main$hw < minimum_wage  & KLoSA_main$wave == 6 ,]
intervention_group <- subset(intervention_group, select = c("pid"))
intervention_group<- semi_join(KLoSA_main, intervention_group)

intervention_group$exposure <- 1
intervention_group$group <- "intervention"

# Control group assignment.
# The hourly wage should fall between 100 - 150% of the baseline minimum hourly wage. 

control_group <- KLoSA_main[KLoSA_main$hw >= minimum_wage * 1.0 & KLoSA_main$hw <= minimum_wage*1.1 & KLoSA_main$wave == 6,] 
control_group <- subset(control_group, select = c("pid"))
control_group <- semi_join(KLoSA_main, control_group)
control_group$exposure <- 0
control_group$group <- "control"

# We merge intervention and control group.
KLoSA_main <- rbind(intervention_group, control_group)
# Time dummy for the minimum wage increase.
KLoSA_main$time = ifelse(KLoSA_main$wave == 7, 1, 0)
# time * exposure is what we call as the event
KLoSA_main$did <- KLoSA_main$time * KLoSA_main$exposure

KLoSA_main$female <- ifelse(KLoSA_main$wgender1 == 5 , 1,0)

KLoSA_main$spouse <- ifelse(KLoSA_main$wmarital == 1, 1, 0)

KLoSA_main$age_category <- case_when(KLoSA_main$wA002_age < 60 ~ 1,
                                     KLoSA_main$wA002_age >= 60 & KLoSA_main$wA002_age <65 ~ 2,
                                     KLoSA_main$wA002_age >= 65 & KLoSA_main$wA002_age <70 ~ 3,
                                     KLoSA_main$wA002_age >= 70 & KLoSA_main$wA002_age <75 ~ 4, 
                                     KLoSA_main$wA002_age >= 75 ~ 5)

KLoSA_main$health <- case_when(KLoSA_main$wC001 == 1 ~ 5,#Best
                               KLoSA_main$wC001 == 2 ~ 4,#Very good
                               KLoSA_main$wC001 == 3 ~ 3,#Good
                               KLoSA_main$wC001 == 4 ~ 2,#Normal
                               KLoSA_main$wC001 == 5 ~ 1)#Bad


KLoSA_main$workingdays <- KLoSA_main$wd_com031

# Pension situation
#E033: national pension, E070m5 social security pension, E044: special pension, E055: personal pension 
KLoSA_main$national_pension <- ifelse(KLoSA_main$wE033 == 4, 0, 1)
KLoSA_main$specific_corporate_pension <- ifelse(KLoSA_main$wE044 == 4, 0, 1)
KLoSA_main$social_security_pension <-KLoSA_main$wE070m5
#KLoSA_main$private_pension <- ifelse(KLoSA_main$wE055 == 4, 0, 1)
KLoSA_main$public_transfer <- KLoSA_main$national_pension + KLoSA_main$specific_corporate_pension + KLoSA_main$social_security_pension
KLoSA_main$public_trasfer_dummy <- ifelse(KLoSA_main$public_transfer != 0, 1, 0)

# Only the pre-exposure period
pre_exposure_data <- KLoSA_main[KLoSA_main$wave == 6, ]

# Estimate propensity scores using CBPS from WeightIt package.
weight_model <- weightit(exposure ~ wA002_age + wgender1 + health + wedu
                         + wd_com032 +  wd_com031, data = pre_exposure_data, method = "cbps",
                         estimand = "ATE")
# Now include the obtained weights.
pre_exposure_data$weights <- weight_model$weights

# Combine the pre and post exposure data.
KLoSA_main$weights <- NA
KLoSA_main <- rbind(pre_exposure_data, KLoSA_main[KLoSA_main$wave == 7,])
# Apply the same weights to the same ID
KLoSA_main <- KLoSA_main %>%
  group_by(pid) %>%
  mutate(weights = first(weights))

#Weighted diff-in-diff

weighted <- plm(wmmse ~ did + factor(age_category) + public_trasfer_dummy + spouse, 
                data = KLoSA_main,
                index = c("pid", "wave"), 
                model = "within", 
                weights = weights,
                effect = "twoways")

summary(weighted)


####################################################################################################
## 120%
####################################################################################################


rm(list=ls())

library(haven)
KLoSA_main <- read_dta("KLoSA_main.dta")
KLoSA_main$wave <- as.numeric(KLoSA_main$wave)
KLoSA_main <- KLoSA_main[!is.na(KLoSA_main$wwgt_c),]

KLoSA_detail <- read_dta("KLoSA_detail.dta")
KLoSA_detail$wave <- as.numeric(KLoSA_detail$wave)

KLoSA_main <- left_join(KLoSA_main, KLoSA_detail,by = c("pid", "wave"))

# We consider two waves, 6 and 7.
KLoSA_main <- KLoSA_main[KLoSA_main$wave == 6 |KLoSA_main$wave == 7,]
KLoSA_detail <- NULL

# Remove individuals who weren't working at the baseline.
notworking_wave6 <- KLoSA_main[!is.na(KLoSA_main$wd_com032) & KLoSA_main$wave == 6,] 
notworking_wave6 <- subset(notworking_wave6, select = c("pid"))
KLoSA_main  <- left_join(notworking_wave6, KLoSA_main) 

notworking_wave6 <- NULL

# We select individuals worked as a paid employee at baseline.
employee <- KLoSA_main[KLoSA_main$wemp == 1 & KLoSA_main$wave == 6,] #1. employee #2. self-employed #3. unpaid family-related labor 
employee  <- subset(employee, select = c("pid"))
KLoSA_main  <- left_join(employee, KLoSA_main) 

employee <- NULL

# We select individuals with main health outcome.
KLoSA_main <- KLoSA_main[!is.na(KLoSA_main$wmmse),]


#We calculate hourly wage from monthly wage and weekly working hours. 

KLoSA_main$ww <- KLoSA_main$wd_com052/(52/12) #weekly wage
KLoSA_main$hw <- KLoSA_main$ww/KLoSA_main$wd_com032 #hourly wage
KLoSA_main[!is.na(KLoSA_main$hw) & KLoSA_main$hw < 0,] <- NA #those without hourly wage information and negative hourly wage are considered as missing.

minimum_wage = 0.753 # minimum wage at year 2018

# Exclude individuals whose minimum wage at baseline was bigger than 150% of it.

attrition <- KLoSA_main[KLoSA_main$wave == 6 & KLoSA_main$hw > 0,]
attrition <- attrition[attrition$hw <= minimum_wage * 1.5 & attrition$wave == 6 ,]

attrition <- subset(attrition, select = c("pid"))
attrition <- semi_join(KLoSA_main, attrition) 

# Exclude unemployed individuals after the minimum wage increase.
# In the sensitivity analysis, we relax this condition. 

attrition <- attrition[!is.na(attrition$hw),]
KLoSA_main <- attrition # without unemployed individuals 

# Include individuals with observations available in both waves.

KLoSA_main <- 
  KLoSA_main %>% 
  dplyr::mutate(wave = as.integer(wave)) %>% 
  group_by(pid) %>% 
  dplyr::mutate(grp = cumsum(c(1, diff(wave) != 1))) %>% 
  filter(n() >= 2)# without unemployed individuals 

# Intervention group assignment
# 1) Hourly wage at baseline is smaller than the minimum wage.
intervention_group <- KLoSA_main[KLoSA_main$hw < minimum_wage  & KLoSA_main$wave == 6 ,]
intervention_group <- subset(intervention_group, select = c("pid"))
intervention_group<- semi_join(KLoSA_main, intervention_group)

intervention_group$exposure <- 1
intervention_group$group <- "intervention"

# Control group assignment.
# The hourly wage should fall between 100 - 150% of the baseline minimum hourly wage. 

control_group <- KLoSA_main[KLoSA_main$hw >= minimum_wage * 1.0 & KLoSA_main$hw <= minimum_wage*1.2 & KLoSA_main$wave == 6,] 
control_group <- subset(control_group, select = c("pid"))
control_group <- semi_join(KLoSA_main, control_group)
control_group$exposure <- 0
control_group$group <- "control"

# We merge intervention and control group.
KLoSA_main <- rbind(intervention_group, control_group)
# Time dummy for the minimum wage increase.
KLoSA_main$time = ifelse(KLoSA_main$wave == 7, 1, 0)
# time * exposure is what we call as the event
KLoSA_main$did <- KLoSA_main$time * KLoSA_main$exposure

KLoSA_main$female <- ifelse(KLoSA_main$wgender1 == 5 , 1,0)

KLoSA_main$spouse <- ifelse(KLoSA_main$wmarital == 1, 1, 0)

KLoSA_main$age_category <- case_when(KLoSA_main$wA002_age < 60 ~ 1,
                                     KLoSA_main$wA002_age >= 60 & KLoSA_main$wA002_age <65 ~ 2,
                                     KLoSA_main$wA002_age >= 65 & KLoSA_main$wA002_age <70 ~ 3,
                                     KLoSA_main$wA002_age >= 70 & KLoSA_main$wA002_age <75 ~ 4, 
                                     KLoSA_main$wA002_age >= 75 ~ 5)

KLoSA_main$health <- case_when(KLoSA_main$wC001 == 1 ~ 5,#Best
                               KLoSA_main$wC001 == 2 ~ 4,#Very good
                               KLoSA_main$wC001 == 3 ~ 3,#Good
                               KLoSA_main$wC001 == 4 ~ 2,#Normal
                               KLoSA_main$wC001 == 5 ~ 1)#Bad


KLoSA_main$workingdays <- KLoSA_main$wd_com031

# Pension situation
#E033: national pension, E070m5 social security pension, E044: special pension, E055: personal pension 
KLoSA_main$national_pension <- ifelse(KLoSA_main$wE033 == 4, 0, 1)
KLoSA_main$specific_corporate_pension <- ifelse(KLoSA_main$wE044 == 4, 0, 1)
KLoSA_main$social_security_pension <-KLoSA_main$wE070m5
#KLoSA_main$private_pension <- ifelse(KLoSA_main$wE055 == 4, 0, 1)
KLoSA_main$public_transfer <- KLoSA_main$national_pension + KLoSA_main$specific_corporate_pension + KLoSA_main$social_security_pension
KLoSA_main$public_trasfer_dummy <- ifelse(KLoSA_main$public_transfer != 0, 1, 0)

# Only the pre-exposure period
pre_exposure_data <- KLoSA_main[KLoSA_main$wave == 6, ]

# Estimate propensity scores using CBPS from WeightIt package.
weight_model <- weightit(exposure ~ wA002_age + wgender1 + health + wedu
                         + wd_com032 +  wd_com031, data = pre_exposure_data, method = "cbps",
                         estimand = "ATE")
# Now include the obtained weights.
pre_exposure_data$weights <- weight_model$weights

# Combine the pre and post exposure data.
KLoSA_main$weights <- NA
KLoSA_main <- rbind(pre_exposure_data, KLoSA_main[KLoSA_main$wave == 7,])
# Apply the same weights to the same ID
KLoSA_main <- KLoSA_main %>%
  group_by(pid) %>%
  mutate(weights = first(weights))

#Weighted diff-in-diff

weighted <- plm(wmmse ~ did + factor(age_category) + public_trasfer_dummy + spouse, 
                data = KLoSA_main,
                index = c("pid", "wave"), 
                model = "within", 
                weights = weights,
                effect = "twoways")

summary(weighted)


####################################################################################################
## 130%
####################################################################################################


rm(list=ls())

library(haven)
KLoSA_main <- read_dta("KLoSA_main.dta")
KLoSA_main$wave <- as.numeric(KLoSA_main$wave)
KLoSA_main <- KLoSA_main[!is.na(KLoSA_main$wwgt_c),]

KLoSA_detail <- read_dta("KLoSA_detail.dta")
KLoSA_detail$wave <- as.numeric(KLoSA_detail$wave)

KLoSA_main <- left_join(KLoSA_main, KLoSA_detail,by = c("pid", "wave"))

# We consider two waves, 6 and 7.
KLoSA_main <- KLoSA_main[KLoSA_main$wave == 6 |KLoSA_main$wave == 7,]
KLoSA_detail <- NULL

# Remove individuals who weren't working at the baseline.
notworking_wave6 <- KLoSA_main[!is.na(KLoSA_main$wd_com032) & KLoSA_main$wave == 6,] 
notworking_wave6 <- subset(notworking_wave6, select = c("pid"))
KLoSA_main  <- left_join(notworking_wave6, KLoSA_main) 

notworking_wave6 <- NULL

# We select individuals worked as a paid employee at baseline.
employee <- KLoSA_main[KLoSA_main$wemp == 1 & KLoSA_main$wave == 6,] #1. employee #2. self-employed #3. unpaid family-related labor 
employee  <- subset(employee, select = c("pid"))
KLoSA_main  <- left_join(employee, KLoSA_main) 

employee <- NULL

# We select individuals with main health outcome.
KLoSA_main <- KLoSA_main[!is.na(KLoSA_main$wmmse),]


#We calculate hourly wage from monthly wage and weekly working hours. 

KLoSA_main$ww <- KLoSA_main$wd_com052/(52/12) #weekly wage
KLoSA_main$hw <- KLoSA_main$ww/KLoSA_main$wd_com032 #hourly wage
KLoSA_main[!is.na(KLoSA_main$hw) & KLoSA_main$hw < 0,] <- NA #those without hourly wage information and negative hourly wage are considered as missing.

minimum_wage = 0.753 # minimum wage at year 2018

# Exclude individuals whose minimum wage at baseline was bigger than 150% of it.

attrition <- KLoSA_main[KLoSA_main$wave == 6 & KLoSA_main$hw > 0,]
attrition <- attrition[attrition$hw <= minimum_wage * 1.5 & attrition$wave == 6 ,]

attrition <- subset(attrition, select = c("pid"))
attrition <- semi_join(KLoSA_main, attrition) 

# Exclude unemployed individuals after the minimum wage increase.
# In the sensitivity analysis, we relax this condition. 

attrition <- attrition[!is.na(attrition$hw),]
KLoSA_main <- attrition # without unemployed individuals 

# Include individuals with observations available in both waves.

KLoSA_main <- 
  KLoSA_main %>% 
  dplyr::mutate(wave = as.integer(wave)) %>% 
  group_by(pid) %>% 
  dplyr::mutate(grp = cumsum(c(1, diff(wave) != 1))) %>% 
  filter(n() >= 2)# without unemployed individuals 

# Intervention group assignment
# 1) Hourly wage at baseline is smaller than the minimum wage.
intervention_group <- KLoSA_main[KLoSA_main$hw < minimum_wage  & KLoSA_main$wave == 6 ,]
intervention_group <- subset(intervention_group, select = c("pid"))
intervention_group<- semi_join(KLoSA_main, intervention_group)

intervention_group$exposure <- 1
intervention_group$group <- "intervention"

# Control group assignment.
# The hourly wage should fall between 100 - 150% of the baseline minimum hourly wage. 

control_group <- KLoSA_main[KLoSA_main$hw >= minimum_wage * 1.0 & KLoSA_main$hw <= minimum_wage*1.3 & KLoSA_main$wave == 6,] 
control_group <- subset(control_group, select = c("pid"))
control_group <- semi_join(KLoSA_main, control_group)
control_group$exposure <- 0
control_group$group <- "control"

# We merge intervention and control group.
KLoSA_main <- rbind(intervention_group, control_group)
# Time dummy for the minimum wage increase.
KLoSA_main$time = ifelse(KLoSA_main$wave == 7, 1, 0)
# time * exposure is what we call as the event
KLoSA_main$did <- KLoSA_main$time * KLoSA_main$exposure

KLoSA_main$female <- ifelse(KLoSA_main$wgender1 == 5 , 1,0)

KLoSA_main$spouse <- ifelse(KLoSA_main$wmarital == 1, 1, 0)

KLoSA_main$age_category <- case_when(KLoSA_main$wA002_age < 60 ~ 1,
                                     KLoSA_main$wA002_age >= 60 & KLoSA_main$wA002_age <65 ~ 2,
                                     KLoSA_main$wA002_age >= 65 & KLoSA_main$wA002_age <70 ~ 3,
                                     KLoSA_main$wA002_age >= 70 & KLoSA_main$wA002_age <75 ~ 4, 
                                     KLoSA_main$wA002_age >= 75 ~ 5)

KLoSA_main$health <- case_when(KLoSA_main$wC001 == 1 ~ 5,#Best
                               KLoSA_main$wC001 == 2 ~ 4,#Very good
                               KLoSA_main$wC001 == 3 ~ 3,#Good
                               KLoSA_main$wC001 == 4 ~ 2,#Normal
                               KLoSA_main$wC001 == 5 ~ 1)#Bad


KLoSA_main$workingdays <- KLoSA_main$wd_com031

# Pension situation
#E033: national pension, E070m5 social security pension, E044: special pension, E055: personal pension 
KLoSA_main$national_pension <- ifelse(KLoSA_main$wE033 == 4, 0, 1)
KLoSA_main$specific_corporate_pension <- ifelse(KLoSA_main$wE044 == 4, 0, 1)
KLoSA_main$social_security_pension <-KLoSA_main$wE070m5
#KLoSA_main$private_pension <- ifelse(KLoSA_main$wE055 == 4, 0, 1)
KLoSA_main$public_transfer <- KLoSA_main$national_pension + KLoSA_main$specific_corporate_pension + KLoSA_main$social_security_pension
KLoSA_main$public_trasfer_dummy <- ifelse(KLoSA_main$public_transfer != 0, 1, 0)

# Only the pre-exposure period
pre_exposure_data <- KLoSA_main[KLoSA_main$wave == 6, ]

# Estimate propensity scores using CBPS from WeightIt package.
weight_model <- weightit(exposure ~ wA002_age + wgender1 + health + wedu
                         + wd_com032 +  wd_com031, data = pre_exposure_data, method = "cbps",
                         estimand = "ATE")
# Now include the obtained weights.
pre_exposure_data$weights <- weight_model$weights

# Combine the pre and post exposure data.
KLoSA_main$weights <- NA
KLoSA_main <- rbind(pre_exposure_data, KLoSA_main[KLoSA_main$wave == 7,])
# Apply the same weights to the same ID
KLoSA_main <- KLoSA_main %>%
  group_by(pid) %>%
  mutate(weights = first(weights))

#Weighted diff-in-diff

weighted <- plm(wmmse ~ did + factor(age_category) + public_trasfer_dummy + spouse, 
                data = KLoSA_main,
                index = c("pid", "wave"), 
                model = "within", 
                weights = weights,
                effect = "twoways")

summary(weighted)


####################################################################################################
## 140%
####################################################################################################


rm(list=ls())

library(haven)
KLoSA_main <- read_dta("KLoSA_main.dta")
KLoSA_main$wave <- as.numeric(KLoSA_main$wave)
KLoSA_main <- KLoSA_main[!is.na(KLoSA_main$wwgt_c),]

KLoSA_detail <- read_dta("KLoSA_detail.dta")
KLoSA_detail$wave <- as.numeric(KLoSA_detail$wave)

KLoSA_main <- left_join(KLoSA_main, KLoSA_detail,by = c("pid", "wave"))

# We consider two waves, 6 and 7.
KLoSA_main <- KLoSA_main[KLoSA_main$wave == 6 |KLoSA_main$wave == 7,]
KLoSA_detail <- NULL

# Remove individuals who weren't working at the baseline.
notworking_wave6 <- KLoSA_main[!is.na(KLoSA_main$wd_com032) & KLoSA_main$wave == 6,] 
notworking_wave6 <- subset(notworking_wave6, select = c("pid"))
KLoSA_main  <- left_join(notworking_wave6, KLoSA_main) 

notworking_wave6 <- NULL

# We select individuals worked as a paid employee at baseline.
employee <- KLoSA_main[KLoSA_main$wemp == 1 & KLoSA_main$wave == 6,] #1. employee #2. self-employed #3. unpaid family-related labor 
employee  <- subset(employee, select = c("pid"))
KLoSA_main  <- left_join(employee, KLoSA_main) 

employee <- NULL

# We select individuals with main health outcome.
KLoSA_main <- KLoSA_main[!is.na(KLoSA_main$wmmse),]


#We calculate hourly wage from monthly wage and weekly working hours. 

KLoSA_main$ww <- KLoSA_main$wd_com052/(52/12) #weekly wage
KLoSA_main$hw <- KLoSA_main$ww/KLoSA_main$wd_com032 #hourly wage
KLoSA_main[!is.na(KLoSA_main$hw) & KLoSA_main$hw < 0,] <- NA #those without hourly wage information and negative hourly wage are considered as missing.

minimum_wage = 0.753 # minimum wage at year 2018

# Exclude individuals whose minimum wage at baseline was bigger than 150% of it.

attrition <- KLoSA_main[KLoSA_main$wave == 6 & KLoSA_main$hw > 0,]
attrition <- attrition[attrition$hw <= minimum_wage * 1.5 & attrition$wave == 6 ,]

attrition <- subset(attrition, select = c("pid"))
attrition <- semi_join(KLoSA_main, attrition) 

# Exclude unemployed individuals after the minimum wage increase.
# In the sensitivity analysis, we relax this condition. 

attrition <- attrition[!is.na(attrition$hw),]
KLoSA_main <- attrition # without unemployed individuals 

# Include individuals with observations available in both waves.

KLoSA_main <- 
  KLoSA_main %>% 
  dplyr::mutate(wave = as.integer(wave)) %>% 
  group_by(pid) %>% 
  dplyr::mutate(grp = cumsum(c(1, diff(wave) != 1))) %>% 
  filter(n() >= 2)# without unemployed individuals 

# Intervention group assignment
# 1) Hourly wage at baseline is smaller than the minimum wage.
intervention_group <- KLoSA_main[KLoSA_main$hw < minimum_wage  & KLoSA_main$wave == 6 ,]
intervention_group <- subset(intervention_group, select = c("pid"))
intervention_group<- semi_join(KLoSA_main, intervention_group)

intervention_group$exposure <- 1
intervention_group$group <- "intervention"

# Control group assignment.
# The hourly wage should fall between 100 - 150% of the baseline minimum hourly wage. 

control_group <- KLoSA_main[KLoSA_main$hw >= minimum_wage * 1.0 & KLoSA_main$hw <= minimum_wage*1.4 & KLoSA_main$wave == 6,] 
control_group <- subset(control_group, select = c("pid"))
control_group <- semi_join(KLoSA_main, control_group)
control_group$exposure <- 0
control_group$group <- "control"

# We merge intervention and control group.
KLoSA_main <- rbind(intervention_group, control_group)
# Time dummy for the minimum wage increase.
KLoSA_main$time = ifelse(KLoSA_main$wave == 7, 1, 0)
# time * exposure is what we call as the event
KLoSA_main$did <- KLoSA_main$time * KLoSA_main$exposure

KLoSA_main$female <- ifelse(KLoSA_main$wgender1 == 5 , 1,0)

KLoSA_main$spouse <- ifelse(KLoSA_main$wmarital == 1, 1, 0)

KLoSA_main$age_category <- case_when(KLoSA_main$wA002_age < 60 ~ 1,
                                     KLoSA_main$wA002_age >= 60 & KLoSA_main$wA002_age <65 ~ 2,
                                     KLoSA_main$wA002_age >= 65 & KLoSA_main$wA002_age <70 ~ 3,
                                     KLoSA_main$wA002_age >= 70 & KLoSA_main$wA002_age <75 ~ 4, 
                                     KLoSA_main$wA002_age >= 75 ~ 5)

KLoSA_main$health <- case_when(KLoSA_main$wC001 == 1 ~ 5,#Best
                               KLoSA_main$wC001 == 2 ~ 4,#Very good
                               KLoSA_main$wC001 == 3 ~ 3,#Good
                               KLoSA_main$wC001 == 4 ~ 2,#Normal
                               KLoSA_main$wC001 == 5 ~ 1)#Bad


KLoSA_main$workingdays <- KLoSA_main$wd_com031

# Pension situation
#E033: national pension, E070m5 social security pension, E044: special pension, E055: personal pension 
KLoSA_main$national_pension <- ifelse(KLoSA_main$wE033 == 4, 0, 1)
KLoSA_main$specific_corporate_pension <- ifelse(KLoSA_main$wE044 == 4, 0, 1)
KLoSA_main$social_security_pension <-KLoSA_main$wE070m5
#KLoSA_main$private_pension <- ifelse(KLoSA_main$wE055 == 4, 0, 1)
KLoSA_main$public_transfer <- KLoSA_main$national_pension + KLoSA_main$specific_corporate_pension + KLoSA_main$social_security_pension
KLoSA_main$public_trasfer_dummy <- ifelse(KLoSA_main$public_transfer != 0, 1, 0)

# Only the pre-exposure period
pre_exposure_data <- KLoSA_main[KLoSA_main$wave == 6, ]

# Estimate propensity scores using CBPS from WeightIt package.
weight_model <- weightit(exposure ~ wA002_age + wgender1 + health + wedu
                         + wd_com032 +  wd_com031, data = pre_exposure_data, method = "cbps",
                         estimand = "ATE")
# Now include the obtained weights.
pre_exposure_data$weights <- weight_model$weights

# Combine the pre and post exposure data.
KLoSA_main$weights <- NA
KLoSA_main <- rbind(pre_exposure_data, KLoSA_main[KLoSA_main$wave == 7,])
# Apply the same weights to the same ID
KLoSA_main <- KLoSA_main %>%
  group_by(pid) %>%
  mutate(weights = first(weights))

#Weighted diff-in-diff

weighted <- plm(wmmse ~ did + factor(age_category) + public_trasfer_dummy + spouse, 
                data = KLoSA_main,
                index = c("pid", "wave"), 
                model = "within", 
                weights = weights,
                effect = "twoways")

summary(weighted)


####################################################################################################
## 150%
####################################################################################################


rm(list=ls())

library(haven)
KLoSA_main <- read_dta("KLoSA_main.dta")
KLoSA_main$wave <- as.numeric(KLoSA_main$wave)
KLoSA_main <- KLoSA_main[!is.na(KLoSA_main$wwgt_c),]

KLoSA_detail <- read_dta("KLoSA_detail.dta")
KLoSA_detail$wave <- as.numeric(KLoSA_detail$wave)

KLoSA_main <- left_join(KLoSA_main, KLoSA_detail,by = c("pid", "wave"))

# We consider two waves, 6 and 7.
KLoSA_main <- KLoSA_main[KLoSA_main$wave == 6 |KLoSA_main$wave == 7,]
KLoSA_detail <- NULL

# Remove individuals who weren't working at the baseline.
notworking_wave6 <- KLoSA_main[!is.na(KLoSA_main$wd_com032) & KLoSA_main$wave == 6,] 
notworking_wave6 <- subset(notworking_wave6, select = c("pid"))
KLoSA_main  <- left_join(notworking_wave6, KLoSA_main) 

notworking_wave6 <- NULL

# We select individuals worked as a paid employee at baseline.
employee <- KLoSA_main[KLoSA_main$wemp == 1 & KLoSA_main$wave == 6,] #1. employee #2. self-employed #3. unpaid family-related labor 
employee  <- subset(employee, select = c("pid"))
KLoSA_main  <- left_join(employee, KLoSA_main) 

employee <- NULL

# We select individuals with main health outcome.
KLoSA_main <- KLoSA_main[!is.na(KLoSA_main$wmmse),]


#We calculate hourly wage from monthly wage and weekly working hours. 

KLoSA_main$ww <- KLoSA_main$wd_com052/(52/12) #weekly wage
KLoSA_main$hw <- KLoSA_main$ww/KLoSA_main$wd_com032 #hourly wage
KLoSA_main[!is.na(KLoSA_main$hw) & KLoSA_main$hw < 0,] <- NA #those without hourly wage information and negative hourly wage are considered as missing.

minimum_wage = 0.753 # minimum wage at year 2018

# Exclude individuals whose minimum wage at baseline was bigger than 150% of it.

attrition <- KLoSA_main[KLoSA_main$wave == 6 & KLoSA_main$hw > 0,]
attrition <- attrition[attrition$hw <= minimum_wage * 1.5 & attrition$wave == 6 ,]

attrition <- subset(attrition, select = c("pid"))
attrition <- semi_join(KLoSA_main, attrition) 

# Exclude unemployed individuals after the minimum wage increase.
# In the sensitivity analysis, we relax this condition. 

attrition <- attrition[!is.na(attrition$hw),]
KLoSA_main <- attrition # without unemployed individuals 

# Include individuals with observations available in both waves.

KLoSA_main <- 
  KLoSA_main %>% 
  dplyr::mutate(wave = as.integer(wave)) %>% 
  group_by(pid) %>% 
  dplyr::mutate(grp = cumsum(c(1, diff(wave) != 1))) %>% 
  filter(n() >= 2)# without unemployed individuals 

# Intervention group assignment
# 1) Hourly wage at baseline is smaller than the minimum wage.
intervention_group <- KLoSA_main[KLoSA_main$hw < minimum_wage  & KLoSA_main$wave == 6 ,]
intervention_group <- subset(intervention_group, select = c("pid"))
intervention_group<- semi_join(KLoSA_main, intervention_group)

intervention_group$exposure <- 1
intervention_group$group <- "intervention"

# Control group assignment.
# The hourly wage should fall between 100 - 150% of the baseline minimum hourly wage. 

control_group <- KLoSA_main[KLoSA_main$hw >= minimum_wage * 1.0 & KLoSA_main$hw <= minimum_wage*1.5 & KLoSA_main$wave == 6,] 
control_group <- subset(control_group, select = c("pid"))
control_group <- semi_join(KLoSA_main, control_group)
control_group$exposure <- 0
control_group$group <- "control"

# We merge intervention and control group.
KLoSA_main <- rbind(intervention_group, control_group)
# Time dummy for the minimum wage increase.
KLoSA_main$time = ifelse(KLoSA_main$wave == 7, 1, 0)
# time * exposure is what we call as the event
KLoSA_main$did <- KLoSA_main$time * KLoSA_main$exposure

KLoSA_main$female <- ifelse(KLoSA_main$wgender1 == 5 , 1,0)

KLoSA_main$spouse <- ifelse(KLoSA_main$wmarital == 1, 1, 0)

KLoSA_main$age_category <- case_when(KLoSA_main$wA002_age < 60 ~ 1,
                                     KLoSA_main$wA002_age >= 60 & KLoSA_main$wA002_age <65 ~ 2,
                                     KLoSA_main$wA002_age >= 65 & KLoSA_main$wA002_age <70 ~ 3,
                                     KLoSA_main$wA002_age >= 70 & KLoSA_main$wA002_age <75 ~ 4, 
                                     KLoSA_main$wA002_age >= 75 ~ 5)

KLoSA_main$health <- case_when(KLoSA_main$wC001 == 1 ~ 5,#Best
                               KLoSA_main$wC001 == 2 ~ 4,#Very good
                               KLoSA_main$wC001 == 3 ~ 3,#Good
                               KLoSA_main$wC001 == 4 ~ 2,#Normal
                               KLoSA_main$wC001 == 5 ~ 1)#Bad


KLoSA_main$workingdays <- KLoSA_main$wd_com031

# Pension situation
#E033: national pension, E070m5 social security pension, E044: special pension, E055: personal pension 
KLoSA_main$national_pension <- ifelse(KLoSA_main$wE033 == 4, 0, 1)
KLoSA_main$specific_corporate_pension <- ifelse(KLoSA_main$wE044 == 4, 0, 1)
KLoSA_main$social_security_pension <-KLoSA_main$wE070m5
#KLoSA_main$private_pension <- ifelse(KLoSA_main$wE055 == 4, 0, 1)
KLoSA_main$public_transfer <- KLoSA_main$national_pension + KLoSA_main$specific_corporate_pension + KLoSA_main$social_security_pension
KLoSA_main$public_trasfer_dummy <- ifelse(KLoSA_main$public_transfer != 0, 1, 0)

# Only the pre-exposure period
pre_exposure_data <- KLoSA_main[KLoSA_main$wave == 6, ]

# Estimate propensity scores using CBPS from WeightIt package.
weight_model <- weightit(exposure ~ wA002_age + wgender1 + health + wedu
                         + wd_com032 +  wd_com031, data = pre_exposure_data, method = "cbps",
                         estimand = "ATE")
# Now include the obtained weights.
pre_exposure_data$weights <- weight_model$weights

# Combine the pre and post exposure data.
KLoSA_main$weights <- NA
KLoSA_main <- rbind(pre_exposure_data, KLoSA_main[KLoSA_main$wave == 7,])
# Apply the same weights to the same ID
KLoSA_main <- KLoSA_main %>%
  group_by(pid) %>%
  mutate(weights = first(weights))

#Weighted diff-in-diff

weighted <- plm(wmmse ~ did + factor(age_category) + public_trasfer_dummy + spouse, 
                data = KLoSA_main,
                index = c("pid", "wave"), 
                model = "within", 
                weights = weights,
                effect = "twoways")

summary(weighted)

####################################################################################################
## Including unemployed individuals
####################################################################################################


rm(list=ls())

library(haven)
KLoSA_main <- read_dta("KLoSA_main.dta")
KLoSA_main$wave <- as.numeric(KLoSA_main$wave)
KLoSA_main <- KLoSA_main[!is.na(KLoSA_main$wwgt_c),]

KLoSA_detail <- read_dta("KLoSA_detail.dta")
KLoSA_detail$wave <- as.numeric(KLoSA_detail$wave)

KLoSA_main <- left_join(KLoSA_main, KLoSA_detail,by = c("pid", "wave"))

# We consider two waves, 6 and 7.
KLoSA_main <- KLoSA_main[KLoSA_main$wave == 6 |KLoSA_main$wave == 7,]
KLoSA_detail <- NULL

# Remove individuals who weren't working at the baseline.
notworking_wave6 <- KLoSA_main[!is.na(KLoSA_main$wd_com032) & KLoSA_main$wave == 6,] 
notworking_wave6 <- subset(notworking_wave6, select = c("pid"))
KLoSA_main  <- left_join(notworking_wave6, KLoSA_main) 

notworking_wave6 <- NULL

# We select individuals worked as a paid employee at baseline.
employee <- KLoSA_main[KLoSA_main$wemp == 1 & KLoSA_main$wave == 6,] #1. employee #2. self-employed #3. unpaid family-related labor 
employee  <- subset(employee, select = c("pid"))
KLoSA_main  <- left_join(employee, KLoSA_main) 

employee <- NULL

# We select individuals with main health outcome.
KLoSA_main <- KLoSA_main[!is.na(KLoSA_main$wmmse),]


#We calculate hourly wage from monthly wage and weekly working hours. 

KLoSA_main$ww <- KLoSA_main$wd_com052/(52/12) #weekly wage
KLoSA_main$hw <- KLoSA_main$ww/KLoSA_main$wd_com032 #hourly wage
KLoSA_main[!is.na(KLoSA_main$hw) & KLoSA_main$hw < 0,] <- NA #those without hourly wage information and negative hourly wage are considered as missing.

minimum_wage = 0.753 # minimum wage at year 2018

# Exclude individuals whose minimum wage at baseline was bigger than 150% of it.

attrition <- KLoSA_main[KLoSA_main$wave == 6 & KLoSA_main$hw > 0,]
attrition <- attrition[attrition$hw <= minimum_wage * 1.5 & attrition$wave == 6 ,]

attrition <- subset(attrition, select = c("pid"))
attrition <- semi_join(KLoSA_main, attrition) 

# Exclude unemployed individuals after the minimum wage increase.
# In the sensitivity analysis, we relax this condition. 

#attrition <- attrition[!is.na(attrition$hw),]
KLoSA_main <- attrition # without unemployed individuals 

# Include individuals with observations available in both waves.

KLoSA_main <- 
  KLoSA_main %>% 
  dplyr::mutate(wave = as.integer(wave)) %>% 
  group_by(pid) %>% 
  dplyr::mutate(grp = cumsum(c(1, diff(wave) != 1))) %>% 
  filter(n() >= 2)# without unemployed individuals 

# Intervention group assignment
# 1) Hourly wage at baseline is smaller than the minimum wage.
intervention_group <- KLoSA_main[KLoSA_main$hw < minimum_wage  & KLoSA_main$wave == 6 ,]
intervention_group <- subset(intervention_group, select = c("pid"))
intervention_group<- semi_join(KLoSA_main, intervention_group)

intervention_group$exposure <- 1
intervention_group$group <- "intervention"

# Control group assignment.
# The hourly wage should fall between 100 - 150% of the baseline minimum hourly wage. 

control_group <- KLoSA_main[KLoSA_main$hw >= minimum_wage * 1.0 & KLoSA_main$hw <= minimum_wage*1.5 & KLoSA_main$wave == 6,] 
control_group <- subset(control_group, select = c("pid"))
control_group <- semi_join(KLoSA_main, control_group)
control_group$exposure <- 0
control_group$group <- "control"

# We merge intervention and control group.
KLoSA_main <- rbind(intervention_group, control_group)
# Time dummy for the minimum wage increase.
KLoSA_main$time = ifelse(KLoSA_main$wave == 7, 1, 0)
# time * exposure is what we call as the event
KLoSA_main$did <- KLoSA_main$time * KLoSA_main$exposure

KLoSA_main$female <- ifelse(KLoSA_main$wgender1 == 5 , 1,0)

KLoSA_main$spouse <- ifelse(KLoSA_main$wmarital == 1, 1, 0)

KLoSA_main$age_category <- case_when(KLoSA_main$wA002_age < 60 ~ 1,
                                     KLoSA_main$wA002_age >= 60 & KLoSA_main$wA002_age <65 ~ 2,
                                     KLoSA_main$wA002_age >= 65 & KLoSA_main$wA002_age <70 ~ 3,
                                     KLoSA_main$wA002_age >= 70 & KLoSA_main$wA002_age <75 ~ 4, 
                                     KLoSA_main$wA002_age >= 75 ~ 5)

KLoSA_main$health <- case_when(KLoSA_main$wC001 == 1 ~ 5,#Best
                               KLoSA_main$wC001 == 2 ~ 4,#Very good
                               KLoSA_main$wC001 == 3 ~ 3,#Good
                               KLoSA_main$wC001 == 4 ~ 2,#Normal
                               KLoSA_main$wC001 == 5 ~ 1)#Bad


KLoSA_main$workingdays <- KLoSA_main$wd_com031

# Pension situation
#E033: national pension, E070m5 social security pension, E044: special pension, E055: personal pension 
KLoSA_main$national_pension <- ifelse(KLoSA_main$wE033 == 4, 0, 1)
KLoSA_main$specific_corporate_pension <- ifelse(KLoSA_main$wE044 == 4, 0, 1)
KLoSA_main$social_security_pension <-KLoSA_main$wE070m5
#KLoSA_main$private_pension <- ifelse(KLoSA_main$wE055 == 4, 0, 1)
KLoSA_main$public_transfer <- KLoSA_main$national_pension + KLoSA_main$specific_corporate_pension + KLoSA_main$social_security_pension
KLoSA_main$public_trasfer_dummy <- ifelse(KLoSA_main$public_transfer != 0, 1, 0)

# Only the pre-exposure period
pre_exposure_data <- KLoSA_main[KLoSA_main$wave == 6, ]

# Estimate propensity scores using CBPS from WeightIt package.
weight_model <- weightit(exposure ~ wA002_age + wgender1 + health + wedu
                         + wd_com032 +  wd_com031, data = pre_exposure_data, method = "cbps",
                         estimand = "ATE")
# Now include the obtained weights.
pre_exposure_data$weights <- weight_model$weights

# Combine the pre and post exposure data.
KLoSA_main$weights <- NA
KLoSA_main <- rbind(pre_exposure_data, KLoSA_main[KLoSA_main$wave == 7,])
# Apply the same weights to the same ID
KLoSA_main <- KLoSA_main %>%
  group_by(pid) %>%
  mutate(weights = first(weights))

#Weighted diff-in-diff

weighted <- plm(wmmse ~ did + factor(age_category) + public_trasfer_dummy + spouse, 
                data = KLoSA_main,
                index = c("pid", "wave"), 
                model = "within", 
                weights = weights,
                effect = "twoways")

summary(weighted)


####################################################################################################
## Sensitivity analysis 2. Alternative definition of intervention group. 
####################################################################################################
rm(list=ls())

library(haven)
KLoSA_main <- read_dta("KLoSA_main.dta")
KLoSA_main$wave <- as.numeric(KLoSA_main$wave)
KLoSA_main <- KLoSA_main[!is.na(KLoSA_main$wwgt_c),]

KLoSA_detail <- read_dta("KLoSA_detail.dta")
KLoSA_detail$wave <- as.numeric(KLoSA_detail$wave)

KLoSA_main <- left_join(KLoSA_main, KLoSA_detail,by = c("pid", "wave"))

# We consider two waves, 6 and 7.
KLoSA_main <- KLoSA_main[KLoSA_main$wave == 6 |KLoSA_main$wave == 7,]
KLoSA_detail <- NULL

# Remove individuals who weren't working at the baseline.
notworking_wave6 <- KLoSA_main[!is.na(KLoSA_main$wd_com032) & KLoSA_main$wave == 6,] 
notworking_wave6 <- subset(notworking_wave6, select = c("pid"))
KLoSA_main  <- left_join(notworking_wave6, KLoSA_main) 

notworking_wave6 <- NULL

# We select individuals worked as a paid employee at baseline.
employee <- KLoSA_main[KLoSA_main$wemp == 1 & KLoSA_main$wave == 6,] #1. employee #2. self-employed #3. unpaid family-related labor 
employee  <- subset(employee, select = c("pid"))
KLoSA_main  <- left_join(employee, KLoSA_main) 

employee <- NULL

# We select individuals with main health outcome.
KLoSA_main <- KLoSA_main[!is.na(KLoSA_main$wmmse),]


#We calculate hourly wage from monthly wage and weekly working hours. 

KLoSA_main$ww <- KLoSA_main$wd_com052/(52/12) #weekly wage
KLoSA_main$hw <- KLoSA_main$ww/KLoSA_main$wd_com032 #hourly wage
KLoSA_main[!is.na(KLoSA_main$hw) & KLoSA_main$hw < 0,] <- NA #those without hourly wage information and negative hourly wage are considered as missing.

minimum_wage = 0.753 # minimum wage at year 2018
# Exclude individuals whose minimum wage at baseline was bigger than 150% of it.

attrition <- KLoSA_main[KLoSA_main$wave == 6 & KLoSA_main$hw > 0,]
attrition <- attrition[attrition$hw <= minimum_wage * 1.5 & attrition$wave == 6 ,]

attrition <- subset(attrition, select = c("pid"))
attrition <- semi_join(KLoSA_main, attrition) 

# Exclude unemployed individuals after the minimum wage increase.
# In the sensitivity analysis, we relax this condition. 

attrition <- attrition[!is.na(attrition$hw),]
KLoSA_main <- attrition # without unemployed individuals 

# Include individuals with observations available in both waves.

KLoSA_main <- 
  KLoSA_main %>% 
  dplyr::mutate(wave = as.integer(wave)) %>% 
  group_by(pid) %>% 
  dplyr::mutate(grp = cumsum(c(1, diff(wave) != 1))) %>% 
  filter(n() >= 2)# without unemployed individuals 

# Intervention group assignment
# 1) Hourly wage at baseline is smaller than the minimum wage.
intervention_group <- KLoSA_main[KLoSA_main$hw < minimum_wage  & KLoSA_main$wave == 6 ,]
intervention_group <- subset(intervention_group, select = c("pid"))
intervention_group<- semi_join(KLoSA_main, intervention_group)

# In the sensitivity analysis, we put additional restriction
# 2) That the post minimum wage increase should fall between 100 - 120% of the new minimum wage.
intervention_group2 <- intervention_group[intervention_group$hw >= minimum_wage & intervention_group$hw <= 1.20 * (minimum_wage) & intervention_group$wave == 7,] 
intervention_group2 <- subset(intervention_group2, select = c("pid"))
intervention_group<- semi_join(intervention_group, intervention_group2)

intervention_group$exposure <- 1
intervention_group$group <- "intervention"

# Control group assignment.
# The hourly wage should fall between 100 - 150% of the baseline minimum hourly wage. 

control_group <- KLoSA_main[KLoSA_main$hw >= minimum_wage * 1.0 & KLoSA_main$hw <= minimum_wage*1.5 & KLoSA_main$wave == 6,] 
control_group <- subset(control_group, select = c("pid"))
control_group <- semi_join(KLoSA_main, control_group)
control_group$exposure <- 0
control_group$group <- "control"

# We merge intervention and control group.
KLoSA_main <- rbind(intervention_group, control_group)
# Time dummy for the minimum wage increase.
KLoSA_main$time = ifelse(KLoSA_main$wave == 7, 1, 0)
# time * exposure is what we call as the event
KLoSA_main$did <- KLoSA_main$time * KLoSA_main$exposure

KLoSA_main$female <- ifelse(KLoSA_main$wgender1 == 5 , 1,0)

KLoSA_main$spouse <- ifelse(KLoSA_main$wmarital == 1, 1, 0)

KLoSA_main$age_category <- case_when(KLoSA_main$wA002_age < 60 ~ 1,
                                     KLoSA_main$wA002_age >= 60 & KLoSA_main$wA002_age <65 ~ 2,
                                     KLoSA_main$wA002_age >= 65 & KLoSA_main$wA002_age <70 ~ 3,
                                     KLoSA_main$wA002_age >= 70 & KLoSA_main$wA002_age <75 ~ 4, 
                                     KLoSA_main$wA002_age >= 75 ~ 5)

KLoSA_main$health <- case_when(KLoSA_main$wC001 == 1 ~ 5,#Best
                               KLoSA_main$wC001 == 2 ~ 4,#Very good
                               KLoSA_main$wC001 == 3 ~ 3,#Good
                               KLoSA_main$wC001 == 4 ~ 2,#Normal
                               KLoSA_main$wC001 == 5 ~ 1)#Bad


KLoSA_main$workingdays <- KLoSA_main$wd_com031

# Pension situation
#E033: national pension, E070m5 social security pension, E044: special pension, E055: personal pension 
KLoSA_main$national_pension <- ifelse(KLoSA_main$wE033 == 4, 0, 1)
KLoSA_main$specific_corporate_pension <- ifelse(KLoSA_main$wE044 == 4, 0, 1)
KLoSA_main$social_security_pension <-KLoSA_main$wE070m5
#KLoSA_main$private_pension <- ifelse(KLoSA_main$wE055 == 4, 0, 1)
KLoSA_main$public_transfer <- KLoSA_main$national_pension + KLoSA_main$specific_corporate_pension + KLoSA_main$social_security_pension
KLoSA_main$public_trasfer_dummy <- ifelse(KLoSA_main$public_transfer != 0, 1, 0)

# Only the pre-exposure period
pre_exposure_data <- KLoSA_main[KLoSA_main$wave == 6, ]

# Estimate propensity scores using CBPS from WeightIt package.
weight_model <- weightit(exposure ~ wA002_age + wgender1 + health + wedu
                         + wd_com032 +  wd_com031, data = pre_exposure_data, method = "cbps",
                         estimand = "ATE")
# Now include the obtained weights.
pre_exposure_data$weights <- weight_model$weights

# Combine the pre and post exposure data.
KLoSA_main$weights <- NA
KLoSA_main <- rbind(pre_exposure_data, KLoSA_main[KLoSA_main$wave == 7,])
# Apply the same weights to the same ID
KLoSA_main <- KLoSA_main %>%
  group_by(pid) %>%
  mutate(weights = first(weights))

#Weighted diff-in-diff
weighted <- plm(wmmse ~ did + factor(age_category) + public_trasfer_dummy + spouse, 
                data = KLoSA_main,
                index = c("pid", "wave"), 
                model = "within", 
                weights = weights,
                effect = "twoways")

summary(weighted)
####################################################################################################
## Sensitivity analysis 3. Alternative outcome variables. 
####################################################################################################

rm(list=ls())

library(haven)
KLoSA_main <- read_dta("KLoSA_main.dta")
KLoSA_main$wave <- as.numeric(KLoSA_main$wave)
KLoSA_main <- KLoSA_main[!is.na(KLoSA_main$wwgt_c),]

KLoSA_detail <- read_dta("KLoSA_detail.dta")
KLoSA_detail$wave <- as.numeric(KLoSA_detail$wave)

KLoSA_main <- left_join(KLoSA_main, KLoSA_detail,by = c("pid", "wave"))

# We consider two waves, 6 and 7.
KLoSA_main <- KLoSA_main[KLoSA_main$wave == 6 |KLoSA_main$wave == 7,]
KLoSA_detail <- NULL

# Remove individuals who weren't working at the baseline.
notworking_wave6 <- KLoSA_main[!is.na(KLoSA_main$wd_com032) & KLoSA_main$wave == 6,] 
notworking_wave6 <- subset(notworking_wave6, select = c("pid"))
KLoSA_main  <- left_join(notworking_wave6, KLoSA_main) 

notworking_wave6 <- NULL

# We select individuals worked as a paid employee at baseline.
employee <- KLoSA_main[KLoSA_main$wemp == 1 & KLoSA_main$wave == 6,] #1. employee #2. self-employed #3. unpaid family-related labor 
employee  <- subset(employee, select = c("pid"))
KLoSA_main  <- left_join(employee, KLoSA_main) 

employee <- NULL

# We select individuals with main health outcome.
KLoSA_main <- KLoSA_main[!is.na(KLoSA_main$wmmse),]


#We calculate hourly wage from monthly wage and weekly working hours. 

KLoSA_main$ww <- KLoSA_main$wd_com052/(52/12) #weekly wage
KLoSA_main$hw <- KLoSA_main$ww/KLoSA_main$wd_com032 #hourly wage
KLoSA_main[!is.na(KLoSA_main$hw) & KLoSA_main$hw < 0,] <- NA #those without hourly wage information and negative hourly wage are considered as missing.

minimum_wage = 0.753 # minimum wage at year 2018


# Exclude individuals whose minimum wage at baseline was bigger than 150% of it.

attrition <- KLoSA_main[KLoSA_main$wave == 6 & KLoSA_main$hw > 0,]
attrition <- attrition[attrition$hw <= minimum_wage * 1.5 & attrition$wave == 6 ,]

attrition <- subset(attrition, select = c("pid"))
attrition <- semi_join(KLoSA_main, attrition) 

# Exclude unemployed individuals after the minimum wage increase.
# In the sensitivity analysis, we relax this condition. 

attrition <- attrition[!is.na(attrition$hw),]
KLoSA_main <- attrition # without unemployed individuals 

# Include individuals with observations available in both waves.

KLoSA_main <- 
  KLoSA_main %>% 
  dplyr::mutate(wave = as.integer(wave)) %>% 
  group_by(pid) %>% 
  dplyr::mutate(grp = cumsum(c(1, diff(wave) != 1))) %>% 
  filter(n() >= 2)# without unemployed individuals 

# Intervention group assignment
# 1) Hourly wage at baseline is smaller than the minimum wage.
intervention_group <- KLoSA_main[KLoSA_main$hw < minimum_wage  & KLoSA_main$wave == 6 ,]
intervention_group <- subset(intervention_group, select = c("pid"))
intervention_group<- semi_join(KLoSA_main, intervention_group)

intervention_group$exposure <- 1
intervention_group$group <- "intervention"

# Control group assignment.
# The hourly wage should fall between 100 - 150% of the baseline minimum hourly wage. 

control_group <- KLoSA_main[KLoSA_main$hw >= minimum_wage * 1.0 & KLoSA_main$hw <= minimum_wage*1.5 & KLoSA_main$wave == 6,] 
control_group <- subset(control_group, select = c("pid"))
control_group <- semi_join(KLoSA_main, control_group)
control_group$exposure <- 0
control_group$group <- "control"

# We merge intervention and control group.
KLoSA_main <- rbind(intervention_group, control_group)
# Time dummy for the minimum wage increase.
KLoSA_main$time = ifelse(KLoSA_main$wave == 7, 1, 0)
# time * exposure is what we call as the event
KLoSA_main$did <- KLoSA_main$time * KLoSA_main$exposure


####################################################################################################
## Data cleaning
####################################################################################################

KLoSA_main$female <- ifelse(KLoSA_main$wgender1 == 5 , 1,0)

KLoSA_main$spouse <- ifelse(KLoSA_main$wmarital == 1, 1, 0)

KLoSA_main$age_category <- case_when(KLoSA_main$wA002_age < 60 ~ 1,
                                     KLoSA_main$wA002_age >= 60 & KLoSA_main$wA002_age <65 ~ 2,
                                     KLoSA_main$wA002_age >= 65 & KLoSA_main$wA002_age <70 ~ 3,
                                     KLoSA_main$wA002_age >= 70 & KLoSA_main$wA002_age <75 ~ 4, 
                                     KLoSA_main$wA002_age >= 75 ~ 5)

KLoSA_main$health <- case_when(KLoSA_main$wC001 == 1 ~ 5,#Best
                               KLoSA_main$wC001 == 2 ~ 4,#Very good
                               KLoSA_main$wC001 == 3 ~ 3,#Good
                               KLoSA_main$wC001 == 4 ~ 2,#Normal
                               KLoSA_main$wC001 == 5 ~ 1)#Bad


KLoSA_main$workingdays <- KLoSA_main$wd_com031

# Pension situation
#E033: national pension, E070m5 social security pension, E044: special pension, E055: personal pension 
KLoSA_main$national_pension <- ifelse(KLoSA_main$wE033 == 4, 0, 1)
KLoSA_main$specific_corporate_pension <- ifelse(KLoSA_main$wE044 == 4, 0, 1)
KLoSA_main$social_security_pension <-KLoSA_main$wE070m5
#KLoSA_main$private_pension <- ifelse(KLoSA_main$wE055 == 4, 0, 1)
KLoSA_main$public_transfer <- KLoSA_main$national_pension + KLoSA_main$specific_corporate_pension + KLoSA_main$social_security_pension
KLoSA_main$public_trasfer_dummy <- ifelse(KLoSA_main$public_transfer != 0, 1, 0)


####################################################################################################
## Propensity score weighted difference-in-difference estimation
####################################################################################################

# Only the pre-exposure period
pre_exposure_data <- KLoSA_main[KLoSA_main$wave == 6, ]

# Estimate propensity scores using CBPS from WeightIt package.
weight_model <- weightit(exposure ~ wA002_age + wgender1 + health + wedu
                         + wd_com032 +  wd_com031, data = pre_exposure_data, method = "cbps",
                         estimand = "ATE")
# Now include the obtained weights.
pre_exposure_data$weights <- weight_model$weights

# Combine the pre and post exposure data.
KLoSA_main$weights <- NA
KLoSA_main <- rbind(pre_exposure_data, KLoSA_main[KLoSA_main$wave == 7,])
# Apply the same weights to the same ID
KLoSA_main <- KLoSA_main %>%
  group_by(pid) %>%
  mutate(weights = first(weights))

###### Memory #########
KLoSA_main$registration <- case_when(KLoSA_main$wC406 <= 3 ~ KLoSA_main$wC406,
                                     KLoSA_main$wC406 == 5 ~ 0 )
KLoSA_main$recall <- case_when(KLoSA_main$wC412 <= 3 ~ KLoSA_main$wC412,
                               KLoSA_main$wC412 == 5 ~ 0 )

###### Date #########
KLoSA_main$year_month_date <- case_when(KLoSA_main$wC401 <= 3 ~  KLoSA_main$wC401,
                                        KLoSA_main$wC401 == 5 ~ 0)
KLoSA_main$day <- ifelse(KLoSA_main$wC402 == 1, 1, 0)
KLoSA_main$season <- ifelse(KLoSA_main$wC403 == 1, 1, 0)

###### Place #########
KLoSA_main$county_district <- case_when(KLoSA_main$wC405 <= 4 ~  KLoSA_main$wC405,
                                        KLoSA_main$wC405 == 5 ~ 0)
KLoSA_main$wC404 <- as.numeric(KLoSA_main$wC404)
KLoSA_main$current_space <- ifelse(KLoSA_main$wC404 == 1, 1, 0)

###### Attention  #########
KLoSA_main$wC407 <- ifelse(KLoSA_main$wC407 == 1, 1, 0) #7 subtraction 1st.
KLoSA_main$wC408 <- ifelse(KLoSA_main$wC408 == 1, 1, 0)
KLoSA_main$wC409 <- ifelse(KLoSA_main$wC409 == 1, 1, 0)
KLoSA_main$wC410 <- ifelse(KLoSA_main$wC410 == 1, 1, 0)
KLoSA_main$wC411 <- ifelse(KLoSA_main$wC411 == 1, 1, 0) #7 subtraction 5th.


###### Language  #########
KLoSA_main$wC413 <- ifelse(KLoSA_main$wC413 == 1, 1, 0) 
KLoSA_main$wC414 <- ifelse(KLoSA_main$wC414 == 1, 1, 0)
KLoSA_main$wC415 <- ifelse(KLoSA_main$wC415 == 1, 1, 0)
KLoSA_main$order <- case_when(KLoSA_main$wC416 <= 3 ~  KLoSA_main$wC416,
                              KLoSA_main$wC416 == 5 ~ 0)

KLoSA_main$order2 <- case_when(KLoSA_main$wC417 <= 3 ~ 1,
                               KLoSA_main$wC417 == 5 ~ 0)

KLoSA_main$wC418 <- ifelse(KLoSA_main$wC418 == 1, 1, 0)

##############################################################################################################################
KLoSA_main$memory <- KLoSA_main$registration + KLoSA_main$recall #6 points

KLoSA_main$place <- KLoSA_main$county_district + KLoSA_main$current_space #5 points

KLoSA_main$temporal <- KLoSA_main$year_month_date + KLoSA_main$day + KLoSA_main$season# 5 points

KLoSA_main$attention_calculation<- KLoSA_main$wC407 + KLoSA_main$wC408 +KLoSA_main$wC409 +KLoSA_main$wC410 +KLoSA_main$wC411 #5 points

KLoSA_main$language <- KLoSA_main$wC413 +KLoSA_main$wC414 +KLoSA_main$wC415 + KLoSA_main$order +KLoSA_main$order2 +KLoSA_main$wC418  #8 points

KLoSA_main$visual_construction <- ifelse(KLoSA_main$wC419 == 1, 1, 0)

# Check for the sum score.
#KLoSA_main$mmse_seperate <- KLoSA_main$temporal + KLoSA_main$place + KLoSA_main$registration + KLoSA_main$attention_calculation + KLoSA_main$recall + 
# KLoSA_main$language + KLoSA_main$visual_construction 

# memory

memory <- plm(memory ~ did + factor(age_category) + public_trasfer_dummy + spouse, 
              data = KLoSA_main,
              index = c("pid", "wave"), 
              model = "within", 
              weights = weights,
              effect = "twoways")

summary(memory)

# temporal

temp <- plm(temporal ~ did + factor(age_category) + public_trasfer_dummy + spouse, 
            data = KLoSA_main,
            index = c("pid", "wave"), 
            model = "within", 
            weights = weights,
            effect = "twoways")

summary(temp)

# place

place <- plm(place ~ did + factor(age_category) + public_trasfer_dummy + spouse, 
             data = KLoSA_main,
             index = c("pid", "wave"), 
             model = "within", 
             weights = weights,
             effect = "twoways")

summary(place)

# attention

attention <- plm(attention_calculation ~ did + factor(age_category) + public_trasfer_dummy + spouse, 
                 data = KLoSA_main,
                 index = c("pid", "wave"), 
                 model = "within", 
                 weights = weights,
                 effect = "twoways")

summary(attention)

# language

language <- plm(language ~ did + factor(age_category) + public_trasfer_dummy + spouse, 
                data = KLoSA_main,
                index = c("pid", "wave"), 
                model = "within", 
                weights = weights,
                effect = "twoways")

summary(language)


####################################################################################################
## Placebo sensitivity analysis: including unemployed individuals
####################################################################################################

rm(list=ls())
library(haven)
KLoSA_main <- read_dta("KLoSA_main.dta")
KLoSA_main$wave <- as.numeric(KLoSA_main$wave)
KLoSA_main <- KLoSA_main[!is.na(KLoSA_main$wwgt_c),]

KLoSA_detail <- read_dta("KLoSA_detail.dta")
KLoSA_detail$wave <- as.numeric(KLoSA_detail$wave)

KLoSA_main <- left_join(KLoSA_main, KLoSA_detail,by = c("pid", "wave"))

# We consider two waves, 5 and 6.
KLoSA_main <- KLoSA_main[KLoSA_main$wave == 5 |KLoSA_main$wave == 6,]
KLoSA_detail <- NULL

# Remove individuals who weren't working at the baseline.
notworking_wave5 <- KLoSA_main[!is.na(KLoSA_main$wd_com032) & KLoSA_main$wave == 5,] 
notworking_wave5 <- subset(notworking_wave5, select = c("pid"))
KLoSA_main  <- left_join(notworking_wave5, KLoSA_main) 

notworking_wave5 <- NULL

# We select individuals worked as a paid employee at baseline.
employee <- KLoSA_main[KLoSA_main$wemp == 1 & KLoSA_main$wave == 5,] #1. employee #2. self-employed #3. unpaid family-related labor 
employee  <- subset(employee, select = c("pid"))
KLoSA_main  <- left_join(employee, KLoSA_main) 

employee <- NULL

# We select individuals with main health outcome.
KLoSA_main <- KLoSA_main[!is.na(KLoSA_main$wmmse),]


#We calculate hourly wage from monthly wage and weekly working hours. 

KLoSA_main$ww <- KLoSA_main$wd_com052/(52/12) #weekly wage
KLoSA_main$hw <- KLoSA_main$ww/KLoSA_main$wd_com032 #hourly wage
KLoSA_main[!is.na(KLoSA_main$hw) & KLoSA_main$hw < 0,] <- NA #those without hourly wage information and negative hourly wage are considered as missing.

minimum_wage = 0.603 # minimum wage at year 2016

# Exclude individuals whose minimum wage at baseline was bigger than 150% of it.

attrition <- KLoSA_main[KLoSA_main$wave == 5 & KLoSA_main$hw > 0,]
attrition <- attrition[attrition$hw <= minimum_wage * 1.5 & attrition$wave == 5 ,]

attrition <- subset(attrition, select = c("pid"))
attrition <- semi_join(KLoSA_main, attrition) 

# Exclude unemployed individuals after the minimum wage increase.
# In the sensitivity analysis, we relax this condition. 

#attrition <- attrition[!is.na(attrition$hw),]
KLoSA_main <- attrition 

# Include individuals with observations available in both waves.

KLoSA_main <- 
  KLoSA_main %>% 
  dplyr::mutate(wave = as.integer(wave)) %>% 
  group_by(pid) %>% 
  dplyr::mutate(grp = cumsum(c(1, diff(wave) != 1))) %>% 
  filter(n() >= 2)

# Intervention group assignment
# 1) Hourly wage at baseline is smaller than the minimum wage.
intervention_group <- KLoSA_main[KLoSA_main$hw < minimum_wage  & KLoSA_main$wave == 5 ,]
intervention_group <- subset(intervention_group, select = c("pid"))
intervention_group<- semi_join(KLoSA_main, intervention_group)

# In the sensitivity analysis, we put additional restriction
# 2) That the post minimum wage increase should fall between 100 - 120% of the new minimum wage.
#intervention_group <- MW[MW$hw >= minimum_wage & MW$hw <= 1.20 * (minimum_wage) & MW$wave == 6,] 

intervention_group$exposure <- 1
intervention_group$group <- "intervention"

# Control group assignment.
# The hourly wage should fall between 100 - 150% of the baseline minimum hourly wage. 

control_group <- KLoSA_main[KLoSA_main$hw >= minimum_wage * 1.0 & KLoSA_main$hw <= minimum_wage*1.5 & KLoSA_main$wave == 5,] 
control_group <- subset(control_group, select = c("pid"))
control_group <- semi_join(KLoSA_main, control_group)
control_group$exposure <- 0
control_group$group <- "control"

# We merge intervention and control group.
KLoSA_main <- rbind(intervention_group, control_group)
# Time dummy for the minimum wage increase.
KLoSA_main$time = ifelse(KLoSA_main$wave == 6, 1, 0)
# time * exposure is what we call as the event
KLoSA_main$did <- KLoSA_main$time * KLoSA_main$exposure

KLoSA_main$female <- ifelse(KLoSA_main$wgender1 == 5 , 1,0)

KLoSA_main$spouse <- ifelse(KLoSA_main$wmarital == 1, 1, 0)

KLoSA_main$age_category <- case_when(KLoSA_main$wA002_age < 60 ~ 1,
                                     KLoSA_main$wA002_age >= 60 & KLoSA_main$wA002_age <65 ~ 2,
                                     KLoSA_main$wA002_age >= 65 & KLoSA_main$wA002_age <70 ~ 3,
                                     KLoSA_main$wA002_age >= 70 & KLoSA_main$wA002_age <75 ~ 4, 
                                     KLoSA_main$wA002_age >= 75 ~ 5)

KLoSA_main$health <- case_when(KLoSA_main$wC001 == 1 ~ 5,#Best
                               KLoSA_main$wC001 == 2 ~ 4,#Very good
                               KLoSA_main$wC001 == 3 ~ 3,#Good
                               KLoSA_main$wC001 == 4 ~ 2,#Normal
                               KLoSA_main$wC001 == 5 ~ 1)#Bad


KLoSA_main$workingdays <- KLoSA_main$wd_com031

# Pension situation
#E033: national pension, E070m5 social security pension, E044: special pension, E055: personal pension 
KLoSA_main$national_pension <- ifelse(KLoSA_main$wE033 == 4, 0, 1)
KLoSA_main$specific_corporate_pension <- ifelse(KLoSA_main$wE044 == 4, 0, 1)
KLoSA_main$social_security_pension <-KLoSA_main$wE070m5
#KLoSA_main$private_pension <- ifelse(KLoSA_main$wE055 == 4, 0, 1)
KLoSA_main$public_transfer <- KLoSA_main$national_pension + KLoSA_main$specific_corporate_pension + KLoSA_main$social_security_pension
KLoSA_main$public_trasfer_dummy <- ifelse(KLoSA_main$public_transfer != 0, 1, 0)

# Only the pre-exposure period
pre_exposure_data <- KLoSA_main[KLoSA_main$wave == 5, ]

# Estimate propensity scores using CBPS from WeightIt package.
weight_model <- weightit(exposure ~ wA002_age + wgender1 + health + wedu
                         + wd_com032 +  wd_com031, data = pre_exposure_data, method = "cbps",
                         estimand = "ATE")
# Now include the obtained weights.
pre_exposure_data$weights <- weight_model$weights

# Combine the pre and post exposure data.
KLoSA_main$weights <- NA
KLoSA_main <- rbind(pre_exposure_data, KLoSA_main[KLoSA_main$wave == 6,])
# Apply the same weights to the same ID
KLoSA_main <- KLoSA_main %>%
  group_by(pid) %>%
  mutate(weights = first(weights))

#Weighted diff-in-diff

weighted <- plm(wmmse ~ did + factor(age_category) + public_trasfer_dummy + spouse, 
                data = KLoSA_main,
                index = c("pid", "wave"), 
                model = "within", 
                weights = weights,
                effect = "twoways")

summary(weighted)



