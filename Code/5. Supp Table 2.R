####################################################################################################
## project: = Health effects of a minimum wage hike: Evidence from South Korea experiments
## author(s): Jung Hyun Kim
## code started: February, 2023
## last update: October, 2024
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
attrition <- semi_join(KLoSA_main, attrition) #1349

# Exclude unemployed individuals after the minimum wage increase.
# In the sensitivity analysis, we relax this condition. 

attrition <- attrition[!is.na(attrition$pid),]
KLoSA_main <- attrition # Including unemployed individuals 

# Include individuals with observations available in both waves.

KLoSA_main <- 
  KLoSA_main %>% 
  dplyr::mutate(wave = as.integer(wave)) %>% 
  group_by(pid) %>% 
  dplyr::mutate(grp = cumsum(c(1, diff(wave) != 1))) %>% 
  filter(n() >= 2)# without unemployed individuals # (1349-1274)/1349

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
KLoSA_main$all_pension <- KLoSA_main$national_pension + KLoSA_main$specific_corporate_pension + KLoSA_main$social_security_pension
KLoSA_main$pension_dummy <- ifelse(KLoSA_main$all_pension != 0, 1, 0)

attrited <- KLoSA_main


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
# (12754 - 4540)/12754 =  64%, 8214
notworking_wave6 <- NULL

# We select individuals worked as a paid employee at baseline.
employee <- KLoSA_main[KLoSA_main$wemp == 1 & KLoSA_main$wave == 6,] #1. employee #2. self-employed #3. unpaid family-related labor 
employee  <- subset(employee, select = c("pid"))
KLoSA_main  <- left_join(employee, KLoSA_main) #1057
# (4540 - 2167)/4540 = 52&, 2373
employee <- NULL

# We select individuals with main health outcome.
KLoSA_main <- KLoSA_main[!is.na(KLoSA_main$wmmse),]
# (2167 - 2075)/2167  = 4%, 92

#We calculate hourly wage from monthly wage and weekly working hours. 

KLoSA_main$ww <- KLoSA_main$wd_com052/(52/12) #weekly wage
KLoSA_main$hw <- KLoSA_main$ww/KLoSA_main$wd_com032 #hourly wage
KLoSA_main[!is.na(KLoSA_main$hw) & KLoSA_main$hw < 0,] <- NA #those without hourly wage information and negative hourly wage are considered as missing.

minimum_wage = 0.753 #minimum wage at year 2018

# Exclude individuals whose minimum wage at baseline was bigger than 150% of it.

attrition <- KLoSA_main[KLoSA_main$wave == 6 & KLoSA_main$hw > 0,]
attrition <- attrition[attrition$hw <= minimum_wage * 1.5 & attrition$wave == 6 ,]

attrition <- subset(attrition, select = c("pid"))
attrition <- semi_join(KLoSA_main, attrition) # (2075 - 1154)/2075 = 44%, 921

# Exclude unemployed individuals after the minimum wage increase.
# In the sensitivity analysis, we relax this condition. 

attrition <- attrition[!is.na(attrition$hw),]
KLoSA_main <- attrition # without unemployed individuals (1154 - 987)/1154 = 14%, 167 

# Include individuals with observations available in both waves.

KLoSA_main <- 
  KLoSA_main %>% 
  dplyr::mutate(wave = as.integer(wave)) %>% 
  group_by(pid) %>% 
  dplyr::mutate(grp = cumsum(c(1, diff(wave) != 1))) %>% 
  filter(n() >= 2)# without unemployed individuals (987 - 790)/987 = 20%, 197

# Intervention group assignment
# 1) Hourly wage at baseline is smaller than the minimum wage.
intervention_group <- KLoSA_main[KLoSA_main$hw < minimum_wage  & KLoSA_main$wave == 6 ,]
intervention_group <- subset(intervention_group, select = c("pid"))
intervention_group<- semi_join(KLoSA_main, intervention_group)

# In the sensitivity analysis, we put additional restriction
# 2) That the post minimum wage increase should fall between 90 - 120% of the new minimum wage.
#intervention_group <- MW[MW$hw >= 0.90 * (minimum_wage + increase) & MW$hw <= 1.20 * (minimum_wage + increase) & MW$wave == 7,] 

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
KLoSA_main$all_pension <- KLoSA_main$national_pension + KLoSA_main$specific_corporate_pension + KLoSA_main$social_security_pension
KLoSA_main$pension_dummy <- ifelse(KLoSA_main$all_pension != 0, 1, 0)

main <- KLoSA_main

main <- main[main$exposure == 1 & main$wave == 6,]
attrited <- attrited[attrited$exposure == 1 & attrited$wave == 6,]

attrited_true <- anti_join(attrited, main)

main$attrition <- "notattrited"
attrited_true$attrition <- "attrited"

KLoSA_main <- rbind(main, attrited_true)


####################################################################################################
## Descriptive statistics
####################################################################################################

library(srvyr)
library(gtsummary)
library(gtsummary)
library(tidyverse)

KLoSA_main$wA002_age <- as.numeric(KLoSA_main$wA002_age)
KLoSA_main$spouse <- ifelse(KLoSA_main$wmarital == 1, 1, 0)
KLoSA_main$wedu <- as.numeric(KLoSA_main$wedu)
KLoSA_main$income <- as.numeric(KLoSA_main$wd_com052) #income
KLoSA_main$working_days <- as.numeric(KLoSA_main$wd_com031) #working days
KLoSA_main$working_hours <- as.numeric(KLoSA_main$wd_com032) #working hours
KLoSA_main$mmse <- as.numeric(KLoSA_main$wmmse)
KLoSA_main$health <- as.numeric(KLoSA_main$health)

sel <- c("female", "wA002_age",  "spouse", 
         "working_days","working_hours", 
         "wedu",  "income",
          "mmse", "health",  "attrition")

statistics <-
  survey::svydesign(KLoSA_main$pid, data = subset(KLoSA_main , select = sel), weights = KLoSA_main$wwgt_c) %>%
  tbl_svysummary(by = attrition, statistic = list(all_continuous() ~ "{mean} ({sd})",
                                              all_categorical() ~ "{n}  ({p}%)"),
                 type = list(c("income",  "working_days", "working_hours") ~ "continuous"),
                 digits = all_continuous() ~ 2,)%>%
  add_p(test = list(all_continuous() ~ "svy.t.test",all_categorical() ~ "svy.wald.test")) 

statistics




