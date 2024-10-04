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
intervention_group$group <- "Intervention"

# Control group assignment.
# The hourly wage should fall between 100 - 150% of the baseline minimum hourly wage. 

control_group <- KLoSA_main[KLoSA_main$hw >= minimum_wage * 1.0 & KLoSA_main$hw <= minimum_wage*1.5 & KLoSA_main$wave == 6,] 
control_group <- subset(control_group, select = c("pid"))
control_group <- semi_join(KLoSA_main, control_group)
control_group$exposure <- 0
control_group$group <- "Control"

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


final_sample <- KLoSA_main[KLoSA_main$wave == 6,] 

final_id <- subset(final_sample, select = c("pid", "group"))


KLoSA_main <- read_dta("KLoSA_main.dta")
KLoSA_main$wave <- as.numeric(KLoSA_main$wave)
KLoSA_main <- KLoSA_main[!is.na(KLoSA_main$wwgt_c),]

KLoSA_detail <- read_dta("KLoSA_detail.dta")
KLoSA_detail$wave <- as.numeric(KLoSA_detail$wave)

KLoSA_main <- left_join(KLoSA_main, KLoSA_detail,by = c("pid", "wave"))

KLoSA_main$health <- case_when(KLoSA_main$wC001 == 1 ~ 5,#Best
                               KLoSA_main$wC001 == 2 ~ 4,#Very good
                               KLoSA_main$wC001 == 3 ~ 3,#Good
                               KLoSA_main$wC001 == 4 ~ 2,#Normal
                               KLoSA_main$wC001 == 5 ~ 1)#Bad

merged_sample <- left_join(final_id, KLoSA_main, by = "pid")


mean_score <- merged_sample %>%
  group_by(wave, group) %>%
  summarize(Mean_Score = mean(wmmse, na.rm = TRUE))


mean_score$wave <- 2004 + 2*(mean_score$wave)


ggplot(mean_score, aes(x = wave, y = Mean_Score, color = group)) +
  geom_line(linewidth = 2, alpha = .8) +
  geom_point() +
  labs(x = "Year", y = "Mean K-MMSE", color = "Group") +
  ggtitle("2018 Minimum wage: Cognitive function trends") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20) ,
    axis.text.y = element_text(size = 20),  
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 20),  
    axis.title.x = element_text(size = 20),
    legend.title = element_text(size = 20), 
    legend.text = element_text(size = 20),
    legend.position = "bottom"
  ) +
  labs(color = "")










