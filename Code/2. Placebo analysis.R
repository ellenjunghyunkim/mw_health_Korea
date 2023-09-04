
#download the data.
rm(list=ls())

library(haven)
KLoSA_MW <- read_dta("KLoSA_MW_augmented.dta")
stress <- read_dta("~/Documents/CognitiveDecline/KLoSA_STATA_2022v3/additional.dta")
KLoSA_MW$wave <- as.numeric(KLoSA_MW$wave)
stress$wave <- as.numeric(stress$wave)

KLoSA_MW <- left_join(KLoSA_MW, stress, by = c("pid", "wave"))
KLoSA_MW <- KLoSA_MW[KLoSA_MW$wave == 5,]
wave5 <- KLoSA_MW[!is.na(KLoSA_MW$wwgt_c),]
stress <- NULL
KLoSA_MW <- NULL

KLoSA_MW2 <- read_dta("KLoSA_MW_augmented.dta")
stress <- read_dta("~/Documents/CognitiveDecline/KLoSA_STATA_2022v3/additional.dta")
stress$wave <- as.numeric(stress$wave)
KLoSA_MW2$wave <- as.numeric(KLoSA_MW2$wave)

KLoSA_MW2 <- left_join(KLoSA_MW2, stress, by = c("pid", "wave"))
KLoSA_MW2 <- KLoSA_MW2[KLoSA_MW2$wave == 6,]
wave6 <- KLoSA_MW2[!is.na(KLoSA_MW2$wwgt_c),]

KLoSA_MW2 <- NULL
stress <- NULL

# Two consecutive survey participation.

KLoSA_MW <- rbind(wave5, wave6)

#missing information on working hours.
notworking_wave5 <- KLoSA_MW[!is.na(KLoSA_MW$wd_com032) & KLoSA_MW$wave == 5,]
notworking_wave5 <- subset(notworking_wave5, select = c("pid"))
KLoSA_MW  <- left_join(notworking_wave5, KLoSA_MW) #2211
# (12754 - 4540)/12754 =  64%, 8214
notworking_wave5 <- NULL

#only employee individuals at wave 6.
employee <- KLoSA_MW[KLoSA_MW$wemp == 1 & KLoSA_MW$wave == 5,] #1. employee #2. self-employed #3. unpaid family-related labor 
employee  <- subset(employee, select = c("pid"))
KLoSA_MW  <- left_join(employee, KLoSA_MW) #1057
# (4540 - 2167)/4540 = 52&, 2373
employee <- NULL
KLoSA_MW <- KLoSA_MW[!is.na(KLoSA_MW$wmmse),]
# (2167 - 2075)/2167  = 4%, 92

#hourly wage calculation

KLoSA_MW$ww <- KLoSA_MW$wd_com052/(52/12)
KLoSA_MW$hw <- KLoSA_MW$ww/KLoSA_MW$wd_com032
KLoSA_MW[!is.na(KLoSA_MW$hw) & KLoSA_MW$hw < 0,] <- NA

minimum_wage = 0.521
increase = 0.603 - 0.521

#exclude individuals whose minimum wage at 2016 was bigger than 150% of it.
attrition <- KLoSA_MW[KLoSA_MW$wave == 5 & KLoSA_MW$hw > 0,]
attrition <- attrition[attrition$hw <= minimum_wage * 1.5 & attrition$wave == 5 ,]

attrition <- subset(attrition, select = c("pid"))
attrition <- semi_join(KLoSA_MW, attrition) 

#including or excluding unemploymetn
attrition <- attrition[!is.na(attrition$hw),]
KLoSA_MW <- attrition
KLoSA_MW <- KLoSA_MW[!is.na(KLoSA_MW$pid),]

KLoSA_MW <- 
  KLoSA_MW %>% 
  dplyr::mutate(wave = as.integer(wave)) %>% 
  group_by(pid) %>% 
  dplyr::mutate(grp = cumsum(c(1, diff(wave) != 1))) %>% 
  filter(n() >= 2) 

MW <- KLoSA_MW[KLoSA_MW$hw < minimum_wage  & KLoSA_MW$wave == 5 ,]
MW <- subset(MW, select = c("pid"))
MW<- semi_join(KLoSA_MW, MW)

treated <- MW
#treated <- MW[MW$hw >= 0.90 * (minimum_wage + increase) & MW$hw <= 1.20 * (minimum_wage + increase) & MW$wave == 6,] 
treated  <- subset(treated , select = c("pid"))
treated <- semi_join(MW, treated)
treated$treatment <- 1
treated$group <- "intervention"

MW <- KLoSA_MW[KLoSA_MW$hw >= minimum_wage * 1.0 & KLoSA_MW$hw <= minimum_wage*1.5 & KLoSA_MW$wave == 5,] #157 individuals
MW <- subset(MW, select = c("pid"))
control1 <- semi_join(KLoSA_MW, MW)
control1$treatment <- 0
control1$group <- "control1"

KLoSA_MW <- rbind(treated, control1)
KLoSA_MW$time = ifelse(KLoSA_MW$wave == 6, 1, 0)
KLoSA_MW$treated = ifelse(KLoSA_MW$hw <= minimum_wage, 1, 0)
KLoSA_MW$did <- KLoSA_MW$time * KLoSA_MW$treatment


KLoSA_MW$registration <- case_when(KLoSA_MW$wC406 <= 3 ~ KLoSA_MW$wC406,
                                   KLoSA_MW$wC406 == 5 ~ 0 )

KLoSA_MW$recall <- case_when(KLoSA_MW$wC412 <= 3 ~ KLoSA_MW$wC412,
                             KLoSA_MW$wC412 == 5 ~ 0 )

KLoSA_MW$memory <- KLoSA_MW$registration + KLoSA_MW$recall #6 points


KLoSA_MW$year_month_date <- case_when(KLoSA_MW$wC401 <= 3 ~  KLoSA_MW$wC401,
                                      KLoSA_MW$wC401 == 5 ~ 0)

KLoSA_MW$day <- ifelse(KLoSA_MW$wC402 == 1, 1, 0)
KLoSA_MW$season <- ifelse(KLoSA_MW$wC403 == 1, 1, 0)

KLoSA_MW$temporal <- as.numeric(KLoSA_MW$year_month_date) + as.numeric(KLoSA_MW$day) + as.numeric(KLoSA_MW$season) #5 points

KLoSA_MW$county_district <- case_when(KLoSA_MW$wC405 <= 4 ~  KLoSA_MW$wC405,
                                      KLoSA_MW$wC405 == 5 ~ 0)
KLoSA_MW$wC404 <- as.numeric(KLoSA_MW$wC404)
KLoSA_MW$current_space <- ifelse(KLoSA_MW$wC404 == 1, 1, 0)

KLoSA_MW$place <- as.numeric(KLoSA_MW$county_district) + as.numeric(KLoSA_MW$current_space) #5 points


KLoSA_MW$wC407 <- ifelse(KLoSA_MW$wC407 == 1, 1, 0) #7 subtraction 1st.
KLoSA_MW$wC408 <- ifelse(KLoSA_MW$wC408 == 1, 1, 0)
KLoSA_MW$wC409 <- ifelse(KLoSA_MW$wC409 == 1, 1, 0)
KLoSA_MW$wC410 <- ifelse(KLoSA_MW$wC410 == 1, 1, 0)
KLoSA_MW$wC411 <- ifelse(KLoSA_MW$wC411 == 1, 1, 0) #7 subtraction 5th.

KLoSA_MW$attention_calculation<- KLoSA_MW$wC407 + KLoSA_MW$wC408 +KLoSA_MW$wC409 +KLoSA_MW$wC410 +KLoSA_MW$wC411 #5 points

KLoSA_MW$wC413 <- ifelse(KLoSA_MW$wC413 == 1, 1, 0) #7 subtraction 1st.
KLoSA_MW$wC414 <- ifelse(KLoSA_MW$wC414 == 1, 1, 0)
KLoSA_MW$wC415 <- ifelse(KLoSA_MW$wC415 == 1, 1, 0)
KLoSA_MW$order <- case_when(KLoSA_MW$wC416 <= 3 ~  KLoSA_MW$wC416,
                            KLoSA_MW$wC416 == 5 ~ 0)

KLoSA_MW$order2 <- case_when(KLoSA_MW$wC417 <= 3 ~ 1,
                             KLoSA_MW$wC417 == 5 ~ 0)

KLoSA_MW$wC418 <- ifelse(KLoSA_MW$wC418 == 1, 1, 0)


KLoSA_MW$language <- KLoSA_MW$wC413 +KLoSA_MW$wC414 +KLoSA_MW$wC415 + KLoSA_MW$order +KLoSA_MW$order2 +KLoSA_MW$wC418  #8 points


KLoSA_MW$visual_construction <- ifelse(KLoSA_MW$wC419 == 1, 1, 0)

KLoSA_MW$mmse_seperate <- KLoSA_MW$temporal + KLoSA_MW$place + KLoSA_MW$registration + KLoSA_MW$attention_calculation + KLoSA_MW$recall + 
  KLoSA_MW$language + KLoSA_MW$visual_construction 

#KLoSA_MW$mmse_validity <- KLoSA_MW$mmse_seperate - KLoSA_MW$wmmse


KLoSA_MW$social <- ifelse(KLoSA_MW$wA032 <= 5, 1, 0)
KLoSA_MW$physical <- ifelse(KLoSA_MW$wC108 == 1, 1, 0)

KLoSA_MW$health <- case_when(KLoSA_MW$wC001 == 1 ~ 5,#Best
                             KLoSA_MW$wC001 == 2 ~ 4,#Very good
                             KLoSA_MW$wC001 == 3 ~ 3,#Good
                             KLoSA_MW$wC001 == 4 ~ 2,#Normal
                             KLoSA_MW$wC001 == 5 ~ 1)#Bad

KLoSA_MW$mental_health <- case_when(KLoSA_MW$wC144 == 1 ~ 4,
                                    KLoSA_MW$wC144 == 2 ~ 3,
                                    KLoSA_MW$wC144 == 3 ~ 2,
                                    KLoSA_MW$wC144 == 4 ~ 1)

KLoSA_MW$workingdays <- KLoSA_MW$wd_com031

KLoSA_MW$age_category <- case_when(KLoSA_MW$wA002_age < 60 ~ 1,
                                   KLoSA_MW$wA002_age >= 60 & KLoSA_MW$wA002_age <65 ~ 2,
                                   KLoSA_MW$wA002_age >= 65 & KLoSA_MW$wA002_age <70 ~ 3,
                                   KLoSA_MW$wA002_age >= 70 & KLoSA_MW$wA002_age <75 ~ 4, 
                                   KLoSA_MW$wA002_age >= 75 ~ 5)

KLoSA_MW[is.na(KLoSA_MW$wpublictrans),]$wpublictrans <- 0
KLoSA_MW[KLoSA_MW$wpublictrans <= 0,]$wpublictrans <- 0
table(KLoSA_MW$wave, KLoSA_MW$treatment)

fit_plm <- plm(wmmse ~ did +  factor(age_category) + wpublictrans, 
               data = KLoSA_MW, 
               index = c("pid", "wave"), 
               model = "within", 
               effect = "twoways")

# Note how this is functionally identical to the lm() way 
coeftest(fit_plm, vcov = vcovHC, type = "HC1")

did.reg2 <- felm(health ~ did +  factor(age_category) + wpublictrans | factor(pid) + factor(wave), 
                 data = KLoSA_MW)
summary(did.reg2)


library(srvyr)
library(gtsummary)
library(gtsummary)
library(tidyverse)


KLoSA_MW$wA002_age <- as.numeric(KLoSA_MW$wA002_age)
KLoSA_MW$age_squared <- as.numeric(KLoSA_MW$wA002_age*KLoSA_MW$wA002_age)
KLoSA_MW$spouse <- ifelse(KLoSA_MW$wmarital == 1, 1, 0)
KLoSA_MW$wospouse <- ifelse(KLoSA_MW$wmarital != 1, 1, 0)
KLoSA_MW$wgender1 <- as.numeric(KLoSA_MW$wgender1)
KLoSA_MW$registration <- as.numeric(KLoSA_MW$registration)
KLoSA_MW$recall <- as.numeric(KLoSA_MW$recall)
KLoSA_MW$wA032 <- as.numeric(KLoSA_MW$wA032)
KLoSA_MW$wedu <- as.numeric(KLoSA_MW$wedu)
KLoSA_MW$wmarital <- as.numeric(KLoSA_MW$wmarital)
KLoSA_MW$wd_com052 <- as.numeric(KLoSA_MW$wd_com052) #income
KLoSA_MW$wd_com031 <- as.numeric(KLoSA_MW$wd_com031) #working days
KLoSA_MW$wd_com032 <- as.numeric(KLoSA_MW$wd_com032) #working hours
KLoSA_MW$edu <- ifelse(KLoSA_MW$wedu >= 3, 1, 0)
KLoSA_MW$mmse <- as.numeric(KLoSA_MW$wmmse)
KLoSA_MW$grip <- as.numeric(KLoSA_MW$wmgrip)
KLoSA_MW$health <- as.numeric(KLoSA_MW$health)
KLoSA_MW$bad_health <- ifelse(KLoSA_MW$health == 1, 1, 0)
KLoSA_MW$wpublictrans <- as.numeric(KLoSA_MW$wpublictrans)

sel <- c("wgender1", "wA002_age", "registration", "recall", "age_squared", "spouse", "wospouse",
         "wA032", "wd_com031","wd_com032", 
         "wedu",  "wd_com052",
         "hw", "edu", "group", "mmse", "grip", "health", "bad_health", "wpublictrans")

tbl_svysummary_ex1 <-
  survey::svydesign(KLoSA_MW[KLoSA_MW$wave == 5,]$pid, data = subset(KLoSA_MW[KLoSA_MW$wave == 5,] , select = sel), weights = NULL) %>%
  tbl_svysummary(by = group, statistic = list(all_continuous() ~ "{mean} ({sd})",
                                              all_categorical() ~ "{n}  ({p}%)"),
                 type = list(c("registration", "recall", "wd_com052", "hw", "wd_com031") ~ "continuous"),
                 digits = all_continuous() ~ 2,)%>%
  add_p(test = list(all_continuous() ~ "svy.t.test",all_categorical() ~ "svy.wald.test")) 








