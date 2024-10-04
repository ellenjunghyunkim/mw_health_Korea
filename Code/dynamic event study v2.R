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

library(haven)
KLoSA_main <- read_dta("KLoSA_main.dta")
KLoSA_main$wave <- as.numeric(KLoSA_main$wave)
KLoSA_main <- KLoSA_main[!is.na(KLoSA_main$wwgt_c),]

KLoSA_detail <- read_dta("KLoSA_detail.dta")
KLoSA_detail$wave <- as.numeric(KLoSA_detail$wave)

KLoSA_main <- left_join(KLoSA_main, KLoSA_detail,by = c("pid", "wave"))

# We consider two waves, 5, 6 and 7.
KLoSA_main <- KLoSA_main[KLoSA_main$wave == 5 | KLoSA_main$wave == 6 |KLoSA_main$wave == 7,]
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

attrition <- KLoSA_main[KLoSA_main$wave == 6 & !is.na(KLoSA_main$hw) & KLoSA_main$hw > 0 ,]
attrition <- attrition[attrition$hw <= minimum_wage * 1.5 & attrition$wave == 6 ,]

attrition <- subset(attrition, select = c("pid"))
attrition <- semi_join(KLoSA_main, attrition) 

KLoSA_main <- attrition 

#Now we include individuals who were employed subsequent to the 2018 minimum wage.
attrition <- KLoSA_main[KLoSA_main$wave == 7 & !is.na(KLoSA_main$hw) & KLoSA_main$hw > 0 ,]
attrition <- subset(attrition, select = c("pid"))
attrition <- semi_join(KLoSA_main, attrition) 

KLoSA_main <- attrition 

KLoSA_main <- 
  KLoSA_main %>% 
  dplyr::mutate(wave = as.integer(wave)) %>% 
  group_by(pid) %>% 
  dplyr::mutate(grp = cumsum(c(1, diff(wave) != 1))) %>% 
  filter(n() >= 3) 

table(KLoSA_main$wave) # 446 individuals and 1338 observations. 
#The different between 462 individuals from the main analysis comes from excluding individuals who did not participate in wave 4.

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
KLoSA_main <- rbind(pre_exposure_data, KLoSA_main[KLoSA_main$wave == 7,], KLoSA_main[KLoSA_main$wave == 5,])
# Apply the same weights to the same ID
KLoSA_main <- KLoSA_main %>%
  group_by(pid) %>%
  mutate(weights = first(weights))

KLoSA_main$wA002_age <- as.numeric(KLoSA_main$wA002_age)
KLoSA_main$spouse <- ifelse(KLoSA_main$wmarital == 1, 1, 0)
KLoSA_main$wedu <- as.numeric(KLoSA_main$wedu)
KLoSA_main$income <- as.numeric(KLoSA_main$wd_com052) #income
KLoSA_main$working_days <- as.numeric(KLoSA_main$wd_com031) #working days
KLoSA_main$working_hours <- as.numeric(KLoSA_main$wd_com032) #working hours
KLoSA_main$mmse <- as.numeric(KLoSA_main$wmmse)
KLoSA_main$health <- as.numeric(KLoSA_main$health)


# Estimate the dynamic effects.

KLoSA_main$g <- case_when(KLoSA_main$exposure == 1 ~ 7,
                          KLoSA_main$exposure == 0 ~ 0)

install.packages("did2s")
library(did2s)

data = KLoSA_main
yname = "health"
idname = "pid"
gname = "g"
weights = "weights"
tname = "wave"
xformla = ~ factor(age_category) + public_trasfer_dummy + spouse

# Treat
data$zz000treat = 1 * (data[[tname]] >= data[[gname]]) * (data[[gname]] > 0)
data[is.na(data$zz000treat), "zz000treat"] = 0

# Set g to zero if NA
data[is.na(data[[gname]]), gname] = 0

# Create event time
data$zz000event_time = ifelse(
  is.na(data[[gname]]) | data[[gname]] == 0 | data[[gname]] == Inf,
  -Inf,
  as.numeric(data[[tname]] - data[[gname]])
)

event_time = unique(data$zz000event_time)
event_time = event_time[!is.na(event_time) & is.finite(event_time)]

# Format xformla for inclusion
if(!is.null(xformla)) {
  xformla_null = paste0("0 + ", as.character(xformla)[[2]])
} else {
  xformla_null = "0"
}

# Format xformla for inclusion


twfe_formula = stats::as.formula(
  paste0(
    yname, " ~ 1 + ", xformla_null, " + i(zz000event_time, ref = c(-1, -Inf)) | ", idname, " + ", tname
  ))

est_twfe = fixest::feols(twfe_formula, data = data, warn = F, notes = F, weights = data$weights)

# Extract coefficients and standard errors
tidy_twfe = broom::tidy(est_twfe)

# Extract zz000event_time
tidy_twfe = tidy_twfe[grep("zz000event_time::", tidy_twfe$term), ]

# Make event time into a numeric
tidy_twfe$term = as.numeric(gsub("zz000event_time::", "", tidy_twfe$term))

# Subset column
tidy_twfe = tidy_twfe[, c("term", "estimate", "std.error")]


did2s_first_stage = stats::as.formula(
  paste0(
    "~ 0 + ", xformla_null, " | ", idname, " + ", tname
  )
)

est_did2s = did2s::did2s(data, yname = yname, weights = weights, first_stage = did2s_first_stage, 
                         second_stage = ~i(zz000event_time, ref = c(-1, -Inf)), treatment = "zz000treat", 
                         cluster_var = idname, verbose = FALSE)

# Extract coefficients and standard errors
tidy_did2s = broom::tidy(est_did2s)

# Extract zz000event_time
tidy_did2s = tidy_did2s[grep("zz000event_time::", tidy_did2s$term), ]

# Make event time into a numeric
tidy_did2s$term = as.numeric(gsub("zz000event_time::", "", tidy_did2s$term))

# Subset columns
tidy_did2s = tidy_did2s[, c("term", "estimate", "std.error")]


out = data.table::rbindlist(list(
  "TWFE" = tidy_twfe,
  "Gardner (2021)" = tidy_did2s
), idcol = "estimator")
head(out)


# Get list of estimators
estimators = unique(out$estimator)

# Subset factor levels
levels = c("TWFE", "Gardner (2021)")
levels = levels[levels %in% estimators]

# Make estimator into factor
out$estimator = factor(out$estimator, levels = levels)


# Subset color scales
color_scale = c("TWFE" = "#374E55", "Gardner (2021)" = "#DF8F44")
color_scale = color_scale[names(color_scale) %in% estimators]


# create confidence intervals
out$ci_lower = out$estimate - 1.96 * out$std.error
out$ci_upper = out$estimate + 1.96 * out$std.error


# max and min of limits
y_lims = c(min(out$ci_lower), max(out$ci_upper)) * 1.05
x_lims = c(min(out$term) - 0.5, max(out$term) + 0.5)

library(ggplot2)

ggplot(data = out,
       mapping = ggplot2::aes(
         x = .data$term, y = .data$estimate,
         color = .data$estimator,
         ymin = .data$ci_lower, ymax = .data$ci_upper
       )
) +
  ggplot2::geom_point(aes(x = -1, y = 0), size = 3) +
  ggplot2::facet_wrap(~ estimator, scales="free")  +
  ggplot2::geom_point(position = "identity", size = 3) +
  ggplot2::geom_errorbar(position = position_dodge(.4), width=.5) +
  ggplot2::geom_vline(xintercept = -0.5, linetype = "dashed", color = "darkgrey") +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey") +
  ggplot2::labs(y = "Self-reported health", x = "Waves since 2018 minimum wage", color = "Estimator") +
  ggplot2::scale_y_continuous(limits = y_lims)  +
  ggplot2::scale_x_continuous(limits = x_lims)  +
  ggplot2::theme_minimal(base_size = 23) +
  ggplot2::theme(
    text = element_text(size = 23),       # General text size
    axis.title = element_text(size = 23), # Axis title size
    axis.text = element_text(size = 23),  # Axis tick label size
    strip.text = element_blank(),  # Legend title size
    legend.text = element_text(size = 23),    # Legend text size
    legend.title = element_text(size = 23),    # Legend text size
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 23, margin = margin(b = 40)),
    # Background and border settings
    panel.background = element_rect(fill = "white", color = "darkgrey"), # White background and black border
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    #panel.border = element_rect(color = "black", fill = NA, size = 1), # Black border around the panel
    #plot.background = element_rect(fill = "transparent", color = NA)  # Transparent plot background
  )  +
 ggplot2::ggtitle("Effects of a large minimum wage increase on self-reported health")

data = KLoSA_main
yname = "health"
idname = "pid"
gname = "g"
weights = "weights"
tname = "wave"
xformla = ~ factor(age_category) + public_trasfer_dummy + spouse

# Treat
data$zz000treat = 1 * (data[[tname]] >= data[[gname]]) * (data[[gname]] > 0)
data[is.na(data$zz000treat), "zz000treat"] = 0

# Set g to zero if NA
data[is.na(data[[gname]]), gname] = 0

# Create event time
data$zz000event_time = ifelse(
  is.na(data[[gname]]) | data[[gname]] == 0 | data[[gname]] == Inf,
  -Inf,
  as.numeric(data[[tname]] - data[[gname]])
)

event_time = unique(data$zz000event_time)
event_time = event_time[!is.na(event_time) & is.finite(event_time)]

# Format xformla for inclusion
if(!is.null(xformla)) {
  xformla_null = paste0("0 + ", as.character(xformla)[[2]])
} else {
  xformla_null = "0"
}

# Format xformla for inclusion


twfe_formula = stats::as.formula(
  paste0(
    yname, " ~ 1 + ", xformla_null, " + i(zz000event_time, ref = c(-1, -Inf)) | ", idname, " + ", tname
  ))

est_twfe = fixest::feols(twfe_formula, data = data, warn = F, notes = F, weights = data$weights)

# Extract coefficients and standard errors
tidy_twfe = broom::tidy(est_twfe)

# Extract zz000event_time
tidy_twfe = tidy_twfe[grep("zz000event_time::", tidy_twfe$term), ]

# Make event time into a numeric
tidy_twfe$term = as.numeric(gsub("zz000event_time::", "", tidy_twfe$term))

# Subset column
tidy_twfe = tidy_twfe[, c("term", "estimate", "std.error")]


did2s_first_stage = stats::as.formula(
  paste0(
    "~ 0 + ", xformla_null, " | ", idname, " + ", tname
  )
)

est_did2s = did2s::did2s(data, yname = yname, weights = weights, first_stage = did2s_first_stage, second_stage = ~i(zz000event_time, ref = c(-1, -Inf)), treatment = "zz000treat", cluster_var = idname, verbose = FALSE)

# Extract coefficients and standard errors
tidy_did2s = broom::tidy(est_did2s)

# Extract zz000event_time
tidy_did2s = tidy_did2s[grep("zz000event_time::", tidy_did2s$term), ]

# Make event time into a numeric
tidy_did2s$term = as.numeric(gsub("zz000event_time::", "", tidy_did2s$term))

# Subset columns
tidy_did2s = tidy_did2s[, c("term", "estimate", "std.error")]


out = data.table::rbindlist(list(
  "TWFE" = tidy_twfe,
  "Gardner (2021)" = tidy_did2s
), idcol = "estimator")
head(out)


# Get list of estimators
estimators = unique(out$estimator)

# Subset factor levels
levels = c("TWFE", "Gardner (2021)")
levels = levels[levels %in% estimators]

# Make estimator into factor
out$estimator = factor(out$estimator, levels = levels)


# Subset color scales
color_scale = c("TWFE" = "#374E55", "Gardner (2021)" = "#DF8F44")
color_scale = color_scale[names(color_scale) %in% estimators]


# create confidence intervals
out$ci_lower = out$estimate - 1.96 * out$std.error
out$ci_upper = out$estimate + 1.96 * out$std.error


# max and min of limits
y_lims = c(min(out$ci_lower), max(out$ci_upper)) * 1.05
x_lims = c(min(out$term) - 0.5, max(out$term) + 0.5)

library(ggplot2)

ggplot(data = out,
       mapping = ggplot2::aes(
         x = .data$term, y = .data$estimate,
         color = .data$estimator,
         ymin = .data$ci_lower, ymax = .data$ci_upper
       )
) +
  ggplot2::geom_point(aes(x = -1, y = 0), size = 3) +
  ggplot2::facet_wrap(~ estimator, scales="free")  +
  ggplot2::geom_point(position = "identity", size = 3) +
  ggplot2::geom_errorbar(position = position_dodge(.4), width=.5) +
  ggplot2::geom_vline(xintercept = -0.5, linetype = "dashed", color = "darkgrey") +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey") +
  ggplot2::labs(y = "Self-reported Health", x = "Waves since 2018 minimum wage", color = "Estimator") +
  ggplot2::scale_y_continuous(limits = y_lims)  +
  ggplot2::scale_x_continuous(limits = x_lims)  +
  ggplot2::theme_minimal(base_size = 23) +
  ggplot2::theme(
    text = element_text(size = 23),       # General text size
    axis.title = element_text(size = 23), # Axis title size
    axis.text = element_text(size = 23),  # Axis tick label size
    strip.text = element_blank(),  # Legend title size
    legend.text = element_text(size = 23),    # Legend text size
    legend.title = element_text(size = 23),    # Legend text size
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 23, margin = margin(b = 40)),
    # Background and border settings
    panel.background = element_rect(fill = "white", color = "darkgrey"), # White background and black border
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    #panel.border = element_rect(color = "black", fill = NA, size = 1), # Black border around the panel
    #plot.background = element_rect(fill = "transparent", color = NA)  # Transparent plot background
  ) +
  ggplot2::ggtitle("Effect of 2018 Minimum Wage on Self-reported Health")

