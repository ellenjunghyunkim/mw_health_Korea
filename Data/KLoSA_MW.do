clear all
set maxvar 30000
query memory

* I put the data inside of the Documents/CognitiveDecline. You create your own and change the name.
* cd means change the current working directory. 

cd "/Users/junghyun.kim/Documents/CognitiveDecline/KLoSA_STATA_2022v3/"

* Now we want to create longitudinal file. 

use "/Users/junghyun.kim/Documents/CognitiveDecline/KLoSA_STATA_2022v3/Lt05.dta"
sort pid
save data05, replace

use "/Users/junghyun.kim/Documents/CognitiveDecline/KLoSA_STATA_2022v3/Lt06.dta"
sort pid
save data06, replace

use "/Users/junghyun.kim/Documents/CognitiveDecline/KLoSA_STATA_2022v3/Lt07.dta"
sort pid
save data07, replace

use "/Users/junghyun.kim/Documents/CognitiveDecline/KLoSA_STATA_2022v3/Lt08.dta"
sort pid
save data08, replace

* We merge all the files with the same ID (pid). 

merge pid using data05 data06 data07 data08
save longitudinal_KLoSA

* Then, we want to subset the data set with variable of interests.

use "/Users/junghyun.kim/Documents/CognitiveDecline/KLoSA_STATA_2022v3/longitudinal_KLoSA.dta"

keep pid  w*C001 w*C126 w*C310  w*G111  w*mniw_y w*mniw_m w*gender1 w*A002_age w*edu w*job w*mmse w*present_labor w*marital w*wgt_c w*emp w*d_com031 w*d_com032 w*d_com052 w*wage w*C003 w*full_parttime w*smoke w*C401 w*C402 w*C406 w*C407 w*C408 w*C409 w*C410 w*C411 w*C412 w*C413 w*C414 w*C419 w*pinc w*earned w*assetinc w*publictrans w*personal w*transferfrom w*other w*socialsecurity w*senior_pension w*d_com085  

reshape long w@C001 w@C126 w@C310  w@G111  w@mniw_y w@mniw_m w@gender1 w@A002_age w@edu w@job w@mmse w@present_labor w@marital w@wgt_c w@emp w@d_com031 w@d_com032 w@d_com052 w@wage w@C003 w@full_parttime w@smoke w@C401 w@C402 w@C406 w@C407 w@C408 w@C409 w@C410 w@C411 w@C412 w@C413 w@C414 w@C419 w@pinc w@earned w@assetinc w@publictrans w@personal w@transferfrom w@other w@socialsecurity w@senior_pension w@d_com085, i(pid) j(wave) string

save KLoSA_main

**** variables
*C001 self-reported health
*C126 frequency of drinking Soju
*C310 private health insurance
*G111 social pension
*mniw_y mniw_m interview date
*d_com031 working days/week
*d_com032 working hours/week
*d_com52 average monthly wage
*wage last year income
*C003 diability diagnosis
*pinc total income last year
*earned earned income
*assetinc asset-related income
*personal personal pension
*transferfrom transfer from adult chidlren
*other other sources of income
*d_com085 job security

*****MMSE-related
*C401 ~ C419



* There are variables not available in the Lt(light) files. So we continue subsetting from more detailed and larger data set.
* Let's start from merging the data to make longitudinal file.

clear all
set maxvar 30000

use "w05.dta"
sort pid
save wdata05, replace

use "w06.dta"
sort pid
save wdata06, replace

use "w07.dta"
sort pid
save wdata07, replace

use "w08.dta"
sort pid
save wdata08, replace

* We merge all the files with the same ID (pid). 

merge pid using wdata05 wdata06 wdata07 wdata08
save wlongitudinal_KLoSA

*C403 MMSE (date - season)
*C404 MMSE (place - current location)
*C405 MMSE (place - below municipal level)
*C415 C416 C417 C418 MMSE (language)

*E033 national pension
*E044 specific corporate pension
*E055 private pension
*E070m5 social security pension
clear all

use "/Users/junghyun.kim/Documents/CognitiveDecline/KLoSA_STATA_2022v3/wlongitudinal_KLoSA.dta"

keep pid w*C403 w*C404 w*C405 w*C415 w*C416 w*C417 w*C418 w*E033 w*E044 w*E055 w*E070m5 

reshape long w@C403 w@C404 w@C405 w@C415 w@C416 w@C417 w@C418 w@E033 w@E044 w@E055 w@E070m5, i(pid) j(wave) string

save KLoSA_detail




