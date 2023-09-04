## Description of the data
- The codes and data are for the paper Health effects of a minimum wage hike: Evidence from South Korea experiments, Kim, Jung Hyun and Suhrcke, Marc and and Leist, Anja (2023).
-  We use The Korean Longitudinal Study of Aging (KLoSA). 

### Data
- KLoSA 1-8th wave (STATA) (https://survey.keis.or.kr/eng/klosa/databoard/List.jsp).

### Software
- STATA code is used to prepare the dataset and R code for the analysis.
- STATA version 17.0, R version 4.3.1 are used.

#$ Steps to reproduce
### Data code (in STATA)
KLoSA_MW.do subsets the variables of interest from the KLoSA longitudinal data.

### Main Estimation code (in R)
1. Use 1. Final data.R to get the final analytical samples.
2. Use 2. Descriptive statistics.R to generate the descriptive statistics. 
3. Use 3-1. Main estimation.R to replicate the main results with 2016 to 2018 survey.
4. Use 3-2. Proxy estimation.R to replicate the placebo results with 2014 to 2016 survey.

If you encounter any errors/problems reproducing the results, please send Jung Hyun an email.

#### Updates 
