## Description of the data
The codes and data are for the paper Health effects of a minimum wage hike: Evidence from South Korea experiments, Kim, Jung Hyun and Suhrcke, Marc and and Leist, Anja (2023).

### Data
- KLoSA 1-8th wave version 2022 v3 (STATA) (https://survey.keis.or.kr/eng/klosa/databoard/List.jsp).
- Please note that access to Mini-Mental Examination Score (MMSE) data is temporarily ceased due to the need for internal review for data disclosure (https://survey.keis.or.kr/madang/notice/Read.jsp?ntt_id=5634). 

### Software
- STATA code is used to prepare the dataset and R code for the analysis.
- STATA version 17.0, R version 4.3.1 are used.
- WeightIt pacakge version 0.14.2 (https://cran.r-project.org/web/packages/WeightIt/index.html) is used.

## Steps to reproduce
### Data code (in STATA)
KLoSA_MW.do subsets the variables of interest from the KLoSA longitudinal data.

### Main Estimation code (in R)
1. Use 1. Main analysis.R for the main estimations and summary statistics.
2. Use 2. Placebo analysis.R for the placebo sample estimations and summary statistics.
3. Use 3. Sensitivity analysis.R contains
   
    1. Alternative definition of control groups (110 to 150%)
    2. Analysis including individuals who were unemployed after the minimum wage increase.
    3. Alternative definition of intervention group (narrower)
    4. Alternative outcome variables. (Separate analysis by the cognitive domain)

If you encounter any errors/problems reproducing the results, please send Jung Hyun an email.

#### Updates 
