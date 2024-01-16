# Unpacking the Overlap between Autism and ADHD in Adults: A Mutli-Method Approach

This Supplementary Materials document contains the analysis code and data associated with Waldren et al. (2024). 
Please see the relevent sections to find the information of interest.


## Table of Contents

### [Network Analysis Code](https://github.com/Lucy-Wal/Autism_ADHD_Adults#network-analysis-code-1)
Network Analysis Code contains the commented analysis script for Study 1, Study 2, and the complimentary redundant node analyses. As similar procedures are run in each Study, many of the analyses have been formatted into a function. Guidance on how to implement this for each data set is commented in the script. Associated data can be found in the Data section.


### [Study 3 Code](https://github.com/Lucy-Wal/Autism_ADHD_Adults#study-3-code-1)  
Study 3 Code contains the commented analysis script for the analyses performed in Study 3. Associated data can be found in the Data section.


### [Data](https://github.com/Lucy-Wal/Autism_ADHD_Adults#data-1)  
Data contains the open-access data sets to accompany all three Studies. Data structure and relevant information has also been reported.



##   



### Network Analysis Code
https://github.com/Lucy-Wal/Autism_ADHD_Adults/blob/3477f62c13ae49b949e13d6ef51aad55b65426e1/Code/Network_Analysis_Code.R#L1-L245

### Study 3 Code 
https://github.com/Lucy-Wal/Autism_ADHD_Adults/blob/3477f62c13ae49b949e13d6ef51aad55b65426e1/Code/Study_3_Code.R#L1-L232

### Data

#### Study 1
- AQ28_1-AQ28_28: Likert AQ28 responses (1-4, already reverse scored where appropriate)
- AQ28: Total AQ28 score for each participant
- ASRS_1-ASRS_18: Likert ASRS responses (0-4)
- ASRS: Total ASRS score for each participant
- Sex: Sex at Birth (0 = Female, 1 = Male)
- Age: Years
- Education: 0 = No formal Education, 7 = PhD (excluding level “Primary Education but no Qualifications”; for further details see UNESCO Institute for Statistics, 2012)

[Download Study 1 data (.xlsx)](https://github.com/Lucy-Wal/Autism_ADHD_Adults/blob/main/Data/Waldren_Cortex_Study1.xlsx)
#### Study 2
- AQ28_1-AQ28_28: Likert AQ28 responses (1-4, already reverse scored where appropriate)
- AQ28: Total AQ28 score for each participant
- ASRS_1-ASRS_18: Likert ASRS responses (0-4)
- ASRS: Total ASRS score for each participant
- Sex: Sex at Birth (0 = Female, 1 = Male, 999 = Neither male nor female)
- Age: Years
- Education: 0 = No formal Education, 8 = PhD, 9 = Other (for further details see UNESCO Institute for Statistics, 2012)
- No_Condition-Other: Dummy Variables (0 = No, 1 = Yes; for the mental health condition checklist see Supplementary Materials S.M.1.)
- CPDC_Total: Total number of diagnosed conditions for each participant

[Download Study 2 data (.xlsx)](https://github.com/Lucy-Wal/Autism_ADHD_Adults/blob/main/Data/Waldren_Cortex_Study2.xlsx)
#### Study 3
- AQ28_1-AQ28_28: Likert AQ28 responses (1-4, already reverse scored where appropriate)
- AQ28: Total AQ28 score for each participant
- ASRS_1-ASRS_18: Likert ASRS responses (0-4)
- ASRS: Total ASRS score for each participant
- Sex: Sex at Birth (0 = Female, 1 = Male)
- Age: Years
- Education: 0 = No formal Education, 8 = PhD (for further details see UNESCO Institute for Statistics, 2012)
- Stroop/Flanker/Simon_Points: Performance score for each task in the Three Minute Squared Tasks (Burgoyne et al., 2023)
- Stroop/Flanker/Simon_N: Total Number of trials each participant completed in each task
- Stroop/Flanker/Simon_Even: Performance score across all even trials in each task
- Stroop/Flanker/Simon_Odd: Performance score across all odd trials in each task

[Download Study 3 data (.xlsx)](https://github.com/Lucy-Wal/Autism_ADHD_Adults/blob/main/Data/Waldren_Cortex_Study3.xlsx)
