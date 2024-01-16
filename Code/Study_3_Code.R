################################**Load Packages**###############################
packages <- c( "dplyr", "expss","Hmisc","lavaan", "lm.beta", "openxlsx", "psych", "report", "scatterPlotMatrix", "semTable", "splithalfr", "tidySEM")
for (package in packages){
  if(!is.element(package, .packages(all.available = TRUE))){install.packages(package)}
  library(package, character.only = TRUE)}

set.seed(1234)   
options(warn = -1)
################################**Read In Data**################################
data <- read.xlsx(Sys.glob("C:\\Users\\*\\Downloads\\Waldren_Cortex_Study3.xlsx"))

#Inspect data 
str(data) 
psych::describe(data)                 

##**Create new variables**##
#Attention variables:
data$Stroop_Z <- as.vector(scale(data$Stroop_Points))                            
data$Simon_Z <- as.vector(scale(data$Simon_Points))
data$Flanker_Z <- as.vector(scale(data$Flanker_Points))
data$Attention_Mean <- mean_row(data$Stroop_Z, data$Flanker_Z, data$Simon_Z)    #Composite Attention Control for correlations and regression 

#Trait variables: 
data$AQ_Z <- as.vector(scale(data$AQ28))                                          
data$ASRS_Z <- as.vector(scale(data$ASRS))
data$Switch <- mean_row(data$AQ28_4 , data$AQ28_8, data$AQ28_18 , data$AQ28_21) #Grouping items by the AQ28 subthemes
data$Imag <- mean_row(data$AQ28_3 , data$AQ28_6 , data$AQ28_11 ,data$AQ28_14 , data$AQ28_20 , data$AQ28_23 , data$AQ28_25 , data$AQ28_28)
data$NumPat <- mean_row(data$AQ28_5 , data$AQ28_7 , data$AQ28_13 , data$AQ28_16 , data$AQ28_22) 
data$Rout <- mean_row(data$AQ28_2 , data$AQ28_17 , data$AQ28_19)
data$SocSki <- mean_row(data$AQ28_1 , data$AQ28_9 , data$AQ28_10 , data$AQ28_12 , data$AQ28_24 , data$AQ28_26 , data$AQ28_27)
data$Inatt <- mean_row(data$ASRS_1 , data$ASRS_2 , data$ASRS_3 , data$ASRS_4 , data$ASRS_7 , data$ASRS_8 , data$ASRS_9 , data$ASRS_10 , data$ASRS_11)
data$Hyp <- mean_row(data$ASRS_5 , data$ASRS_6 , data$ASRS_12 , data$ASRS_13 , data$ASRS_14 , data$ASRS_15 , data$ASRS_16 , data$ASRS_17 , data$ASRS_18)

##**Reliability**##
#Attention Tasks
simon_alpha <- spearman_brown(na.omit(data$Simon_Even), na.omit(data$Simon_Odd)) #Split half reliability across even and odd trial performance
stroop_alpha <- spearman_brown(na.omit(data$Stroop_Even), na.omit(data$Stroop_Odd))
flanker_alpha <- spearman_brown(na.omit(data$Flanker_Even), na.omit(data$Flanker_Odd))
print(paste0("Simon: ", round(simon_alpha, 3), "  Stroop: ", round(stroop_alpha, 3), "  Flanker: ", round(flanker_alpha, 3)))

#AQ28
psych::alpha(data[1:28])[[1]] 
t(psych::omega(data[1:28], nfactor = 1)[1:5])

#ASRS
psych::alpha(data[30:47])[[1]] 
t(psych::omega(data[30:47], nfactor = 1)[1:5])

##**Correlations**##
cor <- data %>% dplyr::select(Attention_Mean, Switch, Imag, NumPat, Rout, SocSki, Inatt, Hyp, Age, Sex, Education)
cor_results <- rcorr(as.matrix(cor))
View(cor_results$r)
View(cor_results$P)

##**Regressions**##
adhd_reg <- lm(ASRS ~ Attention_Mean + Sex + Age + Education , data)
asd_reg <- lm(AQ28 ~ Attention_Mean + Sex + Age + Education , data)
att_reg <- lm(Attention_Mean ~ AQ_Z * ASRS_Z + Sex + Age + Education , data)

summary(asd_reg)
lm.beta(asd_reg)

summary(adhd_reg)
lm.beta(adhd_reg)

summary(att_reg)
lm.beta(att_reg)

####################################**SEM**#####################################
ordinal_indicators <- c("ASRS_1", "ASRS_2", "ASRS_3", "ASRS_4", "ASRS_5", "ASRS_6",    #Accounting for Likert data                
                        "ASRS_7", "ASRS_8", "ASRS_9", "ASRS_10", "ASRS_11", "ASRS_12",
                        "ASRS_13", "ASRS_14", "ASRS_15", "ASRS_16", "ASRS_17", "ASRS_18",
                        "AQ28_1", "AQ28_2", "AQ28_3", "AQ28_4", "AQ28_5", "AQ28_6",
                        "AQ28_7", "AQ28_8", "AQ28_9", "AQ28_10", "AQ28_11", "AQ28_12",
                        "AQ28_13", "AQ28_14", "AQ28_15", "AQ28_16", "AQ28_17", "AQ28_18",
                        "AQ28_19", "AQ28_20", "AQ28_21", "AQ28_22", "AQ28_23", "AQ28_24",
                        "AQ28_25", "AQ28_26", "AQ28_27", "AQ28_28", "Education")

##**Confirm Factor Structures**##
#Attention CFA
attention <- 'Attention =~ Simon_Points + Stroop_Points + Flanker_Points'
attention_fit <- cfa(attention, data = data, std.lv = T)
summary(attention_fit,fit.measures = T, standardized = T)

#Autism CFA
autism <- 'Switching =~ AQ28_4 + AQ28_8+ AQ28_18 + AQ28_21                      
           Imagination =~  AQ28_3 + AQ28_6 + AQ28_11 + AQ28_14 + AQ28_20 + AQ28_23 + AQ28_25 + AQ28_28
           Number_Patterns =~ AQ28_5 + AQ28_7 + AQ28_13 + AQ28_16 + AQ28_22 
           Routines =~ AQ28_2 + AQ28_17 + AQ28_19
           Social_Skills =~ AQ28_1 + AQ28_9 + AQ28_10 + AQ28_12 + AQ28_15 + AQ28_24 + AQ28_26 + AQ28_27'

autism_fit <- cfa(autism, data = data, std.lv = T, ordered = T,  estimator = "WLSMV") 
summary(autism_fit, fit.measures = T, standardized = T)

#ADHD CFA
ADHD <- 'Inattention =~ ASRS_1 + ASRS_2 + ASRS_3 + ASRS_4 + ASRS_7 + ASRS_8 + ASRS_9 + ASRS_10 + ASRS_11
         Hyper =~ ASRS_5 + ASRS_6 + ASRS_12 + ASRS_13 + ASRS_14 + ASRS_15 + ASRS_16 + ASRS_17 + ASRS_18'

ADHD_fit <- cfa(ADHD, data = data, std.lv = T, ordered = T,  estimator = "WLSMV")
summary(ADHD_fit, fit.measures = T, standardized = T)

##**Latent Correlations**##
lat_cor <- 'Switching =~ AQ28_4 + AQ28_8+ AQ28_18 + AQ28_21                      
            Imagination =~  AQ28_3 + AQ28_6 + AQ28_11 + AQ28_14 + AQ28_20 + AQ28_23 + AQ28_25 + AQ28_28
            Number_Patterns =~ AQ28_5 + AQ28_7 + AQ28_13 + AQ28_16 + AQ28_22 
            Routines =~ AQ28_2 + AQ28_17 + AQ28_19
            Social_Skills =~ AQ28_1 + AQ28_9 + AQ28_10 + AQ28_12 + AQ28_15 + AQ28_24 + AQ28_26 + AQ28_27
            Inattention =~ ASRS_1 + ASRS_2 + ASRS_3 + ASRS_4 + ASRS_7 + ASRS_8 + ASRS_9 + ASRS_10 + ASRS_11
            Hyper =~ ASRS_5 + ASRS_6 + ASRS_12 + ASRS_13 + ASRS_14 + ASRS_15 + ASRS_16 + ASRS_17 + ASRS_18
            Attention =~ Simon_Points + Stroop_Points + Flanker_Points'

latcor_fit <- cfa(lat_cor, data, ordered = ordinal_indicators, estimator = "WLSMV")
summary(latcor_fit, fit.measures = T, standardized = T)

##**Autism + ADHD predicting Attention Control**##
#Null Model
null1 <- 'Switching =~ AQ28_4 + AQ28_8+ AQ28_18 + AQ28_21                      
         Imagination =~  AQ28_3 + AQ28_6 + AQ28_11 + AQ28_14 + AQ28_20 + AQ28_23 + AQ28_25 + AQ28_28
         Number_Patterns =~ AQ28_5 + AQ28_7 + AQ28_13 + AQ28_16 + AQ28_22 
         Routines =~ AQ28_2 + AQ28_17 + AQ28_19
         Social_Skills =~ AQ28_1 + AQ28_9 + AQ28_10 + AQ28_12 + AQ28_15 + AQ28_24 + AQ28_26 + AQ28_27
         Inattention =~ ASRS_1 + ASRS_2 + ASRS_3 + ASRS_4 + ASRS_7 + ASRS_8 + ASRS_9 + ASRS_10 + ASRS_11
         Hyper =~ ASRS_5 + ASRS_6 + ASRS_12 + ASRS_13 + ASRS_14 + ASRS_15 + ASRS_16 + ASRS_17 + ASRS_18
         Attention =~ Simon_Points + Stroop_Points + Flanker_Points
         Attention ~ 0*Switching + 0*Imagination + 0*Number_Patterns + 0*Routines + 0*Social_Skills + 0*Inattention + 0*Hyper
         Attention ~ Age + Sex + Education'

null1_fit <- sem(null1, data, ordered = ordinal_indicators, estimator = "WLSMV")
summary(null1_fit, fit.measures = T, standardized = T)

#SEM
sem1 <- 'Switching =~ AQ28_4 + AQ28_8+ AQ28_18 + AQ28_21                      
         Imagination =~  AQ28_3 + AQ28_6 + AQ28_11 + AQ28_14 + AQ28_20 + AQ28_23 + AQ28_25 + AQ28_28
         Number_Patterns =~ AQ28_5 + AQ28_7 + AQ28_13 + AQ28_16 + AQ28_22 
         Routines =~ AQ28_2 + AQ28_17 + AQ28_19
         Social_Skills =~ AQ28_1 + AQ28_9 + AQ28_10 + AQ28_12 + AQ28_15 + AQ28_24 + AQ28_26 + AQ28_27
         Inattention =~ ASRS_1 + ASRS_2 + ASRS_3 + ASRS_4 + ASRS_7 + ASRS_8 + ASRS_9 + ASRS_10 + ASRS_11
         Hyper =~ ASRS_5 + ASRS_6 + ASRS_12 + ASRS_13 + ASRS_14 + ASRS_15 + ASRS_16 + ASRS_17 + ASRS_18
         Attention =~ Simon_Points + Stroop_Points + Flanker_Points
         Attention ~ Switching + Imagination + Number_Patterns + Routines + Social_Skills + Inattention + Hyper + Age + Sex + Education'

sem1_fit <- sem(sem1, data,  std.lv = T, ordered = ordinal_indicators, estimator = "WLSMV")
summary(sem1_fit, fit.measures = T, standardized = T)

#Compare model fit to null
lavTestLRT(sem1_fit, null1_fit)

##**Attention Control predicting Autism + ADHD**##
#Null Model
null2 <- 'Switching =~ AQ28_4 + AQ28_8+ AQ28_18 + AQ28_21                      
         Imagination =~  AQ28_3 + AQ28_6 + AQ28_11 + AQ28_14 + AQ28_20 + AQ28_23 + AQ28_25 + AQ28_28
         Number_Patterns =~ AQ28_5 + AQ28_7 + AQ28_13 + AQ28_16 + AQ28_22 
         Routines =~ AQ28_2 + AQ28_17 + AQ28_19
         Social_Skills =~ AQ28_1 + AQ28_9 + AQ28_10 + AQ28_12 + AQ28_15 + AQ28_24 + AQ28_26 + AQ28_27
         Inattention =~ ASRS_1 + ASRS_2 + ASRS_3 + ASRS_4 + ASRS_7 + ASRS_8 + ASRS_9 + ASRS_10 + ASRS_11
         Hyper =~ ASRS_5 + ASRS_6 + ASRS_12 + ASRS_13 + ASRS_14 + ASRS_15 + ASRS_16 + ASRS_17 + ASRS_18
         Attention =~ Simon_Points + Stroop_Points + Flanker_Points
         Switching + Imagination + Number_Patterns + Routines + Social_Skills + Inattention + Hyper ~ 0*Attention 
         Switching + Imagination + Number_Patterns + Routines + Social_Skills + Inattention + Hyper ~ Age + Sex + Education'

null2_fit <- sem(null2, data, ordered = ordinal_indicators, estimator = "WLSMV")
summary(null2_fit, fit.measures = T, standardized = T)

#SEM
sem2 <- 'Switching =~ AQ28_4 + AQ28_8+ AQ28_18 + AQ28_21                      
        Imagination =~  AQ28_3 + AQ28_6 + AQ28_11 + AQ28_14 + AQ28_20 + AQ28_23 + AQ28_25 + AQ28_28
        Number_Patterns =~ AQ28_5 + AQ28_7 + AQ28_13 + AQ28_16 + AQ28_22 
        Routines =~ AQ28_2 + AQ28_17 + AQ28_19
        Social_Skills =~ AQ28_1 + AQ28_9 + AQ28_10 + AQ28_12 + AQ28_15 + AQ28_24 + AQ28_26 + AQ28_27
        Inattention =~ ASRS_1 + ASRS_2 + ASRS_3 + ASRS_4 + ASRS_7 + ASRS_8 + ASRS_9 + ASRS_10 + ASRS_11
        Hyper =~ ASRS_5 + ASRS_6 + ASRS_12 + ASRS_13 + ASRS_14 + ASRS_15 + ASRS_16 + ASRS_17 + ASRS_18
        Attention =~ Simon_Points + Stroop_Points + Flanker_Points
        Switching + Imagination + Number_Patterns + Routines + Social_Skills + Inattention + Hyper ~ Attention + Age + Sex + Education'

sem2_fit <- sem(sem2, data, std.lv = T, ordered = ordinal_indicators, estimator = "WLSMV")
summary(sem2_fit, fit.measures = T, standardized = T)

#Compare model fit to null
lavTestLRT(sem2_fit, null2_fit)

##**Confirm Transdiagnostic Factor**##
#Null Model
#Confirm null factor structure still okay after adding Transdiagnostic Factor
bridge_null <- 'Switching =~ AQ28_4 + AQ28_8+ AQ28_18 + AQ28_21                      
                Imagination =~  AQ28_3 + AQ28_6 + AQ28_11 + AQ28_14 + AQ28_20 + AQ28_23 + AQ28_25 + AQ28_28
                Number_Patterns =~ AQ28_5 + AQ28_7 + AQ28_13 + AQ28_16 + AQ28_22 
                Routines =~ AQ28_2 + AQ28_17 + AQ28_19
                Social_Skills =~ AQ28_1 + AQ28_9 + AQ28_10 + AQ28_12 + AQ28_15 + AQ28_24 + AQ28_26 + AQ28_27
                Inattention =~ ASRS_1 + ASRS_2 + ASRS_3 + ASRS_4 + ASRS_7 + ASRS_8 + ASRS_9 + ASRS_10 + ASRS_11
                Hyper =~ ASRS_5 + ASRS_6 + ASRS_12 + ASRS_13 + ASRS_14 + ASRS_15 + ASRS_16 + ASRS_17 + ASRS_18
                Bridge =~ 0*AQ28_4 + 0*AQ28_21 + 0*AQ28_26 + 0*ASRS_4 + 0*ASRS_11 + 0*ASRS_9'

bridge_null_fit <- cfa(bridge_null, data, std.lv = T, ordered = ordinal_indicators, estimator = "WLSMV")
summary(bridge_null_fit, fit.measures = T, standardized = T)

#CFA
#Inspect model fit when adding a Transdiagnostic factor 
bridge_cfa <- 'Imagination =~  AQ28_3 + AQ28_6 + AQ28_11 + AQ28_14 + AQ28_20 + AQ28_23 + AQ28_25 + AQ28_28
               Number_Patterns =~ AQ28_5 + AQ28_7 + AQ28_13 + AQ28_16 + AQ28_22 
               Routines =~ AQ28_2 + AQ28_17 + AQ28_19
               Switching =~ AQ28_4 + AQ28_8+ AQ28_18 + AQ28_21                      
               Social_Skills =~ AQ28_1 + AQ28_9 + AQ28_10 + AQ28_12 + AQ28_15 + AQ28_24 + AQ28_26 + AQ28_27
               Inattention =~ ASRS_1 + ASRS_2 + ASRS_3 + ASRS_4 + ASRS_7 + ASRS_8 + ASRS_9 + ASRS_10 + ASRS_11
               Hyper =~ ASRS_5 + ASRS_6 + ASRS_12 + ASRS_13 + ASRS_14 + ASRS_15 + ASRS_16 + ASRS_17 + ASRS_18
               Bridge =~ AQ28_4 + AQ28_21 + AQ28_26 + ASRS_4 + ASRS_11 + ASRS_9'

bridge_cfa_fit <- cfa(bridge_cfa, data, std.lv = T, ordered = ordinal_indicators, estimator = "WLSMV")
summary(bridge_cfa_fit, fit.measures = T, standardized = T)

#Compare model fit to null 
lavTestLRT(bridge_cfa_fit, bridge_null_fit)   #Significant improvement in model fit from addition of bridge factor

##**Attention Control predicting Transdiagnostic Factor**##
#SEM
sem3 <- 'Attention =~ Simon_Points + Stroop_Points + Flanker_Points
         Imagination =~  AQ28_3 + AQ28_6 + AQ28_11 + AQ28_14 + AQ28_20 + AQ28_23 + AQ28_25 + AQ28_28
         Number_Patterns =~ AQ28_5 + AQ28_7 + AQ28_13 + AQ28_16 + AQ28_22 
         Routines =~ AQ28_2 + AQ28_17 + AQ28_19
         Switching =~ AQ28_4 + AQ28_8+ AQ28_18 + AQ28_21                      
         Social_Skills =~ AQ28_1 + AQ28_9 + AQ28_10 + AQ28_12 + AQ28_15 + AQ28_24 + AQ28_26 + AQ28_27
         Inattention =~ ASRS_1 + ASRS_2 + ASRS_3 + ASRS_4 + ASRS_7 + ASRS_8 + ASRS_9 + ASRS_10 + ASRS_11
         Hyper =~ ASRS_5 + ASRS_6 + ASRS_12 + ASRS_13 + ASRS_14 + ASRS_15 + ASRS_16 + ASRS_17 + ASRS_18
         Bridge =~ AQ28_4 + AQ28_21 + AQ28_26 + ASRS_4 + ASRS_11 + ASRS_9
         Bridge ~ Attention + Age + Sex + Education
         Switching + Imagination + Number_Patterns + Social_Skills + Routines + Inattention + Hyper ~ 0*Attention'

sem3_fit<- sem(sem3, data, std.lv = T, ordered = ordinal_indicators, estimator = "WLSMV")
summary(sem3_fit, fit.measures = T, standardized = T)
####################################**End**#####################################
session <- sessionInfo()
report_system(session)
as.data.frame(report::report(session))