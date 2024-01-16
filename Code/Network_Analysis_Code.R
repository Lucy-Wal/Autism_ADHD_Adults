###############################**Load Packages**################################
packages <- c("bootnet", "dplyr", "expss", "ggplot2", "ggpubr", "gtools", "Hmisc", "NetworkComparisonTest", "networktools", "OpenMx", "openxlsx", "PerformanceAnalytics", "ppcor", "psych", "qgraph", "report", "scatterPlotMatrix", "wTO")
for (package in packages){
  if(!is.element(package, .packages(all.available = TRUE))){install.packages(package)}
  library(package, character.only = TRUE)}

set.seed(1234)   
options(warn = -1)
#########################**Network Analysis Function**##########################
##**Adjustable Parameters:**##
#The current script has pre-set these parameters to follow the analyses in Waldren et al. 2023. 
#No user input is required.

#data - A data set containing AQ28 and ASRS data
#lasso - A Boolean specifying if the network should be estimated using EBICglasso (T) or ggmModSelect (F)
#groups - A list specifying which nodes belong to each trait groups for network plotting
#AQ - A vector containing the column numbers for AQ28 items
#ASRS - A vector containing the column numbers for ASRS items 

analysis <- function(data, lasso, groups, AQ, ASRS){
  ##Reliability##
  alpha_Autism <- psych::alpha(data[AQ])[[1]]     
  alpha_ADHD <- psych::alpha(data[ASRS])[[1]]  
  
  omega_Autism <- t(psych::omega(data[AQ], nfactor = 1)[1:5])
  omega_ADHD <- t(psych::omega(data[ASRS], nfactor = 1)[1:5])
  
  ##Estimate Network##
  if(lasso == T){network <- estimateNetwork(data, default = "EBICglasso", corMethod = "spearman",tuning = 0.25, weighted = T)} #Similar networks produced across tuning parameters
  else(network <- estimateNetwork(data, default = "ggmModSelect", stepwise = T, corMethod = "spearman",tuning = 0.25, weighted = T))
  
  print(network)
  plot(network, layout = "spring", labels = colnames(numeric), groups = groups, 
       color = c("deepskyblue", "deepskyblue3", "paleturquoise", "lightseagreen", "lightskyblue1", "hotpink3", "lightpink2"), 
       label.prop = 2, label.scale.equal = T, label.fill.vertical = 1, label.fill.horizontal = 1,legend = F)
  
  ##Edge Weights##
  #All Edges
  all_edges <- network$graph                                                    #Data Frame of all possible edge combinations
  v_all_edges <- vech(all_edges)                                                #Create stacked half matrix of values so can subset and describe
  
  sig_edges <- subset(v_all_edges, v_all_edges != 0)                            #Subset only edges included in model (non-zero)                                  
  pos_edges <- subset(v_all_edges, v_all_edges > 0)                             #Only positive edges
  neg_edges <- subset(v_all_edges, v_all_edges <0)                              #Only negative edges
  
  print("All Significant Edges:"); print(psych::describe(sig_edges))
  print("All Positive Edges:"); print(psych::describe(pos_edges))
  print("All Negative Edges:"); print(psych::describe(neg_edges))
  
  #Autism-Autism Edges
  asd <- network$graph[c(AQ), c(AQ)]                                            #Repeat for Autism-Autism, ADHD-ADHD, and Autism-ADHD specific edges                             
  asd_asd <- vech(asd)    
  
  asd_pres <- subset(asd_asd, asd_asd != 0)                               
  asd_pos <- subset(asd_pres, asd_pres > 0)
  asd_neg <- subset(asd_pres, asd_pres < 0)
  
  print("Autism-Autism Significant Edges:"); print(psych::describe(asd_pres))
  print("Autism-Autism Positive Edges:"); print(psych::describe(asd_pos))
  print("Autism-Autism Negative Edges:"); print(psych::describe(asd_neg))
  
  asd_poss_edges <- length(AQ)*((length(AQ) -1)/2)                              #Number of possible Autism-Autism edges
  asd_density <- (length(asd_pres)/asd_poss_edges) *100                         #% of possible edges included in model
  print("Autism-Autism Edge Density:"); print(asd_density)
  
  #ADHD-ADHD Edges
  adhd <- network$graph[c(ASRS), c(ASRS)]                                       
  adhd_adhd <- vech(adhd)   
  
  adhd_pres <- subset(adhd_adhd, adhd_adhd != 0)                                 
  adhd_pos <- subset(adhd_pres, adhd_pres > 0)
  adhd_neg <- subset(adhd_pres, adhd_pres < 0)
  
  print("ADHD-ADHD Significant Edges:"); print(psych::describe(adhd_pres))
  print("ADHD-ADHD Positive Edges:"); print(psych::describe(adhd_pos))
  print("ADHD-ADHD Negative Edges:"); print(psych::describe(adhd_neg))
  
  adhd_poss_edges <- length(ASRS)*((length(ASRS)-1)/2)
  adhd_density <- (length(adhd_pres)/adhd_poss_edges) * 100
  print("ADHD-ADHD Edge Density:"); print(adhd_density)
  
  #Autism-ADHD Edges
  asd_adhd <- network$graph[c(AQ), c(ASRS)] 
  asd_adhd_pres <- as.vector(asd_adhd)                                          #Don't need stacked half matrix as no diagonal overlap
  
  asd_adhd_pres <- subset(asd_adhd_pres, asd_adhd_pres != 0)      
  asd_adhd_pos <- subset(asd_adhd_pres, asd_adhd_pres > 0)
  asd_adhd_neg <- subset(asd_adhd_pres, asd_adhd_pres < 0)
  
  print("Autism-ADHD Significant Edges:"); print(psych::describe(asd_adhd_pres))
  print("Autism-ADHD Positive Edges:"); print(psych::describe(asd_adhd_pos))
  print("Autism-ADHD Negative Edges:"); print(psych::describe(asd_adhd_neg))
  
  asd_adhd_density <- (length(asd_adhd_pres)/(length(AQ)*length(ASRS))) *100
  print("Autism-ADHD Edge Density:"); print(asd_adhd_density)
  
  #Edge Strength Comparison
  edge_comp <- data.frame(group = c(rep("Autism-Autism",length(asd_pres)), rep("ADHD-ADHD", length(adhd_pres)), rep("Autism-ADHD",length(asd_adhd_pres))), 
                          edges = c(asd_pres, adhd_pres, asd_adhd_pres))        #Format data for Kruskal Wallis and Wilcoxon paired t-tests
  
  print(kruskal.test(edges ~ group, data = edge_comp))                          #Test if significant change in edge strength between groups
  print(pairwise.wilcox.test(edge_comp$edges, edge_comp$group, p.adjust.method = "BH"))  #Follow up paired Wilcoxon comparisons for specific differences
  
  ggline(edge_comp, x = "group", y = "edges",                                   #Visualise difference
         add = c("mean_se", "jitter"), 
         order = c( "Autism-Autism", "ADHD-ADHD", "Autism-ADHD"),
         ylab = "Edge Weight", xlab = "Type of connection")                     #These analyses are in line with Farhat et al., 2022
  
  #Edge Weight Stability                                                        #Edge and Centrality stability are commented out given their long run time. Please uncomment if you require stability estimates
  #nonpara_boot <- bootnet(network, nBoots = 1000, nCores = 8)
  #plot(nonpara_boot, order = "sample", labels = F)                               
  #plot(nonpara_boot, plot = "difference", onlyNonZero = TRUE, order = "sample")
  
  ##Centrality##
  #Expected Influence
  cents <- centralityTable(network,  standardized = T)                          #Generate EI centrality                                                 
  centralityPlot(network, include = c("Closeness",  "ExpectedInfluence", "Betweenness"), scale = "z-scores")
  
  #Bridge Expected Influence
  bridge_groups <- list("ASD" = AQ, "ADHD" = ASRS)
  bridges <- bridge(network$graph, communities = bridge_groups)                 #Generate Bridge EI centrality
  bridges$Bridge_EI_z <- scale(bridges$`Bridge Expected Influence (1-step)`)
  plot(bridges, include=c("Bridge Expected Influence (1-step)"), zscore = TRUE, labels = F)
  
  #Central Stability
  #casedrop_boot <- bootnet(network, nBoots = 1000, nCores = 8, type = "case", statistics = c("betweenness", "closeness", "expectedInfluence"))
  #plot(casedrop_boot, statistics = c( "betweenness", "closeness", "expectedInfluence"))
  #corStability(casedrop_boot)                                                  #Minimum 0.25, ideally above 0.5
  
  return(list(Alpha_Autism = alpha_Autism, Alpha_ADHD = alpha_ADHD,             #Alpha
              Omega_Autism = omega_Autism, Omega_ADHD = omega_ADHD,             #Omega
              Network = network, All_Edges = sig_edges, ASD_Edges = asd_pres,   #Network
              ADHD_Edges = adhd_pres, ASD_ADHD_Edges = asd_adhd_pres,           #Edges
              EI_Centrality = cents, Bridge_EI = bridges))                      #Centrality
}

###########################**Whole Measure Analyses**###########################
##**Set Adjustable Parameters**##
AQ <- 1:28
ASRS <- 29:46
groups <- list("Social Skills" = c(1,9:10,12,15,24,26:27),                       
               "Attention Switching" = c(4,8,18,21),
               "Imagination" = c(3,6,11,14,20,23,25,28), 
               "Routines" = c(2,17,19),              
               "Numbers/Patterns" = c(5,7,13,16,22), 
               "Inattenion" = c(1:4, 7:11) + 28, 
               "Hyperactivity and Impulisivity" = c(5:6,12:18) +28)  

##**Study 1**##
data1 <- read.xlsx(Sys.glob("C:\\Users\\*\\Downloads\\Waldren_Cortex_Study1.xlsx")) 

#Pre-processing
study1_network <- data1[,c(1:28, 30:47)]                                        #AQ28 and ASRS items
study1_descriptives <- data1[,c('AQ28', 'ASRS', 'Sex', 'Age', 'Education')]     #Total scores and demographics
psych::describe(study1_descriptives)
cor1 <- rcorr(as.matrix(study1_descriptives))

#Network Analysis
study1 <- analysis(study1_network, TRUE, groups, AQ, ASRS)

##**Study 2**##
data2 <- read.xlsx(Sys.glob("C:\\Users\\*\\Downloads\\Waldren_Cortex_Study2.xlsx")) 

#Pre-Processing
study2_network <- data2[,c(1:28, 30:47)]                                       
study2_descriptives <- data2[,c('AQ28', 'ASRS', 'Sex', 'Age', 'Education', 'ASD', 'ADHD')] %>%  
  dplyr::filter(Education < 9) %>%                                              #Remove unclassified education levels
  dplyr::filter(Sex != 999)                                                     #Remove non-binary participants

psych::describe(study2_descriptives[1:5])
cor2 <- rcorr(as.matrix(study2_descriptives))
#Network Analysis
study2 <- analysis(study2_network, FALSE, groups, AQ, ASRS)

#Network comparisons
subgroups <- c(list(                                                            #Create 2 subgroups for each socio-demographic factor
  female = dplyr::filter(data2, Sex == 0),
  male = dplyr::filter(data2, Sex == 1),
  young = dplyr::filter(data2, Age < 36),                                       #Median age of sample
  old = dplyr::filter(data2, Age > 35),                                            
  deg = dplyr::filter(data2, Education > 5 & Education < 9),  
  no_deg = dplyr::filter(data2, Education < 6),                                 #University degree or higher
  mh = dplyr::filter(data2, No_Condition == 0),                                 #1 or more clinically diagnosed mental health conditions
  no_mh = dplyr::filter(data2, No_Condition == 1)))                                 

for(network in 1:8){                                                            #Generate Networks
  print(names(subgroups)[network])
  print(psych::describe(subgroups[network][[1]][c(29,48)]))                     #Descriptive statistics of AQ28 and ASRS total
  if(network %% 2 == 0){                                                        #AQ28 and ASRS t-test between socio-demographic subgroups
    print(paste0("Autism T-Test between ", names(subgroups)[network], " and " , names(subgroups)[network -1], " subgroups:"))
    print(t.test(subgroups[network][[1]][29], subgroups[network -1][[1]][29]))    
    print(paste0("ADHD T-Test between ", names(subgroups)[network], " and " , names(subgroups)[network -1], " subgroups:"))
    print(t.test(subgroups[network][[1]][48], subgroups[network -1][[1]][48]))}   
  assign(paste0(names(subgroups)[network], "_network"), estimateNetwork(subgroups[[network]][c(1:28, 30:47)], default = "ggmModSelect", tuning = 0.25,  weighted = TRUE, directed = FALSE, verbose = TRUE))}

#Network Comparisons
networks <- c(list(Sex = c(list(female = female_network, male = male_network)), #Create list of all subgroup networks
                   Age = c(list(young = young_network, old = old_network)), 
                   Education = c(list(deg = deg_network, no_deg = no_deg_network)), 
                   MH = c(list(mh = mh_network, no_mh = no_mh_network))))

for(group in 1:length(names(networks))){                                        #Comparing Bridge Expected Influence cannot currently be run using the version of the NCT function available in the 'NetworkComparisonTest' package
  assign(paste0(names(networks)[group], "_Bridge_Comp"),                        #To run this section of code, please see the associated github page to run the source function: https://github.com/cvborkulo/NetworkComparisonTest/blob/master/R/NCT.R
         NCT(networks[[group]][[1]], networks[[group]][[2]], it = 90, binary.data = F, paired = F, weighted = T, test.edges = F,
             progressbar = T, p.adjust.methods = "bonferroni", test.centrality = T, centrality = "bridgeExpectedInfluence", communities = c(rep("ASD", 28), rep ("ADHD", 18)), useCommunities = "all", nodes = c(4,21,26,32, 37, 39)))}

#######################**Accounting For Redundant Nodes**#######################
##**Set Adjustable Parameters**##
AQ <- 1:27
ASRS <- 28:44
groups <- list("Social Skills" = c(1,7,8,10,13,22,24,25), 
               "Attention Switching" =  c(3,6,16,19),
               "Imagination" = c(27,9,12,18,21,23,26), 
               "Routines" = c(2,15,17),              
               "Numbers/Patterns" = c(4,5,11,14,20), 
               "Inattenion" = c(1:4, 6:10) + 27  , 
               "Hyperactivity and Impulisivity" = c(5,11:17) +27)  

##**Identify and account for redundant nodes**##
for(study in c("Cortex_Study1", "Cortex_Study2")){
  data <- read.xlsx(Sys.glob(paste0("C:\\Users\\*\\Downloads\\Waldren_", study, ".xlsx")))
  network_data <- data[,c(1:28, 30:47)]                                         #AQ28 and ASRS items
  
  pcors <-pcor(network_data)                                                    #Identify node pairs with rp > 0.4
  rp <- pcors$estimate
  wto <- wTO(rp, sign = c("sign"))                                              #Assess their weighted Topological Overlap
  
  network_data$AQ28_36 <- (network_data$AQ28_3 + network_data$AQ28_6)           #Collapse redundant nodes pairs into one item
  network_data$ASRS_513 <- (network_data$ASRS_5  + network_data$ASRS_13) 
  network_data <- network_data[,-c(3,6,33,41)]                                  #Remove redundant nodes
  network_data <- network_data[,c(1:26, 43, 27:42, 44)]                         #Reorganise column order so all AQ28 and ASRS items are together
  finding_rn <- list(rp = rp, wto = wto)                                        #Save rp and wto for each data set
  assign(paste0("Finding_RN_", study), finding_rn)
  assign(paste0(study, "_RN"), network_data)}  

#Study 1
RN_study1 <- analysis(Cortex_Study1_RN, TRUE, groups, AQ, ASRS)

#Study 2
RN_study2 <- analysis(Cortex_Study2_RN, FALSE, groups, AQ, ASRS)

####################################**End**#####################################
session <- sessionInfo()
report_system(session)
as.data.frame(report(session))