remove(list = ls())

############################################ Parameter ############################################
source("MBMFStudy2_Initialization.R")
Datapath = "Raw_Data/"
Output_path = "Output/"

############################################# Frame ###############################################
d <- read.delim(paste0(Output_path,"dTot.txt"))%>%
  filter(OKd == 1)%>%
  filter(OKCort != 0)%>%
  select(subjID, Condition, Sample, StressGr, StressGrM, StressGrSR, StressGrSRM,
         a1, beta1, a2, beta2, pi, w, lambda)%>%
  filter(subjID != 302 & subjID != 275 & subjID != 217 & subjID != 211 & subjID != 235 & subjID != 212)#%>%
  # filter(Sample != "Gambler")

# d1 <- read.delim(paste0(Output_path,"ComputationParameter.txt"))
# d2 <- read.delim(paste0(Output_path,"ComputationParameter2.txt"))
# 
# d1 <- filter(d1, subjID %in% d2$subjID)
############################################ Graphics ##############################################
########## Parameter = w
##### Main ANOVA
# d <- filter(d, Sample != "Gambler")
MainAOV(d, VoI = "w", Groups = c("Sample", "StressGrM"), RemoveOutTechnique = NA)
MainAOV(d, VoI = "w", Groups = c("Sample", "StressGrSR"), RemoveOutTechnique = NA)

# bp(d, "w", "Condition")
# bp(d, "w", "Sample")
# bp(d, "w", c("Sample", "StressGr"))
# bp(d, "w", c("Sample", "StressGrM"))
# bp(d, "w", c("Sample", "StressGrSR"))

##### Parameter = lambda
# bp(d, "lambda", "Condition")
# bp(d, "lambda", "Sample")
# bp(d, "lambda", c("Sample", "StressGr"))
# bp(d, "lambda", c("Sample", "StressGrM"))
# bp(d, "lambda", c("Sample", "StressGrSR"))
# bp(d, "lambda", c("Sample", "StressGrSRM"))

# dA <- OutliersModif(d, c("a1", "beta1", "a2", "beta2", "pi", "w", "lambda"), Groups = "Sample")
# dA <- OutliersModif(d, c("a1", "beta1", "a2", "beta2", "pi", "w", "lambda"), Groups = "Condition")

