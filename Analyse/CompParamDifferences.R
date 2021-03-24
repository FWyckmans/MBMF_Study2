remove(list = ls())

############################################ Parameter ############################################
source("MBMFStudy2_Initialization.R")
Datapath = "Raw_Data/"
Output_path = "Output/"

############################################# Frame ###############################################
d <- read.delim(paste0(Output_path,"dTot.txt"))%>%
  filter(OKd == 1)%>%
  select(subjID, Condition, Sample, StressGr, StressGrM, StressGrSR, StressGrSRM,
         a1, beta1, a2, beta2, pi, w, lambda)

############################################ Graphics ##############################################
########## Parameter = w
##### Main ANOVA
MainAOV(d, VoI = "lambda", Groups = c("Sample", "StressGrM"), RemoveOutTechnique = "MAD")
