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

MainAOV(d, VoI = "w", Groups = c("Condition"), RemoveOutTechnique = "MAD")

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

