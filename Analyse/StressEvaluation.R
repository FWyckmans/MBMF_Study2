remove(list = ls())

############################################ Parameter ############################################
source("MBMFStudy2_1_Initialization.R")
Datapath = "Raw_Data/"
Output_path = "Output/"

############################################ Frame ################################################
dTot <- read.delim(paste0(Output_path,"dTot.txt"))
d <- dTot%>%
  filter(OKd == 1)%>%
  select(subjID, Condition, Sample, StressGr, StressGrM, StressGrSR, StressGrSRM,
         Corti1, Corti2, Corti3, Corti4, dCorti, dCortiM,
         Stress1, Stress2, Stress3, Stress4, dStress, dStressM,
         Craving1, Craving2, Craving3, Craving4, dCraving, dCravingM,
         Resist1, Resist2, Resist3, Resist4, dResist, dResistM,
         Pain1, Pain2, Pain3, Pain4, dPain, dPainM)

