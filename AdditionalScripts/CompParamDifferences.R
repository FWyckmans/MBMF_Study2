remove(list = ls())

############################################ Parameter ############################################
source("MBMFStudy2_1_Initialization.R")
Datapath = "Raw_Data/"
Output_path = "Output/"

############################################ Frame ################################################
d <- read.delim(paste0(Output_path,"dTot.txt"))%>%
  filter(OKd == 1)%>%
  select(subjID, Condition, Sample, StressGr, StressGrM, StressGrSR, StressGrSRM,
         a1, beta1, a2, beta2, pi, w, lambda)

########################################### Graphic ###############################################
##### Parameter = w
boxplot(w ~ Condition, data = d)
boxplot(w ~ StressGr*Sample, data = d)

boxplot(w ~ StressGrM*Sample, data = d)

boxplot(w ~ StressGrSR*Sample, data = d)