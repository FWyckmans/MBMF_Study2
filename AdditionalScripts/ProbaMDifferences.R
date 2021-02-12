remove(list = ls())

############################################ Parameter ############################################
source("MBMFStudy2_1_Initialization.R")
Datapath = "Raw_Data/"
Output_path = "Output/"

############################################ Frame ################################################
d <- read.delim(paste0(Output_path,"dTot.txt"))%>%
  filter(OKd == 1)%>%
  select(subjID, Condition, Sample, StressGr, StressGrM, StressGrSR, StressGrSRM,
         PRCw, PRRw, PUCw, PURw, PRCd, PRRd, PUCd, PURd)%>%
  mutate(MBw = PRCw - PRRw - PUCw + PURw, MFw = PRCw + PRRw - PUCw - PURw,
         MBRw = PRCw - PRRw, MBUw = PUCw - PURw,
         MBd = PRCd - PRRd - PUCd + PURd, MFd = PRCd + PRRd - PUCd - PURd,
         MBRd = PRCd - PRRd, MBUd = PUCd - PURd)

########################################### Graphic ###############################################
########## My parameter (MBw:MBUw)
##### Parameter = MB
boxplot(MBw ~ Condition, data = d)
boxplot(MBw ~ Sample, data = d)

boxplot(MBw ~ StressGr*Sample, data = d)

boxplot(MBw ~ StressGrM*Sample, data = d)

boxplot(MBw ~ StressGrSR*Sample, data = d)