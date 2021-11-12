remove(list = ls())

############################################ Parameter ############################################
source("MBMFStudy2_Initialization.R")
Datapath = "Raw_Data/"
Output_path = "Output/"
Graphic_path = "Graphics/Graph/"

############################################# Frame ###############################################
d <- read.delim(paste0(Output_path,"dOKGamFE_Comp7P_OK_HCPG.txt"))

########################################### Regressions ###########################################
cor.test(d$zw, d$SampleC)
cor.test(d$zw, d$zdCortiM)
summary(lm(zw ~ SampleC + zdCortiM + zGrpxdCortiM, data = d))
summary(lm(zw ~ zdCortiM + zGrpxdCortiM, data = d))
summary(lm(zw ~ zOSPANxdCortiM + zGrpxOSPANxdCortiM, data = d))

summary(lm(zw ~ zdCortiM + zGrpxdCortiM, data = d))
summary(lm(zw ~ SampleC + zGrpxdCortiM, data = d))
summary(lm(zw ~ zdCortiM + zGrpxdCortiM, data = d))