remove(list = ls())

############################################ Parameter ############################################
source("MBMFStudy2_Initialization.R")
Datapath = "Raw_Data/"
Output_path = "Output/"
Graphic_path = "Graphics/Graph/"

############################################# Frame ###############################################
d <- read.delim(paste0(Output_path,"dOKGamFE_Comp7P_OK_HCPG.txt"))

########################################### Regressions ###########################################
# d <- filter(d, zdCortiM < 3)

cor.test(d$zw, d$SampleC)
cor.test(d$zw, d$zdCortiM)
cor.test(d$zw, d$StressGrM)

##### With dCorti
# Start with dCorti
summary(lm(zw ~ zdCortiM, data = d))
summary(lm(zw ~ zdCortiM + zGrpxdCortiM, data = d))

summary(lm(zw ~ zOSPANxdCortiM + zGrpxOSPANxdCortiM, data = d))


#Start with Sample
summary(lm(zw ~ SampleC, data = d))
summary(lm(zw ~ SampleC + zdCortiM, data = d))

summary(lm(zw ~ SampleC + zdCortiM + zGrpxdCortiM, data = d))
# summary(lm(zw ~ SampleC + zGrpxdCortiM, data = d))
summary(lm(zw ~ zdCortiM + zGrpxdCortiM, data = d))
# summary(lm(zw ~ zGrpxdCortiM, data = d))

summary(lm(zw ~ zdCortiM + zOSPANxdCortiM + zGrpxOSPANxdCortiM, data = d))
summary(lm(zw ~ zOSPANxdCortiM + zGrpxOSPANxdCortiM, data = d))
summary(lm(zw ~ zGrpxOSPANxdCortiM, data = d))

# With StressGrM
summary(lm(zw ~ SampleC + StressGrM, data = d))

summary(lm(zw ~ SampleC + StressGrM + SampleC:StressGrM, data = d))
summary(lm(zw ~ SampleC + SampleC:StressGrM, data = d))
summary(lm(zw ~ StressGrM + SampleC:StressGrM, data = d))
summary(lm(zw ~ SampleC:StressGrM, data = d))

summary(lm(zw ~ zOSPAN + SampleC:StressGrM, data = d))
summary(lm(zw ~ zOSPAN + SampleC:StressGrM + SampleC:StressGrM:zOSPAN, data = d))
summary(lm(zw ~ SampleC:zOSPAN + SampleC:StressGrM + SampleC:StressGrM:zOSPAN, data = d))
summary(lm(zw ~ SampleC:StressGrM + SampleC:StressGrM:zOSPAN, data = d))
summary(lm(zw ~ zOSPAN:StressGrM + SampleC:StressGrM:zOSPAN, data = d))
