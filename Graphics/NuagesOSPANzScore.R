remove(list = ls())

############################################ Parameter ############################################
source("MBMFStudy2_Initialization.R")
Datapath = "Raw_Data/"
Output_path = "Output/"
Graphic_path = "Graphics/Graph/"
multEB = 1

############################################# Frame ###############################################
d <- read.delim(paste0(Output_path,"dOKGamFE_Comp7P_OK_HCPG.txt"))

#################### OSPAN
OSPAN <- Nuage(d,
               paste0(Graphic_path, "zOSPAN.tiff"),
               "OSPAN (z-score)", "w (z-score)", d$zOSPAN, d$zw, SE = T, EqY = -10)
OSPAN

cor.test(d$zOSPAN, d$zw)

######## Low Stress
dlows <- d%>%
  # filter(zOSPAN > -3)%>%
  filter(StressGrM == "NotStressed")

OSPANlowdCortiM <- Nuage(dlows,
                         paste0(Graphic_path, "zOSPANLowCort.tiff"),
                         "OSPAN (z-score)", "w (z-score)", dlows$zOSPAN, dlows$zw, SE = T, Title = "Not Stressed",
                         EqY = -10)
OSPANlowdCortiM

cor.test(dlows$zw, dlows$zOSPAN)

######## High Stress
dhighs <- d%>%
  filter(StressGrM != "NotStressed")
OSPANhighdCortiM <- Nuage(dhighs,
                          paste0(Graphic_path, "zOSPANHighCort.tiff"),
                          "OSPAN (z-score)", "w (z-score)", dhighs$zOSPAN, dhighs$zw, SE = T, Title = "Stressed",
                          EqY = -10)
OSPANhighdCortiM

cor.test(dhighs$zw, dhighs$zOSPAN)

################# By group
######### PG
######## Low Stress
dlowOPG <- d%>%
  filter(SampleC == -1)%>%
  # filter(zdCortiM > -2)%>%
  filter(StressGrM == "NotStressed")
  

dCortiMlowOPG <- Nuage(dlowOPG,
                         paste0(Graphic_path, "zOSPANLowSPG.tiff"),
                         "OSPAN (z-score)", "w (z-score)", dlowOPG$zOSPAN, dlowOPG$zw, SE = T,
                       Title = "Not Stressed PG",
                       EqY = -10)
dCortiMlowOPG

cor.test(dlowOPG$zOSPAN, dlowOPG$zw)

######## High Stress
dHighOPG <- d%>%
  filter(SampleC == -1)%>%
  filter(StressGrM != "NotStressed")

dCortiMHighOPG <- Nuage(dHighOPG,
                       paste0(Graphic_path, "zOSPANHighSPG.tiff"),
                       "OSPAN (z-score)", "w (z-score)", dHighOPG$OSPAN, dHighOPG$zw, SE = T,
                       Title = "Stressed PG",
                       EqY = -10)
dCortiMHighOPG

cor.test(dHighOPG$zOSPAN, dHighOPG$zw)

################# By group
######### HC
######## Low Stress
dlowOHC <- d%>%
  filter(SampleC == 1)%>%
  filter(StressGrM == "NotStressed")


dCortiMlowOHC <- Nuage(dlowOHC,
                       paste0(Graphic_path, "zOSPANLowSHC.tiff"),
                       "OSPAN (z-score)", "w (z-score)", dlowOHC$zOSPAN, dlowOHC$zw, SE = T,
                       Title = "Not Stressed HC",
                       EqY = -10)
dCortiMlowOHC

cor.test(dlowOHC$zOSPAN, dlowOHC$zw)

######## High Stress
dHighOHC <- d%>%
  filter(SampleC == 1)%>%
  filter(StressGrM != "NotStressed")

dCortiMHighOHC <- Nuage(dHighOHC,
                        paste0(Graphic_path, "zOSPANHighSHC.tiff"),
                        "OSPAN (z-score)", "w (z-score)", dHighOHC$OSPAN, dHighOHC$zw, SE = T,
                        Title = "Stressed HC",
                        EqY = -10)
dCortiMHighOHC

cor.test(dHighOHC$zOSPAN, dHighOHC$zw)
