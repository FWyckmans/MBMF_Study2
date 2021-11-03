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
               "OSPAN", "w", d$zOSPAN, d$zw, SE = T)
OSPAN

cor.test(d$zOSPAN, d$zw)

######## Low Stress
dlows <- d%>%
  filter(dCortiM < median(d$dCortiM))
OSPANlowdCortiM <- Nuage(dlows,
                         paste0(Graphic_path, "zOSPANLowCort.tiff"),
                         "OSPAN (z-score)", "w (z-score)", dlows$zOSPAN, dlows$zw, SE = T, Title = "No Stress")
OSPANlowdCortiM

cor.test(dlows$zw, dlows$zOSPAN)

######## High Stress
dhighs <- d%>%
  filter(dCortiM >= median(d$dCortiM))
OSPANhighdCortiM <- Nuage(dhighs,
                          paste0(Graphic_path, "zOSPANHighCort.tiff"),
                          "OSPAN (z-score)", "w (z-score)", dhighs$zOSPAN, dhighs$zw, SE = T, Title = "Stressed")
OSPANhighdCortiM

cor.test(dhighs$zw, dhighs$zOSPAN)

#################### dCortiM
############## Total sample
######## Total
dCortiM <- Nuage(d,
                 paste0(Graphic_path, "zdCortiM.tiff"),
                 "Log Cortisol Delta (z-score)", "w parameter (z-score)",
                 d$zdCortiM, d$zw, SE = T, EqY = -10)
dCortiM

cor.test(d$zdCortiM, d$zw)

######## Low OSPAN
dlowO <- d%>%
  filter(OSPAN < median(d$OSPAN))
dCortiMlowO <- Nuage(dlowO,
                     paste0(Graphic_path, "zdCortiMlowO.tiff"),
                     "Log Cortisol Delta (z-score)", "w parameter (z-score)",
                     dlowO$zdCortiM, dlowO$zw, SE = T, Title = "Low OSPAN", EqY = -10)
dCortiMlowO

cor.test(dlowO$zdCortiM, dlowO$zw)

######## High OSPAN
dhighO <- d%>%
  filter(OSPAN >= median(d$OSPAN))
dCortiMhighO <- Nuage(dhighO,
                      paste0(Graphic_path, "zdCortiMhighO.tiff"),
                      "Log Cortisol Delta (z-score)", "w parameter (z-score)",
                      dhighO$zdCortiM, dhighO$zw, SE = T, Title = "High OSPAN", EqY = -10)
dCortiMhighO

cor.test(dhighO$zdCortiM, dhighO$zw)

############## Pathological Gamblers
######## Total
dPG <- d%>%
  # filter(dCortiM < 0.2)%>% # Removed one observation for visual representation (does not change any results - remove for correlation)
  filter(SampleC == -1)

dCortiMPG <- Nuage(dPG,
                   paste0(Graphic_path, "zdCortiMPG.tiff"),
                   "Log Cortisol Delta (z-score)", "w parameter (z-score)",
                   dPG$zdCortiM, dPG$zw, SE = T, Title = "PG")
dCortiMPG

cor.test(dPG$zdCortiM, dPG$zw)

######## Low OSPAN
dlowOPG <- dPG%>%
  filter(OSPAN < median(d$OSPAN))
dCortiMlowOPG <- Nuage(dlowOPG,
                       paste0(Graphic_path, "zdCortiMlowOPG.tiff"),
                       "Log Cortisol Delta (z-score)", "w parameter (z-score)",
                       dlowOPG$zdCortiM, dlowOPG$zw, SE = T, Title = "Low Ospan PG", EqY = -10)
dCortiMlowOPG

cor.test(dlowOPG$zdCortiM, dlowOPG$zw)

######## High OSPAN
dhighOPG <- dPG%>%
  filter(OSPAN >= median(d$OSPAN))
dCortiMhighOPG <- Nuage(dhighOPG,
                        paste0(Graphic_path, "zdCortiMhighOPG.tiff"),
                        "Log Cortisol Delta (z-score)", "w parameter (z-score)",
                        dhighOPG$zdCortiM, dhighOPG$zw, SE = T, Title  = "High Ospan PG", EqY = -10)
dCortiMhighOPG

cor.test(dhighOPG$zdCortiM, dhighOPG$zw)

############## Controls
######## Total
dHC <- d%>%
  filter(SampleC == 1)
dCortiMHC <- Nuage(dHC,
                   paste0(Graphic_path, "zdCortiMHC.tiff"),
                   "Log Cortisol Delta (z-score)", "w parameter (z-score)",
                   dHC$zdCortiM, dHC$zw, SE = T, Title = "HC", EqY = -10)

dCortiMHC

cor.test(dHC$zdCortiM, dHC$zw)

######## Low OSPAN
dlowOHC <- dHC%>%
  filter(OSPAN < median(d$OSPAN))
dCortiMlowOHC <- Nuage(dlowOHC,
                       paste0(Graphic_path, "zdCortiMlowOHC.tiff"),
                       "Log Cortisol Delta (z-score)", "w parameter (z-score)",
                       dlowOHC$zdCortiM, dlowOHC$zw, SE = T, Title = "Low Ospan HC", EqY = -10)
dCortiMlowOHC

cor.test(dlowOHC$zdCortiM, dlowOHC$zw)

######## High OSPAN
dhighOHC <- dHC%>%
  filter(OSPAN >= median(d$OSPAN))
dCortiMhighOHC <- Nuage(dhighOHC,
                        paste0(Graphic_path, "zdCortiMhighOHC.tiff"),
                        "Log Cortisol Delta (z-score)", "w parameter (z-score)",
                        dhighOHC$zdCortiM, dhighOHC$zw, SE = T, Title = "High Ospan HC")
dCortiMhighOHC

cor.test(dhighOHC$zdCortiM, dhighOHC$zw)
