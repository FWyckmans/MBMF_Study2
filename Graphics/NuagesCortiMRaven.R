remove(list = ls())

############################################ Parameter ############################################
source("MBMFStudy2_Initialization.R")
Datapath = "Raw_Data/"
Output_path = "Output/"
Graphic_path = "Graphics/Graph/"
multEB = 1

############################################# Frame ###############################################
d <- read.delim(paste0(Output_path,"dOKGamFE_Comp7P_OK_HCPG.txt"))

#################### Raven
Raven <- Nuage(d,
               paste0(Graphic_path, "zRaven.tiff"),
               "Raven", "w", d$zRaven, d$zw, SE = T)
Raven

cor.test(d$zRaven, d$zw)

######## Low Stress
dlows <- d%>%
  filter(dCortiM < median(d$dCortiM))
RavenlowdCortiM <- Nuage(dlows,
                         paste0(Graphic_path, "zRavenLowCort.tiff"),
                         "Raven (z-score)", "w (z-score)", dlows$zRaven, dlows$zw, SE = T, Title = "No Stress")
RavenlowdCortiM

cor.test(dlows$zw, dlows$zRaven)

######## High Stress
dhighs <- d%>%
  filter(dCortiM >= median(d$dCortiM))
RavenhighdCortiM <- Nuage(dhighs,
                          paste0(Graphic_path, "zRavenHighCort.tiff"),
                          "Raven (z-score)", "w (z-score)", dhighs$zRaven, dhighs$zw, SE = T, Title = "Stressed")
RavenhighdCortiM

cor.test(dhighs$zw, dhighs$zRaven)

#################### dCortiM
############## Total sample
######## Total
dCortiM <- Nuage(d,
                 paste0(Graphic_path, "zdCortiM.tiff"),
                 "Log Cortisol Delta (z-score)", "w parameter (z-score)",
                 d$zdCortiM, d$zw, SE = T, EqY = -10)
dCortiM

cor.test(d$zdCortiM, d$zw)

######## Low Raven
dlowO <- d%>%
  filter(Raven < median(d$Raven))
dCortiMlowO <- Nuage(dlowO,
                     paste0(Graphic_path, "zdCortiMlowR.tiff"),
                     "Log Cortisol Delta (z-score)", "w parameter (z-score)",
                     dlowO$zdCortiM, dlowO$zw, SE = T, Title = "Low Raven", EqY = -10)
dCortiMlowO

cor.test(dlowO$zdCortiM, dlowO$zw)

######## High Raven
dhighO <- d%>%
  filter(Raven >= median(d$Raven))
dCortiMhighO <- Nuage(dhighO,
                      paste0(Graphic_path, "zdCortiMhighR.tiff"),
                      "Log Cortisol Delta (z-score)", "w parameter (z-score)",
                      dhighO$zdCortiM, dhighO$zw, SE = T, Title = "High Raven", EqY = -10)
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

######## Low Raven
dlowOPG <- dPG%>%
  # filter(zdCortiM < 3)%>%
  filter(Raven < median(d$Raven))
dCortiMlowOPG <- Nuage(dlowOPG,
                       paste0(Graphic_path, "zdCortiMlowRPG.tiff"),
                       "Log Cortisol Delta (z-score)", "w parameter (z-score)",
                       dlowOPG$zdCortiM, dlowOPG$zw, SE = T, Title = "Low Raven PG", EqY = -10)
dCortiMlowOPG

cor.test(dlowOPG$zdCortiM, dlowOPG$zw)

######## High Raven
dhighOPG <- dPG%>%
  # filter(zdCortiM < 3)%>%
  filter(Raven >= median(d$Raven))
dCortiMhighOPG <- Nuage(dhighOPG,
                        paste0(Graphic_path, "zdCortiMhighRPG.tiff"),
                        "Log Cortisol Delta (z-score)", "w parameter (z-score)",
                        dhighOPG$zdCortiM, dhighOPG$zw, SE = T, Title  = "High Raven PG", EqY = -10)
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

######## Low Raven
dlowOHC <- dHC%>%
  filter(Raven < median(d$Raven))
dCortiMlowOHC <- Nuage(dlowOHC,
                       paste0(Graphic_path, "zdCortiMlowRHC.tiff"),
                       "Log Cortisol Delta (z-score)", "w parameter (z-score)",
                       dlowOHC$zdCortiM, dlowOHC$zw, SE = T, Title = "Low Raven HC", EqY = -10)
dCortiMlowOHC

cor.test(dlowOHC$zdCortiM, dlowOHC$zw)

######## High Raven
dhighOHC <- dHC%>%
  filter(Raven >= median(d$Raven))
dCortiMhighOHC <- Nuage(dhighOHC,
                        paste0(Graphic_path, "zdCortiMhighRHC.tiff"),
                        "Log Cortisol Delta (z-score)", "w parameter (z-score)",
                        dhighOHC$zdCortiM, dhighOHC$zw, SE = T, Title = "High Raven HC", EqY = -10)
dCortiMhighOHC

cor.test(dhighOHC$zdCortiM, dhighOHC$zw)
