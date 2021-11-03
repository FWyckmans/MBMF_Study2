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
               paste0(Graphic_path, "OSPAN.tiff"),
               "OSPAN", "w", d$OSPAN, d$w, SE = T)
OSPAN

cor.test(d$OSPAN, d$w)

######## Low Stress
dlows <- d%>%
  filter(dCortiM < median(d$dCortiM))
OSPANlowdCortiM <- Nuage(dlows,
                         paste0(Graphic_path, "OSPANLowCort.tiff"),
                         "OSPAN", "w", dlows$OSPAN, dlows$w, SE = T)
OSPANlowdCortiM

cor.test(dlows$w, dlows$OSPAN)

######## High Stress
dhighs <- d%>%
  filter(dCortiM >= median(d$dCortiM))
OSPANhighdCortiM <- Nuage(dhighs,
                         paste0(Graphic_path, "OSPANHighCort.tiff"),
                         "OSPAN", "w", dhighs$OSPAN, dhighs$w, SE = T)
OSPANhighdCortiM

cor.test(dhighs$w, dhighs$OSPAN)

#################### dCortiM
############## Total sample
######## Total
dCortiM <- Nuage(d,
               paste0(Graphic_path, "dCortiM.tiff"),
               "Log Cortisol Delta", "w parameter", d$dCortiM, d$w, SE = T)
dCortiM

cor.test(d$dCortiM, d$w)

######## Low OSPAN
dlowO <- d%>%
  filter(OSPAN < median(d$OSPAN))
dCortiMlowO <- Nuage(dlowO,
                     paste0(Graphic_path, "dCortiMlowO.tiff"),
                     "Log Cortisol Delta", "w parameter", dlowO$dCortiM, dlowO$w, SE = T, Title = "Low OSPAN")
dCortiMlowO

cor.test(dlowO$dCortiM, dlowO$w)

######## High OSPAN
dhighO <- d%>%
  filter(OSPAN >= median(d$OSPAN))
dCortiMhighO <- Nuage(dhighO,
                     paste0(Graphic_path, "dCortiMhighO.tiff"),
                     "Log Cortisol Delta", "w parameter", dhighO$dCortiM, dhighO$w, SE = T, Title = "High OSPAN")
dCortiMhighO

cor.test(dhighO$dCortiM, dhighO$w)

############## Pathological Gamblers
######## Total
dPG <- d%>%
  filter(SampleC == -1)%>%
  filter(dCortiM < 2) # Removed one observation for visual representation (does not change any results - remove for correlation)

dCortiMPG <- Nuage(dPG,
                 paste0(Graphic_path, "dCortiMPG.tiff"),
                 "Log Cortisol Delta", "w parameter", dPG$dCortiM, dPG$w, SE = T, Title = "PG")
dCortiMPG

cor.test(dPG$dCortiM, dPG$w)

######## Low OSPAN
dlowOPG <- dPG%>%
  filter(OSPAN < median(d$OSPAN))
dCortiMlowOPG <- Nuage(dlowOPG,
                     paste0(Graphic_path, "dCortiMlowOPG.tiff"),
                     "Log Cortisol Delta", "w parameter", dlowOPG$dCortiM, dlowOPG$w, SE = T, Title = "Low Ospan PG")
dCortiMlowOPG

cor.test(dlowOPG$dCortiM, dlowOPG$w)

######## High OSPAN
dhighOPG <- dPG%>%
  filter(OSPAN >= median(d$OSPAN))
dCortiMhighOPG <- Nuage(dhighOPG,
                      paste0(Graphic_path, "dCortiMhighOPG.tiff"),
                      "Log Cortisol Delta", "w parameter", dhighOPG$dCortiM, dhighOPG$w, SE = T, Title  = "High Ospan PG")
dCortiMhighOPG

cor.test(dhighOPG$dCortiM, dhighOPG$w)

############## Controls
######## Total
dHC <- d%>%
  filter(SampleC == 1)
dCortiMHC <- Nuage(dHC,
                   paste0(Graphic_path, "dCortiMHC.tiff"),
                   "Log Cortisol Delta", "w parameter", dHC$dCortiM, dHC$w, SE = T, Title = "HC")

dCortiMHC

cor.test(dHC$dCortiM, dHC$w)

######## Low OSPAN
dlowOHC <- dHC%>%
  filter(OSPAN < median(d$OSPAN))
dCortiMlowOHC <- Nuage(dlowOHC,
                       paste0(Graphic_path, "dCortiMlowOHC.tiff"),
                       "Log Cortisol Delta", "w parameter", dlowOHC$dCortiM, dlowOHC$w, SE = T, Title = "Low Ospan HC")
dCortiMlowOHC

cor.test(dlowOHC$dCortiM, dlowOHC$w)

######## High OSPAN
dhighOHC <- dHC%>%
  filter(OSPAN >= median(d$OSPAN))
dCortiMhighOHC <- Nuage(dhighOHC,
                      paste0(Graphic_path, "dCortiMhighOHC.tiff"),
                      "Log Cortisol Delta", "w parameter", dhighOHC$dCortiM, dhighOHC$w, SE = T, Title = "High Ospan HC")
dCortiMhighOHC

cor.test(dhighOHC$dCortiM, dhighOHC$w)
