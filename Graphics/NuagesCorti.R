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
  filter(dCorti < median(d$dCorti))
OSPANlowdCorti <- Nuage(dlows,
                         paste0(Graphic_path, "OSPANLowCorti.tiff"),
                         "OSPAN", "w", dlows$OSPAN, dlows$w, SE = T)
OSPANlowdCorti

cor.test(dlows$w, dlows$OSPAN)

######## High Stress
dhighs <- d%>%
  filter(dCorti >= median(d$dCorti))
OSPANhighdCorti <- Nuage(dhighs,
                          paste0(Graphic_path, "OSPANHighCorti.tiff"),
                          "OSPAN", "w", dhighs$OSPAN, dhighs$w, SE = T)
OSPANhighdCorti

cor.test(dhighs$w, dhighs$OSPAN)

#################### dCorti
############## Total sample
######## Total
dCorti <- Nuage(d,
                 paste0(Graphic_path, "dCorti.tiff"),
                 "Log Cortisol Delta", "w parameter", d$dCorti, d$w, SE = T)
dCorti

cor.test(d$dCorti, d$w)

######## Low OSPAN
dlowO <- d%>%
  filter(OSPAN < median(d$OSPAN))
dCortilowO <- Nuage(dlowO,
                     paste0(Graphic_path, "dCortilowO.tiff"),
                     "Log Cortisol Delta", "w parameter", dlowO$dCorti, dlowO$w, SE = T, Title = "Low OSPAN")
dCortilowO

cor.test(dlowO$dCorti, dlowO$w)

######## High OSPAN
dhighO <- d%>%
  filter(OSPAN >= median(d$OSPAN))
dCortihighO <- Nuage(dhighO,
                      paste0(Graphic_path, "dCortihighO.tiff"),
                      "Log Cortisol Delta", "w parameter", dhighO$dCorti, dhighO$w, SE = T, Title = "High OSPAN")
dCortihighO

cor.test(dhighO$dCorti, dhighO$w)

############## Pathological Gamblers
######## Total
dPG <- d%>%
  filter(SampleC == -1)%>%
  filter(dCorti < 2) # Removed one observation for visual representation (does not change any results - remove for correlation)

dCortiPG <- Nuage(dPG,
                   paste0(Graphic_path, "dCortiPG.tiff"),
                   "Log Cortisol Delta", "w parameter", dPG$dCorti, dPG$w, SE = T, Title = "PG")
dCortiPG

cor.test(dPG$dCorti, dPG$w)

######## Low OSPAN
dlowOPG <- dPG%>%
  filter(OSPAN < median(d$OSPAN))
dCortilowOPG <- Nuage(dlowOPG,
                       paste0(Graphic_path, "dCortilowOPG.tiff"),
                       "Log Cortisol Delta", "w parameter", dlowOPG$dCorti, dlowOPG$w, SE = T, Title = "Low Ospan PG")
dCortilowOPG

cor.test(dlowOPG$dCorti, dlowOPG$w)

######## High OSPAN
dhighOPG <- dPG%>%
  filter(OSPAN >= median(d$OSPAN))
dCortihighOPG <- Nuage(dhighOPG,
                        paste0(Graphic_path, "dCortihighOPG.tiff"),
                        "Log Cortisol Delta", "w parameter", dhighOPG$dCorti, dhighOPG$w, SE = T, Title  = "High Ospan PG")
dCortihighOPG

cor.test(dhighOPG$dCorti, dhighOPG$w)

############## Controls
######## Total
dHC <- d%>%
  filter(SampleC == 1)
dCortiHC <- Nuage(dHC,
                   paste0(Graphic_path, "dCortiHC.tiff"),
                   "Log Cortisol Delta", "w parameter", dHC$dCorti, dHC$w, SE = T, Title = "HC")

dCortiHC

cor.test(dHC$dCorti, dHC$w)

######## Low OSPAN
dlowOHC <- dHC%>%
  filter(OSPAN < median(d$OSPAN))
dCortilowOHC <- Nuage(dlowOHC,
                       paste0(Graphic_path, "dCortilowOHC.tiff"),
                       "Log Cortisol Delta", "w parameter", dlowOHC$dCorti, dlowOHC$w, SE = T, Title = "Low Ospan HC")
dCortilowOHC

cor.test(dlowOHC$dCorti, dlowOHC$w)

######## High OSPAN
dhighOHC <- dHC%>%
  filter(OSPAN >= median(d$OSPAN))
dCortihighOHC <- Nuage(dhighOHC,
                        paste0(Graphic_path, "dCortihighOHC.tiff"),
                        "Log Cortisol Delta", "w parameter", dhighOHC$dCorti, dhighOHC$w, SE = T, Title = "High Ospan HC")
dCortihighOHC

cor.test(dhighOHC$dCorti, dhighOHC$w)
