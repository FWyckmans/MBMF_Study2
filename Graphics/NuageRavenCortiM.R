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
               paste0(Graphic_path, "Raven.tiff"),
               "Raven", "w", d$Raven, d$w, SE = T)
Raven

cor.test(d$Raven, d$w)

######## Low Stress
dlows <- d%>%
  filter(dCortiM < median(d$dCortiM))
RavenlowdCortiM <- Nuage(dlows,
                         paste0(Graphic_path, "RavenLowCort.tiff"),
                         "Raven", "w", dlows$Raven, dlows$w, SE = T)
RavenlowdCortiM

cor.test(dlows$w, dlows$Raven)

######## High Stress
dhighs <- d%>%
  filter(dCortiM >= median(d$dCortiM))
RavenhighdCortiM <- Nuage(dhighs,
                          paste0(Graphic_path, "RavenHighCort.tiff"),
                          "Raven", "w", dhighs$Raven, dhighs$w, SE = T)
RavenhighdCortiM

cor.test(dhighs$w, dhighs$Raven)

#################### dCortiM
############## Total sample
######## Total
dCortiM <- Nuage(d,
                 paste0(Graphic_path, "dCortiM.tiff"),
                 "Log Cortisol Delta", "w parameter", d$dCortiM, d$w, SE = T)
dCortiM

cor.test(d$dCortiM, d$w)

######## Low Raven
dlowRav <- d%>%
  filter(Raven < median(d$Raven))
dCortiMlowRav <- Nuage(dlowRav,
                     paste0(Graphic_path, "dCortiMlowRav.tiff"),
                     "Log Cortisol Delta", "w parameter", dlowRav$dCortiM, dlowRav$w, SE = T, Title = "Low Raven")
dCortiMlowRav

cor.test(dlowRav$dCortiM, dlowRav$w)

######## High Raven
dhighRav <- d%>%
  filter(Raven >= median(d$Raven))
dCortiMhighRav <- Nuage(dhighRav,
                      paste0(Graphic_path, "dCortiMhighRav.tiff"),
                      "Log Cortisol Delta", "w parameter", dhighRav$dCortiM, dhighRav$w, SE = T, Title = "High Raven")
dCortiMhighRav

cor.test(dhighRav$dCortiM, dhighRav$w)

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

######## Low Raven
dlowRavPG <- dPG%>%
  filter(Raven < median(d$Raven))
dCortiMlowRavPG <- Nuage(dlowRavPG,
                       paste0(Graphic_path, "dCortiMlowRavPG.tiff"),
                       "Log Cortisol Delta", "w parameter", dlowRavPG$dCortiM, dlowRavPG$w, SE = T, Title = "Low Raven PG")
dCortiMlowRavPG

cor.test(dlowRavPG$dCortiM, dlowRavPG$w)

######## High Raven
dhighRavPG <- dPG%>%
  filter(Raven >= median(d$Raven))
dCortiMhighRavPG <- Nuage(dhighRavPG,
                        paste0(Graphic_path, "dCortiMhighRavPG.tiff"),
                        "Log Cortisol Delta", "w parameter", dhighRavPG$dCortiM, dhighRavPG$w, SE = T, Title  = "High Raven PG")
dCortiMhighRavPG

cor.test(dhighRavPG$dCortiM, dhighRavPG$w)

############## Controls
######## Total
dHC <- d%>%
  filter(SampleC == 1)
dCortiMHC <- Nuage(dHC,
                   paste0(Graphic_path, "dCortiMHC.tiff"),
                   "Log Cortisol Delta", "w parameter", dHC$dCortiM, dHC$w, SE = T, Title = "HC")

dCortiMHC

cor.test(dHC$dCortiM, dHC$w)

######## Low Raven
dlowRavHC <- dHC%>%
  filter(Raven < median(d$Raven))
dCortiMlowRavHC <- Nuage(dlowRavHC,
                       paste0(Graphic_path, "dCortiMlowRavHC.tiff"),
                       "Log Cortisol Delta", "w parameter", dlowRavHC$dCortiM, dlowRavHC$w, SE = T, Title = "Low Raven HC")
dCortiMlowRavHC

cor.test(dlowRavHC$dCortiM, dlowRavHC$w)

######## High Raven
dhighRavHC <- dHC%>%
  filter(Raven >= median(d$Raven))
dCortiMhighRavHC <- Nuage(dhighRavHC,
                        paste0(Graphic_path, "dCortiMhighRavHC.tiff"),
                        "Log Cortisol Delta", "w parameter", dhighRavHC$dCortiM, dhighRavHC$w, SE = T, Title = "High Raven HC")
dCortiMhighRavHC

cor.test(dhighRavHC$dCortiM, dhighRavHC$w)
