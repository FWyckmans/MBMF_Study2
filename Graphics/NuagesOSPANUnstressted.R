remove(list = ls())

############################################ Parameter ############################################
source("MBMFStudy2_Initialization.R")
Datapath = "Raw_Data/"
Output_path = "Output/"
Graphic_path = "Graphics/Graph/"
multEB = 1

############################################# Frame ###############################################
d <- read.delim(paste0(Output_path,"dOKGamFE_Comp7P_OK_HCPG.txt"))

dHC <- d%>%
  filter(StressGrM == "NotStressed")%>%
  filter(SampleC == 1)

UnstressedHC_OSPAN <- Nuage(dHC,
                            paste0(Graphic_path, "UnstressedHC_zOSPAN.tiff"),
                            "OSPAN (z-score)", "w (z-score)", dHC$zOSPAN, dHC$zw, SE = T, EqY = -10,
                            Title = "Unstressed HC")
UnstressedHC_OSPAN

cor.test(dHC$zOSPAN, dHC$zw)


dPG <- d%>%
  filter(StressGrM == "NotStressed")%>%
  filter(SampleC == -1)

UnstressedPG_OSPAN <- Nuage(dPG,
                            paste0(Graphic_path, "UnstressedPG_zOSPAN.tiff"),
                            "OSPAN (z-score)", "w (z-score)", dPG$zOSPAN, dPG$zw, SE = T, EqY = -10,
                            Title = "Unstressed PG")
UnstressedPG_OSPAN

cor.test(dPG$zOSPAN, dPG$zw)
