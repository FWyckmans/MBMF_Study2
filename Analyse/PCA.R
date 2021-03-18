remove(list = ls())

############################################ Parameter ############################################
source("MBMFStudy2_Initialization.R")
Datapath = "Raw_Data/"
Output_path = "Output/"

############################################# Frame ###############################################
d <- read.delim(paste0(Output_path,"dTot.txt"))%>%
  select(AllCol$ID, AllCol$Condition, AllCol$PCA)

PCA = c("SOGS", "AUDIT", "Fagerstrom",
        "Beck", "PANASPos", "PANASNeg", "SCL90R",
        "STAIA", "STAIB", "SRRS", "PunitionSens", "RewardSens",
        "UPPS_Total", "NegUr", "PosUr", "LackOfPrem", "LackOfPers", "Sensation",
        "Routine", "Auto")



PCA = c("SOGS", "AUDIT", "Fagerstrom",
        "Beck", "PANASPos", "PANASNeg", "SCL90R",
        "STAIA", "STAIB", "SRRS", "PunitionSens", "RewardSens",
        "UPPS_Total", "NegUr", "PosUr", "LackOfPrem", "LackOfPers", "Sensation")

pca <- PCA(d[AllCol$PCA], scale.unit = TRUE, ncp = 5, graph = TRUE)
fviz_eig(pca, addlabels = TRUE)

var <- get_pca_var(pca)
corrplot(var$cos2, is.corr=FALSE)
