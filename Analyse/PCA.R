remove(list = ls())

############################################ Parameter ############################################
source("MBMFStudy2_Initialization.R")
Datapath = "Raw_Data/"
Output_path = "Output/"
Reference = "http://www.sthda.com/french/articles/38-methodes-des-composantes-principales-
dans-r-guide-pratique/73-acp-analyse-en-composantes-principales-avec-r-l-essentiel/#calcul"
############################################# Frame ###############################################
d <- read.delim(paste0(Output_path,"dTot.txt"))%>%
  select(AllCol$ID, AllCol$Condition, AllCol$PCA)%>%
  filter(Sample != "Alc")

PCA = c("SOGS", "AUDIT", "Fagerstrom",
        "Beck", "PANASPos", "PANASNeg", "SCL90R",
        "STAIA", "STAIB", "SRRS", "PunitionSens", "RewardSens",
        "UPPS_Total", "NegUr", "PosUr", "LackOfPrem", "LackOfPers", "Sensation",
        "Routine", "Auto")

AllCol$PCA = c("SOGS", "AUDIT",
        "Beck", "PANASPos", "PANASNeg", "SCL90R",
        "STAIA", "STAIB", "SRRS", "PunitionSens", "RewardSens",
        "NegUr", "PosUr", "LackOfPrem", "LackOfPers", "Sensation")

AllCol$PCA = c("SOGS", "AUDIT",
               "Beck", "PANASNeg", "SCL90R",
               "STAIA",
               "NegUr", "PosUr", "LackOfPrem", "LackOfPers", "Sensation")


pca <- PCA(d[AllCol$PCA], scale.unit = TRUE, ncp = 4, graph = TRUE)
fviz_eig(pca, addlabels = TRUE)

var <- get_pca_var(pca)
corrplot(var$cos2, is.corr=FALSE)
