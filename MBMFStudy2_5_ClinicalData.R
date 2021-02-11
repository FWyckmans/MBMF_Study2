remove(list = ls())
source("MBMFStudy2_1_Initialization.R")

############################################ Parameter ############################################
Datapath = "Raw_Data/"
Output_path = "Output/"

############################################ Frame ################################################
########## Clinical frame
dClin <- read_excel(paste0(Datapath, "Questionnaires.xlsx"), range = "A1:BN500")

##### Remove useless rows
bad <- is.na(dClin$NumDaw)
dClin <- dClin[!bad, ]

##### Change NumDaw to subId to be coherent with the other frames
dClin <- rename(dClin, subjID = NumDaw)

##### Select the necessary columns
ID <- c("subjID", "Initiales")
Condition <- c("Condition", "Patho")
Demo <- c("Age", "Annee_Reussie")
Cog <- c("OSPAN", "WAIS", "Raven")
FR <- c("Envie_1", "Envie_2", "Envie_3", "Envie_4",
        "Resister_1", "Resister_2", "Resister_3", "Resister_4",
        "Stress_1", "Stress_2", "Stress_3", "Stress_4",
        "Douleur_1", "Douleur_2", "Douleur_3", "Douleur_4")
Corti <- c("Analyse_1", "Analyse_2", "Analyse_3", "Analyse_4")
Alc <- c("AUDIT")
Gamb <- c("SOGS", "DSM", "Craving")
Perso <- c("SCL90R", "Fagerstrom", "Beck",
           "Affect_positif", "Affect_Negatif", "STAIA", "STAIB", "SRRS",
           "Sensibilite_Punition", "Sensibilite_Recompense",
           "Routine", "Auto",
           "UPPS_Total", "Urgence", "UrgencePos", "Manquedepremeditation", "Manquedeperseverance", "Sensation")
dClin <- select(dClin, ID, Demo, Condition, Gamb, Alc, FR, Corti, Cog, Perso)

########## Other frames
dComputationParameter <- read.delim(paste0(Output_path, "ComputationParameter.txt"))
dOspan <- read.delim(paste0(Output_path, "dOspan.txt"))
dRegLogInd <- read.delim(paste0(Output_path, "dRegLogIndLarge.txt"))
dProba <- read.delim(paste0(Output_path, "ProbaLarge.txt"))
