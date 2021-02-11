remove(list = ls())

############################################ Parameter ############################################
source("MBMFStudy2_1_Initialization.R")
Datapath = "Raw_Data/"
Output_path = "Output/"

############################################ Frame ################################################
########## Clinical frame
dClin <- read_excel(paste0(Datapath, "Questionnaires.xlsx"), range = "A1:BN500")

##### Remove useless rows
bad <- is.na(dClin$NumDaw)
dClin <- dClin[!bad, ]

##### Select the necessary columns
dClin <- dClin%>%
  mutate(dCraving = Envie_3-Envie_2, dCravingM = ((Envie_3+Envie_4)/2)-((Envie_2+Envie_1)/2),
         dResist = Resister_3-Resister_2, dResistM = ((Resister_3+Resister_4)/2)-((Resister_2+Resister_1)/2),
         dStress = Stress_3-Stress_2, dStressM = ((Stress_3+Stress_4)/2)-((Stress_2+Stress_1)/2),
         dPain = Douleur_3-Douleur_2, dPainM = ((Douleur_3+Douleur_4)/2)-((Douleur_2+Douleur_1)/2),
         dAnalyse = Analyse_3-Analyse_2, dAnalyseM = ((Analyse_3+Analyse_4)/2)-((Analyse_2+Analyse_1)/2))%>%
  select(subjID = NumDaw, Initiales, Age, StudyLevel = Annee_Reussie,
         
         Condition, Patho,
         AUDIT, SOGS, DSM, Craving,
         
         dCraving, dResist, dStress, dPain, dAnalyse,
         dCravingM, dResistM, dStressM, dPainM, dAnalyseM,
         
         OSPAN, WAIS, Raven,
         
         SCL90R, Fagerstrom, Beck,
         PANASPos = Affect_positif, PANASNeg = Affect_Negatif, STAIA, STAIB, SRRS,
         PunitionSens = Sensibilite_Punition, RewardSens = Sensibilite_Recompense,
         
         Routine, Auto,
         
         UPPS_Total, NegUr = Urgence, PosUr = UrgencePos, LackOfPrem = Manquedepremeditation,
         LackOfPers = Manquedeperseverance, Sensation,
         
         Craving1 = Envie_1, Craving2 = Envie_2, Craving3 = Envie_3, Craving4 = Envie_4,
         Resist1 = Resister_1, Resist2 = Resister_2, Resist3 = Resister_3, Resist4 = Resister_4,
         Stress1 = Stress_1, Stress2 = Stress_2, Stress3 = Stress_3, Stress4 = Stress_4,
         Pain1 = Douleur_1, Pain2 = Douleur_2, Pain3 = Douleur_3, Pain4 = Douleur_4,
         Analyse_1, Analyse_2, Analyse_3, Analyse_4)

##### Vector with column names by type of variable for further references
ID <- c("subjID", "Initiales")
Demo <- c("Age", "StudyLevel")
Condition <- c("Condition", "Patho")
Gamb <- c("SOGS", "DSM", "Craving")
Alc <- c("AUDIT")
Cog <- c("OSPAN", "WAIS", "Raven")
FR <- c("dCraving", "dResist", "dStress", "dPain", "dAnalyse",
        "dCravingM", "dResistM", "dStressM", "dPainM", "dAnalyseM",
        "Envie_1", "Envie_2", "Envie_3", "Envie_4",
        "Resister_1", "Resister_2", "Resister_3", "Resister_4",
        "Stress_1", "Stress_2", "Stress_3", "Stress_4",
        "Douleur_1", "Douleur_2", "Douleur_3", "Douleur_4",
        "Analyse_1", "Analyse_2", "Analyse_3", "Analyse_4")
Perso <- c("SCL90R", "Fagerstrom", "Beck",
           "Affect_positif", "Affect_Negatif", "STAIA", "STAIB", "SRRS",
           "Sensibilite_Punition", "Sensibilite_Recompense",
           "Routine", "Auto",
           "UPPS_Total", "Urgence", "UrgencePos", "Manquedepremeditation", "Manquedeperseverance", "Sensation")

##### Add needed columns to dClin
ToAdd = c("a1", "beta1", "a2", "beta2", "pi", "w", "lambda",
          "MB", "MF", "RewMB", "UnrewMB", "MBp", "MFp", "RewMBp", "UnrewMBp",
          "PRCw", "PRRw", "PUCw", "PURw", "PRCd", "PRRd", "PUCd", "PURd",
          "OKDaw")

dClin <- AddDummyCol(dClin, ToAdd)

########## Other frames
dComputationParameter <- read.delim(paste0(Output_path, "ComputationParameter.txt"))
dOspan <- read.delim(paste0(Output_path, "dOspan.txt"))%>%
  rename(OSPAN = nWord)
dRegLogInd <- read.delim(paste0(Output_path, "dRegLogIndLarge.txt"))
dProba <- read.delim(paste0(Output_path, "ProbaLarge.txt"))
dProbaD <- read.table(paste0(Datapath, "/DataFromORScript/choice_probs.dat"))%>%
  rename(subjID = V1, PRCd = V2, PRRd = V3, PUCd = V4, PURd = V5)

########## Add all these columns to dClin and creation of the final df: dTot
AdditionnalDF <- list(dComputationParameter, dOspan, dRegLogInd, dProba, dProbaD)
ToFillbyDF <- list(dCP = colnames(dComputationParameter)[-1],
                   dOsp = colnames(dOspan)[-1],
                   dRegLogInd = colnames(dRegLogInd)[-1],
                   dProba = colnames(dProba)[-1],
                   dProbaD = colnames(dProbaD)[-1])

# Under maintenance
# for (i in 1:length(AdditionnalDF)) {
#   dt <- as.data.frame(AdditionnalDF[i])
#   ToFill <- ToFillbyDF[[i]]
#   dClin <- FillCol(dClin, dProba, ToFill)
# }
