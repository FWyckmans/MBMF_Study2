remove(list = ls())

############################################ Parameter ############################################
source("DataPrep/MBMFStudy2_1_Initialization.R")
Datapath = "Raw_Data/"
Output_path = "Output/"
StressThreshold = 0 # Indicate the threshold of deltaCortisol to be considered as stressed

############################################ Frame ################################################
########## Clinical frame
dClin <- read_excel(paste0(Datapath, "Questionnaires.xlsx"), range = "A1:BN500")

##### Remove useless rows
bad <- is.na(dClin$NumDaw)
dClin <- dClin[!bad, ]

##### Add Group columns to specify gamblers, alcoholics and healthy controls
dClin <- AddDummyCol(dClin, "Sample")
dClin$Sample[dClin$Condition=="A_CPT"|dClin$Condition=="A_WPT"] <- "Alc"
dClin$Sample[dClin$Condition=="G_CPT"|dClin$Condition=="G_WPT"] <- "Gambler"
dClin$Sample[dClin$Condition=="HC_CPT"|dClin$Condition=="HC_WPT"] <- "HC"

##### Add StressGr to specify participants which saw their cortisol level or their self-reported measure Rise
dClin <- AddDummyCol(dClin, c("StressGr", "StressGrM", "StressGrSR", "StressGrSRM"), -1)

##### Select the necessary columns
dClin <- dClin%>%
  mutate(dCraving = Envie_3-Envie_2, dCravingM = ((Envie_3+Envie_4)/2)-((Envie_2+Envie_1)/2),
         dResist = Resister_3-Resister_2, dResistM = ((Resister_3+Resister_4)/2)-((Resister_2+Resister_1)/2),
         dStress = Stress_3-Stress_2, dStressM = ((Stress_3+Stress_4)/2)-((Stress_2+Stress_1)/2),
         dPain = Douleur_3-Douleur_2, dPainM = ((Douleur_3+Douleur_4)/2)-((Douleur_2+Douleur_1)/2),
         dCorti = Analyse_3-Analyse_2, dCortiM = ((Analyse_3+Analyse_4)/2)-((Analyse_2+Analyse_1)/2))%>%
  select(subjID = NumDaw, NS, Initiales, Age, StudyLevel = Annee_Reussie,
         
         Condition, Sample, StressGr, StressGrM, StressGrSR, StressGrSRM, Patho,
         AUDIT, SOGS, DSM, Craving,
         
         dCraving, dResist, dStress, dPain, dCorti,
         dCravingM, dResistM, dStressM, dPainM, dCortiM,
         
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
         Corti1 = Analyse_1, Corti2 = Analyse_2, Corti3 = Analyse_3, Corti4 = Analyse_4)

##### Add needed columns to dClin
ToAdd = c("a1", "beta1", "a2", "beta2", "pi", "w", "lambda",
          "MB", "MF", "RewMB", "UnrewMB", "MBp", "MFp", "RewMBp", "UnrewMBp",
          "PRCw", "PRRw", "PUCw", "PURw", "PRCd", "PRRd", "PUCd", "PURd",
          "OKd")

dClin <- AddDummyCol(dClin, ToAdd)

########## Other frames
dComputationParameter <- read.delim(paste0(Output_path, "ComputationParameter.txt"))
dOspan <- read.delim(paste0(Output_path, "dOspan.txt"))%>%
  rename(OSPAN = nWord)
dRegLogInd <- read.delim(paste0(Output_path, "dRegLogIndLarge.txt"))
dProba <- read.delim(paste0(Output_path, "ProbaLarge.txt"))
dProbaD <- read.table(paste0(Datapath, "/DataFromORScript/choice_probs.dat"))%>%
  rename(subjID = V1, PRCd = V2, PRRd = V3, PUCd = V4, PURd = V5)

########## Add all these columns to dClin and creation of the complete DF
AdditionnalDF <- list(dComputationParameter, dOspan, dRegLogInd, dProba, dProbaD)
ToFillbyDF <- list(dCP = colnames(dComputationParameter)[-1],
                   dOsp = colnames(dOspan)[2],
                   dRegLogInd = colnames(dRegLogInd)[-1],
                   dProba = colnames(dProba)[-1],
                   dProbaD = colnames(dProbaD)[-1])

for (i in 1:length(AdditionnalDF)) {
  dt <- as.data.frame(AdditionnalDF[i])
  ToFill <- ToFillbyDF[[i]]
  dClin <- FillCol(dClin, dt, ToFill)
}

########## Indicate if the task is good according to Otto Ross script
dClin$OKd <- 1
dClin$OKd[is.na(dClin$PRCd)] <- 0

########## Indicate if the participant was stressed (1) or not (-1)
##### With Cortisol
dClin$StressGr[dClin$dCorti > 0] <- 1
dClin$StressGr[is.na(dClin$dCorti)] <- NA

dClin$StressGrM[dClin$dCortiM > 0] <- 1
dClin$StressGrM[is.na(dClin$dCortiM)] <- NA

##### With self-reported measures
dClin$StressGrSR[dClin$dStress > 0] <- 1
dClin$StressGrSR[is.na(dClin$dStress)] <- NA

dClin$StressGrSRM[dClin$dStressM > 0] <- 1
dClin$StressGrSRM[is.na(dClin$dStressM)] <- NA

############################################# Export ##############################################
write.table(dClin, paste0(Output_path, "dTot.txt"), col.names = T, row.names = F, sep = "\t", dec = ".")