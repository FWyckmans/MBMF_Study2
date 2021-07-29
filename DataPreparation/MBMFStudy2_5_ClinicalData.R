remove(list = ls())

############################################ Parameter ############################################
source("MBMFStudy2_Initialization.R")
Datapath = "Raw_Data/"
Output_path = "Output/"
StressThreshold = 0.02 # Indicate the threshold of deltaCortisol to be considered as stressed

############################################ Frame ################################################
########## Clinical frame
dClin <- read_excel(paste0(Datapath, "Questionnaires.xlsx"), range = "A1:BO500", na = "N/A")

##### Remove useless rows
bad <- is.na(dClin$NumDaw)
dClin <- dClin[!bad, ]

##### Add Group columns to specify gamblers, alcoholics and healthy controls
dClin <- AddDummyCol(dClin, "Sample")
dClin$Sample[dClin$Condition=="A_CPT"|dClin$Condition=="A_WPT"] <- "Alc"
dClin$Sample[dClin$Condition=="G_CPT"|dClin$Condition=="G_WPT"] <- "Gambler"
dClin$Sample[dClin$Condition=="HC_CPT"|dClin$Condition=="HC_WPT"] <- "HC"

dClin$Sample <- "HC"
# dClin$Sample[((dClin$AUDIT < 7) & (dClin$SOGS < 5))] <- "HC"
dClin$Sample[(dClin$AUDIT >= 13 & dClin$DSMal >= 2) | dClin$DSMal >= 3] <- "Alc"
# dClin$Sample[dClin$AUDIT >= 12] <- "Alc"
dClin$Sample[dClin$SOGS >= 6 | dClin$DSM >= 2] <- "Gambler"
# dClin$Sample[dClin$SOGS >= 6] <- "Gambler"

dClin$SampleC <- -1
dClin$SampleC[dClin$Sample == "HC"] <- 1

##### Add Group columns
# PG 3 groups
dClin$PG3 <- 0
dClin$PG3[dClin$SOGS < 6] <- 1
dClin$PG3[dClin$SOGS >= 12] <- -1
dClin$PG3[is.na(dClin$SOGS)] <- 1

# Alc 3 groups
dClin$Alc3 <- 1
dClin$Alc3[dClin$AUDIT>=12] <- 0
dClin$Alc3[dClin$AUDIT >= 16] <- -1

##### Add StressGr to specify participants which saw their cortisol level or their self-reported measure Rise
dClin <- AddDummyCol(dClin, c("StressGr", "StressGrM", "StressGrSR", "StressGrSRM", "FinalCondition", "OKCort"), -1)
dClin$OKCort[dClin$Analyse_1==9999] <- 0
dClin$OKCort[dClin$Analyse_1!=9999] <- 1
dClin$Analyse_1[dClin$Analyse_1 == 9999] <- NA
dClin$Analyse_2[dClin$Analyse_2 == 9999] <- NA
dClin$Analyse_3[dClin$Analyse_3 == 9999] <- NA
dClin$Analyse_4[dClin$Analyse_4 == 9999] <- NA

##### Select the necessary columns
dClin <- dClin%>%
  mutate(dCraving = Envie_3-Envie_2, dCravingM = ((Envie_3+Envie_4)/2)-((Envie_2+Envie_1)/2),
         dResist = Resister_3-Resister_2, dResistM = ((Resister_3+Resister_4)/2)-((Resister_2+Resister_1)/2),
         dStress = Stress_3-Stress_2, dStressM = ((Stress_3+Stress_4)/2)-((Stress_2+Stress_1)/2),
         dPain = Douleur_3-Douleur_2, dPainM = ((Douleur_3+Douleur_4)/2)-((Douleur_2+Douleur_1)/2),
         dCorti = Analyse_3-Analyse_2, dCortiM = ((Analyse_3+Analyse_4)/2)-((Analyse_2+Analyse_1)/2))%>%
  select(subjID = NumDaw, NS, Initiales, Age, StudyLevel = Annee_Reussie,
         
         FinalCondition, Condition, Sample, OKCort, StressGr, StressGrM, StressGrSR, StressGrSRM, Patho,
         AUDIT, DSMal, SOGS, DSM, Craving,
         
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
Threshold = 0.02 # OR Threshold = 0.02
dClin$StressGr[dClin$dCorti >= Threshold] <- 1
dClin$StressGr[is.na(dClin$dCorti)] <- NA

dClin$StressGrM[dClin$dCortiM >= Threshold] <- 1
dClin$StressGrM[is.na(dClin$dCortiM)] <- NA

##### With self-reported measures
dClin$StressGrSR[dClin$dStress > 0.0] <- 1
dClin$StressGrSR[is.na(dClin$dStress)] <- NA

dClin$StressGrSRM[dClin$dStressM > 0.0] <- 1
dClin$StressGrSRM[is.na(dClin$dStressM)] <- NA

dClin$dCorti <- log10(dClin$dCorti + 1)
dClin$dCortiM <- log10(dClin$dCortiM + 1)

##### Final stress group
# dClin$FinalCondition[((dClin$Sample == i) & (dClin[x] == 1)] <- ""
dClin$FinalCondition[((dClin$Sample == "Gambler") & (dClin$StressGrM == 1))] <- "G_Str"
dClin$FinalCondition[((dClin$Sample == "Gambler") & (dClin$StressGrM == -1))] <- "G_NoStr"

dClin$FinalCondition[((dClin$Sample == "Alc") & (dClin$StressGrM == 1))] <- "A_Str"
dClin$FinalCondition[((dClin$Sample == "Alc") & (dClin$StressGrM == -1))] <- "A_NoStr"

dClin$FinalCondition[((dClin$Sample == "HC") & (dClin$StressGrM == 1))] <- "HC_Str"
dClin$FinalCondition[((dClin$Sample == "HC") & (dClin$StressGrM == -1))] <- "HC_NoStr"

##### Simplified analyses DV
dClin <- dClin%>%
  mutate(MBsw = PRCw - PRRw - PUCw + PURw, MFsw = PRCw + PRRw - PUCw - PURw,
         MBURsw = PURw - PUCw, MBRsw = PRCw - PRRw,
         MBsd = PRCd - PRRd - PUCd + PURd, MFsd = PRCd + PRRd - PUCd - PURd,
         MBURsd = PURd - PUCd, MBRsd = PRCd - PRRd)

##### Changed 0 in auto and routine to NA
dClin$Auto[dClin$Auto == 0] <- NA
dClin$Routine[dClin$Routine == 0] <- NA

##### Change stress group
dClin$StressGr[dClin$StressGr == -1] <- "NotStressed"
dClin$StressGr[dClin$StressGr == 1] <- "Stressed"
dClin$StressGr <- as.factor(dClin$StressGr)

dClin$StressGrM[dClin$StressGrM == -1] <- "NotStressed"
dClin$StressGrM[dClin$StressGrM == 1] <- "Stressed"
dClin$StressGrM <- as.factor(dClin$StressGrM)

dClin$StressGrSR[dClin$StressGrSR == -1] <- "NotStressed"
dClin$StressGrSR[dClin$StressGrSR == 1] <- "Stressed"
dClin$StressGrSR <- as.factor(dClin$StressGrSR)

dClin$StressGrSRM[dClin$StressGrSRM == -1] <- "NotStressed"
dClin$StressGrSRM[dClin$StressGrSRM == 1] <- "Stressed"
dClin$StressGrSRM <- as.factor(dClin$StressGrSRM)

###################################### Features engineering #######################################
##### Interaction # Pour le moment, ineraction avec les cortisols 2-3, pas avec grp moyen!
dClin <- dClin%>%
  mutate(RavenXdCortM = Raven * dCorti,  # Raven*dCorti
         OSPANxdCortM = OSPAN * dCorti,  # OSPAN*dCorti
         GrpXdCortM = SampleC * dCorti,
         GrpXRaven = SampleC*Raven,
         GrpXRavenXdCortM = SampleC*Raven*dCorti)  

# zScores
ScaleCol <- function(d, ScaleToDo){
  d <- AddDummyCol(d, ScaleToDo[[2]], NA)
  Compt = 1
  for (i in ScaleToDo[["CoI"]]) {
    M = mean(d[[i]], na.rm = T)
    SD = sd(d[[i]], na.rm = T)
    for (p in c(1:length(d[[i]]))) {
      NewVal <- (d[[p, i]]-M)/SD
      d[p, ScaleToDo$NewCol[Compt]] <- NewVal
    }
    Compt = Compt+1
  }
  return(d)
}

ScaleToDo <- list(CoI = c("MFsw", "MBsw", "MBURsw", "w"), NewCol = c("zMF", "zMB", "zMBUR", "zw"))
dClin <- ScaleCol(dClin, ScaleToDo)

############################################# Export ##############################################
# Create dFinal where we only keep participant who did OK at DAW task AND get their Cortisol analyses
dOKAlc <- dClin%>%
  filter(OKCort == 1)%>%
  filter(OKd == 1)%>%
  filter(Sample != "Gambler")

dOKGam <- dClin%>%
  filter(OKCort == 1)%>%
  filter(OKd == 1)%>%
  filter(Sample != "Alc")

dOKTot <- dClin%>%
  filter(OKCort == 1)%>%
  filter(OKd == 1)

# Write tables
write.table(dClin, paste0(Output_path, "dTot.txt"), col.names = T, row.names = F, sep = "\t", dec = ".")
write.table(dOKAlc, paste0(Output_path, "dOKAlc.txt"), col.names = T, row.names = F, sep = "\t", dec = ".")
write.table(dOKGam, paste0(Output_path, "dOKGam.txt"), col.names = T, row.names = F, sep = "\t", dec = ".")
write.table(dOKTot, paste0(Output_path, "dOKTot.txt"), col.names = T, row.names = F, sep = "\t", dec = ".")
# dClinAlc <- select(dClin, subjID, NS, Initiales, Sample, Condition, AUDIT, DSMal)