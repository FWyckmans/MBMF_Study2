remove(list = ls())

############################################ Parameter ############################################
source("MBMFStudy2_1_Initialization.R")
Datapath = "Raw_Data/"
Output_path = "Output/"

############################################ Frame ################################################
dCort <- read.delim(paste0(Output_path,"dTot.txt"))%>%
  filter(OKd == 1)%>%
  select(NS, Initiales, Condition, Corti1, Corti2, Corti3, Corti4)

dExp <- select(dCort, NS, Initiales, Corti1, Corti2, Corti3, Corti4)

############################################# Export ##############################################
write_xlsx(dExp, paste0(Output_path, "Cortisol.xlsx"))

########################################### Descriptive ###########################################
dCort <- AddDummyCol(dCort, "CortiOK")
dCort$CortiOK <- 1
dCort$CortiOK[is.na(dCort$Corti1)] <- 0

########## Total missing
nDone <- sum(dCort$CortiOK)
nMissing <- length(dCort$CortiOK) - nDone
nTotal <- nDone + nMissing
TextTot <- paste0("You already have ", nDone, " analyses\n", nMissing, " analyses are missing\nFor a total of ", nTotal, " analyses")

########## Missing by condition
dBC <- dCort%>%
  group_by(Condition)%>%
  summarise(nDone = sum(CortiOK), Total = n())%>%
  mutate(nMissing = Total - nDone)%>%
  select(Condition, nDone, nMissing, Total)

MissingAlc <- dBC$nMissing[dBC$Condition=="A_CPT"] + dBC$nMissing[dBC$Condition=="A_WPT"]
TotAlc <- dBC$Total[dBC$Condition=="A_CPT"] + dBC$Total[dBC$Condition=="A_WPT"]

MissingG <- dBC$nMissing[dBC$Condition=="G_CPT"] + dBC$nMissing[dBC$Condition=="G_WPT"]
TotG <- dBC$Total[dBC$Condition=="G_CPT"] + dBC$Total[dBC$Condition=="G_WPT"]

MissingHC <- dBC$nMissing[dBC$Condition=="HC_CPT"] + dBC$nMissing[dBC$Condition=="HC_WPT"]
TotHC <- dBC$Total[dBC$Condition=="HC_CPT"] + dBC$Total[dBC$Condition=="HC_WPT"]

TextBC <- paste0("Chez les alcooliques, ", MissingAlc, " analyses sont manquantes (sur ", TotAlc, " participants)\n",
                 "Chez les gamblers, ", MissingG, " analyses sont manquantes (sur ", TotG, " participants)\n",
                 "Chez les contrôles, ", MissingHC, " analyses sont manquantes (sur ", TotHC, " participants)\n")

######################################### Display results #########################################
cat(TextTot)

cat(TextBC)
dBC
