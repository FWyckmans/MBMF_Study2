remove(list = ls())

############################################ Parameter ############################################
source("MBMFStudy2_1_Initialization.R")
Datapath = "Raw_Data/"
Output_path = "Output/"

############################################ Frame ################################################
dCort <- read.delim(paste0(Output_path,"dTot.txt"))%>%
  filter(OKd == 1)%>%
  select(NS, Initiales, Condition, Analyse_1, Analyse_2, Analyse_3, Analyse_4)

dExp <- select(dCort, NS, Initiales, Analyse_1, Analyse_2, Analyse_3, Analyse_4)

############################################# Export ##############################################
write_xlsx(dExp, paste0(Output_path, "Cortisol.xlsx"))

########################################### Descriptive ###########################################
########## Total missing
nDone <- sum(!is.na(dCort$Analyse_1))
nMissing <- sum(is.na(dCort$Analyse_1))
nTotal <- nDone + nMissing
TextTot <- paste0("You already have ", nDone, " analyses\n", nMissing, " analyses are missing\nFor a total of ", nTotal, " analyses")
cat(TextTot)
