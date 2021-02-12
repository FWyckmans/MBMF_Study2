remove(list = ls())

############################################ Parameter ############################################
source("MBMFStudy2_1_Initialization.R")
Datapath = "Raw_Data/"
Output_path = "Output/"

############################################ Frame ################################################
dCort <- read.delim(paste0(Output_path,"dTot.txt"))%>%
  filter(OKd == 1)%>%
  select(NS, Initiales, Analyse_1, Analyse_2, Analyse_3, Analyse_4)


############################################# Export ##############################################
write_xlsx(dCort, paste0(Output_path, "Cortisol.xlsx"))

########################################### Descriptive ###########################################
nDone <- sum(!is.na(dCort$Analyse_1))
nMissing <- sum(is.na(dCort$Analyse_1))
nTotal <- nDone + nMissing

cat(paste0("You already have ", nDone, " analyses\n", nMissing, " analyses are missing\nFor a total of ", nTotal, " analyses"))
