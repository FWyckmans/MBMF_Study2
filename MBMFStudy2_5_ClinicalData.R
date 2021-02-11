remove(list = ls())
source("MBMFStudy2_1_Initialization.R")

############################################ Parameter ############################################
Datapath = "Raw_Data/"
Output_path = "Output/"

############################################ Frame ################################################
########## Clinical frame
dClin <- read_excel(paste0(Datapath, "Questionnaires.xlsx"), range = "A1:BN500")

bad <- is.na(dClin$NumDaw)
dClin <- dClin[!bad, ]

########## Other frames
dComputationParameter <- read.delim(paste0(Output_path, "ComputationParameter.txt"))
dOspan <- read.delim(paste0(Output_path, "dOspan.txt"))
dRegLogInd <- read.delim(paste0(Output_path, "dRegLogIndLarge.txt"))
dProba <- read.delim(paste0(Output_path, "ProbaLarge.txt"))
