remove(list = ls())

############################################ Parameter ############################################
source("MBMFStudy2_Initialization.R")
Datapath = "Output/"
Output_path = "Output/"
StressThreshold = 0 # Indicate the threshold of deltaCortisol to be considered as stressed

############################################ Frame ################################################
########## Clinical frame
dTot <- read.delim(paste0(Datapath, "dTot.txt"))

a <- length(dTot$FinalCondition[dTot$FinalCondition=="G_NoStr"])
b <- length(dTot$FinalCondition[dTot$FinalCondition=="G_Str"])

c <- length(dTot$FinalCondition[dTot$FinalCondition=="A_NoStr"])
d <- length(dTot$FinalCondition[dTot$FinalCondition=="A_Str"])

e <- length(dTot$FinalCondition[dTot$FinalCondition=="HC_NoStr"])
f <- length(dTot$FinalCondition[dTot$FinalCondition=="HC_Str"])

dTot <- filter(dTot, OKd == 1)

aa <- length(dTot$FinalCondition[dTot$FinalCondition=="G_NoStr"])
bb <- length(dTot$FinalCondition[dTot$FinalCondition=="G_Str"])

cc <- length(dTot$FinalCondition[dTot$FinalCondition=="A_NoStr"])
dd <- length(dTot$FinalCondition[dTot$FinalCondition=="A_Str"])

ee <- length(dTot$FinalCondition[dTot$FinalCondition=="HC_NoStr"])
ff <- length(dTot$FinalCondition[dTot$FinalCondition=="HC_Str"])

cat("En tout, nous avons", a, "gamblers non stressés et", b, "gamblers stressés.\n
    En retirant les participants n'ayant pas réussi la tache de Markov,\n
    nous avons", aa, "gamblers non stressés et", bb, "gamblers stressés.")

cat("En tout, nous avons", c, "alcooliques non stressés et", d, "alcooliques stressés.\n
    En retirant les participants n'ayant pas réussi la tache de Markov,\n
    nous avons", cc, "alcooliques non stressés et", dd, "alcooliques stressés.")

cat("En tout, nous avons", e, "contrôles non stressés et", f, "contrôles stressés.\n
    En retirant les participants n'ayant pas réussi la tache de Markov,\n
    nous avons", ee, "contrôles non stressés et", ff, "contrôles stressés.")