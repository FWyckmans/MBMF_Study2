remove(list = ls())

############################################ Parameter ############################################
source("MBMFStudy2_Initialization.R")
Datapath = "Output/"
Output_path = "Output/"
# StressThreshold = 0.1 # Indicate the threshold of deltaCortisol to be considered as stressed

############################################ Frame ################################################
########## Clinical frame
dTot <- read.delim(paste0(Datapath, "dTot.txt"))

At <- sum(dTot$Sample=="Alc")
Gt <- sum(dTot$Sample=="Gambler")
HCt <- sum(dTot$Sample=="HC")
Tott <- length(dTot$Sample)

dTot <- filter(dTot, OKd == 1)

Aokd <- sum(dTot$Sample=="Alc")
Gokd <- sum(dTot$Sample=="Gambler")
HCokd <- sum(dTot$Sample=="HC")
Totokd <- length(dTot$Sample)

dTot <- filter(dTot, OKCort != 0)

Aokc <- sum(dTot$Sample=="Alc")
Gokc <- sum(dTot$Sample=="Gambler")
HCokc <- sum(dTot$Sample=="HC")
Totokc <- length(dTot$Sample)

dTot <- filter(dTot, OKCort == 1)
Aok <- sum(dTot$Sample=="Alc")
Gok <- sum(dTot$Sample=="Gambler")
HCok <- sum(dTot$Sample=="HC")
Totok <- length(dTot$Sample)

a <- length(dTot$FinalCondition[dTot$FinalCondition=="G_NoStr"])
b <- length(dTot$FinalCondition[dTot$FinalCondition=="G_Str"])

c <- length(dTot$FinalCondition[dTot$FinalCondition=="A_NoStr"])
d <- length(dTot$FinalCondition[dTot$FinalCondition=="A_Str"])

e <- length(dTot$FinalCondition[dTot$FinalCondition=="HC_NoStr"])
f <- length(dTot$FinalCondition[dTot$FinalCondition=="HC_Str"])

g = paste0(a + b, " (+", Gokc-Gok, ")")
h = paste0(c + d, " (+", Aokc-Aok, ")")
i = paste0(e + f, " (+", HCokc-HCok, ")")

j = a + c + e
k = b + d + f
l = paste0(a+b+c+d+e+f, " (+", Gokc-Gok + Aokc-Aok + HCokc-HCok, ")")

cat("En tout,\n",
    "nous avons", At, "alcooliques,", Gt, "gamblers, et", HCt, "contrôles\n",
    "Pour un total de", Tott, "participants recrutés")

cat("Après avoir retiré les participants ayant raté la tâche de Markov,\n",
    "nous avons", Aokd, "alcooliques,", Gokd, "gamblers, et", HCokd, "contrôles\n",
    "Pour un total de", Totokd, "Participants recrutés")

cat("Après avoir retiré les participants dont le cortisol n'a pu être analysé,\n",
    "nous avons", Aokc, "alcooliques,", Gokc, "gamblers, et", HCokc, "contrôles\n",
    "Pour un total de", Totokc, "Participants recrutés")

cat("L'échantillon final avec les participants qui ont déjà toutes leurs analyses compte,\n",
    Aok, "alcooliques,", Gok, "gamblers, et", HCok, "contrôles\n",
    "Pour un total de", Totok, "Participants recrutés")

cat("Il faut encore recruter\n",
    60-Aokc, "alcooliques", 60-Gokc, "gamblers et", 60-HCokc, "contrôles.")

cat("Il manque les analyses de\n",
    Aokc-Aok, "alcooliques", Gokc-Gok, "Gamblers et", HCokc-HCok, "contrôles.")

Mat <- matrix(c(a, c, e, j, b, d, f, k, g, h, i, l), ncol = 3)
colnames(Mat) <- c("Non stressé", "Stressé", "Total")
rownames(Mat) <- c("Gambler", "Alcoolique", "Controle", "Total")
Mat
