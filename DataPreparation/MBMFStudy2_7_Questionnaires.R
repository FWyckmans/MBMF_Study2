remove(list = ls())

############################################ Parameter ############################################
source("MBMFStudy2_Initialization.R")
Datapath = "Raw_Data/"
Output_path = "Output/"

############################################# Frame ###############################################
d <- read.delim(paste0(Output_path,"dOKGamFE_Comp7P_OK_HCPG.txt"))

dAUDIT <- read_excel(paste0(Datapath, "/Questionnaires.xlsx"), sheet = "AUDIT")%>%
  filter(NS %in% d$NS)

dSOGS <- read_excel(paste0(Datapath, "/Questionnaires.xlsx"), sheet = "SOGS", skip = 1)%>%
  filter(NS %in% d$NS)%>%
  select(1:37)

dDSM <- read_excel(paste0(Datapath, "/Questionnaires.xlsx"), sheet = "DSM_Jeu")%>%
  filter(NS %in% d$NS)

dSCL90R <- read_excel(paste0(Datapath, "/Questionnaires.xlsx"), sheet = "SCL90-R")%>%
  filter(NS %in% d$NS)%>%
  select(1:105)

dBDI <- read_excel(paste0(Datapath, "/Questionnaires.xlsx"), sheet = "Beck")%>%
  filter(NS %in% d$NS)

dPANAS <- read_excel(paste0(Datapath, "/Questionnaires.xlsx"), sheet = "PANAS")%>%
  filter(NS %in% d$NS)

dSTAIA <- read_excel(paste0(Datapath, "/Questionnaires.xlsx"), sheet = "STAI-A")%>%
  filter(NS %in% d$NS)%>%
  select(1:23)

dSTAIB <- read_excel(paste0(Datapath, "/Questionnaires.xlsx"), sheet = "STAI-B")%>%
  filter(NS %in% d$NS)

dSPSRQ <- read_excel(paste0(Datapath, "/Questionnaires.xlsx"), sheet = "SPSRQ")%>%
  filter(NS %in% d$NS)%>%
  select(1:38)

dCoH <- read_excel(paste0(Datapath, "/Questionnaires.xlsx"), sheet = "CoH")%>%
  rename(NS = Num)%>%
  filter(NS %in% d$NS)

dUPPS <- read_excel(paste0(Datapath, "/Questionnaires.xlsx"), sheet = "UPPS")%>%
  filter(NS %in% d$NS)%>%
  select(1:26)


############################################ Cronbach #############################################
## AUDIT
cronbach(dAUDIT[3:12])

## SOGS
cronbach(dSOGS[c(15:22,24:35)])

## DSM
cronbach(dDSM[c(3:11)])

## SCL90R
cronbach(dSCL90R[c(16:105)])

## BDI
cronbach(dBDI[c(3:15)])

## PANAS
cronbach(dPANAS[c(4, 6, 8, 12, 13, 15, 17, 19, 20, 22)]) # Positive
cronbach(dPANAS[c(5, 7, 9, 10, 11, 14, 16, 18, 21, 23)]) # Negative

## STAIYA
# Reverse items
dSTAIA[3] <- 5-dSTAIA[3]
dSTAIA[4] <- 5-dSTAIA[4]
dSTAIA[7] <- 5-dSTAIA[7]
dSTAIA[10] <- 5-dSTAIA[10]
dSTAIA[12] <- 5-dSTAIA[12]
dSTAIA[13] <- 5-dSTAIA[13]
dSTAIA[17] <- 5-dSTAIA[17]
dSTAIA[18] <- 5-dSTAIA[18]
dSTAIA[21] <- 5-dSTAIA[21]
dSTAIA[22] <- 5-dSTAIA[22]

# Cronbach
cronbach(dSTAIA[c(3:22)])

## STAIYB
# Reverse items
dSTAIB[3] <- 5-dSTAIB[3]
dSTAIB[5] <- 5-dSTAIB[5]
dSTAIB[8] <- 5-dSTAIB[8]
dSTAIB[9] <- 5-dSTAIB[9]
dSTAIB[12] <- 5-dSTAIB[12]
dSTAIB[15] <- 5-dSTAIB[15]
dSTAIB[16] <- 5-dSTAIB[16]
dSTAIB[18] <- 5-dSTAIB[18]
dSTAIB[21] <- 5-dSTAIB[21]

# Cronbach
cronbach(dSTAIB[c(3:22)])

## SPSRQ
cronbach(dSPSRQ[c(5, 6, 7, 10, 12, 13, 15, 17, 20, 23, 25:28, 30, 32, 34, 37)]) # Punition
cronbach(dSPSRQ[c(4, 8, 9, 11, 14, 16, 18, 19, 21, 22, 24, 29, 31, 33, 35, 36, 38)]) # Reward

## CoH
cronbach(dCoH[c(4, 5, 7, 9, 10, 13, 15, 16, 17, 18, 20, 21, 23, 25, 27, 30)]) # Routine
cronbach(dCoH[c(6, 8, 11, 12, 14, 19, 22, 24, 26, 28, 29)]) # Automatism

## UPPS
# Reverse items
dUPPS[10] <- 5- dUPPS[10]
dUPPS[13] <- 5- dUPPS[13]
dUPPS[18] <- 5- dUPPS[18]
dUPPS[23] <- 5- dUPPS[23]
dUPPS[8] <- 5- dUPPS[8]
dUPPS[16] <- 5- dUPPS[16]
dUPPS[21] <- 5- dUPPS[21]
dUPPS[26] <- 5- dUPPS[26]
dUPPS[9] <- 5- dUPPS[9]
dUPPS[15] <- 5- dUPPS[15]
dUPPS[20] <- 5- dUPPS[20]
dUPPS[24] <- 5- dUPPS[24]

cronbach(dUPPS[c(10, 13, 18, 23)]) # NegUr
cronbach(dUPPS[c(8, 16, 21, 26)]) # PosUr
cronbach(dUPPS[c(7, 12, 19, 25)]) # Lack of Prem
cronbach(dUPPS[c(11, 14, 17, 22)]) # Lack of Pers
cronbach(dUPPS[c(9, 15, 20, 24)]) # Sensation seeking


##### DSM and SOGS together with computation
# Clinical frame
dDS <- cbind(d, dDSM[2:length(dDSM)], dSOGS[2:length(dSOGS)])
write.table(dDS, paste0(Output_path, "DSMSOGSindiv.txt"), col.names = T, row.names = F, sep = "\t")
