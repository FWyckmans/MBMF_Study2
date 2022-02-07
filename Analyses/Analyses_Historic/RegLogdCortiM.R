remove(list = ls())

############################################ Parameter ############################################
source("MBMFStudy2_Initialization.R")
Datapath = "Raw_Data/"
Output_path = "Output/"
Redo = 0

dRL <- read.csv(paste0(Datapath, "DataFromORScript/choice_regress.dat"), sep="")

d <- read.delim(paste0(Output_path,"dOKGamFE_Comp7P_OK_HCPG.txt"))

dRL <- dRL%>%
  filter(subj %in% d$subjID)

dRL <- AddDummyCol(dRL, c("zdCortiM", "SampleC", "zRaven", "zOSPAN"))

dRLF <- data.frame()

for (i in unique(dRL$subj)) {
  # print(i)
  dRLt <- filter(dRL, subj==i)
  dRLt$zdCortiM <- d$zdCortiM[d$subjID == i]
  dRLt$SampleC <- d$SampleC[d$subjID == i]
  dRLt$zRaven <- d$zRaven[d$subjID == i]
  dRLt$zOSPAN <- d$zOSPAN[d$subjID == i]
  dRLF <- rbind(dRLF, dRLt)
}

dRLFR <- dRLF%>%
  filter(mn == 1)

dRLFUR <- dRLF%>%
  filter(mn == -1)

########################################## Modelisation ###########################################
# MixLogReg <- function(SaveName, DF, Redo = 0, CogMeasure = NA){
#   SN <- paste0(SaveName, ".Rdata")
#   if(SN %in% dir(paste0(Output_path, "Models/")) & Redo == 0){
#     RegLog <- readRDS(paste0(Output_path, "Models/", SN))
#   } else {
#     if (is.na(CogMeasure)){
#       RegLog <- glmer(stay ~ (1+mn*common|subj) + zdCortiM*SampleC*mn*common,
#                       family = binomial, data = dRLF, control = glmerControl(optimizer = "bobyqa"), nAGQ = 1)
#     }
#     if (CogMeasure == "OSPAN"){
#       RegLog <- glmer(stay ~ (1+mn*common|subj) + zdCortiM*SampleC*zOSPAN*mn*common,
#                       family = binomial, data = dRLF, control = glmerControl(optimizer = "bobyqa"), nAGQ = 1)
#     }
#     if (CogMeasure == "OSPAN"){
#       RegLog <- glmer(stay ~ (1+mn*common|subj) + zdCortiM*SampleC*zRaven*mn*common,
#                       family = binomial, data = dRLF, control = glmerControl(optimizer = "bobyqa"), nAGQ = 1)
#     }
#     saveRDS(RegLog, file=paste0(Output_path, "Models/", SaveName, ".Rdata"))
#   }
#   return(RegLog)
# }
# summary(MixLogReg("LRMtCortiM_OK_HCPG"))

########## Total model
if("LRMtCortiM_OK_HCPG.Rdata" %in% dir(paste0(Output_path, "Models/")) & Redo == 0){
  LRM <- readRDS(paste0(Output_path, "Models/LRMtCortiM_OK_HCPG.Rdata"))
} else {
  LRM <- glmer(stay ~ (1+mn*common|subj) + zdCortiM*SampleC*zOSPAN*mn*common,
               family = binomial, data = dRLF, control = glmerControl(optimizer = "bobyqa"), nAGQ = 1)
  saveRDS(LRM, file=paste0(Output_path, "Models/LRMtCortiM_OK_HCPG.Rdata"))
}

if("LRMRtCortiM_OK_HCPG.Rdata" %in% dir(paste0(Output_path, "Models/")) & Redo == 0){
  LRMR <- readRDS(paste0(Output_path, "Models/LRMRtCortiM_OK_HCPG.Rdata"))
} else {
  LRMR <- glmer(stay ~ (1+common|subj) + zdCortiM*SampleC*zOSPAN*common,
                family = binomial, data = dRLFR, control = glmerControl(optimizer = "bobyqa"), nAGQ = 1)
  saveRDS(LRMR, file=paste0(Output_path, "Models/LRMRtCortiM_OK_HCPG.Rdata"))
}

if("LRMURtCortiM_OK_HCPG.Rdata" %in% dir(paste0(Output_path, "Models/")) & Redo == 0){
  LRMUR <- readRDS(paste0(Output_path, "Models/LRMURtCortiM_OK_HCPG.Rdata"))
} else {
  LRMUR <- glmer(stay ~ (1+common|subj) + zdCortiM*SampleC*zOSPAN*common,
                 family = binomial, data = dRLFUR, control = glmerControl(optimizer = "bobyqa"), nAGQ = 1)
  saveRDS(LRMUR, file=paste0(Output_path, "Models/LRMURtCortiM_OK_HCPG.Rdata"))
}

########## Without OSPAN
if("LRM2PCortiM_OK_HCPG.Rdata" %in% dir(paste0(Output_path, "Models/")) & Redo == 0){
  LRM2P <- readRDS(paste0(Output_path, "Models/LRM2PCortiM_OK_HCPG.Rdata"))
} else {
  LRM2P <- glmer(stay ~ (1+mn*common|subj) + zdCortiM*SampleC*mn*common,
               family = binomial, data = dRLF, control = glmerControl(optimizer = "bobyqa"), nAGQ = 1)
  saveRDS(LRM2P, file=paste0(Output_path, "Models/LRM2PCortiM_OK_HCPG.Rdata"))
}

if("LRMR2PCortiM_OK_HCPG.Rdata" %in% dir(paste0(Output_path, "Models/")) & Redo == 0){
  LRMR2P <- readRDS(paste0(Output_path, "Models/LRMR2PCortiM_OK_HCPG.Rdata"))
} else {
  LRMR2P <- glmer(stay ~ (1+common|subj) + zdCortiM*SampleC*common,
                family = binomial, data = dRLFR, control = glmerControl(optimizer = "bobyqa"), nAGQ = 1)
  saveRDS(LRMR2P, file=paste0(Output_path, "Models/LRMR2PCortiM_OK_HCPG.Rdata"))
}

if("LRMUR2PCortiM_OK_HCPG.Rdata" %in% dir(paste0(Output_path, "Models/")) & Redo == 0){
  LRMUR2P <- readRDS(paste0(Output_path, "Models/LRMUR2PCortiM_OK_HCPG.Rdata"))
} else {
  LRMUR2P <- glmer(stay ~ (1+common|subj) + zdCortiM*SampleC*common,
                 family = binomial, data = dRLFUR, control = glmerControl(optimizer = "bobyqa"), nAGQ = 1)
  saveRDS(LRMUR2P, file=paste0(Output_path, "Models/LRMUR2PCortiM_OK_HCPG.Rdata"))
}

########## Output
summary(LRM)
summary(LRMR)
summary(LRMUR)

summary(LRM2P)
summary(LRMR2P)
summary(LRMUR2P)

# summary(LRMp)
# summary(LRMRp)
# summary(LRMURp)
# 
# summary(LRMr)
# summary(LRMRr)
# summary(LRMURr)
