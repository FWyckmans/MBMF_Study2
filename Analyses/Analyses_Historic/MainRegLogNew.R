remove(list = ls())

############################################ Parameter ############################################
source("MBMFStudy2_Initialization.R")
Datapath = "Raw_Data/"
Output_path = "Output/"
Redo = 0

OutliersScale <- function(d, ToCheck, Proxy = "MAD"){
  for (i in ToCheck) {
    d <- OutliersModif(d, i, Proxy = Proxy)
    d <- d[complete.cases(d[,i]),] 
  }
  
  ScaleToDo <- list(CoI = c("MFsw", "MBsw", "MBURsw",
                            "w", "beta1", "beta2", "a1", "a2", "pi", "lambda",
                            "MBv", "MFv",
                            "RewRT1", "UnRewRT1", "CommonRT2", "RareRT2", "dRT1", "dRT2",
                            "OSPAN", "Raven","dCortiM", "dCorti",
                            "SRRS", "STAIA", "Beck", "SCL90R", "RewardSens", "DSM", "SOGS"),
                    NewCol = c("zMFs", "zMBs", "zMBURs",
                               "zw", "zbeta1", "zbeta2", "za1", "za2", "zpi", "zlambda",
                               "zMBv", "zMFv",
                               "zRewRT1", "zUnRewRT1", "zCommonRT2", "zRareRT2", "zdRT1", "zdRT2",
                               "zOSPAN", "zRaven", "zdCortiM", "zdCorti",
                               "zSRRS", "zSTAIA", "zBeck", "zSCL90R", "zRewardSens", "zDSM", "zSOGS"))
  
  d <- ScaleCol(d, ScaleToDo, method = "standardize")
  return(d)
}


dRL <- read.csv(paste0(Output_path, "ComputationsReady.txt"), sep="")

d <- read.delim(paste0(Output_path,"dOKGamFE_Comp7P_OK_HCPG.txt"))

dRL <- dRL%>%
  filter(subjID %in% d$subjID)

dRL <- AddDummyCol(dRL, c("StressGrM", "zdCortiM", "SampleC", "zRaven", "zOSPAN"))

dRLF <- data.frame()

for (i in unique(dRL$subj)) {
  # print(i)
  dRLt <- filter(dRL, subjID==i)
  dRLt$StressGrM <- d$StressGrM[d$subjID == i]
  dRLt$dCortiM <- d$dCortiM[d$subjID == i]
  dRLt$SampleC <- d$SampleC[d$subjID == i]
  dRLt$zRaven <- d$zRaven[d$subjID == i]
  dRLt$zOSPAN <- d$zOSPAN[d$subjID == i]
  dRLF <- rbind(dRLF, dRLt)
}

dRLF$PrReward[dRLF$PrReward == 0] <- -1

dRLFR <- dRLF%>%
  filter(PrReward == 1)

dRLFUR <- dRLF%>%
  filter(PrReward == -1)

########################################## Modelisation ###########################################
########## Total
# Data Cleaning
dClean <- OutliersScale(d, "dCortiM")
dRLF <- filter(dRLF, subjID %in% dClean$subjID)
dRLF$Sample <- as.factor(dRLF$Sample)

# RegLog
LRM <- glmer(Stay ~ (1+PrReward*PrTransition|subjID) + dCortiM*Sample*PrReward*PrTransition,
             family = binomial, data = dRLF, control = glmerControl(optimizer = "bobyqa"), nAGQ = 1)
summary(LRM)

Inter_RL <- interact_plot(LRM, pred = PrReward, modx = Sample, plot.points = T,
                         interval = T,
                         x.label = "PrReward",
                         y.label = "STAY",
                         legend.main = "DG",
                         colors = "Qual1")
Inter_RL

# if("LRMtStressGR_OK_HCPG_MyDF.Rdata" %in% dir(paste0(Output_path, "Models/")) & Redo == 0){
#   LRM <- readRDS(paste0(Output_path, "Models/LRMtStressGR_OK_HCPG_MyDF.Rdata"))
# } else {
#   LRM <- glmer(Stay ~ (1+PrReward*PrTransition|subjID) + dCortiM*Sample*PrReward*PrTransition,
#                family = binomial, data = dRLF, control = glmerControl(optimizer = "bobyqa"), nAGQ = 1)
#   saveRDS(LRM, file=paste0(Output_path, "Models/LRMtStressGR_OK_HCPG_MyDF.Rdata"))
# }
