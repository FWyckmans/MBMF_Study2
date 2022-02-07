remove(list = ls())

############################################ Parameter ############################################
source("MBMFStudy2_Initialization.R")
Datapath = "Raw_Data/"
Output_path = "Output/"
Redo = 0

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
  dRLt$zdCortiM <- d$zdCortiM[d$subjID == i]
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
########## Total model
if("LRMtStressGR_OK_HCPG_MyDF.Rdata" %in% dir(paste0(Output_path, "Models/")) & Redo == 0){
  LRM <- readRDS(paste0(Output_path, "Models/LRMtStressGR_OK_HCPG_MyDF.Rdata"))
} else {
  LRM <- glmer(Stay ~ (1+PrReward*PrTransition|subjID) + StressGrM*SampleC*PrReward*PrTransition,
               family = binomial, data = dRLF, control = glmerControl(optimizer = "bobyqa"), nAGQ = 1)
  saveRDS(LRM, file=paste0(Output_path, "Models/LRMtStressGR_OK_HCPG_MyDF.Rdata"))
}

if("LRMRtStressGR_OK_HCPG_MyDF.Rdata" %in% dir(paste0(Output_path, "Models/")) & Redo == 0){
  LRMRew <- readRDS(paste0(Output_path, "Models/LRMRtStressGR_OK_HCPG_MyDF.Rdata"))
} else {
  LRMRew <- glmer(Stay ~ (1+PrTransition|subjID) + StressGrM*SampleC*PrTransition,
                  family = binomial, data = dRLFR, control = glmerControl(optimizer = "bobyqa"), nAGQ = 1)
  saveRDS(LRMRew, file=paste0(Output_path, "Models/LRMRtStressGR_OK_HCPG_MyDF.Rdata"))
}

if("LRMURtStressGR_OK_HCPG_MyDF.Rdata" %in% dir(paste0(Output_path, "Models/")) & Redo == 0){
  LRMURew <- readRDS(paste0(Output_path, "Models/LRMURtStressGR_OK_HCPG_MyDF.Rdata"))
} else {
  LRMURew <- glmer(Stay ~ (1+PrTransition|subjID) + StressGrM*SampleC*PrTransition,
                   family = binomial, data = dRLFUR, control = glmerControl(optimizer = "bobyqa"), nAGQ = 1)
  saveRDS(LRMURew, file=paste0(Output_path, "Models/LRMURtStressGR_OK_HCPG_MyDF.Rdata"))
}

########## Output
summary(LRM)
summary(LRMRew)
summary(LRMURew)

interact_plot(LRMURew, pred = StressGrM, modx = SampleC, plot.points = T,
              interval = T,
              # modx.values = "plus-minus",
              # x.label = "Cortisol increase (z-score)",
              # y.label = "w-parameter (z-score)",
              # legend.main = "WM score",
              colors = "Qual1")
