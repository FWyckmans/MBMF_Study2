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

dRL <- AddDummyCol(dRL, c("StressGrM", "zdCortiM", "SampleC", "zRaven", "zOSPAN"))

dRLF <- data.frame()

for (i in unique(dRL$subj)) {
  # print(i)
  dRLt <- filter(dRL, subj==i)
  dRLt$StressGrM <- d$StressGrM[d$subjID == i]
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
########## Total model
if("LRMtStressGR_OK_HCPG.Rdata" %in% dir(paste0(Output_path, "Models/")) & Redo == 0){
  LRM <- readRDS(paste0(Output_path, "Models/LRMtStressGR_OK_HCPG.Rdata"))
} else {
  LRM <- glmer(stay ~ (1+mn*common|subj) + StressGrM*SampleC*mn*common,
               family = binomial, data = dRLF, control = glmerControl(optimizer = "bobyqa"), nAGQ = 1)
  saveRDS(LRM, file=paste0(Output_path, "Models/LRMtStressGR_OK_HCPG.Rdata"))
}

if("LRMRtStressGR_OK_HCPG.Rdata" %in% dir(paste0(Output_path, "Models/")) & Redo == 0){
  LRMRew <- readRDS(paste0(Output_path, "Models/LRMRtStressGR_OK_HCPG.Rdata"))
} else {
  LRMRew <- glmer(stay ~ (1+common|subj) + StressGrM*SampleC*common,
               family = binomial, data = dRLFR, control = glmerControl(optimizer = "bobyqa"), nAGQ = 1)
  saveRDS(LRMRew, file=paste0(Output_path, "Models/LRMRtStressGR_OK_HCPG.Rdata"))
}

if("LRMURtStressGR_OK_HCPG.Rdata" %in% dir(paste0(Output_path, "Models/")) & Redo == 0){
  LRMURew <- readRDS(paste0(Output_path, "Models/LRMURtStressGR_OK_HCPG.Rdata"))
} else {
  LRMURew <- glmer(stay ~ (1+common|subj) + StressGrM*SampleC*common,
                  family = binomial, data = dRLFUR, control = glmerControl(optimizer = "bobyqa"), nAGQ = 1)
  saveRDS(LRMURew, file=paste0(Output_path, "Models/LRMURtStressGR_OK_HCPG.Rdata"))
}

########## Output
summary(LRM)
summary(LRMRew)
summary(LRMURew)


interact_plot(LRMRew, pred = StressGrM, modx = SampleC, plot.points = T,
              interval = T,
              # modx.values = "plus-minus",
              # x.label = "Cortisol increase (z-score)",
              # y.label = "w-parameter (z-score)",
              # legend.main = "WM score",
              colors = "Qual1")
