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

############################################### FE ################################################
##### Initialization
dRL <- AddDummyCol(dRL, c("State2", "SamePrSecondState", "Stay2", "Stay2_2"), 0)
dRL2 <- data.frame()

for (p in unique(dRL$subjID)) {
  dt <- dRL%>%
    filter(subjID == p)
  ChoiceStep2_State1 <- c(0)
  ComptCS2S1 <- 2
  PreviousChoiceS1 <- 0
  ChoiceStep2_State2 <- c(0)
  ComptCS2S2 <- 2
  PreviousChoiceS2 <- 0
  
  # Compute state for the first line
  if (dt$level2_choice[1] <= 2){
    dt$State2[1] <- 1
    ChoiceStep2_State1[ComptCS2S1] <- dt$level2_choice[1]
    ComptCS2S1 <- ComptCS2S1 + 1
  } else {
    dt$State2[1] <- 2
    ChoiceStep2_State2[ComptCS2S2] <- dt$level2_choice[1]
    ComptCS2S2 <- ComptCS2S2 + 1
  }
  
  for (i in 2:length(dt$subjID)) {
    # Compute State 2
    if (dt$level2_choice[i] <= 2){
      dt$State2[i] <- 1
      ChoiceStep2_State1[ComptCS2S1] <- dt$level2_choice[i]
      PreviousChoiceS1 <- ChoiceStep2_State1[ComptCS2S1 - 1]
      ComptCS2S1 <- ComptCS2S1 + 1
    } else {
      dt$State2[i] <- 2
      ChoiceStep2_State2[ComptCS2S2] <- dt$level2_choice[i]
      PreviousChoiceS2 <- ChoiceStep2_State2[ComptCS2S2 - 1]
      ComptCS2S2 <- ComptCS2S2 + 1
    }
    
    # Compute if it is the same State 2
    if (dt$State2[i] == dt$State2[i-1]){
      dt$SamePrSecondState[i] <- 1
    }
    
    # Compute Stay 2
    if (dt$level2_choice[i] == dt$level2_choice[i-1]){
      dt$Stay2[i] <- 1
    }
    
    if (dt$State2[i] == 1 & dt$level2_choice[i] == PreviousChoiceS1){
      dt$Stay2_2[i] <- 1
    }
    
    if (dt$State2[i] == 2 & dt$level2_choice[i] == PreviousChoiceS2){
      dt$Stay2_2[i] <- 1
    }
  }
  
  dRL2 <- rbind(dRL2, dt)
}

dRL2 <- dRL2%>%
  # filter(SamePrSecondState == 1)%>%
  select(subjID, Trial, Stay2, Stay2_2, PrReward, PrTransition, transition, RT2)

######################################### Complete Frame ##########################################
dRL2 <- AddDummyCol(dRL2, c("StressGrM", "zdCortiM", "SampleC", "zRaven", "zOSPAN", "UrNeg"))

dRLF2 <- data.frame()

for (i in unique(dRL2$subj)) {
  # print(i)
  dRLt <- filter(dRL2, subjID==i)
  dRLt$StressGrM <- d$StressGrM[d$subjID == i]
  dRLt$zdCortiM <- d$zdCortiM[d$subjID == i]
  dRLt$SampleC <- d$SampleC[d$subjID == i]
  dRLt$zRaven <- d$zRaven[d$subjID == i]
  dRLt$zOSPAN <- d$zOSPAN[d$subjID == i]
  dRLt$UrNeg <- d$NegUr[d$subjID == i]
  dRLF2 <- rbind(dRLF2, dRLt)
}

########################################## Modelisation ###########################################
if("LRM2StressGR_OK_HCPG.Rdata" %in% dir(paste0(Output_path, "Models/")) & Redo == 0){
  LRM <- readRDS(paste0(Output_path, "Models/LRM2StressGR_OK_HCPG.Rdata"))
} else {
  LRM <- glmer(Stay2_2 ~ (1+PrReward*transition|subjID) + StressGrM + SampleC + StressGrM:SampleC +
                 PrReward + PrReward:StressGrM + PrReward:SampleC + PrReward:StressGrM:SampleC +
                 transition + transition:StressGrM + transition:SampleC + transition:SampleC:StressGrM,
               family = binomial, data = dRLF2, control = glmerControl(optimizer = "bobyqa"), nAGQ = 1)
  saveRDS(LRM, file=paste0(Output_path, "Models/LRM2StressGR_OK_HCPG.Rdata"))
}

summary(LRM)

if("LRM2StressGRUrneg_OK_HCPG.Rdata" %in% dir(paste0(Output_path, "Models/")) & Redo == 0){
  LRM <- readRDS(paste0(Output_path, "Models/LRM2StressGRUrneg_OK_HCPG.Rdata"))
} else {
  LRM <- glmer(Stay2 ~ (1+PrReward*transition|subjID) + StressGrM + UrNeg + StressGrM:UrNeg +
                 PrReward + PrReward:StressGrM + PrReward:UrNeg + PrReward:StressGrM:UrNeg +
                 transition + transition:StressGrM + transition:UrNeg + transition:UrNeg:StressGrM,
               family = binomial, data = dRLF2, control = glmerControl(optimizer = "bobyqa"), nAGQ = 1)
  saveRDS(LRM, file=paste0(Output_path, "Models/LRM2StressGRUrneg_OK_HCPG.Rdata"))
}

summary(LRM)
