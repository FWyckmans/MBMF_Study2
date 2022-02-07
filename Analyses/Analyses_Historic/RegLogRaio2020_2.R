remove(list = ls())

############################################ Parameter ############################################
source("MBMFStudy2_Initialization.R")
Datapath = "Raw_Data/"
Output_path = "Output/"

dRL <- read.csv(paste0(Output_path, "ComputationsReady.txt"), sep="")

d <- read.delim(paste0(Output_path,"dOKGamFE_Comp7P_OK_HCPG.txt"))

dRL <- dRL%>%
  filter(subjID %in% d$subjID)

############################################### FE ################################################
##### Initialization
dRL <- AddDummyCol(dRL, c("State2", "SamePrSecondState", "RewardedPreviousSameSecondState", "Stay2", "Stay2_2"), 0)
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
  RewardStep2_State1 <- c(NA)
  RewardStep2_State2 <- c(NA)
  
  # Compute state for the first line
  if (dt$level2_choice[1] <= 2){
    dt$State2[1] <- 1
    ChoiceStep2_State1[ComptCS2S1] <- dt$level2_choice[1]
    RewardStep2_State1[ComptCS2S1] <- dt$reward[1]
    ComptCS2S1 <- ComptCS2S1 + 1
  } else {
    dt$State2[1] <- 2
    ChoiceStep2_State2[ComptCS2S2] <- dt$level2_choice[1]
    RewardStep2_State2[ComptCS2S2] <- dt$reward[1]
    ComptCS2S2 <- ComptCS2S2 + 1
  }
  
  for (i in 2:length(dt$subjID)) {
    # Compute State 2
    if (dt$level2_choice[i] <= 2){
      dt$State2[i] <- 1
      ChoiceStep2_State1[ComptCS2S1] <- dt$level2_choice[i]
      RewardStep2_State1[ComptCS2S1] <- dt$reward[i]
      
      PreviousRewardS1 <- RewardStep2_State1[ComptCS2S1 - 1]
      PreviousChoiceS1 <- ChoiceStep2_State1[ComptCS2S1 - 1]
      ComptCS2S1 <- ComptCS2S1 + 1
    } else {
      dt$State2[i] <- 2
      ChoiceStep2_State2[ComptCS2S2] <- dt$level2_choice[i]
      RewardStep2_State2[ComptCS2S2] <- dt$reward[i]
      
      PreviousRewardS2 <- RewardStep2_State2[ComptCS2S2 - 1]
      PreviousChoiceS2 <- ChoiceStep2_State2[ComptCS2S2 - 1]
      ComptCS2S2 <- ComptCS2S2 + 1
    }
    
    # Compute if it is the same State 2
    if (dt$State2[i] == dt$State2[i-1]){
      dt$SamePrSecondState[i] <- 1
    }
    
    # Compute Stay 2
    # This is not the good one, if you want to use it, keep only trials following a same second step
    if (dt$level2_choice[i] == dt$level2_choice[i-1]){
      dt$Stay2[i] <- 1
    }
    
    # This is the one like Raio et al. 2020
    if (dt$State2[i] == 1 & dt$level2_choice[i] == PreviousChoiceS1){
      dt$Stay2_2[i] <- 1
    }
    
    if (dt$State2[i] == 2 & dt$level2_choice[i] == PreviousChoiceS2){
      dt$Stay2_2[i] <- 1
    }
    
    # Compute Previous Reward in Previous Same State
    if (dt$State2[i] == 1){
      dt$RewardedPreviousSameSecondState[i] <- PreviousRewardS1
    } else {
      dt$RewardedPreviousSameSecondState[i] <- PreviousRewardS2
    }
  }
  
  dRL2 <- rbind(dRL2, dt)
}

dRL2$RewardedPreviousSameSecondState[dRL2$Trial == 1] <- NA

dRL2 <- dRL2%>%
  gather(key = "Step", value = "value", level1_choice:level2_choice)%>%
  rename(PrSSReward = RewardedPreviousSameSecondState)%>%
  arrange(subjID, Trial)

  # filter(SamePrSecondState == 1)%>%
  # select(subjID, Trial, Stay2, Stay2_2, PrReward, PrTransition, transition, RT2)


######################################### Complete Frame ##########################################
dRLF2 <- data.frame()

for (i in unique(dRL2$subj)) {
  dt <- data.frame()
  dRLt <- filter(dRL2, subjID == i)
  for (s in unique(dRLt$Trial)) {
    dRLtt <- filter(dRLt, Trial == s)
    dRLtt$Stay[2] <- dRLtt$Stay2_2[2]
    dRLtt$RT1[2] <- dRLtt$RT2[2]
    dt <- rbind(dt, dRLtt)
  }
  
  dRLF2 <- rbind(dRLF2, dt)
}

dRLF2$reward[dRLF2$reward == 0] <- -1
dRLF2$PrReward[dRLF2$PrReward == 0] <- -1
dRLF2$PrSSReward[dRLF2$PrSSReward == 0] <- -1

dRLF2$Stay[dRLF2$Trial == 1] <- NA

dRLF2 <- AddDummyCol(dRLF2, c("Stage1", "Stage2"), -1)
dRLF2$Stage1[dRLF2$Step == "level1_choice"] <- 1
dRLF2$Stage2[dRLF2$Step == "level2_choice"] <- 1
dRLF <- select(dRLF2, subjID, Trial, Stage1, Stage2, Stay, RT = RT1, PrSSReward, transition, PrReward, PrTransition)

# Export table
write.table(dRLF, paste0(Output_path, "dRaioAnalyses.txt"), col.names = T, row.names = F, sep = "\t", dec = ".")

# Scale and LogTransform RT
dRLF2 <- data.frame()

for (i in unique(dRLF$subjID)){
  dt <- filter(dRLF, subjID == i)
  dt <- OutliersModif(dt, "RT")
  dt <- filter(dt, !is.na(RT))
  dt$RT <- log(dt$RT + 1)
  ScaleToDo <- list(CoI = c("RT"),
  NewCol = c("zRT"))
  dt <- ScaleCol(dt, ScaleToDo)
  dRLF2 <- rbind(dRLF2, dt)
}

########################################## Modelisation ###########################################
# Remove dCortiM outliers
d$dCortiM <- log10(d$dCortiM + 11)

dNoO <- OutliersScale(d, c("dCortiM"))

dRL2 <- filter(dRLF2, subjID %in% dNoO$subjID)

# Add VI to dRL2
dRL2 <- AddDummyCol(dRL2, c("StressGrM", "zdCortiM", "Sample", "SampleC", "Raven", "OSPAN", "UrNeg"))

dRLF2 <- data.frame()

for (i in unique(dRL2$subj)) {
  # print(i)
  dRLt <- filter(dRL2, subjID==i)
  dRLt$StressGrM <- dNoO$StressGrM[dNoO$subjID == i]
  dRLt$zdCortiM <- dNoO$zdCortiM[dNoO$subjID == i]
  dRLt$Sample <- dNoO$Sample[dNoO$subjID == i]
  dRLt$SampleC <- dNoO$SampleC[dNoO$subjID == i]
  dRLt$Raven <- dNoO$Raven[dNoO$subjID == i]
  dRLt$OSPAN <- dNoO$OSPAN[dNoO$subjID == i]
  dRLt$UrNeg <- dNoO$NegUr[dNoO$subjID == i]
  
  dRLF2 <- rbind(dRLF2, dRLt)
}

dRLF2 <- AddDummyCol(dRLF2, "Step", -1)
dRLF2$Step[dRLF2$Stage1 == 1] <- 1

##### RegLog
## Exactly like Raio (no Stage1:PrTransition)
LRM_Raio <- glmer(Stay ~ (0 + Stage1 + Stage2 + Stage1:PrReward + Stage1:PrTransition +
                       Stage2:PrSSReward + Stage2:transition |subjID) + 
               0 + Stage1 + Stage2 +
               Stage1:(SampleC + zdCortiM + PrReward + SampleC:zdCortiM +
                         zdCortiM:(PrReward + PrTransition) +
                         SampleC:(PrReward + PrTransition) +
                         SampleC:zdCortiM:(PrReward + PrTransition)) +
               Stage2:(SampleC + zdCortiM + PrSSReward + transition + SampleC:zdCortiM +
                         zdCortiM:(PrSSReward + transition) +
                         SampleC:(PrSSReward + transition) +
                         SampleC:zdCortiM:(PrSSReward + transition)),
             family = binomial, data = dRLF2)

summary(LRM_Raio)

## Nearly like Raio but with Stage1:PrTransition
LRM <- glmer(Stay ~ (0 + Step + Stage1:PrReward + Stage1:PrTransition +
                       Stage2:PrSSReward + Stage2:transition |subjID) + 
               0 + Step +
               Stage1:(SampleC + zdCortiM + PrReward + PrTransition + SampleC:zdCortiM +
                         zdCortiM:(PrReward + PrTransition) +
                         SampleC:(PrReward + PrTransition) +
                         SampleC:zdCortiM:(PrReward + PrTransition)) +
               Stage2:(SampleC + zdCortiM + PrSSReward + transition + SampleC:zdCortiM +
                         zdCortiM:(PrSSReward + transition) +
                         SampleC:(PrSSReward + transition) +
                         SampleC:zdCortiM:(PrSSReward + transition)),
             family = binomial, data = dRLF2)

summary(LRM)

## Remove all nested main effects
LRM <- glmer(Stay ~ (0 + Stage1:PrReward + Stage1:PrTransition +
                       Stage2:PrSSReward + Stage2:transition |subjID) + 
               0 + Stage1 + Stage2 + zdCortiM + SampleC +
               Stage1:(PrReward + PrTransition +
                         zdCortiM:(PrReward + PrTransition) +
                         SampleC:(PrReward + PrTransition) +
                         SampleC:zdCortiM:(PrReward + PrTransition)) +
               Stage2:(PrSSReward + transition +
                         zdCortiM:(PrSSReward + transition) +
                         SampleC:(PrSSReward + transition) +
                         SampleC:zdCortiM:(PrSSReward + transition)),
             family = binomial, data = dRLF2)

summary(LRM)

## Remove all main effects (nested main effects removed too)
LRM <- glmer(Stay ~ (0 + Stage1:PrReward + Stage1:PrTransition +
                       Stage2:PrSSReward + Stage2:transition |subjID) + 
               0 +
               Stage1:(PrReward + PrTransition +
                         zdCortiM:(PrReward + PrTransition) +
                         SampleC:(PrReward + PrTransition) +
                         SampleC:zdCortiM:(PrReward + PrTransition)) +
               Stage2:(PrSSReward + transition +
                         zdCortiM:(PrSSReward + transition) +
                         SampleC:(PrSSReward + transition) +
                         SampleC:zdCortiM:(PrSSReward + transition)),
             family = binomial, data = dRLF2)

summary(LRM)

##### Linear regression RT
LRT <- lmer(RT ~ (0 + Stage1 + Stage2 + Stage1:PrReward + Stage1:PrTransition +
                       Stage2:PrSSReward + Stage2:transition |subjID) + 
               0 + Stage1 + Stage2 +
               Stage1:(SampleC + zdCortiM + PrReward + SampleC:zdCortiM +
                         zdCortiM:(PrReward + PrTransition) +
                         SampleC:(PrReward + PrTransition) +
                         SampleC:zdCortiM:(PrReward + PrTransition)) +
               Stage2:(SampleC + zdCortiM + PrSSReward + transition + SampleC:zdCortiM +
                         zdCortiM:(PrSSReward + transition) +
                         SampleC:(PrSSReward + transition) +
                         SampleC:zdCortiM:(PrSSReward + transition)),
             data = dRLF2)

summary(LRT)
