remove(list = ls())
source("MBMFStudy2_1_Initialization.R")

############################################ Parameter ############################################
Test = 0
Way = 'Wyck'
Datapath = "Raw_Data/RLTaskData/"
Output_path = "Output/"

############################################ Frame ################################################
d <- read.csv(paste0(Output_path, "/ComputationsReady.txt"), sep = " ")

###################################### Features engineering #######################################
##### Recode PrReward
d$PrReward[d$PrReward==0] <- "Unrewarded"
d$PrReward[d$PrReward==1] <- "Rewarded"

##### Recode PrTransition
d$PrTransition[d$PrTransition==-1] <- "Rare"
d$PrTransition[d$PrTransition==1] <- "Common"

##### Mean - This part computes the same probabilities as those used by Sebold et al. (2014)
## Long format
dProbaLong <- d%>%
  group_by(subjID, PrReward, PrTransition)%>%
  summarise(Proba = mean(Stay))%>%
  unite("Type", c(PrReward, PrTransition), sep = " ")%>%
  group_by()

dProbaLong$Type[dProbaLong$Type=="Rewarded Common"] <- "PRCw"
dProbaLong$Type[dProbaLong$Type=="Rewarded Rare"] <- "PRRw"
dProbaLong$Type[dProbaLong$Type=="Unrewarded Common"] <- "PUCw"
dProbaLong$Type[dProbaLong$Type=="Unrewarded Rare"] <- "PURw"

## Large format
dProbaLarge <- dProbaLong%>%
  spread(key = Type, value = Proba)

########################################## Save Output ###########################################
write.table(dProbaLarge, paste0(Output_path, "ProbaLarge.txt"), sep = "\t", dec = ".")
write.table(dProbaLong, paste0(Output_path, "ProbaLong.txt"), sep = "\t", dec = ".")