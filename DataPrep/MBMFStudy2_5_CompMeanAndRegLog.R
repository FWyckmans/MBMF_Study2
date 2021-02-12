remove(list = ls())

############################################ Parameter ############################################
source("DataPrep/MBMFStudy2_1_Initialization.R")
Datapath = "Raw_Data/"
Output_path = "Output/"

############################################ Frame ################################################
d <- read.csv(paste0(Output_path, "/ComputationsReady.txt"), sep = " ")

###################################### Features engineering #######################################
########## Mean Probabilities - This part computes the same probabilities as those used by Sebold et al. (2014)
##### Recode PrReward
d$PrReward[d$PrReward==0] <- "Unrewarded"
d$PrReward[d$PrReward==1] <- "Rewarded"

##### Recode PrTransition
d$PrTransition[d$PrTransition==-1] <- "Rare"
d$PrTransition[d$PrTransition==1] <- "Common"

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

########## Individual RegLog indices
##### Frame creation
dRegLog <- d%>%
  select(subjID, Trial, PrReward, PrTransition, Stay)

dRegLog$PrReward[dRegLog$PrReward=="Rewarded"] <- 1
dRegLog$PrReward[dRegLog$PrReward=="Unrewarded"] <- -1

dRegLog$PrTransition[dRegLog$PrTransition=="Common"] <- 1
dRegLog$PrTransition[dRegLog$PrTransition=="Rare"] <- -1

##### Logistic Regression computation
RegLog <- function(d, NS){
  ### Give the df and the column containing subject numbers.
  
  # Create empty vectors for estimates
  MB <- c()
  MF <- c()
  RewMB <- c()
  UnrewMB <- c()
  
  # Create empty vectors for p-value
  MBp <- c()
  MFp <- c()
  RewMBp <- c()
  UnrewMBp <- c()
  
  # Create empty subjID vector
  subjID <- c()
  
  # Creation of a token
  c <- 1
  
  # Main loop
  for (i in unique(d[[NS]])) {
    # Create an individual frame for each participant
    dt <- filter(d, .data[[NS]] == i)
    
    # Total Logistic Regression
    RegLogInd <- glm(Stay ~ PrReward*PrTransition,
                     family = binomial(logit), data = dt)
    
    dsummary <- as.data.frame(summary(RegLogInd)[12])
    MB[c] <- dsummary[4, 1]
    MF[c] <- dsummary[2, 1]
    
    MBp[c] <- dsummary[4, 4]
    MFp[c] <- dsummary[2, 4]
    
    # Rewarded trials LogReg
    dR <- filter(dt, PrReward == 1)
    
    RegLogInd <- glm(Stay ~ PrTransition,
                     family = binomial(logit), data = dR)
    
    dsummary <- as.data.frame(summary(RegLogInd)[12])
    RewMB[c] <- dsummary[2,1]
    RewMBp[c] <- dsummary[2, 4]
    
    # Unrewarded trials LogReg
    dU <- filter(dt, PrReward == -1)
    
    RegLogInd <- glm(Stay ~ PrTransition,
                     family = binomial(logit), data = dU)
    
    dsummary <- as.data.frame(summary(RegLogInd)[12])
    UnrewMB[c] <- dsummary[2,1]
    UnrewMBp[c] <- dsummary[2, 4]
    
    subjID[c] <- i
    
    # Increase token
    c <- c+1
  }
  
  # Create final frame
  dfinal <- data.frame(subjID,
                       MB, MF, RewMB, UnrewMB,
                       MBp, MFp, RewMBp, UnrewMBp)
  return(dfinal)
}

dRL <- RegLog(dRegLog, "subjID")

########################################## Save Output ###########################################
write.table(dProbaLarge, paste0(Output_path, "ProbaLarge.txt"), sep = "\t", dec = ".")
write.table(dProbaLong, paste0(Output_path, "ProbaLong.txt"), sep = "\t", dec = ".")
write.table(dRL, paste0(Output_path, "dRegLogIndLarge.txt"), sep = "\t", dec = ".")
