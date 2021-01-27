#################################### Initialisation ###############################################
remove(list = ls())

library(ggplot2)
library(BayesFactor)
library(car)
library(readxl)
library(stats)
library(gridExtra)
library(cowplot)
library(Hmisc)
library(rms)
library(ISLR)
library(nlme)
library(lmerTest)
library(e1071)
library(dplyr)
library(tidyr)
# library(rprime)
library(readr)
library(stringr)

############################################ Parameter ############################################
Test = 110
Way = 'Wyck'
Datapath = "Raw_Data/RLTaskData/"
Output_path = "Output/"

############################################ Frame ################################################
if (Test != 0){
  PS <- paste0(Datapath , Test, ".dat")
  d <- read.delim(PS, header=FALSE, stringsAsFactors=FALSE)
}

if (Test == 0){
  listeFichiers <- dir(pattern = ".dat")
  d <- data.frame()
  for (f in listeFichiers){
    s <- read.delim(f, header=FALSE, stringsAsFactors=FALSE)
    d <- rbind(d, s)}}

d <- d%>%
  select(NS = V1, Trial = V3, Step = V4, Choice1 = V5, Choice2 = V6, Resp1 = V7, Resp2 = V9, Reward = V11)%>%
  filter(!is.na(Trial))%>%
  arrange(Trial) #%>%filter(Trial >= 10)  # Here I keep the 10th trial to compute the PrReward and PrTransition of the 11th but I remove it afterwards

setwd(output_path)
dClin <- read_excel("D:/Dropbox/Stage_Labo/Questionnaires.xlsx")
dClin <- filter(dClin, !is.na(NumDaw))

###################################### Features engineering #######################################
Resp <- rep(0, length(d$NS))
Transition <- rep(0, length(d$NS))
Stay <- rep(0, length(d$NS))
PrReward <- rep(0, length(d$NS))
PrTransition <- rep(0, length(d$NS))
OK1 <- rep(1, length(d$NS))
OK2 <- rep(1, length(d$NS))
d <- cbind(d, Resp, Transition, Stay, PrReward, PrTransition, OK1, OK2)
d <- d%>%
  arrange(NS, Trial)

d$Step <- str_sub(d$Step, start = 6, end = -9)
d$Resp1[d$Resp1 < 1] <- NA  # Resp smaller than 1 correspond to RT so we remove them

# One Resp column
for (i in (1:length(d$NS))){
  if (d$Step[i] == "1"){
    d$Resp[i] = d$Resp1[i]}
  if (d$Step[i] == "2"){
    d$Resp[i] = d$Resp2[i]}
  if (d$Step[i] == "1" & d$Step[i+1] == "1"){   # Missed first step
    d$Resp[i] = -1
    d$OK1[i] = -1}}

# Remove unanswered
d <- filter(d, d$OK1 != -1)   # Totally remove trials where participant did not answer the 1st step

for (i in (1:length(d$NS))){
  if (d$Step[i] == "2" & d$Resp[i] == "-1"){   # Missed second step
    d$OK2[i] = -1
    d$OK2[i-1] = -1}}

# Remove trials after unanswered trial (Daw's way)
if (Way == "Daw"){
  d <- filter(d, OK2 != -1)
}

# Compute transitions
for (i in (1:length(d$NS))){
  if (d$Step[i] == "1"){
    if (d$Resp1[i] == 1 & d$Choice1[i+1] == 3){
      d$Transition[i] <- 1
      d$Transition[i+1] <- 1}
    
    if (d$Resp1[i] == 1 & d$Choice1[i+1] == 5){
      d$Transition[i] <- -1
      d$Transition[i+1] <- -1}
    
    if (d$Resp1[i] == 2 & d$Choice1[i+1] == 3){
      d$Transition[i] <- -1
      d$Transition[i+1] <- -1}
    
    if (d$Resp1[i] == 2 & d$Choice1[i+1] == 5){
      d$Transition[i] <- 1
      d$Transition[i+1] <- 1}}
}

# Compute Stay
for (i in c(1:length(d$NS))){
  if (d$Trial[i] != min(d$Trial)){
      if (d$Step[i] == "1" & d$Step[i-1] == "2"){  # Compute stay only for trials where participants answer to the second step
        if (d$Resp1[i] == d$Resp1[i-2]){
          d$Stay[i] <- 1
          d$Stay[i+1] <- 1}
        if (d$Resp1[i] != d$Resp1[i-2]){
          d$Stay[i] <- 0
          d$Stay[i+1] <- 0}}
      if (d$Step[i] == "1" & d$Step[i-1] == "1"){ # Select trials for which the participant missed the SECOND step !
        d$Stay[i] <- -1  
    }}}

# Compute PrReward & PrTransition
for (i in c(1:length(d$NS))){
  if (d$Trial[i] != min(d$Trial) & d$Step[i] == "1"){
    d$Reward[i] = d$Reward[i+1]
    
    d$PrReward[i] = d$Reward[i-1]
    d$PrReward[i+1] = d$Reward[i-1]
    
    d$PrTransition[i] = d$Transition[i-1]
    d$PrTransition[i+1] = d$Transition[i-1]}}

# Remove trials after unanswered trial (My way)
if (Way == "Mine"){
  d <- filter(d, PrReward != -1)
}

# Change Unrewarded code from 0 to -1
d$PrReward[d$PrReward == 0] <- -1
d$Reward[d$Reward == 0] <- -1

# Remove Trial 10 and above 175
d <- d%>%
  filter(Trial > 10)%>%filter(Trial <= 175)

dComp <- filter(d, Step == 1)

############################################# Export #########################################
dComp <- select(dComp, NS, Trial, Resp, Reward, Transition, PrReward, PrTransition, Stay)
write.table(dComp, "Choice_RegressWyck.txt", row.names = F, col.names = T)

########################################## Computation ############################################
##### My way (individual logistic regression)
## Add columns to dTot
MB <- rep(NA, length(dClin$NS))
MBp <- rep(NA, length(dClin$NS))
MF <- rep(NA, length(dClin$NS))
MFp <- rep(NA, length(dClin$NS))
AIC <- rep(NA, length(dClin$NS))
MBRew <- rep(NA, length(dClin$NS))
MBRewp <- rep(NA, length(dClin$NS))
MBRewAIC <- rep(NA, length(dClin$NS))
MBUnrew <- rep(NA, length(dClin$NS))
MBUnrewp <- rep(NA, length(dClin$NS))
MBUnrewAIC <- rep(NA, length(dClin$NS))
RFw <- rep(NA, length(dClin$NS))
RRw <- rep(NA, length(dClin$NS))
UFw <- rep(NA, length(dClin$NS))
URw <- rep(NA, length(dClin$NS))

dTot <- cbind(dClin, MB, MBp, MF, MFp, AIC, MBRew, MBRewp, MBRewAIC, MBUnrew, MBUnrewp, MBUnrewAIC, RFw, RRw, UFw, URw)

## Compute the logistic regressions
# Retrait des participants foireux
NumDawF <- c(193, 304)
dComp <- filter(dComp, NS %in% dClin$NumDaw)
dTot <- filter(dTot, !NumDaw %in% NumDawF)

if (Test == 0){
  for (i in c(dTot$NumDaw)){ #dTot$NumDaw
    dt <- filter(dComp, NS==i)
    
    RegLogInd <- glm(Stay ~ PrReward*PrTransition,
                       family = binomial(logit), data = dt)

    dc <- as.data.frame(summary(RegLogInd)[12])
    # dTot$MB[dTot$NumDaw==i] <- exp(dc[3,1])/(exp(dc[3,1])+1)
    dTot$MB[dTot$NumDaw==i] <- dc[4,1]
    dTot$MBp[dTot$NumDaw==i] <- dc[4,4]
    # dTot$MF[dTot$NumDaw==i] <- exp(dc[2,1])/(exp(dc[2,1])+1)
    dTot$MF[dTot$NumDaw==i] <- dc[2,1]
    dTot$MFp[dTot$NumDaw==i] <- dc[2,4]
    dTot$AIC[dTot$NumDaw==i] <- RegLogInd$aic

    # Rewarded trials
    dtRew <- filter(dt, PrReward == 1)
    RegLogInd <- glm(Stay ~ PrTransition,
                     family = binomial(logit), data = dtRew)
    
    dc <- as.data.frame(summary(RegLogInd)[12])
    # dTot$MBRew[dTot$NumDaw==i] <- exp(dc[2,1])/(exp(dc[2,1])+1)
    dTot$MBRew[dTot$NumDaw==i] <- dc[2,1]
    dTot$MBRewp[dTot$NumDaw==i] <- dc[2,4]
    dTot$MBRewAIC[dTot$NumDaw==i] <- RegLogInd$aic

    # Unrewarded trials
    dtUnrew <- filter(dt, PrReward == -1)
    RegLogInd <- glm(Stay ~ PrTransition,
                     family = binomial(logit), data = dtUnrew)
    dc <- as.data.frame(summary(RegLogInd)[12])
    # dTot$MBUnrew[dTot$NumDaw==i] <- exp(dc[2,1])/(exp(dc[2,1])+1)
    dTot$MBUnrew[dTot$NumDaw==i] <- dc[2,1]
    dTot$MBUnrewp[dTot$NumDaw==i] <- dc[2,4]
    # dTot$MBUnrewAIC[dTot$NumDaw==i] <- RegLogInd$aic
    
    # Test same results as DAW
    dtPStay <- dt%>%
      group_by(PrReward, PrTransition)%>%
      summarise(PStay = mean(Stay))
    dTot$RFw[dTot$NumDaw==i] <- dtPStay$PStay[(dtPStay$PrReward == 1 & dtPStay$PrTransition == 1)]
    dTot$RRw[dTot$NumDaw==i] <- dtPStay$PStay[(dtPStay$PrReward == 1 & dtPStay$PrTransition == -1)]
    dTot$UFw[dTot$NumDaw==i] <- dtPStay$PStay[(dtPStay$PrReward == -1 & dtPStay$PrTransition == 1)]
    dTot$URw[dTot$NumDaw==i] <- dtPStay$PStay[(dtPStay$PrReward == -1 & dtPStay$PrTransition == -1)]
}}

if(Test != 0){
  dt <- filter(dComp, NS==Test)
  dtPStay <- dt%>%
    group_by(PrReward, PrTransition)%>%
    summarise(PStay = mean(Stay))
  dTot$RFw[dTot$NumDaw==Test] <- dtPStay$PStay[(dtPStay$PrReward == 1 & dtPStay$PrTransition == 1)]
  dTot$RRw[dTot$NumDaw==Test] <- dtPStay$PStay[(dtPStay$PrReward == 1 & dtPStay$PrTransition == -1)]
  dTot$UFw[dTot$NumDaw==Test] <- dtPStay$PStay[(dtPStay$PrReward == -1 & dtPStay$PrTransition == 1)]
  dTot$URw[dTot$NumDaw==Test] <- dtPStay$PStay[(dtPStay$PrReward == -1 & dtPStay$PrTransition == -1)]
  ##### Comparison with Daw
  dTest <- read.csv("D:/Dropbox/Doc_Florent/MB_MF_2/Result_Daw/Data_Daw/choice_regress.dat", sep="")
  dCompTest <- dComp%>%
    filter(NS == Test)%>%
    filter(PrReward == 1)%>%
    filter(PrTransition == -1)
  dTest <- dTest%>%
    filter(subj == Test)#%>%filter(mn == 1)%>%filter(common == -1)
}
# summary(RegLogInd)

##### Coefficients : BMB, BMF, p, B2, g, a

############################################# Export #########################################
write.table(dTot, "dTotal.txt", row.names = F, col.names = T)