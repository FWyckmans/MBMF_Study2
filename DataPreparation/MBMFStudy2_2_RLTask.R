remove(list = ls())

############################################ Parameter ############################################
source("MBMFStudy2_Initialization.R")
Datapath = "Raw_Data/RLTaskData/"
Output_path = "Output/"
Test = 0
Way = 'Wyck'
Playlist = "This is: Elton John by Spotify, Benny and the Jets, random"

############################################ Frame ################################################
if (Test != 0){
  PS <- paste0(Datapath , Test, ".dat")
  d <- read.delim(PS, header=FALSE, stringsAsFactors=FALSE)
}

if (Test == 0){
  listeFichiers <- dir(path = Datapath, pattern = ".dat")
  d <- data.frame()
  for (f in listeFichiers){
    s <- read.delim(paste0(Datapath, f), header=FALSE, stringsAsFactors=FALSE)
    d <- rbind(d, s)}}

d <- d%>%
  select(NS = V1, Trial = V3, Step = V4, Choice1 = V5, Choice2 = V6, Resp1 = V7, Resp2 = V9, Reward = V11,
         RT1 = V8, RT2 = V10)%>%
  filter(!is.na(Trial))%>%
  arrange(Trial) #%>%filter(Trial >= 10)  # Here I keep the 10th trial to compute the PrReward and PrTransition of the 11th but I remove it afterwards

## Change Step columns
d$Step <- str_sub(d$Step, start = 6, end = -9)

## Remove transition probabilities
d$RT1[d$Step == 2] <- NA  # The RT < than 0 corresponds to transition probabilities
d$Resp1[d$Step == 2] <- NA # The Resp1 < than 0 corresponds to transition probabilities

###################################### Features engineering #######################################
##### New columns
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

##### One Resp column
for (i in (1:length(d$NS))){
  if (d$Step[i] == "1"){
    d$Resp[i] = d$Resp1[i]}
  if (d$Step[i] == "2"){
    d$Resp[i] = d$Resp2[i]}
  if (d$Step[i] == "1" & d$Step[i+1] == "1"){   # Missed first step
    d$Resp[i] = -1
    d$OK1[i] = -1}}

##### Remove unanswered
d <- filter(d, d$OK1 != -1)   # Totally remove trials where participant did not answer the 1st step

for (i in (1:length(d$NS))){
  if (d$Step[i] == "2" & d$Resp[i] == "-1"){   # Missed second step
    d$OK2[i] = -1
    d$OK2[i-1] = -1}}

##### Remove trials after unanswered trial (Daw's way)
if (Way == "Daw"){
  d <- filter(d, OK2 != -1)
}

##### Compute transitions
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

##### Compute Stay
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

##### Compute PrReward & PrTransition
for (i in c(1:length(d$NS))){
  if (d$Trial[i] != min(d$Trial) & d$Step[i] == "1"){
    d$Reward[i] = d$Reward[i+1]
    
    d$PrReward[i] = d$Reward[i-1]
    d$PrReward[i+1] = d$Reward[i-1]
    
    d$PrTransition[i] = d$Transition[i-1]
    d$PrTransition[i+1] = d$Transition[i-1]}}

##### Remove trials after unanswered trial (My way)
if (Way == "Wyck"){
  d <- filter(d, PrReward != -1)
}

##### Remove Trial 10 and above 175
d <- d%>%
  filter(Trial > 10)#%>%filter(Trial <= 175) Keep trial until max!

# dComp <- filter(d, Step == 1)

##### Separate dataframe into 3: Step1, Step2 and misc
dS1 <- d%>%
  filter(Step == "1")%>%
  select(NS, Trial,
         level1_choice = Resp1, RT1)%>%
  arrange(NS, Trial)

dS2 <- d%>%
  filter(Step == "2")%>%
  select(NS, Trial,
         level2_choice = Resp2, RT2)%>%
  arrange(NS, Trial)

dMisc <- d%>%
  filter(Step == "1")%>%
  select(subjID = NS, Trial, # Changed NS to subjID to fit in the ts_par7 function
         reward = Reward, transition = Transition,  # Removed the uppercase to fit in the ts_par7 function
         Stay, PrReward, PrTransition)%>%
  arrange(subjID, Trial)

d <- cbind(dMisc[c(1, 2)], dS1[3], dS2[3], dMisc[c(3: length(dMisc))], dS1[4], dS2[4])

##### Change Step 2 choice codes
d <- filter(d, level2_choice > 0)

d$level2_choice <- d$level2_choice - 2 

############################################### RT ###########################################
dRT1 <- d%>%
  group_by(subjID, PrReward)%>%
  summarise(RT1 = mean(RT1), .groups = 'drop')%>%
  spread(key = PrReward, value = "RT1")%>%
  arrange(subjID)

colnames(dRT1)[2] <- "UnRewRT1"
colnames(dRT1)[3] <- "RewRT1"

dRT2 <- d%>%
  group_by(subjID, transition)%>%
  summarise(RT2 = mean(RT2), .groups = 'drop')%>%
  spread(key = transition, value = "RT2")%>%
  arrange(subjID)

colnames(dRT2)[2] <- "RareRT2"
colnames(dRT2)[3] <- "CommonRT2"

dRT <- cbind(dRT1, dRT2[c(2,3)])

dRT <- dRT%>%
  mutate(dRT1 = RewRT1 - UnRewRT1,
         dRT2 = RareRT2 - CommonRT2)

############################################# Export #########################################
write.table(d, paste0(Output_path, "ComputationsReady.txt"), row.names = F, col.names = T)
write.table(dRT, paste0(Output_path, "dRT.txt"), row.names = F, col.names = T, sep = "\t", dec = ".")
