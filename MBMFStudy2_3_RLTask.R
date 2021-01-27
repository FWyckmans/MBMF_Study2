remove(list = ls())

############################################ Parameter ############################################
Test = 0
Way = 'Wyck'
Datapath = "Raw_Data/RLTaskData/"
Output_path = "Output/"

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
  filter(Trial > 10)%>%filter(Trial <= 175)

# dComp <- filter(d, Step == 1)

############################################# Export #########################################
dComp <- select(dComp, NS, Trial, Resp, Reward, Transition, PrReward, PrTransition, Stay)
write.table(dComp, paste0(Output_path, "Choice_Regress.txt"), row.names = F, col.names = T)