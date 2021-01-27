#################################### Initialisation ###############################################
remove(list = ls())

library(dplyr)
library(tidyr)
library(stringr)

############################################ Parameter ############################################
Test = 0
Datapath = "Raw_Data/OSPAN/"
Output_path = "Output/"
NSInverseOspan = c()

############################################ Frame ################################################
if (Test != 0){
  d <- read.delim(paste0(Datapath, Test, ".txt"), header=TRUE, stringsAsFactors=FALSE)
  NS <- rep(Test, length(d$block.number))
  Acc <- rep(NA, length(d$block.number))
  WordAcc <- rep(NA, length(d$block.number))
  d <- cbind(NS, d, Acc, WordAcc)
  d <- d%>%
    filter(block.number > 8)%>%
    select(NS, Block = block.number, Trial = trial.number, Content = trial.contents,
           WordResp = set1, WordAcc, RespCorr = correct_operation, Resp = response, Acc, RT)}

if (Test == 0){
  listeFichiers <- dir(pattern = ".txt")
  d <- data.frame()
  for (f in listeFichiers){
    s <- read.delim(f, header=TRUE, stringsAsFactors=FALSE)
    NS <- rep(str_sub(f, start = 1, end = -5), length(s$block.number))
    Acc <- rep(NA, length(s$block.number))
    WordAcc <- rep(NA, length(s$block.number))
    s <- cbind(NS, s, Acc, WordAcc)
    d <- rbind(d, s)}
  d <- d%>%
    filter(block.number > 8)%>%
    select(NS, Block = block.number, Trial = trial.number, Content = trial.contents, WordResp = set1, WordAcc, RespCorr = correct_operation, Resp = response, Acc, RT)}

########################################## Modify frame ###########################################
ComputeAcc <- function(d){     # Fonction pour calculer les reponses correctes
  if (d$NS %in% NSInverseOspan){  
    d$Acc[d$Resp == "correct" & d$RespCorr == 1] <- 0
    d$Acc[d$Resp == "incorrect" & d$RespCorr == 0] <- 0
    d$Acc[d$Resp == "correct" & d$RespCorr == 0] <- 1
    d$Acc[d$Resp == "incorrect" & d$RespCorr == 1] <- 1}
  
  else{  
    d$Acc[d$Resp == "correct" & d$RespCorr == 1] <- 1
    d$Acc[d$Resp == "incorrect" & d$RespCorr == 0] <- 1
    d$Acc[d$Resp == "correct" & d$RespCorr == 0] <- 0
    d$Acc[d$Resp == "incorrect" & d$RespCorr == 1] <- 0}
  return(d)
}

WordClean <- function(d){
  unwanted_array = list(    'S'='S', 's'='s', 'Z'='Z', 'z'='z', '?'='A', '?'='A', '?'='A', '?'='A', '?'='A', '?'='A', '?'='A', '?'='C', '?'='E', '?'='E',
                            '?'='E', '?'='E', '?'='I', '?'='I', '?'='I', '?'='I', '?'='N', '?'='O', '?'='O', '?'='O', '?'='O', '?'='O', '?'='O', '?'='U',
                            '?'='U', '?'='U', '?'='U', '?'='Y', '?'='B', '?'='Ss', '?'='a', '?'='a', '?'='a', '?'='a', '?'='a', '?'='a', '?'='a', '?'='c',
                            '?'='e', '?'='e', '?'='e', '?'='e', '?'='i', '?'='i', '?'='i', '?'='i', '?'='o', '?'='n', '?'='o', '?'='o', '?'='o', '?'='o',
                            '?'='o', '?'='o', '?'='u', '?'='u', '?'='u', '?'='y', '?'='y', '?'='b', '?'='y' )
  d$WordResp[d$WordResp==1|d$WordResp==0] <- NA
  d$WordResp <- str_remove_all(d$WordResp, "[?-]")
  for (i in c(1:length(d$NS))){
    d$Word[i] <- chartr(paste(names(unwanted_array), collapse=''),
                        paste(unwanted_array, collapse=''),
                        d$Word[i])
    d$WordResp[i] <- chartr(paste(names(unwanted_array), collapse=''),
                        paste(unwanted_array, collapse=''),
                        d$WordResp[i])
  }
  return(d)
}

WordRespCol <- function(d){
  dT <- data.frame()
  for (i in unique(d$Block[d$Block %% 2 == 1])){
    dt <- filter(d, (d$Block == i|d$Block == i+1))
    for (l in c(1:max(dt$Trial))){
      dt$WordResp[l] <- dt$WordResp[l+max(dt$Trial)]
    }
    dt <- filter(dt, Block%%2 == 1)
    dT <- rbind(dT, dt)
  }
  return(dT)}

CorrectWord <- function(d){
  d$WordAcc[tolower(str_remove_all(d$Word, "[ ]")) == tolower(str_remove_all(d$WordResp, "[ ]"))] <- 1
  d$WordAcc[tolower(str_remove_all(d$Word, "[ ]")) != tolower(str_remove_all(d$WordResp, "[ ]"))] <- 0
  return(d)
}

dT <- data.frame()
for (i in unique(d$NS)){
  print(i)
  dt <- filter(d, NS == i) # Get frame for participant
  
  dt <- ComputeAcc(dt) # Compute the accuracy for each trial
  dt <- separate(dt, Content, into = c("Calc", "Word"), sep = " ?, ")
  dt <- WordClean(dt) # Word = NA for calcul, remove accents and strange symbols
  dt$WordResp[dt$WordResp == " "] <- "NoResp"  # Code forgotten words
  dt <- WordRespCol(dt) # Create a col with the word written
  dt <- CorrectWord(dt) # Check if the word is correct
  dT <- rbind(dT, dt) # Final tab
}

d <- dT%>%
  select(NS, Block, Trial, Word, WordResp, WordAcc, Acc, RT)%>%
  group_by(NS, Block)%>%
  summarise(nWordAcc = sum(WordAcc, na.rm = T), WordAcc = mean(WordAcc, na.rm = T), Acc = mean(Acc, na.rm = T), RT = mean(RT, na.rm = T),
            nTrial = length(Trial))%>%
  group_by()%>%
  select(NS, WordAcc, nWordAcc, nTrial, Acc, RT)

######################################### Compute score ###########################################
CalcOK <- rep(0, length(d$NS))
d <- cbind(d, CalcOK)
d$CalcOK[d$Acc>0.79] <- 1

d$nWordAcc[d$CalcOK==0] <- 0

dOspan <- d%>%
  # filter(CalcOK == 1)%>%
  group_by(NS)%>%
  summarise(nWord = sum(nWordAcc, na.rm = T), RT = mean(RT, na.rm = T))

############################################# Export ##############################################
write.table(dOspan, paste0(Output_path, "dOSPAN.txt"), row.names = F, col.names = T, dec = ".", sep = "\t")
