remove(list = ls())

############################################ Parameter ############################################
source("MBMFStudy2_Initialization.R")
Datapath = "Raw_Data/OSPAN/"
Output_path = "Output/"
Test = 0
# Participant who were instructed to press left for correct and right for false
NSInverseOspan = c(122, 126, 128, 129, 134, 135, 138, 139, 140, 142,
                   124, 121, 136, 144, 114, 130, 220)
Criterion = 0

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
  listeFichiers <- dir(path = Datapath, pattern = ".txt")
  d <- data.frame()
  for (f in listeFichiers){
    s <- read.delim(paste0(Datapath, f), header=TRUE, stringsAsFactors=FALSE)
    NS <- rep(str_sub(f, start = 1, end = -5), length(s$block.number))
    Acc <- rep(NA, length(s$block.number))
    WordAcc <- rep(NA, length(s$block.number))
    s <- cbind(NS, s, Acc, WordAcc)
    d <- rbind(d, s)
    }
  d <- d%>%
    filter(block.number > 8)%>%
    select(NS, Block = block.number, Trial = trial.number, Content = trial.contents, WordResp = set1, WordAcc, RespCorr = correct_operation, Resp = response, Acc, RT)
}

########################################## Modify frame ###########################################
ComputeAcc <- function(d){     # Fonction pour calculer les reponses correctes
  if (d$NS[1] %in% NSInverseOspan){  
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

rm_accent <- function(str,pattern="all") {
  if(!is.character(str))
    str <- as.character(str)
  
  pattern <- unique(pattern)
  
  if(any(pattern=="Ç"))
    pattern[pattern=="Ç"] <- "ç"
  
  symbols <- c(
    acute = "áéíóúÁÉÍÓÚýÝ",
    grave = "àèìòùÀÈÌÒÙ",
    circunflex = "âêîôûÂÊÎÔÛ",
    tilde = "ãõÃÕñÑ",
    umlaut = "äëïöüÄËÏÖÜÿ",
    cedil = "çÇ"
  )
  
  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )
  
  accentTypes <- c("´","`","^","~","¨","ç")
  if(any(c("all","al","a","todos","t","to","tod","todo") %in% pattern)) # opcao retirar todos
    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
  # print("OK")
  for(i in which(accentTypes%in%pattern))
    str <- chartr(symbols[i],nudeSymbols[i], str) 
  
  return(str)
}

WordClean <- function(d){
  # unwanted_array = list(    'S'='S', 's'='s', 'Z'='Z', 'z'='z', '?'='A', '?'='A', '?'='A', '?'='A', '?'='A', '?'='A', '?'='A', '?'='C', '?'='E', '?'='E',
                            # '?'='E', '?'='E', '?'='I', '?'='I', '?'='I', '?'='I', '?'='N', '?'='O', '?'='O', '?'='O', '?'='O', '?'='O', '?'='O', '?'='U',
                            # '?'='U', '?'='U', '?'='U', '?'='Y', '?'='B', '?'='Ss', '?'='a', '?'='a', '?'='a', '?'='a', '?'='a', '?'='a', '?'='a', '?'='c',
                            # '?'='e', '?'='e', '?'='e', '?'='e', '?'='i', '?'='i', '?'='i', '?'='i', '?'='o', '?'='n', '?'='o', '?'='o', '?'='o', '?'='o',
                            # '?'='o', '?'='o', '?'='u', '?'='u', '?'='u', '?'='y', '?'='y', '?'='b', '?'='y', '?' = 'ê')
  d$WordResp[d$WordResp==1|d$WordResp==0] <- NA
  d$WordResp <- str_remove_all(d$WordResp, "[?-]")
  d$WordResp <- str_remove_all(d$WordResp, "[²]")
  for (i in c(1:length(d$NS))){
    d$Word[i] <- rm_accent(d$Word[i])
    d$Word[i] <- tolower(d$Word[i])
    d$WordResp[i] <- rm_accent(d$WordResp[i])
    d$WordResp[i] <- tolower(d$WordResp[i])
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
  dt <- filter(d, NS == i) # Get frame for participant
  
  dt <- ComputeAcc(dt) # Compute the accuracy for each trial
  dt <- separate(dt, Content, into = c("Calc", "Word"), sep = " ?, ")
  dt <- WordClean(dt) # Word = NA for calcul, remove accents and strange symbols
  dt$WordResp[dt$WordResp == " "] <- "NoResp"  # Code forgotten words
  dt$WordResp[dt$WordResp == "byciclette"] <- "bicyclette"
  # dt$WordResp[dt$WordResp == "voix"] <- "voie"
  # dt$WordResp[dt$WordResp == "cris"] <- "cri"
  # dt$WordResp[dt$WordResp == "crie"] <- "cri"
  
  dt <- WordRespCol(dt) # Create a col with the word written
  dt <- CorrectWord(dt) # Check if the word is correct
  dT <- rbind(dT, dt) # Final tab
}

d <- dT%>%
  select(NS, Block, Trial, Word, WordResp, WordAcc, Acc, RT)%>%
  group_by(NS, Block)%>%
  summarise(nWordAcc = sum(WordAcc, na.rm = T), WordAcc = mean(WordAcc, na.rm = T), Acc = mean(Acc, na.rm = T), RT = mean(RT, na.rm = T),
            nTrial = length(Trial), .groups = 'drop')%>%
  group_by()%>%
  select(NS, WordAcc, nWordAcc, nTrial, Acc, RT)

######################################### Compute score ###########################################
CalcOK <- rep(0, length(d$NS))
LenOK <- rep(0, length(d$NS))
d <- cbind(d, CalcOK, LenOK)

# Changed to NA scores from PS who did not finish the OSPAN task
NSunfinished <- c()
Compt = 1
for (i in unique(d$NS)) {
  if (length(d$NS[d$NS == i]) == 15){
  d$LenOK[d$NS == i] <- 1
  } else {
    d$LenOK[d$NS == i] <- 0
    NSunfinished[Compt] <- i
    Compt = Compt + 1
  }
}

d$CalcOK[d$Acc >= Criterion] <- 1

# d$nWordAcc[d$CalcOK==0] <- 0

##### Scoring
# dOspan <- d%>%   # Original
#   filter(CalcOK == 1)%>%
#   group_by(NS)%>%
#   summarise(nWord = sum(nWordAcc, na.rm = T), RT = mean(RT, na.rm = T))%>%
#   rename(subjID = NS)

dOspan <- d%>% # PCU (Unsworth, 2005)
  # filter(CalcOK == 1)%>%
  mutate(Prop = nWordAcc/nTrial)%>%
  group_by(NS)%>%
  summarise(nWord = mean(Prop, na.rm = T), RT = mean(RT, na.rm = T))%>%
  rename(subjID = NS)

dOspan$nWord[dOspan$subjID %in% NSunfinished] <- NA

############################################# Export ##############################################
write.table(dOspan, paste0(Output_path, "dOSPAN.txt"), row.names = F, col.names = T, dec = ".", sep = "\t")

dCh <- dT%>%
  filter(WordAcc == 0)%>%
  filter(NS > 200)%>%
  filter(WordResp != "NoResp")%>%
  arrange(NS)
