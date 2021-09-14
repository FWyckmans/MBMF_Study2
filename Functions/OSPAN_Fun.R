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