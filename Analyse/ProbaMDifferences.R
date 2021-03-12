remove(list = ls())

############################################ Parameter ############################################
source("MBMFStudy2_Initialization.R")
Datapath = "Raw_Data/"
Output_path = "Output/"

############################################# Frame ###############################################
d <- read.delim(paste0(Output_path,"dTot.txt"))%>%
  filter(OKd == 1)%>%
  filter(!is.na(StressGr))%>%
  select(subjID, Condition, Sample, StressGr, StressGrM, StressGrSR, StressGrSRM,
         PRCw, PRRw, PUCw, PURw, PRCd, PRRd, PUCd, PURd)%>%
  mutate(MBw = PRCw - PRRw - PUCw + PURw, MFw = PRCw + PRRw - PUCw - PURw,
         MBRw = PRCw - PRRw, MBUw = PUCw - PURw,
         MBd = PRCd - PRRd - PUCd + PURd, MFd = PRCd + PRRd - PUCd - PURd,
         MBRd = PRCd - PRRd, MBUd = PUCd - PURd)

############################################ Graphics #############################################
########## My parameter (MBw:MBUw)
##### Parameter = MB
boxplot(MBw ~ Condition, data = d)
boxplot(MBw ~ Sample, data = d)

boxplot(MBw ~ StressGr*Sample, data = d)

boxplot(MBw ~ StressGrM*Sample, data = d)

boxplot(MBw ~ StressGrSR*Sample, data = d)

##### Classic Graphic
dG <- d%>%
  select(subjID, Condition, Sample, StressGr, StressGrM, StressGrSR, StressGrSRM,
         PRCw, PRRw, PUCw, PURw, PRCd, PRRd, PUCd, PURd)%>%
  gather(key = "Score", value = "Value", PRCw:PURd)
dG$StressGrM[dG$StressGrM == 1] <- "Stressed"
dG$StressGrM[dG$StressGrM == -1] <- "Not Stressed"


ClassicGraph <- function(d, Method = "Mine"){
  if (Method == "Mine"){
    dI <- filter(d, Score %in% c("PRCw", "PRRw", "PUCw", "PURw"))
  }
  
  if (Method == "Daw"){
    dI <- filter(d, Score %in% c("PRCd", "PRRd", "PUCd", "PURd"))
  }
  
  dI <- dI%>%
    group_by(Sample, StressGrM, Score)%>%
    summarise(M = mean(Value, na.rm = T), SD = sd(Value, na.rm = T), n = length(Value))
  
  for (j in c("Stressed", "Not Stressed")){  
    di <- filter(dI, StressGrM == j)
    for (i in c("HC", "Gambler", "Alc")) {
      di2 <- filter(di, Sample == i)
    
      Plot <- ggplot(data = di2, aes(x = Score, y = M)) +
        geom_bar(stat = "identity") +
        ggtitle(paste0(i, " ", j))
      print(Plot)
    }
  }
}

ClassicGraph(dG)
