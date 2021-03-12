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
    summarise(M = mean(Value, na.rm = T), SD = sd(Value, na.rm = T), n = length(Value))%>%
    mutate(ErBar = 2*(SD/sqrt(n)))
  
  dI$Score[dI$Score == "PRCw"] <- "Rewarded Common"
  dI$Score[dI$Score == "PRRw"] <- "Rewarded Rare"
  dI$Score[dI$Score == "PUCw"] <- "Unrewarded Common"
  dI$Score[dI$Score == "PURw"] <- "Unrewarded Rare"
  
  dI$Score[dI$Score == "PRCd"] <- "Rewarded Common"
  dI$Score[dI$Score == "PRRd"] <- "Rewarded Rare"
  dI$Score[dI$Score == "PUCd"] <- "Unrewarded Common"
  dI$Score[dI$Score == "PURd"] <- "Unrewarded Rare"
  
  dI <<- dI
  
  for (j in c("Stressed", "Not Stressed")){  
    di <- filter(dI, StressGrM == j)
    for (i in c("HC", "Gambler", "Alc")) {
      di2 <- filter(di, Sample == i)
    
      Plot <- ggplot(data = di2, aes(x = Score, y = M)) +
        geom_bar(stat = "identity") +
        geom_errorbar(aes(ymin = M - ErBar, ymax = M + ErBar), width = .2) +
        # scale_y_continuous("Proba") +
        # ylim(0.5, 1) +
        coord_cartesian(ylim=c(0.5,1)) +
        labs(title=paste0(i, " ", j),
             x ="Score", y = "Proba")
      print(Plot)
    }
  }
}

ClassicGraph(dG)
