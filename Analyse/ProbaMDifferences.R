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
# dbp <- OutliersModif(d, c("MBw", "MFw", "MBRw", "MBUw"), Groups = c("Sample", "StressGrM"))
boxplot(MBw ~ Condition, data = d)
boxplot(MBw ~ Sample, data = d)

boxplot(MBw ~ StressGr*Sample, data = d)

boxplot(MBw ~ StressGrM*Sample, data = dbp)

boxplot(MBw ~ StressGrSR*Sample, data = dbp)

##### Classic Graphic

dG <- d%>%
  select(subjID, Condition, Sample, StressGr, StressGrM, StressGrSR, StressGrSRM,
         PRCw, PRRw, PUCw, PURw, PRCd, PRRd, PUCd, PURd)%>%
  gather(key = "Score", value = "Value", PRCw:PURd)

ClassicGraph <- function(d, Method = "Mine", byStress = 1, MultErBar = 2){
  
  # Plotting function
  Plotting <- function(){
    Plot <- ggplot(data = di2, aes(x = Score, y = M)) +
      geom_bar(stat = "identity") +
      geom_errorbar(aes(ymin = M - ErBar, ymax = M + ErBar), width = .2) +
      coord_cartesian(ylim=c(0.5,1)) +
      labs(title = Title,
           x ="Score", y = "Proba")
    print(Plot)}
  
  # Data preparation
  if (Method == "Mine"){
    dI <- filter(d, Score %in% c("PRCw", "PRRw", "PUCw", "PURw"))
  }
  
  if (Method == "Daw"){
    dI <- filter(d, Score %in% c("PRCd", "PRRd", "PUCd", "PURd"))
  }
  
  dI$Score[dI$Score == "PRCw"] <- "Rewarded Common"
  dI$Score[dI$Score == "PRRw"] <- "Rewarded Rare"
  dI$Score[dI$Score == "PUCw"] <- "Unrewarded Common"
  dI$Score[dI$Score == "PURw"] <- "Unrewarded Rare"
  
  dI$Score[dI$Score == "PRCd"] <- "Rewarded Common"
  dI$Score[dI$Score == "PRRd"] <- "Rewarded Rare"
  dI$Score[dI$Score == "PUCd"] <- "Unrewarded Common"
  dI$Score[dI$Score == "PURd"] <- "Unrewarded Rare"
  
  
  # While taking stress into account
  if(byStress == 1){
    dI <- dI%>%
      group_by(Sample, StressGrM, Score)%>%
      summarise(M = mean(Value, na.rm = T), SD = sd(Value, na.rm = T), n = length(Value))%>%
      mutate(ErBar = MultErBar*(SD/sqrt(n)))
    
    dI <<- dI
    
    # lplot <- list("HCStressed" = NULL, "PGStressed" = NULL, "AlcStressed" = NULL,
    #               "HCNotStressed" = NULL, "PGNotStressed" = NULL, "AlcNotStressed" = NULL)
    # compt = 1
    
    for (j in c("Stressed", "NotStressed")){  
      di <- filter(dI, StressGrM == j)
      for (i in c("HC", "Gambler", "Alc")) {
        di2 <- filter(di, Sample == i)
        Title <- paste0(i, " ", j, " (n = ", di2$n, ")")
        # lplot[compt] <- Plotting()
        # compt = compt + 1
        Plotting()
      }
    }
  }
  
  # Without taking stress into account
  if(byStress == 0){
    
    dI <- dI%>%
      group_by(Sample, Score)%>%
      summarise(M = mean(Value, na.rm = T), SD = sd(Value, na.rm = T), n = length(Value))%>%
      mutate(ErBar = MultErBar*(SD/sqrt(n)))
    
    dI <<- dI
    
    # lplot <- list("HC" = NULL, "PG" = NULL, "Alc" = NULL)
    # compt = 1
    
    for (i in c("HC", "Gambler", "Alc")) {
      di2 <- filter(dI, Sample == i)
      Title <- paste0(i, " (n = ", di2$n, ")")
      # lplot[compt] <- Plotting()
      # compt = compt + 1
      Plotting()
    }
  }
  # lplot
}

ClassicGraph(dG, byStress = 1)
