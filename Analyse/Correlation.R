remove(list = ls())

############################################ Parameter ############################################
source("MBMFStudy2_Initialization.R")
Datapath = "Raw_Data/"
Output_path = "Output/"

############################################# Frame ###############################################
d <- read.delim(paste0(Output_path,"dTot.txt"))%>%
  filter(OKd == 1)%>%
  filter(!is.na(StressGr))%>%
  filter(subjID != 334)

# d$WAIS = 1

# Outlierremoval
# d <- OutliersModif(d, c(AllCol$Demo, AllCol$Gamb, AllCol$Alc, AllCol$Cog, AllCol$FR, AllCol$Perso), Groups = "Sample")

############################################ Graphics ##############################################
HeatMap <- function(VD = "Computation", Pop = "All"){
  
  # Independent variables
  X = c(AllCol$Demo, AllCol$Gamb, AllCol$Alc, AllCol$Cog, AllCol$FR, AllCol$Perso, AllCol$Interaction)
  
  # Dependent variables
  if (VD == "Computation"){
    CoI = AllCol$Computation
  }
  
  if (VD == "RegLogInd"){
    CoI = AllCol$RegLogInd[1:4]
  }
  
  if (VD == "Seboldw"){
    CoI = AllCol$ProbaM[9:12]
  }
  
  if (VD == "Seboldd"){
    CoI = AllCol$ProbaM[13:16]
  }
  
  Y = rep(CoI, each = length(X))
  
  # Select appropriate sample
  if (Pop == "Alc"){
    d <- filter(d, Sample == "Alc")
  }
  
  if (Pop == "PG"){
    d <- filter(d, Sample == "Gambler")
  }
  
  if (Pop == "HC"){
    d <- filter(d, Sample == "HC")
  }
  
  # Creation of a correlation dataframe
  BigCorr <- function(){
    Z = c()
    p = c()
    Compt = 1
    
    for (i in unique(Y)) {
      for (x in X) {
        if (sum(!is.na(d[,grep(paste0("\\b", x, "\\b"), colnames(d))])) < 10){
          d[,grep(paste0("\\b", x, "\\b"), colnames(d))] <- 0
        }
        correlation <- cor.test(d[,grep(paste0("\\b", i, "\\b"), colnames(d))],
                                d[,grep(paste0("\\b", x, "\\b"), colnames(d))])
        Z[Compt] = correlation$estimate
        p[Compt] = correlation$p.value
        Compt = Compt+1
      }
    }
    dCorr <- data.frame(X, Y, Z, p)
    dCorr$Z[is.na(dCorr$Z)] <- 0
    dCorr
  }
  
  dCorr <- BigCorr()
  
  # Add * for significant correlation
  dCorr <- AddDummyCol(dCorr, c("Sig", "Label"))
  dCorr$Sig[dCorr$p < .1] <- "."
  dCorr$Sig[dCorr$p < .05] <- "*"
  dCorr$Sig[dCorr$p < .01] <- "**"
  dCorr$Sig[dCorr$p < .001] <- "***"
  dCorr$Sig[dCorr$p >= .1] <- ""
  
  dCorr$Label <- paste0(round(dCorr$Z, 3), dCorr$Sig)
  dCorr$Label[dCorr$Label=="NANA"] <- "0"
  dCorr$Label[dCorr$Label=="0NA"] <- "0"
  
  # Creation of the Heatmap
  TilePlot <- function(d, Title = ""){
    Tile <- ggplot(data = d, aes(x = X, y = Y, fill = Z)) +
      geom_tile() +
      scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
      # scale_fill_gradientn(colours = c("blue", "white", "red"), values = c(-0.5, 0, 0.5)) +
      geom_text(aes(label = Label)) +
      theme(axis.text.x = element_text(face="bold", color="#993333", 
                                       size=8, angle=45),
            axis.text.y = element_text(face="bold", color="#993333", 
                                       size=8, angle=0)) +
      # scale_fill_distiller(palette = "RdPu") +
      # theme_ipsum() +
      # scale_fill_gradient(low = "white", high = "red") +
      xlab("") +
      ylab("") +
      ggtitle(Title)
  }
  
  Tile1 <- TilePlot(dCorr, paste0("Tout - ", Pop))
  Tile2 <- TilePlot(dCorr[dCorr$X %in% AllCol$Demo,], paste0("Correlation demographique - ", Pop))
  Tile3 <- TilePlot(dCorr[dCorr$X %in% AllCol$Gamb,], paste0("Correlation gambling - ", Pop))
  Tile4 <- TilePlot(dCorr[dCorr$X %in% AllCol$Alc,], paste0("Correlation alcool - ", Pop))
  Tile5 <- TilePlot(dCorr[dCorr$X %in% AllCol$Cog,], paste0("Correlation facteur cognitif - ", Pop))
  Tile6 <- TilePlot(dCorr[dCorr$X %in% AllCol$FR,], paste0("Correlation Facteur Risque - ", Pop))
  Tile7 <- TilePlot(dCorr[dCorr$X %in% AllCol$Perso,], paste0("Correlation Facteur Personnalite - ", Pop))
  Tile8 <- TilePlot(dCorr[dCorr$X %in% AllCol$Interaction,], paste0("Correlation interaction - ", Pop))
  
  print(Tile1)
  print(Tile2)
  print(Tile3)
  print(Tile4)
  print(Tile5)
  print(Tile6)
  print(Tile7)
  print(Tile8)
  
  dCorr <<- dCorr
}

d <- filter(d, Sample != "Alc")

HeatMap("Computation", "All")
# HeatMap("Computation", "Alc")
# HeatMap("Computation", "PG")
# HeatMap("Computation", "HC")
# summary(lm(zw ~ OSPAN*SampleC*dCorti, data = d))
summary(lm(zw ~ OSPAN * dCorti, data = d))

summary(lm(zw ~ Raven*SampleC*dCorti, data = d))
summary(lm(zw ~ Raven*dCorti, data = d))

# HeatMap("RegLogInd", "All")
# HeatMap("RegLogInd", "Alc")
# HeatMap("RegLogInd", "PG")
# HeatMap("RegLogInd", "HC")

HeatMap("Seboldw", "All")
HeatMap("Seboldw", "Alc")
HeatMap("Seboldw", "PG")
HeatMap("Seboldw", "HC")


d <- filter(d, Sample != "Alc")
dl <- filter(d, dCorti < median(d$dCorti, na.rm = T))
dm <- filter(d, dCorti >= median(d$dCorti, na.rm = T))

Nuage(d,
      paste0(Output_path, "CorrWxdCort.tiff"),
      VIname = "Elevation du Cortisol",
      VI = d$dCorti,
      VDname = "Paramètre w",
      VD = d$w)

Nuage(d,
      paste0(Output_path, "CorrWxRaven.tiff"),
      VIname = "Score aux matrices de Raven",
      VI = d$Raven,
      VDname = "Paramètre w",
      VD = d$w)

Nuage(d,
      paste0(Output_path, "CorrWxSOGS.tiff"),
      VIname = "SOGS",
      VI = d$SOGS,
      VDname = "Paramètre w",
      VD = d$w)

Nuage(dm,
      paste0(Output_path, "CorrWxRaven_Stressed.tiff"),
      VIname = "Score Raven",
      VI = dm$Raven,
      VDname = "Paramètre w",
      VD = dm$w)

Nuage(dl,
      paste0(Output_path, "CorrWxRaven_NotStressed.tiff"),
      VIname = "Score Raven",
      VI = dl$Raven,
      VDname = "Paramètre w",
      VD = dl$w)

dl <- filter(d, StressGr == "NotStressed")
dm <- filter(d, StressGr == "Stressed")

Nuage(dl,
      paste0(Output_path, "CorrWxRaven_NotStressed.tiff"),
      VIname = "Score Raven",
      VI = dl$Raven,
      VDname = "Paramètre w",
      VD = dl$w)

Nuage(dm,
      paste0(Output_path, "CorrWxRaven_Stressed.tiff"),
      VIname = "Score Raven",
      VI = dm$Raven,
      VDname = "Paramètre w",
      VD = dm$w)
