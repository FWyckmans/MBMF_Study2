remove(list = ls())

############################################ Parameter ############################################
source("MBMFStudy2_Initialization.R")
Datapath = "Raw_Data/"
Output_path = "Output/"

############################################# Frame ###############################################
d <- read.delim(paste0(Output_path,"dTot.txt"))%>%
  filter(OKd == 1)

# d$WAIS = 1

############################################ Graphics ##############################################
HeatMap <- function(VD = "Computation", Pop = "All"){
  
  # Independent variables
  X = c(AllCol$Demo, AllCol$Gamb, AllCol$Alc, AllCol$Cog, AllCol$FR, AllCol$Perso)
  
  # Dependent variables
  if (VD == "Computation"){
    CoI = AllCol$Computation
  }
  
  if (VD == "RegLogInd"){
    CoI = AllCol$RegLogInd[1:4]
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
    # X = c(AllCol$Demo, AllCol$Gamb, AllCol$Alc, AllCol$Cog, AllCol$FR, AllCol$Perso)
    Z = c()
    Compt = 1
    
    for (i in unique(Y)) {
      for (x in X) {
        if (sum(!is.na(d[,grep(paste0("\\b", x, "\\b"), colnames(d))])) < 10){
          d[,grep(paste0("\\b", x, "\\b"), colnames(d))] <- 0
        }
        correlation <- cor.test(d[,grep(paste0("\\b", i, "\\b"), colnames(d))],
                                d[,grep(paste0("\\b", x, "\\b"), colnames(d))])
        Z[Compt] = correlation$estimate
        Compt = Compt+1
      }
    }
    dCorr <- data.frame(X, Y, Z)
    dCorr$Z[is.na(dCorr$Z)] <- 0
    dCorr
  }
  
  dCorr <- BigCorr()
  
  # Creation of the Heatmap
  TilePlot <- function(d, Title = ""){
    Tile <- ggplot(data = d, aes(x = X, y = Y, fill = Z)) +
      geom_tile() +
      geom_text(aes(label = round(Z, 2))) +
      theme(axis.text.x = element_text(face="bold", color="#993333", 
                                       size=8, angle=45),
            axis.text.y = element_text(face="bold", color="#993333", 
                                       size=8, angle=0)) +
      # scale_fill_distiller(palette = "RdPu") +
      # theme_ipsum() +
      scale_fill_gradient(low = "white", high = "red") +
      xlab("") +
      ylab("") +
      ggtitle(Title)
  }
  
  Tile1 <- TilePlot(dCorr, "Tout")
  Tile2 <- TilePlot(dCorr[dCorr$X %in% AllCol$Demo,], paste0("Correlation demographique - ", Pop))
  Tile3 <- TilePlot(dCorr[dCorr$X %in% AllCol$Gamb,], paste0("Correlation gambling - ", Pop))
  Tile4 <- TilePlot(dCorr[dCorr$X %in% AllCol$Alc,], paste0("Correlation alcool - ", Pop))
  Tile5 <- TilePlot(dCorr[dCorr$X %in% AllCol$Cog,], paste0("Correlation facteur cognitif - ", Pop))
  Tile6 <- TilePlot(dCorr[dCorr$X %in% AllCol$FR,], paste0("Correlation Facteur Risque - ", Pop))
  Tile7 <- TilePlot(dCorr[dCorr$X %in% AllCol$Perso,], paste0("Correlation Facteur Personnalite - ", Pop))
  
  print(Tile1)
  print(Tile2)
  print(Tile3)
  print(Tile4)
  print(Tile5)
  print(Tile6)
  print(Tile7)
}

HeatMap("Computation", "All")
HeatMap("Computation", "Alc")
HeatMap("Computation", "PG")
HeatMap("Computation", "HC")

HeatMap("RegLogInd", "All")
HeatMap("RegLogInd", "Alc")
HeatMap("RegLogInd", "PG")
HeatMap("RegLogInd", "HC")
