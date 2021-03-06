#################################### Initialization ###############################################
##### Data path
Datapath = "Raw_Data/"
Output_path = "Output/"

##### Cran packages
## Install
packages <- c("dplyr", "tidyr",
              "ggplot2", "gridExtra", "cowplot", "corrplot",
              "nlme", "lmerTest", "BayesFactor", "stats",
              "car", "readxl", "readr", "Hmisc", "rms", "ISLR", "e1071", "stringr", "writexl",
              "hBayesDM", "FactoMineR", "factoextra")

new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## Data
library(dplyr)
library(tidyr)

## Graphics
library(ggplot2)
library(gridExtra)
library(cowplot)
library(corrplot)

## Stat and ML
library(nlme)
library(lmerTest)
library(BayesFactor)
library(stats)
library(FactoMineR)
library(factoextra)

## Misc (or don't remember and imported anyway)
library(car)
library(readxl)
library(readr)
library(Hmisc)
library(rms)
library(ISLR)
library(e1071)
library(stringr)
library(writexl)

## Specific
library(hBayesDM)
Sys.setenv(BUILD_ALL='true')  # Build all the models on installation
# Sys.setenv(MAKEFLAGS='-j 4')  # Use 4 cores for compilation (or the number you want)

##### Functions
for (Fun in dir("Functions/")) {
  source(paste0("Functions/", Fun))
}

TestDF()

##### Vector with column names by type of variable for further references
AllCol <- list(
  ID = c("subjID", "NS", "Initiales"),
  Demo = c("Age", "StudyLevel"),
  Condition = c("FinalCondition", "Condition", "Sample", "StressGr", "StressGrM", "StressGrSR", "StressGrSRM", "Patho"),
  Gamb = c("SOGS", "DSM", "Craving"),
  Alc = c("AUDIT", "DSMal"),
  Cog = c("OSPAN", "WAIS", "Raven"),
  FR = c("dCraving", "dResist", "dStress", "dPain", "dCorti",
         "dCravingM", "dResistM", "dStressM", "dPainM", "dCortiM",
         "Craving1", "Craving2", "Craving3", "Craving4",
         "Resist1", "Resist2", "Resist3", "Resist4",
         "Stress1", "Stress2", "Stress3", "Stress4",
         "Pain1", "Pain2", "Pain3", "Pain4",
         "Corti1", "Corti2", "Corti3", "Corti4"),
  Perso = c("SCL90R", "Fagerstrom", "Beck",
            "PANASPos", "PANASNeg", "STAIA", "STAIB", "SRRS",
            "PunitionSens", "RewardSens",
            "Routine", "Auto",
            "UPPS_Total", "NegUr", "PosUr", "LackOfPrem", "LackOfPers", "Sensation"),
  Computation = c("a1", "beta1", "a2", "beta2", "pi", "w", "lambda"),
  RegLogInd = c("MB", "MF", "RewMB", "UnrewMB", "MBp", "MFp", "RewMBp", "UnrewMBp"),
  ProbaM = c("PRCw", "PRRw", "PUCw", "PURw", "PRCd", "PRRd", "PUCd", "PURd",
             "MBsw", "MFsw", "MBURsw", "MBRsw",
             "MBsd", "MFsd", "MBURsd", "MBRsd"),
  Interaction = c("RavenXdCortM", "OSPANxdCortM"),
  ManipCheck = c("OKd"),
  PCA = c("SOGS", "AUDIT", "Fagerstrom",
          "Beck", "PANASPos", "PANASNeg", "SCL90R",
          "STAIA", "STAIB", "SRRS", "PunitionSens", "RewardSens",
          "UPPS_Total", "NegUr", "PosUr", "LackOfPrem", "LackOfPers", "Sensation",
          "Routine", "Auto"))
