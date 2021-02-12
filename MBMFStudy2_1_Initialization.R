#################################### Initialization ###############################################
##### Data path
Datapath = "Raw_Data/"
Output_path = "Output/"

##### Cran packages
## Data
library(dplyr)
library(tidyr)

## Graphics
library(ggplot2)
library(gridExtra)
library(cowplot)

## Stat and ML
library(nlme)
library(lmerTest)
library(BayesFactor)
library(stats)

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
# Sys.setenv(BUILD_ALL='true')  # Build all the models on installation
# Sys.setenv(MAKEFLAGS='-j 4')  # Use 4 cores for compilation (or the number you want)

##### Functions
source("Functions/AddDummyCol.R")
source("Functions/FillCol.R")

##### Vector with column names by type of variable for further references
AllCol <- list(
  ID = c("subjID", "NS", "Initiales"),
  Demo = c("Age", "StudyLevel"),
  Condition = c("Condition", "Sample", "StressGr", "StressGrM", "StressGrSR", "StressGrSRM", "Patho"),
  Gamb = c("SOGS", "DSM", "Craving"),
  Alc = c("AUDIT"),
  Cog = c("OSPAN", "WAIS", "Raven"),
  FR = c("dCraving", "dResist", "dStress", "dPain", "dAnalyse",
         "dCravingM", "dResistM", "dStressM", "dPainM", "dAnalyseM",
         "Craving1", "Craving2", "Craving3", "Craving4",
         "Resist1", "Resist2", "Resist3", "Resist4",
         "Stress1", "Stress2", "Stress3", "Stress4",
         "Pain1", "Pain2", "Pain3", "Pain4",
         "Analyse_1", "Analyse_2", "Analyse_3", "Analyse_4"),
  Perso = c("SCL90R", "Fagerstrom", "Beck",
            "Affect_positif", "Affect_Negatif", "STAIA", "STAIB", "SRRS",
            "Sensibilite_Punition", "Sensibilite_Recompense",
            "Routine", "Auto",
            "UPPS_Total", "Urgence", "UrgencePos", "Manquedepremeditation", "Manquedeperseverance", "Sensation"),
  Computation = c("a1", "beta1", "a2", "beta2", "pi", "w", "lambda"),
  RegLogInd = c("MB", "MF", "RewMB", "UnrewMB", "MBp", "MFp", "RewMBp", "UnrewMBp"),
  ProbaM = c("PRCw", "PRRw", "PUCw", "PURw", "PRCd", "PRRd", "PUCd", "PURd"),
  ManipCheck = c("OKd"))
