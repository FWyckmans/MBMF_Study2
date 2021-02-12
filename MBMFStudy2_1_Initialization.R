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
         "Envie_1", "Envie_2", "Envie_3", "Envie_4",
         "Resister_1", "Resister_2", "Resister_3", "Resister_4",
         "Stress_1", "Stress_2", "Stress_3", "Stress_4",
         "Douleur_1", "Douleur_2", "Douleur_3", "Douleur_4",
         "Analyse_1", "Analyse_2", "Analyse_3", "Analyse_4"),
  Perso = c("SCL90R", "Fagerstrom", "Beck",
            "Affect_positif", "Affect_Negatif", "STAIA", "STAIB", "SRRS",
            "Sensibilite_Punition", "Sensibilite_Recompense",
            "Routine", "Auto",
            "UPPS_Total", "Urgence", "UrgencePos", "Manquedepremeditation", "Manquedeperseverance", "Sensation"))
