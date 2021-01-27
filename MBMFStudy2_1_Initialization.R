#################################### Initialization ###############################################
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

## Specific
library(hBayesDM)
Sys.setenv(BUILD_ALL='true')  # Build all the models on installation
Sys.setenv(MAKEFLAGS='-j 4')  # Use 4 cores for compilation (or the number you want)