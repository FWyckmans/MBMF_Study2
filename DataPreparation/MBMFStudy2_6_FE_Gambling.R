remove(list = ls())

############################################ Parameter ############################################
source("MBMFStudy2_Initialization.R")
# source("DataPreparation/MBMFStudy2_5_ClinicalData.R")
Datapath = "Output/"
Output_path = "Output/"
OSPANImputer = "RavenSample"
TypeComp = "PGHC"

############################################ Frame ################################################
########## Clinical frame
d <- read.delim(paste0(Output_path, "dOKGam.txt"))

if (TypeComp == "Tot"){
  dCompParameter <- read.delim(paste0(Output_path, "ComputationParameter7P_OK_HCPGAL.txt"))
  OutputName = "dOKGamFE_Comp7P_OK_HCPGAL.txt"
}

if (TypeComp == "PGHC"){
  dCompParameter <- read.delim(paste0(Output_path, "ComputationParameter7P_OK_HCPG.txt"))
  OutputName = "dOKGamFE_Comp7P_OK_HCPG.txt"
}

dCompParameter1 <- read.delim(paste0(Output_path, "ComputationParameter7P_OK_HCPG_1stHalf.txt"))
dCompParameter2 <- read.delim(paste0(Output_path, "ComputationParameter7P_OK_HCPG_2ndHalf.txt"))

# ModelFit <- readRDS(paste0(Output_path, "Models/output7P_OKHCPG.Rdata"))

###################################### Features engineering #######################################
########## Impute OSPAN
### Impute OSPAN with Group
if (OSPANImputer == "Mean"){
  for (i in 1:length(d$OSPAN)) {
    if (is.na(d$OSPAN[i])){
      d$OSPAN[i] <- mean(d$OSPAN[d$Sample == d$Sample[i]], na.rm = T)
    }
  }
}

### Impute OSPAN with Raven only
if (OSPANImputer == "Raven"){
  ModRavOsp <- lm(OSPAN ~ Raven, data = d)
  summary(ModRavOsp)


  for (i in 1:length(d$OSPAN)) {
    if (is.na(d$OSPAN[i])){
      d$OSPAN[i] <- ModRavOsp$coefficients[1] + ModRavOsp$coefficients[2] * d$Raven[i]
    }
  }
}

### Impute OSPAN with Raven + Sample
if (OSPANImputer == "RavenSample"){
  ModRavOsp <- lm(OSPAN ~ SampleC + Raven, data = d)
  summary(ModRavOsp)


  for (i in 1:length(d$OSPAN)) {
    if (is.na(d$OSPAN[i])){
      d$OSPAN[i] <- ModRavOsp$coefficients[1] + ModRavOsp$coefficients[2] * d$SampleC[i] +
        ModRavOsp$coefficients [3] * d$Raven[i]
    }
  }
}

######### OSPAN group
d <- AddDummyCol(d, "OSPANgr", -1)
d$OSPANgr[d$OSPAN>=median(d$OSPAN, na.rm = T)] <- 1

########## Correct study Level
d$StudyLevel[d$StudyLevel == 0] <- NA

########## Change Stress Grp to coded
d$StressGrM[d$StressGrM == "Stressed"] <- -1
d$StressGrM[d$StressGrM == "NotStressed"] <- 1

########## Stress Response Baseline
## Compute composite score
d <- AddDummyCol(d, c("CrtBM12", "CrtBM34", "dCrtBM", "CrtB32_2", "dCrtB32"))
# Like OR (difference between the means)
d$CrtM12 <- (d$Corti1+d$Corti2)/2
d$CrtM34 <- (d$Corti3+d$Corti4)/2
d$dCrtM <- d$CrtM34 - d$CrtM12

# Baseline cortisol for graphics
d <- AddDummyCol(d, c("CrtB1", "CrtB2", "CrtB3", "CrtB4"))
d$CrtB1[!is.na(d$Corti1)] <- 0
d$CrtB2 <- d$Corti2 - d$Corti1
d$CrtB3 <- d$Corti3 - d$Corti1
d$CrtB4 <- d$Corti4 - d$Corti1

d$CrtBM12 <- 0
d$CrtBM34 <- d$CrtM34 - d$CrtM12
d$dCrtBM <- d$CrtBM34 - d$CrtBM12

# Other test (only keep T2 and T3)
d$CrtB32_2 <- 0
d$dCrtB32 <- d$Corti3-d$Corti2

##### Compute Baseline cortisol group (cfr Biback et al. 2015)
d$BaselineGr[d$Corti1 < median(d$Corti1)] <- -1

########## Interaction
d <- d%>%
  mutate(GrpxRaven = SampleC*Raven,
         GrpxOSPAN = SampleC*OSPAN,

         # With normal Cortisol Measures (3-2)
         RavenxdCorti = Raven * dCorti,  # Raven*dCorti
         OSPANxdCorti = OSPAN * dCorti,  # OSPAN*dCorti
         GrpxdCorti = SampleC * dCorti,
         GrpxRavenxdCorti = SampleC*Raven*dCorti,
         GrpxOSPANxdCorti = SampleC*OSPAN*dCorti,

         # With normal Cortisol Measures ((3_4)-(1_2))
         RavenxdCortiM = Raven * dCortiM,  # Raven*dCortiM
         OSPANxdCortiM = OSPAN * dCortiM,  # OSPAN*dCortiM
         GrpxdCortiM = SampleC * dCortiM,
         GrpxRavenxdCortiM = SampleC*Raven*dCortiM,
         GrpxOSPANxdCortiM = SampleC*OSPAN*dCortiM,

         # With cortisol from baseline measures (like OR 2013)
         RavenxdCrtBM = Raven * dCrtBM,  # Raven*dCortiM
         OSPANxdCrtBM = OSPAN * dCrtBM,  # OSPAN*dCortiM
         GrpxdCrtBM = SampleC * dCrtBM,
         GrpxRavenxdCrtBM = SampleC*Raven*dCrtBM,
         GrpxOSPANxdCrtBM = SampleC*OSPAN*dCrtBM,

         # With cortisol from baseline measures but only considering 3 - 2 (for visualization purpose)
         RavenxdCrtB32 = Raven * dCrtB32,  # Raven*dCortiM
         OSPANxdCrtB32 = OSPAN * dCrtB32,  # OSPAN*dCortiM
         GrpxdCrtB32 = SampleC * dCrtB32,
         GrpxRavenxdCrtB32 = SampleC*Raven*dCrtB32,
         GrpxOSPANxdCrtB32 = SampleC*OSPAN*dCrtB32)

########## Add computations
##### Main
AdditionnalDF <- list(dCompParameter)
ToFillbyDF <- list(dCP = colnames(dCompParameter)[-1])

for (i in 1:length(AdditionnalDF)) {
  dt <- as.data.frame(AdditionnalDF[i])
  ToFill <- ToFillbyDF[[i]]
  d <- FillCol(d, dt, ToFill)
}

##### Halves
dCompParameter1 <- dCompParameter1%>%
  rename(a1_12 = a1, beta1_12 = beta1, a2_12 = a2, beta2_12 = beta2, pi_12 = pi, w_12 = w, lambda_12 = lambda)
dCompParameter2 <- dCompParameter2%>%
  rename(a1_22 = a1, beta1_22 = beta1, a2_22 = a2, beta2_22 = beta2, pi_22 = pi, w_22 = w, lambda_22 = lambda)

AdditionnalDF <- list(dCompParameter1)
ToFillbyDF <- list(dCP = colnames(dCompParameter1)[-1])

for (i in 1:length(AdditionnalDF)) {
  dt <- as.data.frame(AdditionnalDF[i])
  ToFill <- ToFillbyDF[[i]]
  d <- FillCol(d, dt, ToFill)
}

AdditionnalDF <- list(dCompParameter2)
ToFillbyDF <- list(dCP = colnames(dCompParameter2)[-1])

for (i in 1:length(AdditionnalDF)) {
  dt <- as.data.frame(AdditionnalDF[i])
  ToFill <- ToFillbyDF[[i]]
  d <- FillCol(d, dt, ToFill)
}

########## MB/MF scores
d <- d%>%
  mutate(MBv = beta1*w, MFv = beta1*(1-w),
         MBv_12 = beta1_12*w_12, MFv_12 = beta1_12*(1-w_12),
         MBv_22 = beta1_22*w_22, MFv_22 = beta1_22*(1-w_22))

## LogTransform cortisol values to remove skew
# Single Measures
# d$dCorti <- log10(d$dCorti + 1)
# d$dCortiM <- log10(d$dCortiM + 11) # Done later
#
#
# Baselines
# d$CrtB1 <- log10(d$CrtB1 + 1)
# d$CrtB2 <- log10(d$CrtB2 + 1)
# d$CrtB3 <- log10(d$CrtB3 + 1)
# d$CrtB4 <- log10(d$CrtB4 + 1)
# 
# # Composite scores
mean(d$CrtM12[d$Water == 1])
sd(d$CrtM12[d$Water == 1])
mean(d$CrtM34[d$Water == 1])
sd(d$CrtM34[d$Water == 1])

mean(d$CrtM12[d$Water == -1])
sd(d$CrtM12[d$Water == -1])
mean(d$CrtM34[d$Water == -1])
sd(d$CrtM34[d$Water == -1])

d$CrtM12 <- log10(d$CrtM12)
d$CrtM34 <- log10(d$CrtM34)

# # d$dCrtM <- log10(d$dCrtM + 1)
# #
# d$CrtBM12 <- log10(d$CrtBM12 + 1)
# d$CrtBM34 <- log10(d$CrtBM34 + 1)
# d$dCrtBM <- log10(d$dCrtBM + 1)
# d$dCrtB32 <- log10(d$dCrtB32 + 1)

# Smaller df for testing
dMod <- d[c("NS", "subjID", "Sample", "FinalCondition", "SOGS", "DSM", "AUDIT", "DSMal", "w",
                 "dCortiM", "OSPAN", "Raven")]
sum(d$Sample == "Gambler")
sum(d$Sample == "HC")

############################################# Export ##############################################
write.table(d, paste0(Output_path, OutputName), col.names = T, row.names = F, sep = "\t", dec = ".")
write.table(d, paste0(Output_path, "dOKGamFE_Comp7P_OK_HCPG_commaDec.txt"), col.names = T, row.names = F, sep = "\t", dec = ",")