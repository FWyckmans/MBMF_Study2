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

########## Stress Response Baseline
##### Compute baseline
## For single measures
d <- AddDummyCol(d, c("CrtB1", "CrtB2", "CrtB3", "CrtB4"))
d$CrtB1[!is.na(d$Corti1)] <- 0
d$CrtB2 <- d$Corti2 - d$Corti1
d$CrtB3 <- d$Corti3 - d$Corti1
d$CrtB4 <- d$Corti4 - d$Corti1

## Compute composite score
d <- AddDummyCol(d, c("CrtBM12", "CrtBM34", "dCrtBM", "CrtB32_2", "dCrtB32"))

# Like OR (difference between the means)
d$CrtM12 <- (d$Corti1+d$Corti2)/2
d$CrtM34 <- (d$Corti3+d$Corti4)/2
d$dCrtM <- d$CrtM34 - d$CrtM12

d$CrtBM12 <- 0
d$CrtBM34 <- d$CrtM34 - d$CrtM12
d$dCrtBM <- d$CrtBM34 - d$CrtBM12

# Other test (only keep T2 and T3)
d$CrtB32_2 <- 0
d$dCrtB32 <- d$Corti3-d$Corti2

## LogTransform cortisol values to remove skew
# Single Measures
d$dCorti <- log10(d$dCorti + 1)
d$dCortiM <- log10(d$dCortiM + 1)

d$Corti1 <- log10(d$Corti1 + 1)
d$Corti2 <- log10(d$Corti2 + 1)
d$Corti3 <- log10(d$Corti3 + 1)
d$Corti4 <- log10(d$Corti4 + 1)

# Baselines
d$CrtB1 <- log10(d$CrtB1 + 1)
d$CrtB2 <- log10(d$CrtB2 + 1)
d$CrtB3 <- log10(d$CrtB3 + 1)
d$CrtB4 <- log10(d$CrtB4 + 1)

# Composite scores
d$CrtM12 <- log10(d$CrtM12 + 1)
d$CrtM34 <- log10(d$CrtM34 + 1)
d$dCrtM <- log10(d$dCrtM + 1)

d$CrtBM12 <- log10(d$CrtBM12 + 1)
d$CrtBM34 <- log10(d$CrtBM34 + 1)
d$dCrtBM <- log10(d$dCrtBM + 1)
d$dCrtB32 <- log10(d$dCrtB32 + 1)

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

########## Add test computations
AdditionnalDF <- list(dCompParameter)
ToFillbyDF <- list(dCP = colnames(dCompParameter)[-1])

for (i in 1:length(AdditionnalDF)) {
  dt <- as.data.frame(AdditionnalDF[i])
  ToFill <- ToFillbyDF[[i]]
  d <- FillCol(d, dt, ToFill)
}

########## zScores
ScaleToDo <- list(CoI = c("SRRS", "MFsw", "MBsw", "MBURsw", "w",
                          "RewRT1", "UnRewRT1", "CommonRT2", "RareRT2", "dRT1", "dRT2",
                          "OSPAN", "Raven",
                          "GrpxOSPAN", "GrpxRaven",
                          "dCorti", "RavenxdCorti", "OSPANxdCorti", "GrpxdCorti", "GrpxRavenxdCorti", "GrpxOSPANxdCorti",
                          "dCortiM", "RavenxdCortiM", "OSPANxdCortiM", "GrpxdCortiM", "GrpxRavenxdCortiM", "GrpxOSPANxdCortiM",
                          "dCrtBM", "RavenxdCrtBM", "OSPANxdCrtBM", "GrpxdCrtBM", "GrpxRavenxdCrtBM", "GrpxOSPANxdCrtBM",
                          "dCrtB32", "RavenxdCrtB32", "OSPANxdCrtB32", "GrpxdCrtB32", "GrpxRavenxdCrtB32", "GrpxOSPANxdCrtB32"),
                  NewCol = c("zSRRS", "zMF", "zMB", "zMBUR", "zw",
                             "zRewRT1", "zUnRewRT1", "zCommonRT2", "zRareRT2", "zdRT1", "zdRT2",
                             "zOSPAN", "zRaven",
                             "zGrpxOSPAN", "zGrpxRaven",
                             "zdCorti", "zRavenxdCorti", "zOSPANxdCorti", "zGrpxdCorti", "zGrpxRavenxdCorti", "zGrpxOSPANxdCorti",
                             "zdCortiM", "zRavenxdCortiM", "zOSPANxdCortiM", "zGrpxdCortiM", "zGrpxRavenxdCortiM", "zGrpxOSPANxdCortiM",
                             "zdCrtBM", "zRavenxdCrtBM", "zOSPANxdCrtBM", "zGrpxdCrtBM", "zGrpxRavenxdCrtBM", "zGrpxOSPANxdCrtBM",
                             "zdCrtB32", "zRavenxdCrtB32", "zOSPANxdCrtB32", "zGrpxdCrtB32", "zGrpxRavenxdCrtB32", "zGrpxOSPANxdCrtB32"))

d <- ScaleCol(d, ScaleToDo)

# Smaller df for testing
dMod <- d[c("NS", "subjID", "Sample", "FinalCondition", "SOGS", "DSM", "AUDIT", "DSMal", "zw", "w",
                 "dCorti", "OSPAN", "zOSPAN", "Raven")]
sum(d$Sample == "Gambler")
sum(d$Sample == "HC")

############################################# Export ##############################################
write.table(d, paste0(Output_path, OutputName), col.names = T, row.names = F, sep = "\t", dec = ".")

########################################## Manip check ############################################
cor.test(d$zw, d$zOSPAN)
cor.test(d$zw, d$zRaven)

cor.test(d$w[d$dCorti>median(d$dCorti)], d$OSPAN[d$dCorti>median(d$dCorti)])
cor.test(d$w[d$dCorti<median(d$dCorti)], d$OSPAN[d$dCorti<median(d$dCorti)])

cor.test(d$w[d$dCortiM>median(d$dCortiM)], d$OSPAN[d$dCortiM>median(d$dCortiM)])
cor.test(d$w[d$dCortiM<median(d$dCortiM)], d$OSPAN[d$dCortiM<median(d$dCortiM)])

cor.test(d$w[d$OSPAN>median(d$OSPAN)], d$dCorti[d$OSPAN>median(d$OSPAN)])
cor.test(d$w[d$OSPAN<median(d$OSPAN)], d$dCorti[d$OSPAN<median(d$OSPAN)])

cor.test(d$w[d$OSPAN>median(d$OSPAN)], d$dCortiM[d$OSPAN>median(d$OSPAN)])
cor.test(d$w[d$OSPAN<median(d$OSPAN)], d$dCortiM[d$OSPAN<median(d$OSPAN)])

t.test(d$OSPAN[d$Sample == "Gambler"], d$OSPAN[d$Sample != "Gambler"])#, alternative = "less")
wilcox.test(d$OSPAN[d$Sample == "Gambler"], d$OSPAN[d$Sample != "Gambler"])

summary(lm(zw ~ zOSPAN + zdCorti + zOSPANxdCorti, data = d))
summary(lm(zw ~ zOSPAN + zdCortiM + zOSPANxdCortiM, data = d))

cor.test(d$w, d$dCorti)
cor.test(d$w, d$dCortiM)

summary(lm(w ~ OSPAN*dCorti*SampleC, data = d))
summary(lm(zw ~ zOSPAN*zdCortiM*SampleC, data = d))

summary(lm(w ~ Raven*dCortiM, data = d))
summary(lm(w ~ Raven*dCortiM*SampleC, data = d))


# rhat(ModelFit)
t.test(d$w[d$Sample == "Gambler"], d$w[d$Sample != "Gambler"])#, alternative = "less")
wilcox.test(d$w[d$Sample == "Gambler"], d$w[d$Sample != "Gambler"])

t.test(d$OSPAN[d$Sample == "Gambler"], d$OSPAN[d$Sample != "Gambler"])#, alternative = "less")
wilcox.test(d$OSPAN[d$Sample == "Gambler"], d$OSPAN[d$Sample != "Gambler"])

t.test(d$Raven[d$Sample == "Gambler"], d$Raven[d$Sample != "Gambler"])#, alternative = "less")
wilcox.test(d$Raven[d$Sample == "Gambler"], d$Raven[d$Sample != "Gambler"])

t.test(d$w[d$Sample == "Gambler" & d$StressGr == "NotStressed"],
       d$w[d$Sample == "HC" & d$StressGr == "NotStressed"])

wilcox.test(d$w[d$Sample == "Gambler" & d$StressGr == "NotStressed"],
            d$w[d$Sample != "Gambler" & d$StressGr == "NotStressed"])

t.test(d$w[d$Sample == "Gambler" & d$StressGr == "Stressed"],
       d$w[d$Sample == "HC" & d$StressGr == "Stressed"])

wilcox.test(d$w[d$Sample == "Gambler" & d$StressGr == "Stressed"],
            d$w[d$Sample == "HC" & d$StressGr == "Stressed"])

t.test(d$w[d$Sample == "Gambler" & d$Water == 1],
       d$w[d$Sample == "HC" & d$Water == 1])

wilcox.test(d$w[d$Sample == "Gambler" & d$Water == 1],
            d$w[d$Sample != "Gambler" & d$Water == 1])

cor.test(d$w, d$OSPAN)
cor.test(d$w[d$StressGr == "NotStressed"], d$OSPAN[d$StressGr == "NotStressed"])
cor.test(d$w[d$StressGr == "Stressed"], d$OSPAN[d$StressGr == "Stressed"])

cor.test(d$w, d$Raven)
summary(lm(w ~ OSPAN, data = d))

summary(lm(w ~ OSPAN + Raven, data = d))

# summary(lm(OSPAN ~ SampleC + Raven, data = d))
summary(lm(w ~ SampleC + OSPAN + dCortiM, data = d))
t.test(d$OSPAN[d$Sample == "Gambler"], d$OSPAN[d$Sample != "Gambler"])#, alternative = "less")
wilcox.test(d$OSPAN[d$Sample == "Gambler"], d$OSPAN[d$Sample != "Gambler"])

##### LASSO regression
# Fit the LASSO model (Lasso: Alpha = 1)
# set.seed(100)
# x <- as.matrix(d[c("zdCorti", "zOSPAN", "SampleC", "zOSPANxdCortM", "zGrpXdCortM", "zGrpXOSPAN", "zGrpXOSPANXdCortM")])
# y <- as.double(d$zw)
# 
# cv.lasso <- cv.glmnet(x, y, nfolds = 30)
# 
# # Results
# plot(cv.lasso)
# 
# # plot(cv.lasso$glmnet.fit, xvar="lambda", label=TRUE)
# cat('Min Lambda: ', cv.lasso$lambda.min, '\n 1Sd Lambda: ', cv.lasso$lambda.1se)
# df_coef <- round(as.matrix(coef(cv.lasso, s=cv.lasso$lambda.min)), 2)
# 
# # See all contributing variables
# df_coef[df_coef[, 1] != 0, ]
