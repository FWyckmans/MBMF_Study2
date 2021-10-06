remove(list = ls())

############################################ Parameter ############################################
source("MBMFStudy2_Initialization.R")
Datapath = "Output/"
Output_path = "Output/"
OSPANImputer = "RavenSample"

############################################ Frame ################################################
########## Clinical frame
d <- read.delim(paste0(Output_path, "dOKGam.txt"))
dCompParameter <- read.delim(paste0(Output_path, "ComputationParameter7P_OK_HCPGAL.txt"))
# ModelFit <- readRDS(paste0(Output_path, "Models/output7P_OKHCPG.Rdata"))

###################################### Features engineering #######################################
##### Impute OSPAN
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

##### Interaction
d <- d%>%
  mutate(RavenXdCortM = Raven * dCorti,  # Raven*dCorti
         OSPANxdCortM = OSPAN * dCorti,  # OSPAN*dCorti
         GrpXdCortM = SampleC * dCorti,
         GrpXRaven = SampleC*Raven,
         GrpXOSPAN = SampleC*OSPAN,
         GrpXRavenXdCortM = SampleC*Raven*dCorti,
         GrpXOSPANXdCortM = SampleC*OSPAN*dCorti)  

##### Add test computations
AdditionnalDF <- list(dCompParameter)
ToFillbyDF <- list(dCP = colnames(dCompParameter)[-1])

for (i in 1:length(AdditionnalDF)) {
  dt <- as.data.frame(AdditionnalDF[i])
  ToFill <- ToFillbyDF[[i]]
  d <- FillCol(d, dt, ToFill)
}

# zScores
ScaleToDo <- list(CoI = c("MFsw", "MBsw", "MBURsw", "w", "OSPAN", "dCorti",
                          "OSPANxdCortM", "GrpXdCortM", "GrpXOSPAN", "GrpXOSPANXdCortM",
                          "RewRT1", "UnRewRT1", "CommonRT2", "RareRT2", "dRT1", "dRT2"),
                  NewCol = c("zMF", "zMB", "zMBUR", "zw", "zOSPAN", "zdCorti",
                             "zOSPANxdCortM", "zGrpXdCortM", "zGrpXOSPAN", "zGrpXOSPANXdCortM",
                             "zRewRT1", "zUnRewRT1", "zCommonRT2", "zRareRT2", "zdRT1", "zdRT2"))

d <- ScaleCol(d, ScaleToDo)

# Smaller df for testing
dMod <- d[c("NS", "subjID", "Sample", "FinalCondition", "SOGS", "DSM", "AUDIT", "DSMal", "zw", "w",
                 "dCorti", "OSPAN", "zOSPAN", "Raven")]
sum(d$Sample == "Gambler")
sum(d$Sample == "HC")

############################################# Export ##############################################
write.table(d, paste0(Output_path, "dOKGamFE.txt"), col.names = T, row.names = F, sep = "\t", dec = ".")

########################################## Manip check ############################################
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
cor.test(d$w, d$Raven)
summary(lm(w ~ OSPAN, data = d))
summary(lm(w ~ OSPAN*dCorti, data = d))
summary(lm(w ~ OSPAN*dCorti*SampleC, data = d))

summary(lm(w ~ Raven*dCorti, data = d))
summary(lm(w ~ Raven*dCorti*SampleC, data = d))

summary(lm(w ~ OSPAN + Raven, data = d))

# summary(lm(OSPAN ~ SampleC + Raven, data = d))
summary(lm(w ~ SampleC + OSPAN + dCorti, data = d))
t.test(d$OSPAN[d$Sample == "Gambler"], d$OSPAN[d$Sample != "Gambler"])#, alternative = "less")
wilcox.test(d$OSPAN[d$Sample == "Gambler"], d$OSPAN[d$Sample != "Gambler"])

##### LASSO regression
# Fit the LASSO model (Lasso: Alpha = 1)
# set.seed(100)
x <- as.matrix(d[c("zdCorti", "zOSPAN", "SampleC", "zOSPANxdCortM", "zGrpXdCortM", "zGrpXOSPAN", "zGrpXOSPANXdCortM")])
y <- as.double(d$zw)

cv.lasso <- cv.glmnet(x, y, nfolds = 30)

# Results
plot(cv.lasso)

# plot(cv.lasso$glmnet.fit, xvar="lambda", label=TRUE)
cat('Min Lambda: ', cv.lasso$lambda.min, '\n 1Sd Lambda: ', cv.lasso$lambda.1se)
df_coef <- round(as.matrix(coef(cv.lasso, s=cv.lasso$lambda.min)), 2)

# See all contributing variables
df_coef[df_coef[, 1] != 0, ]
