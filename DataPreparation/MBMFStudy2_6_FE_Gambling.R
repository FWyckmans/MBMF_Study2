remove(list = ls())

############################################ Parameter ############################################
source("MBMFStudy2_Initialization.R")
Datapath = "Output/"
Output_path = "Output/"
OSPANImputer = "RavenSample"

############################################ Frame ################################################
########## Clinical frame
d <- read.delim(paste0(Output_path, "dOKGam.txt"))

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

# zScores
ScaleToDo <- list(CoI = c("MFsw", "MBsw", "MBURsw", "w", "OSPAN"), NewCol = c("zMF", "zMB", "zMBUR", "zw", "zOSPAN"))

d <- ScaleCol(d, ScaleToDo)

# Smaller df for testing
dMod <- d[c("NS", "subjID", "Sample", "FinalCondition", "SOGS", "DSM", "AUDIT", "DSMal", "zw", "w",
                 "dCorti", "OSPAN", "zOSPAN", "Raven")]
sum(d$Sample == "Gambler")
sum(d$Sample == "HC")

############################################# Export ##############################################
write.table(d, paste0(Output_path, "dOKGamFE.txt"), col.names = T, row.names = F, sep = "\t", dec = ".")

########################################## Manip check ############################################
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
cor.test(d$w, d$Raven)
summary(lm(w ~ OSPAN, data = d))
summary(lm(w ~ OSPAN*dCorti, data = d))
summary(lm(w ~ OSPAN*dCorti*SampleC, data = d))

summary(lm(w ~ Raven*dCorti, data = d))
summary(lm(w ~ Raven*dCorti*SampleC, data = d))

summary(lm(w ~ OSPAN + Raven, data = d))

# summary(lm(OSPAN ~ SampleC + Raven, data = d))
