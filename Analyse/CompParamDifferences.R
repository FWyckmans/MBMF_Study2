remove(list = ls())

############################################ Parameter ############################################
source("MBMFStudy2_Initialization.R")
Datapath = "Raw_Data/"
Output_path = "Output/"

############################################# Frame ###############################################
d <- read.delim(paste0(Output_path,"dTot.txt"))%>%
  filter(OKd == 1)%>%
  select(subjID, Condition, Sample, StressGr, StressGrM, StressGrSR, StressGrSRM,
         a1, beta1, a2, beta2, pi, w, lambda)

############################################ Graphics ##############################################
##### Parameter = w
bp(d, "w", "Condition", F)
bp(d, "w", "Sample")
bp(d, "w", c("Sample", "StressGr"))
bp(d, "w", c("Sample", "StressGrM"))
bp(d, "w", c("Sample", "StressGrSR"))
# bp(d, "w", c("Sample", "StressGrSRM"))

##### Parameter = lambda
bp(d, "lambda", "Condition")
bp(d, "lambda", "Sample")
bp(d, "lambda", c("Sample", "StressGr"))
bp(d, "lambda", c("Sample", "StressGrM"))
bp(d, "lambda", c("Sample", "StressGrSR"))
# bp(d, "lambda", c("Sample", "StressGrSRM"))

d <- OutliersModif(d, "w", Groups = "Sample")

dDescr <- DescrFrame(d, Btwn = "Sample")
dDescr <- DescrFrame(d, Btwn = "Condition")
anova <- aov(w ~ Sample, data = d)
summary(anova)
t.test(d$w[d$Sample=="Alc"], d$w[d$Sample=="HC"])
