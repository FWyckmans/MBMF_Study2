remove(list = ls())

############################################ Parameter ############################################
source("MBMFStudy2_Initialization.R")
Datapath = "Raw_Data/"
Output_path = "Output/"
Graphic_path = "Graphics/Graph/"

############################################# Frame ###############################################
d <- read.delim(paste0(Output_path,"dOKGamFE_Comp7P_OK_HCPG.txt"))%>%
  filter(SampleC == -1)

d$dCortiM <- log10(d$dCortiM + 11)

d$Sample <- as.factor(d$Sample)

d <- AddDummyCol(d, "SRRSGrp", "LCS")
boxplot(SRRS~Sample, data = d)
d$SRRSGrp[d$SRRS > median(d$SRRS, na.rm = T)] <- "HCS"


########################################### Regressions ###########################################
##### w analyses
# Data cleaning
Dw <- OutliersScale(d, c("OSPAN", "w"))

# Regression
mw <- lm(zw ~ StressGrM*zOSPAN, data = Dw)
summary(mw)

# Graphic
Inter_w <- interact_plot(mw, pred = zOSPAN, modx = StressGrM, plot.points = T,
                         interval = T,
                         x.label = "Cortisol increase (z-score)",
                         y.label = "w (z-score)",
                         legend.main = "DG",
                         colors = "Qual1")

Inter_w

##### MBv analyses
# Data cleaning
DMBv <- OutliersScale(d, c("OSPAN", "MBv"))

# Regression
mMBv <- lm(zMBv ~ StressGrM*zOSPAN, data = DMBv)
summary(mMBv)

# Graphic
Inter_MBv <- interact_plot(mMBv, pred = zOSPAN, modx = StressGrM, plot.points = T,
                           interval = T,
                           x.label = "Cortisol increase (z-score)",
                           y.label = "MBv (z-score)",
                           legend.main = "DG",
                           colors = "Qual1")

Inter_MBv

##### MFv analyses
# Data cleaning
DMFv <- OutliersScale(d, c("OSPAN", "MFv"))

# Regression
mMFv <- lm(zMFv ~ StressGrM*zOSPAN, data = DMFv)
summary(mMFv)

# Graphic
Inter_MFv <- interact_plot(mMFv, pred = zOSPAN, modx = StressGrM, plot.points = T,
                           interval = T,
                           x.label = "Cortisol increase (z-score)",
                           y.label = "MFv (z-score)",
                           legend.main = "DG",
                           colors = "Qual1")

Inter_MFv
