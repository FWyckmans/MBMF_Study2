remove(list = ls())

############################################ Parameter ############################################
source("MBMFStudy2_Initialization.R")
Datapath = "Raw_Data/"
Output_path = "Output/"
Graphic_path = "Graphics/Graph/"

############################################# Frame ###############################################
d <- read.delim(paste0(Output_path,"dOKGamFE_Comp7P_OK_HCPG.txt"))
d$dCortiM <- log10(d$dCortiM + 11)
d$dCorti <- log10(d$dCorti + 11)

d$Sample <- as.factor(d$Sample)

d <- AddDummyCol(d, "SRRSGrp", "LCS")
boxplot(SRRS~Sample, data = d)
d$SRRSGrp[d$SRRS > median(d$SRRS, na.rm = T)] <- "HCS"

########################################### Regressions ###########################################
##### w analyses
# Data cleaning
Dw <- OutliersScale(d, c("dCortiM", "w"), OutRem = T)

# Regression
mw <- lm(zw ~ zdCortiM*Sample, data = Dw)
summary(mw)

# Graphic
Inter_w <- interact_plot(mw, pred = zdCortiM, modx = Sample, plot.points = T,
                           interval = T,
                           x.label = "Cortisol increase (z-score)",
                           y.label = "ω (z-score)",
                           legend.main = "DG",
                           colors = "Qual1")

Inter_w
# ggsave(paste0(Graphic_path, "NewAnalyses_w.tiff"), dpi = 300)

# PostHoc
cor.test(Dw$dCortiM[Dw$SampleC == 1], Dw$w[Dw$SampleC == 1])
cor.test(Dw$dCortiM[Dw$SampleC == -1], Dw$w[Dw$SampleC == -1])

t.test(Dw$w[Dw$SampleC == 1 & Dw$StressGrM == 1], Dw$w[Dw$SampleC == -1 & Dw$StressGrM == 1])
t.test(Dw$w[Dw$SampleC == 1 & Dw$StressGrM == -1], Dw$w[Dw$SampleC == -1 & Dw$StressGrM == -1])

# t.test(d$w[d$SampleC == 1 & d$StressGrM == 1], d$w[d$SampleC == -1 & d$StressGrM == 1])
# t.test(d$w[d$SampleC == 1 & d$StressGrM == -1], d$w[d$SampleC == -1 & d$StressGrM == -1])

# Bayesian Kendall Correlation
# dHC <- filter(Dw, SampleC == 1)
# dPG <- filter(Dw, SampleC == -1)
# 
# dcorHC <- dHC[c("dCortiM", "w")]
# dcorPG <- dPG[c("dCortiM", "w")]
# 
# correlation(dcorHC, bayesian = T)
# correlation(dcorPG, bayesian = T)

##### MF analyses
# Data Cleaning
DMF <- OutliersScale(d, c("dCortiM", "MFv"))

# Regression
mMF <- lm(zMFv ~ zdCortiM*Sample, data = DMF)
summary(mMF)

# Graphic
Inter_MF <- interact_plot(mMF, pred = zdCortiM, modx = Sample, plot.points = T,
                           interval = T,
                           x.label = "Cortisol increase (z-score)",
                           y.label = "Model-Free (z-score)",
                           legend.main = "DG",
                           colors = "Qual1")

Inter_MF
# ggsave(paste0(Graphic_path, "NewAnalyses_MF.tiff"), dpi = 300)

# PostHoc
cor.test(DMF$zdCortiM[DMF$SampleC == 1], DMF$zMFv[DMF$SampleC == 1])
cor.test(DMF$zdCortiM[DMF$SampleC == -1], DMF$zMFv[DMF$SampleC == -1])

t.test(DMF$MFv[DMF$SampleC == 1 & DMF$StressGrM == 1], DMF$MFv[DMF$SampleC == -1 & DMF$StressGrM == 1])
t.test(DMF$MFv[DMF$SampleC == 1 & DMF$StressGrM == -1], DMF$MFv[DMF$SampleC == -1 & DMF$StressGrM == -1])

# Bayesian Kendall Correlation
# dHC <- filter(DMF, SampleC == 1)
# dPG <- filter(DMF, SampleC == -1)
# 
# dHC <- filter(d, SampleC == 1)
# dPG <- filter(d, SampleC == -1)
# 
# dcorHC <- dHC[c("dCortiM", "MFv")]
# dcorPG <- dPG[c("dCortiM", "MFv")]
# 
# correlation(dcorHC, method = "kendall", bayesian = T)
# correlation(dcorPG, method = "kendall", bayesian = T)

##### MB analyses
# Data Cleaning
DMB <- OutliersScale(d, c("dCortiM", "MBv"))

# Regression
mMB <- lm(zMBv ~ zdCortiM*Sample, data = DMB)
summary(mMB)

# Graphic
Inter_MB <- interact_plot(mMB, pred = zdCortiM, modx = Sample, plot.points = T,
                           interval = T,
                           x.label = "Cortisol increase (z-score)",
                           y.label = "Model-Based (z-score)",
                           legend.main = "DG",
                           colors = "Qual1")

Inter_MB
# ggsave(paste0(Graphic_path, "NewAnalyses_MB.tiff"), dpi = 300)

# PostHoc
cor.test(DMB$dCortiM[DMB$SampleC == 1], DMB$MBv[DMB$SampleC == 1])
cor.test(DMB$dCortiM[DMB$SampleC == -1], DMB$MBv[DMB$SampleC == -1])

t.test(DMB$MBv[DMB$SampleC == 1 & DMB$StressGrM == 1], DMB$MBv[DMB$SampleC == -1 & DMB$StressGrM == 1])
t.test(DMB$MBv[DMB$SampleC == 1 & DMB$StressGrM == -1], DMB$MBv[DMB$SampleC == -1 & DMB$StressGrM == -1])

# Bayesian Kendall Correlation
# dHC <- filter(DMB, SampleC == 1)
# dPG <- filter(DMB, SampleC == -1)
# 
# dcorHC <- dHC[c("dCortiM", "MBv")]
# dcorPG <- dPG[c("dCortiM", "MBv")]
# 
# correlation(dcorHC, method = "kendall", bayesian = T)
# correlation(dcorPG, method = "kendall", bayesian = T)