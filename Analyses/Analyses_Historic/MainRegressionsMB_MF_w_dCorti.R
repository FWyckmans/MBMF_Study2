remove(list = ls())

############################################ Parameter ############################################
source("MBMFStudy2_Initialization.R")
Datapath = "Raw_Data/"
Output_path = "Output/"
Graphic_path = "Graphics/Graph/"

############################################# Frame ###############################################
d <- read.delim(paste0(Output_path,"dOKGamFE_Comp7P_OK_HCPG.txt"))
# d$dCorti <- log10(d$dCorti + 15)

d$Sample <- as.factor(d$Sample)
d$dStress <- as.numeric(d$dStress)
d <- AddDummyCol(d, "SRRSGrp", "LCS")
boxplot(SRRS~Sample, data = d)
d$SRRSGrp[d$SRRS > median(d$SRRS, na.rm = T)] <- "HCS"

########################################### Regressions ###########################################
##### w analyses
# Data cleaning
Dw <- OutliersScale(d, c("dCorti", "w"), OutRem = F)

# Regression
mw <- lm(zw ~ zdStress*Sample, data = Dw)
summary(mw)

# Graphic
Inter_w <- interact_plot(mw, pred = zdStress, modx = Sample, plot.points = T,
                         interval = T,
                         x.label = "Cortisol increase (z-score)",
                         y.label = "w (z-score)",
                         legend.main = "DG",
                         colors = "Qual1")

Inter_w
# ggsave(paste0(Graphic_path, "NewAnalyses_w.tiff"), dpi = 300)

dHC <- filter(d, SampleC == 1)
dPG <- filter(d, SampleC == -1)
DwHC <- OutliersScale(dHC, c("w", "dCorti"))
DwPG <- OutliersScale(dPG, c("w", "dCorti"))

Dw <- OutliersScale(d, c("dCorti", "w", "SRRS"))

mw <- lm(zw ~ zdCorti*zSRRS*Sample, data = Dw)
summary(mw)

Inter_w <- interact_plot(mw, pred = zdCorti, modx = zSRRS, mod2 = Sample, plot.points = T,
                         interval = T,
                         x.label = "Cortisol increase (z-score)",
                         modx.values = "plus-minus",
                         y.label = "w (z-score)",
                         legend.main = "SRRS",
                         colors = "Qual1")

Inter_w

# PostHoc
cor.test(Dw$dCorti[Dw$SampleC == 1], Dw$w[Dw$SampleC == 1])
cor.test(Dw$dCorti[Dw$SampleC == -1], Dw$w[Dw$SampleC == -1])

cor.test(d$dCorti[d$SampleC == 1], d$w[d$SampleC == 1])
cor.test(d$dCorti[d$SampleC == -1], d$w[d$SampleC == -1])

# t.test(Dw$w[Dw$SampleC == 1 & Dw$StressGrM == 1], Dw$w[Dw$SampleC == -1 & Dw$StressGrM == 1])
# t.test(Dw$w[Dw$SampleC == 1 & Dw$StressGrM == -1], Dw$w[Dw$SampleC == -1 & Dw$StressGrM == -1])

t.test(d$w[d$SampleC == 1 & d$StressGr == 1], d$w[d$SampleC == -1 & d$StressGr == 1])
t.test(d$w[d$SampleC == 1 & d$StressGr == -1], d$w[d$SampleC == -1 & d$StressGr == -1])

# Bayesian Kendall Correlation
dHC <- filter(Dw, SampleC == 1)
dPG <- filter(Dw, SampleC == -1)

dcorHC <- dHC[c("dCorti", "w")]
dcorPG <- dPG[c("dCorti", "w")]

correlation(dcorHC, bayesian = T)
correlation(dcorPG, bayesian = T)

##### MF analyses
# Data Cleaning
DMF <- OutliersScale(d, c("dCorti", "MFv"))

# Regression
mMF <- lm(zMFv ~ zdCorti*Sample, data = DMF)
summary(mMF)

# Graphic
Inter_MF <- interact_plot(mMF, pred = zdCorti, modx = Sample, plot.points = T,
                          interval = T,
                          x.label = "Cortisol increase (z-score)",
                          y.label = "Model-Free (z-score)",
                          legend.main = "DG",
                          colors = "Qual1")

Inter_MF
# ggsave(paste0(Graphic_path, "NewAnalyses_MF.tiff"), dpi = 300)

# PostHoc
cor.test(DMF$zdCorti[DMF$SampleC == 1], DMF$zMFv[DMF$SampleC == 1])
cor.test(DMF$zdCorti[DMF$SampleC == -1], DMF$zMFv[DMF$SampleC == -1])

t.test(DMF$MFv[DMF$SampleC == 1 & DMF$StressGrM == 1], DMF$MFv[DMF$SampleC == -1 & DMF$StressGrM == 1])
t.test(DMF$MFv[DMF$SampleC == 1 & DMF$StressGrM == -1], DMF$MFv[DMF$SampleC == -1 & DMF$StressGrM == -1])

# Bayesian Kendall Correlation
dHC <- filter(DMF, SampleC == 1)
dPG <- filter(DMF, SampleC == -1)

# dHC <- filter(d, SampleC == 1)
# dPG <- filter(d, SampleC == -1)

dcorHC <- dHC[c("dCorti", "MFv")]
dcorPG <- dPG[c("dCorti", "MFv")]

correlation(dcorHC, method = "kendall", bayesian = T)
correlation(dcorPG, method = "kendall", bayesian = T)

##### MB analyses
# Data Cleaning
DMB <- OutliersScale(d, c("dCorti", "MBv"))

# Regression
mMB <- lm(zMBv ~ zdCorti*Sample, data = DMB)
summary(mMB)

# Graphic
Inter_MB <- interact_plot(mMB, pred = zdCorti, modx = Sample, plot.points = T,
                          interval = T,
                          x.label = "Cortisol increase (z-score)",
                          y.label = "Model-Based (z-score)",
                          legend.main = "DG",
                          colors = "Qual1")

Inter_MB
# ggsave(paste0(Graphic_path, "NewAnalyses_MB.tiff"), dpi = 300)

# PostHoc
cor.test(DMB$dCorti[DMB$SampleC == 1], DMB$MBv[DMB$SampleC == 1])
cor.test(DMB$dCorti[DMB$SampleC == -1], DMB$MBv[DMB$SampleC == -1])

t.test(DMB$MBv[DMB$SampleC == 1 & DMB$StressGrM == 1], DMB$MBv[DMB$SampleC == -1 & DMB$StressGrM == 1])
t.test(DMB$MBv[DMB$SampleC == 1 & DMB$StressGrM == -1], DMB$MBv[DMB$SampleC == -1 & DMB$StressGrM == -1])

cor.test(d$dCorti[d$SampleC == 1], d$MBv[d$SampleC == 1])
cor.test(d$dCorti[d$SampleC == -1], d$MBv[d$SampleC == -1])

t.test(d$MBv[d$SampleC == 1 & d$StressGrM == 1], d$MBv[d$SampleC == -1 & d$StressGrM == 1])
t.test(d$MBv[d$SampleC == 1 & d$StressGrM == -1], d$MBv[d$SampleC == -1 & d$StressGrM == -1])


# Bayesian Kendall Correlation
dHC <- filter(DMB, SampleC == 1)
dPG <- filter(DMB, SampleC == -1)

dcorHC <- dHC[c("dCorti", "MBv")]
dcorPG <- dPG[c("dCorti", "MBv")]

correlation(dcorHC, method = "kendall", bayesian = T)
correlation(dcorPG, method = "kendall", bayesian = T)

##### General correlations
dHC <- filter(d, SampleC == 1)
dPG <- filter(d, SampleC == -1)
dcorHC <- dHC[c("dCorti", "w", "MFv", "MBv")]
dcorPG <- dPG[c("dCorti", "w", "MFv", "MBv")]

correlation(dcorHC, method = "kendall", bayesian = T)
correlation(dcorPG, method = "kendall", bayesian = T)

##### Total graphic
Mg <- ggarrange(Inter_w, Inter_MB, Inter_MF, ncol = 3, nrow = 1,
                labels = c("w", "MB", "MF"),
                common.legend = TRUE, legend="bottom")
Mg
ggsave(paste0(Graphic_path, "NewAnalyses.tiff"), dpi = 300)

##### Beta1 analyses
DBeta1 <- OutliersScale(d, c("dCorti", "beta1"))

mBeta1 <- lm(zbeta1 ~ zdCorti*Sample, data = DBeta1)
summary(mBeta1)

MainInter <- interact_plot(mBeta1, pred = zdCorti, modx = Sample, plot.points = T,
                           interval = T,
                           x.label = "Cortisol increase (z-score)",
                           y.label = "Beta1 (z-score)",
                           legend.main = "DG",
                           colors = "Qual1")

MainInter

##### OSPAN analyses - w
DOSPANw <- OutliersScale(d, c("dCorti", "OSPAN", "w"))

mOSPANw <- lm(zw ~ zdCorti*zOSPAN*Sample, data = DOSPANw)
summary(mOSPANw)

MainInter <- interact_plot(mOSPANw, pred = zdCorti, modx = zOSPAN, mod2 = Sample, plot.points = T,
                           interval = T,
                           x.label = "Cortisol increase (z-score)",
                           y.label = "w (z-score)",
                           legend.main = "OSPAN",
                           colors = "Qual1")

MainInter

##### OSPAN analyses - MB
DOSPAN_MB <- OutliersScale(d, c("dCorti", "OSPAN", "MBv"))
DOSPAN_MB <- OutliersScale(d, c("dCorti", "MBv"))
DOSPAN_MB <- ScaleCol(DOSPAN_MB, ScaleToDo = list(CoI = c("OSPAN"), NewCol = c("zOSPAN")))

mOSPAN_MB <- lm(zMBv ~ zdCorti*zOSPAN*Sample, data = DOSPAN_MB)
summary(mOSPAN_MB)

MainInter <- interact_plot(mOSPAN_MB, pred = zdCorti, modx = zOSPAN, mod2 = Sample, plot.points = T,
                           interval = T,
                           modx.values = "plus-minus",
                           x.label = "Cortisol increase (z-score)",
                           y.label = "w (z-score)",
                           legend.main = "OSPAN",
                           colors = "Qual1")

MainInter

##### OSPAN analyses - MF
DOSPAN_MF <- OutliersScale(d, c("dCorti", "OSPAN", "MFv"))

mOSPAN_MF <- lm(zMFv ~ zdCorti*zOSPAN*Sample, data = DOSPAN_MF)
summary(mOSPAN_MF)

MainInter <- interact_plot(mOSPAN_MF, pred = zdCorti, modx = zOSPAN, mod2 = Sample, plot.points = T,
                           interval = T,
                           x.label = "Cortisol increase (z-score)",
                           y.label = "w (z-score)",
                           legend.main = "OSPAN",
                           colors = "Qual1")

MainInter
