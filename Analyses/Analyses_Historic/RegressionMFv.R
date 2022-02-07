remove(list = ls())

############################################ Parameter ############################################
source("MBMFStudy2_Initialization.R")
Datapath = "Raw_Data/"
Output_path = "Output/"
Graphic_path = "Graphics/Graph/"

############################################# Frame ###############################################
d <- read.delim(paste0(Output_path,"dOKGamFE_Comp7P_OK_HCPG.txt"))
boxplot(d$dCortiM)
# d <- filter(d, dCortiM < 1)
# d$dCortiM <- log(d$dCortiM + 1)
boxplot(d$dCortiM)
PrepForReg()

########################################### Regressions ###########################################
######### No Outliers Removal
DF = d
##### Grp Effect analyses
m1 <- lm(zMFv ~ zdCortiM*SampleC, data = DF)
summary(m1)

interact_plot(m1, pred = zdCortiM, modx = SampleC, plot.points = T,
              interval = T,
              x.label = "Cortisol elevetion (z-score)",
              y.label = "MF-parameter (z-score)",
              legend.main = "DG",
              colors = "Qual1")
ggsave(paste0(Graphic_path, "InteractPlot_MFv_Grp.tiff"), dpi = 300)

##### OSPAN effect analyses
mO <- lm(zMFv ~ zdCortiM*zOSPAN, data = DF)
summary(mO)

interact_plot(mO, pred = zdCortiM, modx = zOSPAN, plot.points = T,
              interval = T,
              modx.values = "plus-minus",
              x.label = "dCort (z-score)",
              y.label = "MF-parameter (z-score)",
              legend.main = "WM score",
              colors = "Qual1")
ggsave(paste0(Graphic_path, "InteractPlot_MFv_OSPAN.tiff"), dpi = 300)

##### Moderation-Mediation
mTModOSPAN <- lm(zMFv ~ zdCortiM*SampleC*zOSPAN, data = DF)
mTMedOSPAN <- lm(zMFv ~ zdCortiM + SampleC + zOSPAN + zdCortiM:SampleC + zdCortiM:zOSPAN + zOSPAN:SampleC,
                 data = DF)
summary(mTModOSPAN)
summary(mTMedOSPAN)

interact_plot(mTModOSPAN, pred = zdCortiM, modx = zOSPAN, mod2 = SampleC, plot.points = T,
              interval = T,
              modx.values = "plus-minus",
              x.label = "dCort (z-score)",
              y.label = "MF-parameter (z-score)",
              legend.main = "WM score",
              colors = "Qual1")
ggsave(paste0(Graphic_path, "InteractPlot_MFv_Mod_OSPANxSampleC.tiff"), dpi = 300)

interact_plot(mTMedOSPAN, pred = zdCortiM, modx = SampleC, plot.points = T,
              interval = T,
              # modx.values = "plus-minus",
              x.label = "dCort (z-score)",
              y.label = "MF-parameter (z-score)",
              legend.main = "Grp",
              colors = "Qual1")
ggsave(paste0(Graphic_path, "InteractPlot_MFv_Med_SampleCxOSPAN.tiff"), dpi = 300)

interact_plot(mTMedOSPAN, pred = zdCortiM, modx = zOSPAN, plot.points = T,
              interval = T,
              modx.values = "plus-minus",
              x.label = "dCort (z-score)",
              y.label = "MF-parameter (z-score)",
              legend.main = "WM score",
              colors = "Qual1")
ggsave(paste0(Graphic_path, "InteractPlot_MFv_Med_OSPANxSampleC.tiff"), dpi = 300)

anova(mO, mTMed)

##### Raven effect analyses
mR <- lm(zMFv ~ zdCortiM*zRaven, data = DF)
summary(mR)

interact_plot(mR, pred = zdCortiM, modx = zRaven, plot.points = T,
              interval = T,
              modx.values = "plus-minus",
              x.label = "dCort (z-score)",
              y.label = "MF-parameter (z-score)",
              legend.main = "Raven score",
              colors = "Qual1")
ggsave(paste0(Graphic_path, "InteractPlot_MFv_Raven.tiff"), dpi = 300)

##### Moderation-Mediation
mTModRaven <- lm(zMFv ~ zdCortiM*SampleC*zRaven, data = DF)
mTMedRaven <- lm(zMFv ~ zdCortiM + SampleC + zRaven + zdCortiM:SampleC + zdCortiM:zRaven + zRaven:SampleC,
                 data = DF)
summary(mTModRaven)
summary(mTMedRaven)

interact_plot(mTModRaven, pred = zdCortiM, modx = zRaven, mod2 = SampleC, plot.points = T,
              interval = T,
              modx.values = "plus-minus",
              x.label = "dCort (z-score)",
              y.label = "MF-parameter (z-score)",
              legend.main = "Raven score",
              colors = "Qual1")
ggsave(paste0(Graphic_path, "InteractPlot_MFv_Mod_RavenxSampleC.tiff"), dpi = 300)

interact_plot(mTMedRaven, pred = zdCortiM, modx = SampleC, plot.points = T,
              interval = T,
              # modx.values = "plus-minus",
              x.label = "dCort (z-score)",
              y.label = "MF-parameter (z-score)",
              legend.main = "Grp",
              colors = "Qual1")
ggsave(paste0(Graphic_path, "InteractPlot_MFv_Med_SampleCxRaven.tiff"), dpi = 300)

interact_plot(mTMedRaven, pred = zdCortiM, modx = zRaven, plot.points = T,
              interval = T,
              modx.values = "plus-minus",
              x.label = "dCort (z-score)",
              y.label = "MF-parameter (z-score)",
              legend.main = "Grp",
              colors = "Qual1")
ggsave(paste0(Graphic_path, "InteractPlot_MFv_Med_RavenxSampleC.tiff"), dpi = 300)

##### With Outliers Removal
DF = dNoO
##### Grp Effect analyses
m1 <- lm(zMFv ~ zdCortiM*SampleC, data = DF)
summary(m1)

interact_plot(m1, pred = zdCortiM, modx = SampleC, plot.points = T,
              interval = T,
              x.label = "Cortisol elevetion (z-score)",
              y.label = "MF-parameter (z-score)",
              legend.main = "DG",
              colors = "Qual1")
ggsave(paste0(Graphic_path, "InteractPlot_MFv_Grp_NoO.tiff"), dpi = 300)

##### OSPAN effect analyses
mO <- lm(zMFv ~ zdCortiM*zOSPAN, data = DF)
summary(mO)

interact_plot(mO, pred = zdCortiM, modx = zOSPAN, plot.points = T,
              interval = T,
              modx.values = "plus-minus",
              x.label = "dCort (z-score)",
              y.label = "MF-parameter (z-score)",
              legend.main = "WM score",
              colors = "Qual1")
ggsave(paste0(Graphic_path, "InteractPlot_MFv_OSPAN_NoO.tiff"), dpi = 300)

##### Moderation-Mediation
mTModOSPAN <- lm(zMFv ~ zdCortiM*SampleC*zOSPAN, data = DF)
mTMedOSPAN <- lm(zMFv ~ zdCortiM + SampleC + zOSPAN + zdCortiM:SampleC + zdCortiM:zOSPAN + zOSPAN:SampleC,
                 data = DF)
summary(mTModOSPAN)
summary(mTMedOSPAN)

interact_plot(mTModOSPAN, pred = zdCortiM, modx = zOSPAN, mod2 = SampleC, plot.points = T,
              interval = T,
              modx.values = "plus-minus",
              x.label = "dCort (z-score)",
              y.label = "MF-parameter (z-score)",
              legend.main = "WM score",
              colors = "Qual1")
ggsave(paste0(Graphic_path, "InteractPlot_MFv_Mod_OSPANxSampleC_NoO.tiff"), dpi = 300)

interact_plot(mTMedOSPAN, pred = zdCortiM, modx = SampleC, plot.points = T,
              interval = T,
              # modx.values = "plus-minus",
              x.label = "dCort (z-score)",
              y.label = "MF-parameter (z-score)",
              legend.main = "Grp",
              colors = "Qual1")
ggsave(paste0(Graphic_path, "InteractPlot_MFv_Med_SampleCxOSPAN_NoO.tiff"), dpi = 300)

interact_plot(mTMedOSPAN, pred = zdCortiM, modx = zOSPAN, plot.points = T,
              interval = T,
              modx.values = "plus-minus",
              x.label = "dCort (z-score)",
              y.label = "MF-parameter (z-score)",
              legend.main = "WM score",
              colors = "Qual1")
ggsave(paste0(Graphic_path, "InteractPlot_MFv_Med_OSPANxSampleC_NoO.tiff"), dpi = 300)

anova(mO, mTMed)

##### Raven effect analyses
mR <- lm(zMFv ~ zdCortiM*zRaven, data = DF)
summary(mR)

interact_plot(mR, pred = zdCortiM, modx = zRaven, plot.points = T,
              interval = T,
              modx.values = "plus-minus",
              x.label = "dCort (z-score)",
              y.label = "MF-parameter (z-score)",
              legend.main = "Raven score",
              colors = "Qual1")
ggsave(paste0(Graphic_path, "InteractPlot_MFv_Raven_NoO.tiff"), dpi = 300)

##### Moderation-Mediation
mTModRaven <- lm(zMFv ~ zdCortiM*SampleC*zRaven, data = DF)
mTMedRaven <- lm(zMFv ~ zdCortiM + SampleC + zRaven + zdCortiM:SampleC + zdCortiM:zRaven + zRaven:SampleC,
                 data = DF)
summary(mTModRaven)
summary(mTMedRaven)

interact_plot(mTModRaven, pred = zdCortiM, modx = zRaven, mod2 = SampleC, plot.points = T,
              interval = T,
              modx.values = "plus-minus",
              x.label = "dCort (z-score)",
              y.label = "MF-parameter (z-score)",
              legend.main = "Raven score",
              colors = "Qual1")
ggsave(paste0(Graphic_path, "InteractPlot_MFv_Mod_RavenxSampleC_NoO.tiff"), dpi = 300)

interact_plot(mTMedRaven, pred = zdCortiM, modx = SampleC, plot.points = T,
              interval = T,
              # modx.values = "plus-minus",
              x.label = "dCort (z-score)",
              y.label = "MF-parameter (z-score)",
              legend.main = "Grp",
              colors = "Qual1")
ggsave(paste0(Graphic_path, "InteractPlot_MFv_Med_SampleCxRaven_NoO.tiff"), dpi = 300)

interact_plot(mTMedRaven, pred = zdCortiM, modx = zRaven, plot.points = T,
              interval = T,
              modx.values = "plus-minus",
              x.label = "dCort (z-score)",
              y.label = "MF-parameter (z-score)",
              legend.main = "Raven Score",
              colors = "Qual1")
ggsave(paste0(Graphic_path, "InteractPlot_MFv_Med_RavenxSampleC_NoO.tiff"), dpi = 300)

anova(mR, mTMedRaven)
