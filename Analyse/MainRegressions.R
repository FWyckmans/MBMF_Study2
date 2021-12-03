remove(list = ls())

############################################ Parameter ############################################
source("MBMFStudy2_Initialization.R")
Datapath = "Raw_Data/"
Output_path = "Output/"
Graphic_path = "Graphics/Graph/"

############################################# Frame ###############################################
d <- read.delim(paste0(Output_path,"dOKGamFE_Comp7P_OK_HCPG.txt"))
PrepForReg()

########################################### Regressions ###########################################
######### No Outliers Removal
DF = d

##### Grp Effect analyses
m1 <- lm(zw ~ zdCortiM*SampleC, data = DF)
summary(m1)

MainInter <- interact_plot(m1, pred = zdCortiM, modx = SampleC, plot.points = T,
              interval = T,
              x.label = "Cortisol increase (z-score)",
              y.label = "w-parameter (z-score)",
              legend.main = "DG",
              colors = "Qual1")

MainInter
ggsave(paste0(Graphic_path, "InteractPlot_w_Grp.tiff"), dpi = 300)

##### OSPAN effect analyses
mO <- lm(zw ~ zdCortiM*zOSPAN, data = DF)
summary(mO)
 
interact_plot(mO, pred = zdCortiM, modx = zOSPAN, plot.points = T,
              interval = T,
              modx.values = "plus-minus",
              x.label = "Cortisol elevation (z-score)",
              y.label = "w-parameter (z-score)",
              legend.main = "WM score",
              colors = "Qual1")
# ggsave(paste0(Graphic_path, "InteractPlot_w_OSPAN.tiff"), dpi = 300)

##### Moderation-Mediation
# mTModOSPAN <- lm(zw ~ zdCortiM*SampleC*zOSPAN, data = DF)
mTMedOSPAN <- lm(zw ~ zdCortiM + SampleC + zOSPAN + zdCortiM:SampleC + zdCortiM:zOSPAN,
                 data = DF)
# mTMedOSPAN <- lm(zw ~ zdCortiM + SampleC + zOSPAN + zdCortiM:SampleC + zdCortiM:zOSPAN + zOSPAN:SampleC,
#                  data = DF)
# summary(mTModOSPAN)
summary(mTMedOSPAN)

# interact_plot(mTModOSPAN, pred = zdCortiM, modx = zOSPAN, mod2 = SampleC, plot.points = T,
#               interval = T,
#               modx.values = "plus-minus",
#               x.label = "dCort (z-score)",
#               y.label = "w-parameter (z-score)",
#               legend.main = "WM score",
#               colors = "Qual1")
# ggsave(paste0(Graphic_path, "InteractPlot_w_Mod_OSPANxSampleC.tiff"), dpi = 300)

interact_plot(mTMedOSPAN, pred = zdCortiM, modx = SampleC, plot.points = T,
              interval = T,
              # modx.values = "plus-minus",
              x.label = "Cortisol increase (z-score)",
              y.label = "w-parameter (z-score)",
              legend.main = "Grp",
              colors = "Qual1")
ggsave(paste0(Graphic_path, "InteractPlot_w_Med_SampleCxdCortiM.tiff"), dpi = 300)

interact_plot(mTMedOSPAN, pred = zdCortiM, modx = zOSPAN, plot.points = T,
              interval = T,
              modx.values = "plus-minus",
              x.label = "Cortisol increase (z-score)",
              y.label = "w-parameter (z-score)",
              legend.main = "WM score",
              colors = "Qual1")
ggsave(paste0(Graphic_path, "InteractPlot_w_Med_OSPANxdCortiMC.tiff"), dpi = 300)

########## With Outliers Removal
DF = dNoO
##### Grp Effect analyses
m1 <- lm(zw ~ zdCortiM*SampleC, data = DF)
summary(m1)

MainInterNoO <- interact_plot(m1, pred = zdCortiM, modx = SampleC, plot.points = T,
              interval = T,
              x.label = "Cortisol increase (z-score)",
              y.label = "w-parameter (z-score)",
              legend.main = "DG",
              colors = "Qual1")
MainInterNoO
ggsave(paste0(Graphic_path, "InteractPlot_w_Grp_NoO.tiff"), dpi = 300)

##### OSPAN effect analyses
mO <- lm(zw ~ zdCortiM*zOSPAN, data = DF)
summary(mO)

# interact_plot(mO, pred = zdCortiM, modx = zOSPAN, plot.points = T,
#               interval = T,
#               modx.values = "plus-minus",
#               x.label = "dCort (z-score)",
#               y.label = "w-parameter (z-score)",
#               legend.main = "WM score",
#               colors = "Qual1")
# ggsave(paste0(Graphic_path, "InteractPlot_w_OSPAN_NoO.tiff"), dpi = 300)

##### Moderation-Mediation
mTModOSPAN <- lm(zw ~ zdCortiM*SampleC*zOSPAN, data = DF)
mTMedOSPAN <- lm(zw ~ zdCortiM + SampleC + zOSPAN + zOSPAN:SampleC + zdCortiM:zOSPAN + zdCortiM:SampleC,
                 data = DF)
summary(mTModOSPAN)
summary(mTMedOSPAN)

# interact_plot(mTModOSPAN, pred = zdCortiM, modx = zOSPAN, mod2 = SampleC, plot.points = T,
#               interval = T,
#               modx.values = "plus-minus",
#               x.label = "dCort (z-score)",
#               y.label = "w-parameter (z-score)",
#               legend.main = "WM score",
#               colors = "Qual1")
# ggsave(paste0(Graphic_path, "InteractPlot_w_Mod_OSPANxSampleC_NoO.tiff"), dpi = 300)
# 
# interact_plot(mTModOSPAN, pred = zdCortiM, modx = SampleC, plot.points = T,
#               interval = T,
#               # modx.values = "plus-minus",
#               x.label = "dCort (z-score)",
#               y.label = "w-parameter (z-score)",
#               legend.main = "WM score",
#               colors = "Qual1")
# ggsave(paste0(Graphic_path, "InteractPlot_w_Mod_OSPANxSampleC_NoO.tiff"), dpi = 300)

interact_plot(mTMedOSPAN, pred = zdCortiM, modx = SampleC, plot.points = T,
              interval = T,
              # modx.values = "plus-minus",
              x.label = "Cortisol increase (z-score)",
              y.label = "w-parameter (z-score)",
              legend.main = "Grp",
              colors = "Qual1")
ggsave(paste0(Graphic_path, "InteractPlot_w_Med_SampleCxdCortiM_NoO.tiff"), dpi = 300)

# interact_plot(mTMedOSPAN, pred = zdCortiM, modx = zOSPAN, plot.points = T,
#               interval = T,
#               modx.values = "plus-minus",
#               x.label = "dCort (z-score)",
#               y.label = "w-parameter (z-score)",
#               legend.main = "WM score",
#               colors = "Qual1")
# ggsave(paste0(Graphic_path, "InteractPlot_w_Med_OSPANxSampleC_NoO.tiff"), dpi = 300)

########################################## Main graphic ###########################################
Mg <- ggarrange(MainInter, MainInterNoO, ncol = 2, nrow = 1,
                labels = c("A", "B"),
                common.legend = TRUE, legend="bottom")
Mg
ggsave(paste0(Graphic_path, "InteractPlotWithAndWithoutOutliers.tiff"), dpi = 300)
