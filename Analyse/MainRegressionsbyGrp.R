remove(list = ls())

############################################ Parameter ############################################
source("MBMFStudy2_Initialization.R")
Datapath = "Raw_Data/"
Output_path = "Output/"
Graphic_path = "Graphics/Graph/"

############################################# Frame ###############################################
dTot <- read.delim(paste0(Output_path,"dOKGamFE_Comp7P_OK_HCPG.txt"))
dTot$StressGrM[dTot$StressGrM == 1] <- "Not-Stressed"
dTot$StressGrM[dTot$StressGrM == -1] <- "Stressed"
# dTot$StressGrM <- as.factor(dTot$StressGrM)

########################################### Regressions ###########################################
########## HC
d <- filter(dTot, SampleC == 1)
PrepForReg()
DF = d

##### OSPAN Effect analyses
# m1 <- lm(zw ~ zdCortiM*zOSPAN, data = DF)
m1 <- lm(zw ~ StressGrM, data = DF)
summary(m1)

# interact_plotHC <- interact_plot(m1, pred = zdCortiM, modx = zOSPAN, plot.points = T,
#                                  interval = T,
#                                  modx.values = "plus-minus",
#                                  x.label = "Cortisol increase (z-score)",
#                                  y.label = "w-parameter (z-score)",
#                                  legend.main = "OSPAN score",
#                                  colors = "Qual1")

interact_plotHC <- interact_plot(m1, pred = zOSPAN, modx = StressGrM, plot.points = T,
                                 interval = T,
                                 x.label = "OSPAN (z-score)",
                                 y.label = "w-parameter (z-score)",
                                 legend.main = "Stress group",
                                 colors = "Qual1")

interact_plotHC

########## PG
d <- dTot%>%
  filter(SampleC == -1)
PrepForReg()

DF = d

##### OSPAN Effect analyses
# m1 <- lm(zw ~ zdCortiM*zOSPAN, data = DF)
m1 <- lm(zw ~ StressGrM*zOSPAN, data = DF)
summary(m1)

# interact_plotPG <- interact_plot(m1, pred = zdCortiM, modx = zOSPAN, plot.points = T,
#                                  interval = T,
#                                  modx.values = "plus-minus",
#                                  x.label = "Cortisol increase (z-score)",
#                                  y.label = "w-parameter (z-score)",
#                                  legend.main = "OSPAN score",
#                                  colors = "Qual1")

interact_plotPG <- interact_plot(m1, pred = zOSPAN, modx = StressGrM, plot.points = T,
                                 interval = T,
                                 x.label = "OSPAN (z-score)",
                                 y.label = "w-parameter (z-score)",
                                 legend.main = "Cortisol Increase",
                                 colors = "Qual1")

interact_plotPG

########################################## Main graphic ###########################################
Mg <- ggarrange(interact_plotHC, interact_plotPG, ncol = 2, nrow = 1,
                labels = c("HC", "PG"),
                common.legend = TRUE, legend="bottom")
Mg
ggsave(paste0(Graphic_path, "InteractPlotByGroup.tiff"), dpi = 300)
