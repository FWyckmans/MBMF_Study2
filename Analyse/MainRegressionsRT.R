remove(list = ls())

############################################ Parameter ############################################
source("MBMFStudy2_Initialization.R")
Datapath = "Raw_Data/"
Output_path = "Output/"
Graphic_path = "Graphics/Graph/"

############################################# Frame ###############################################
d <- read.delim(paste0(Output_path,"dOKGamFE_Comp7P_OK_HCPG.txt"))

########################################### Regressions ###########################################
########## dRT1
cor.test(d$dRT1, d$zdCortiM)
cor.test(d$dRT1, d$zOSPAN)
cor.test(d$dRT1, d$StressGrM)

d$SampleC <- factor(d$SampleC, c(-1,1), labels = c("PG", "HC"))

# Main analyses
m1 <- lm(dRT1 ~ zdCortiM*SampleC, data = d)
summary(m1)

# Inclusion of OSPAN score
mO <- lm(dRT1 ~ zdCortiM + SampleC + zOSPAN + zdCortiM:SampleC + zdCortiM:zOSPAN + zOSPAN:SampleC, data = d)
summary(mO)

anova(m1, mO)

interact_plot(mO, pred = zdCortiM, modx = zOSPAN, plot.points = T,
              x.label = "dCort (centered)",
              y.label = "w-parameter (centered)",
              legend.main = "WM score")
ggsave(paste0(Graphic_path, "InteractPlotRT1.tiff"), dpi = 300)

########## dRT2
cor.test(d$dRT2, d$zdCortiM)
cor.test(d$dRT2, d$zOSPAN)
cor.test(d$dRT2, d$StressGrM)

# Main analyses
m1 <- lm(dRT2 ~ zdCortiM*SampleC, data = d)
summary(m1)

interact_plot(m1, pred = zdCortiM, modx = SampleC, plot.points = T,
              x.label = "Cortisol elevation (centered)",
              y.label = "dRT (centered)",
              legend.main = "Group")

# Inclusion of OSPAN score
mO1 <- lm(dRT2 ~ zdCortiM*zOSPAN, data = d)
summary(mO1)

mO2 <- lm(dRT2 ~ zdCortiM + SampleC + zOSPAN + zdCortiM:SampleC + zdCortiM:zOSPAN + zOSPAN:SampleC, data = d)
summary(mO2)

mO3 <- lm(dRT2 ~ zdCortiM*SampleC*zOSPAN, data = d)
summary(mO3)

anova(mO1, mO2)

interact_plot(mO3, pred = zOSPAN, modx = SampleC, plot.points = T,
              x.label = "OSPAN (centered)",
              y.label = "w-parameter (centered)",
              legend.main = "Group")
ggsave(paste0(Graphic_path, "InteractPlotRT2.tiff"), dpi = 300)
