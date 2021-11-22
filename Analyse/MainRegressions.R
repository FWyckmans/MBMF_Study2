remove(list = ls())

############################################ Parameter ############################################
source("MBMFStudy2_Initialization.R")
Datapath = "Raw_Data/"
Output_path = "Output/"
Graphic_path = "Graphics/Graph/"

############################################# Frame ###############################################
d <- read.delim(paste0(Output_path,"dOKGamFE_Comp7P_OK_HCPG.txt"))

########################################### Regressions ###########################################
cor.test(d$zw, d$SampleC)
cor.test(d$zw, d$zdCortiM)
cor.test(d$zw, d$zOSPAN)
cor.test(d$zw, d$StressGrM)

d$SampleC <- factor(d$SampleC, c(-1,1), labels = c("PG", "HC"))

# m1 <- lm(zw ~ zdCortiM*SampleC, data = d)
# summary(m1)

# Main analyses
m1 <- lm(zw ~ zdCortiM*SampleC, data = d)
summary(m1)

# Inclusion of OSPAN score
mO <- lm(zw ~ zdCortiM + SampleC + zOSPAN + zdCortiM:SampleC + zdCortiM:zOSPAN + zOSPAN:SampleC, data = d)
summary(mO)

anova(m1, mO)

interact_plot(mO, pred = zdCortiM, modx = zOSPAN, plot.points = T,
              x.label = "dCort (centered)",
              y.label = "w-parameter (centered)",
              legend.main = "WM score")
ggsave(paste0(Graphic_path, "InteractPlot.tiff"), dpi = 300)

# Inclusion of Raven score
mR <- lm(zw ~ zdCortiM + SampleC + zRaven + zGrpxdCortiM + zRavenxdCortiM, data = d)
summary(mR)

anova(m1, mR)

# Inclusion of SCL90R
# mSCL90R <- lm(zw ~ zdCortiM + SampleC + zRaven + zGrpxdCortiM + zRavenxdCortiM, data = d)
# summary(mSCL90R)

# Inclusion of Beck
# mSCL90R <- lm(zw ~ zdCortiM + SampleC + zRaven + zGrpxdCortiM + zRavenxdCortiM, data = d)
# summary(mSCL90R)

# Inclusion of STAIYA
# mSCL90R <- lm(zw ~ zdCortiM + SampleC + zRaven + zGrpxdCortiM + zRavenxdCortiM, data = d)
# summary(mSCL90R)

# Inclusion of Reward sensitivity
# mSCL90R <- lm(zw ~ zdCortiM + SampleC + zRaven + zGrpxdCortiM + zRavenxdCortiM, data = d)
# summary(mSCL90R)
