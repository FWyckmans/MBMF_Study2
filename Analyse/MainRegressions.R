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

########### Compare each group coefficients
# dPG <- filter(d, SampleC == "PG")
# dHC <- filter(d, SampleC == "HC")
# 
# eq = zw ~ zdCortiM + zOSPAN + zOSPANxdCortiM
# mPG <- lm(eq, data = dPG)
# summary(mPG)
# anova(mPG)
# 
# mHC <- lm(eq, data = dHC)
# summary(mHC)
# anova(mHC)
# 
# model <- lm(zw ~ zdCortiM + zOSPAN + zOSPANxdCortiM, d)
# anova(model) #check the results
# 
# slopes <- emtrends(model, "zOSPAN", var = "zdCortiM")
# slopes
# data <- data.frame(age = factor(c(1,1,1,2,2,2,3,3,3)), 
#                    height = c(56,60,64,56,60,64,74,75,82), 
#                    weight = c(140,155,142,117,125,133,245,241,269))



# slopes <- emtrends(model, 'age', var = 'height') #gets each slope
# slopes
# age height.trend   SE df lower.CL upper.CL
# 1           0.25 1.28  3    -3.84     4.34
# 2           2.00 1.28  3    -2.09     6.09
# 3           3.37 1.18  3    -0.38     7.12
# 
# Confidence level used: 0.95 
# 
# 
# pairs(slopes) #gets their comparisons two by two
# contrast estimate   SE df t.ratio p.value
# 1 - 2       -1.75 1.82  3 -0.964  0.6441 
# 1 - 3       -3.12 1.74  3 -1.790  0.3125 
# 2 - 3       -1.37 1.74  3 -0.785  0.7363 
# 
# P value adjustment: tukey method for comparing a family of 3 estimates 
