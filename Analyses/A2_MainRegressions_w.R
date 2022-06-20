remove(list = ls())

############################################ Parameter ############################################
source("MBMFStudy2_Initialization.R")
Datapath = "Raw_Data/"
Output_path = "Output/"
Graphic_path = "Graphics/Graph/"

############################################# Frame ###############################################
d <- read.delim(paste0(Output_path,"dOKGamFE_Comp7P_OK_HCPG.txt"))
d$dCortiM <- log10(d$dCortiM + 11)


d$Sample[d$Sample == "Gambler"] <- 'PG'
d$Sample <- as.factor(d$Sample)
d$Sample <- relevel(d$Sample, "PG")
dt <- d[c("NS", "Sample", "w", "dCortiM")]

########################################### Regressions ###########################################
##### w analyses
# Data cleaning
Dw <- OutliersScale(d, c("dCortiM", "w"), OutRem = T)

# Regression
mw <- lm(zw ~ zdCortiM*SampleC, data = Dw)
summary(mw)

# Graphic
col = c("Qual1")
col = c("aquamarine2", "cadetblue2")
col = c("lightgreen", "lightskyblue")


Inter_w <- interact_plot(mw, pred = zdCortiM, modx = SampleC, plot.points = T,
                         interval = T,
                         x.label = "Cortisol increase (z-score)",
                         y.label = "ω (z-score)",
                         modx.labels = c("PG", "HC"),
                         legend.main = "DG",
                         colors = col)

Inter_w
ggsave(paste0(Graphic_path, "Final_w.tiff"), dpi = 300)

# PostHoc
cor.test(Dw$dCortiM[Dw$SampleC == 1], Dw$w[Dw$SampleC == 1])
cor.test(Dw$dCortiM[Dw$SampleC == -1], Dw$w[Dw$SampleC == -1])

t.test(Dw$w[Dw$SampleC == 1 & Dw$StressGrM == 1], Dw$w[Dw$SampleC == -1 & Dw$StressGrM == 1])
t.test(Dw$w[Dw$SampleC == 1 & Dw$StressGrM == -1], Dw$w[Dw$SampleC == -1 & Dw$StressGrM == -1])

t.test(d$w[d$SampleC == 1 & d$StressGrM == 1], d$w[d$SampleC == -1 & d$StressGrM == 1])
t.test(d$w[d$SampleC == 1 & d$StressGrM == -1], d$w[d$SampleC == -1 & d$StressGrM == -1])

# Bayesian Kendall Correlation
dHC <- filter(d, SampleC == 1)
dPG <- filter(d, SampleC == -1)
 
dcorHC <- dHC[c("dCortiM", "w")]
dcorPG <- dPG[c("dCortiM", "w")]

correlation(dcorHC, bayesian = T, method = "kendall")
correlation(dcorPG, bayesian = T, method = "kendall")

correlation(dcorHC, bayesian = T, method = "kendall", bayesian_prior = "ultrawide")
correlation(dcorPG, bayesian = T, method = "kendall", bayesian_prior = "ultrawide")

##### OSPAN mediation
# Data cleaning
Dw <- OutliersScale(d, c("OSPAN", "dCortiM", "w"), OutRem = T)

# Regression
mw <- lm(zw ~ zdCortiM + Sample + zOSPAN +
           zdCortiM*Sample + zdCortiM*zOSPAN + Sample*zOSPAN,
         data = Dw)
mw <- lm(zw ~ zdCortiM*Sample*zOSPAN, data = Dw)

summary(mw)

Inter_w_MedOspan <- interact_plot(mw, pred = zdCortiM, modx = Sample, mod2 = zOSPAN, plot.points = T,
                         interval = T,
                         x.label = "Cortisol increase (z-score)",
                         y.label = "ω (z-score)",
                         mod2.values = 'plus-minus',
                         mod2.labels = c('Low OSPAN score', 'High OSPAN score'),
                         legend.main = "DG",
                         colors = "Qual1")
Inter_w_MedOspan
# ggsave(paste0(Graphic_path, "Final_w_OSPAN.tiff"), dpi = 300)

# Bayesian Kendall Correlation
dStr <- filter(d, StressGrM == -1)
dNStr <- filter(d, StressGrM == 1)

dcorStr <- dStr[c("OSPAN", "w")]
dcorNStr <- dNStr[c("OSPAN", "w")]

correlation(dcorStr, bayesian = T, method = "kendall")
correlation(dcorNStr, bayesian = T, method = "kendall")

correlation(dcorStr, bayesian = T, method = "kendall", bayesian_prior = "ultrawide")
correlation(dcorNStr, bayesian = T, method = "kendall", bayesian_prior = "ultrawide")
            
##### RAVEN mediation
# Data cleaning
Dw <- OutliersScale(d, c("Raven", "dCortiM", "w"), OutRem = T)

# Regression
mw <- lm(zw ~ zdCortiM + Sample + zRaven +
           zdCortiM*Sample + zdCortiM*zRaven + Sample*zRaven,
         data = Dw)
mw <- lm(zw ~ zdCortiM*Sample*zRaven, data = Dw)

summary(mw)

Inter_w_MedRaven <- interact_plot(mw, pred = zdCortiM, modx = Sample, mod2 = zRaven, plot.points = T,
                         interval = T,
                         x.label = "Cortisol increase (z-score)",
                         y.label = "ω (z-score)",
                         mod2.values = 'plus-minus',
                         mod2.labels = c('Low Raven score', 'High Raven score'),
                         legend.main = "DG",
                         colors = "Qual1")
Inter_w_MedRaven
# ggsave(paste0(Graphic_path, "Final_w_Raven.tiff"), dpi = 300)

# Bayesian Kendall Correlation
dStr <- filter(d, StressGrM == -1)
dNStr <- filter(d, StressGrM == 1)

dcorStr <- dStr[c("Raven", "w")]
dcorNStr <- dNStr[c("Raven", "w")]

correlation(dcorStr, bayesian = T, method = "kendall")
correlation(dcorNStr, bayesian = T, method = "kendall")

correlation(dcorStr, bayesian = T, method = "kendall", bayesian_prior = "ultrawide")
correlation(dcorNStr, bayesian = T, method = "kendall", bayesian_prior = "ultrawide")

##### w analyses - SCL 90R
# Data cleaning
Dw <- OutliersScale(d, c("dCortiM", "w", "SCL90R"), OutRem = T)

# Regression
mw <- lm(zw ~ zdCortiM + Sample + zSCL90R +
           zdCortiM*Sample + zdCortiM*zSCL90R + Sample*zSCL90R,
         data = Dw)
mw <- lm(zw ~ zdCortiM*Sample*zSCL90R, data = Dw)

summary(mw)

##### w analyses - BDI
# Data cleaning
Dw <- OutliersScale(d, c("dCortiM", "w", "Beck"), OutRem = T)

# Regression
mw <- lm(zw ~ zdCortiM + Sample + zBeck +
           zdCortiM*Sample + zdCortiM*zBeck + Sample*zBeck,
         data = Dw)
mw <- lm(zw ~ zdCortiM*Sample*zBeck, data = Dw)

summary(mw)

##### w analyses - SPSRQ Reward
# Data cleaning
Dw <- OutliersScale(d, c("dCortiM", "w", "RewardSens"), OutRem = T)

# Regression
mw <- lm(zw ~ zdCortiM + Sample + zRewardSens +
           zdCortiM*Sample + zdCortiM*zRewardSens + Sample*zRewardSens,
         data = Dw)
mw <- lm(zw ~ zdCortiM*Sample*zRewardSens, data = Dw)

summary(mw)


##### w analyses - STAIA
# Data cleaning
Dw <- OutliersScale(d, c("dCortiM", "w", "STAIA"), OutRem = T)

# Regression
mw <- lm(zw ~ zdCortiM + Sample + zSTAIA +
           zdCortiM*Sample + zdCortiM*zSTAIA + Sample*zSTAIA,
         data = Dw)
mw <- lm(zw ~ zdCortiM*Sample*zSTAIA, data = Dw)

summary(mw)


Mg <- ggarrange(Inter_w, Inter_w_MedOspan, Inter_w_MedRaven, ncol = 2, nrow = 2,
                labels = c("A", "B", "C"),
                common.legend = TRUE, legend="bottom")
Mg
MgTop <- ggarrange(Inter_w, Inter_w_MedOspan, ncol = 2, nrow = 1,
                labels = c("A", "B"),
                common.legend = TRUE, legend="none")

MgBot <- ggarrange(NULL, Inter_w_MedRaven, NULL, ncol = 3, nrow = 1,
                   labels = c(NULL, "C", NULL),
                   common.legend = TRUE, legend="none")
Mg <- ggarrange(MgTop, Inter_w_MedRaven, ncol = 1, nrow = 2)
Mg

ggsave(paste0(Graphic_path, "Final_w_inter.tiff"), dpi = 300)


title = cowplot::ggdraw() + cowplot::draw_label("", size = 0)
top_row = cowplot::plot_grid(Inter_w_MedOspan, Inter_w_MedRaven, ncol=2)
bottom_row = cowplot::plot_grid(NULL, Inter_w, NULL, ncol=3, rel_widths=c(0.25,0.5,0.25))

cowplot::plot_grid(title, top_row, bottom_row, ncol=1, rel_heights=c(0.1,1,1))
