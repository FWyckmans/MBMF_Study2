remove(list = ls())

############################################ Parameter ############################################
source("MBMFStudy2_Initialization.R")
Datapath = "Raw_Data/"
Output_path = "Output/"
Graphic_path = "Graphics/Graph/"
multEB = 1

############################################# Frame ###############################################
d <- read.delim(paste0(Output_path,"dOKGamFE_Comp7P_OK_HCPG.txt"))

dBP <- d%>%
  select(NS, Water, Sample, StressGrM, dCortiM, OSPAN, w)

dBP <- AddDummyCol(dBP, "OSPANGR", "LowOSPAN")
dBP$OSPANGR[dBP$OSPAN >= median(dBP$OSPAN)] <- "HighOSPAN"

# COmpute mean/SD
dBPg <- dBP%>%
  unite("Group", c(Sample, StressGrM, OSPANGR), sep = " ")%>%
  group_by(Group)%>%
  summarise(wM = mean(w), wSD = sd(w))%>%
  rename(w = wM)

# Compute SE
dBPg <- AddDummyCol(dBPg, "n")

dBPg$n[dBPg$Group == "Gambler NotStressed HighOSPAN"] <- length(dBP$NS[(dBP$Sample == "Gambler") &
                                                                         (dBP$StressGrM == "NotStressed") &
                                                                         (dBP$OSPANGR == "HighOSPAN")])

dBPg$n[dBPg$Group == "Gambler NotStressed LowOSPAN"] <- length(dBP$NS[(dBP$Sample == "Gambler") &
                                                                         (dBP$StressGrM == "NotStressed") &
                                                                         (dBP$OSPANGR == "LowOSPAN")])

dBPg$n[dBPg$Group == "Gambler Stressed HighOSPAN"] <- length(dBP$NS[(dBP$Sample == "Gambler") &
                                                                         (dBP$StressGrM == "Stressed") &
                                                                         (dBP$OSPANGR == "HighOSPAN")])

dBPg$n[dBPg$Group == "Gambler Stressed LowOSPAN"] <- length(dBP$NS[(dBP$Sample == "Gambler") &
                                                                        (dBP$StressGrM == "Stressed") &
                                                                        (dBP$OSPANGR == "LowOSPAN")])


dBPg$n[dBPg$Group == "HC NotStressed HighOSPAN"] <- length(dBP$NS[(dBP$Sample == "HC") &
                                                                         (dBP$StressGrM == "NotStressed") &
                                                                         (dBP$OSPANGR == "HighOSPAN")])

dBPg$n[dBPg$Group == "HC NotStressed LowOSPAN"] <- length(dBP$NS[(dBP$Sample == "HC") &
                                                                        (dBP$StressGrM == "NotStressed") &
                                                                        (dBP$OSPANGR == "LowOSPAN")])

dBPg$n[dBPg$Group == "HC Stressed HighOSPAN"] <- length(dBP$NS[(dBP$Sample == "HC") &
                                                                      (dBP$StressGrM == "Stressed") &
                                                                      (dBP$OSPANGR == "HighOSPAN")])

dBPg$n[dBPg$Group == "HC Stressed LowOSPAN"] <- length(dBP$NS[(dBP$Sample == "HC") &
                                                                     (dBP$StressGrM == "Stressed") &
                                                                     (dBP$OSPANGR == "LowOSPAN")])


dBPg <- mutate(dBPg, SE = (wSD/(sqrt(n))), EBmin = w - multEB*SE, EBmax = w + multEB*SE)

# Order groups
dBPg$Group <- factor(dBPg$Group, levels = c("Gambler NotStressed HighOSPAN", "Gambler Stressed HighOSPAN",
                                            "Gambler NotStressed LowOSPAN", "Gambler Stressed LowOSPAN",
                                            "HC NotStressed HighOSPAN", "HC Stressed HighOSPAN",
                                            "HC NotStressed LowOSPAN", "HC Stressed LowOSPAN"))

# Graphic
Graphic <- ggplot(dBPg, aes(x = Group, y = w)) +
  geom_col() +
  geom_errorbar(aes(ymin = EBmin, ymax = EBmax), width = 0.2) +
  theme_classic()

Graphic




dNS <- filter(d, StressGr == "NotStressed")
dNSM <- filter(d, StressGrM == "NotStressed")

boxplot(w~ Sample, data = dNS)
boxplot(w~ Sample, data = dNSM)
