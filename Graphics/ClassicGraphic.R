remove(list = ls())

############################################ Parameter ############################################
source("MBMFStudy2_Initialization.R")
Datapath = "Raw_Data/"
Output_path = "Output/"
Graphic_path = "Graphics/Graph/"
multEB = 2

############################################# Frame ###############################################
d <- read.delim(paste0(Output_path,"dOKGamFE_Comp7P_OK_HCPG.txt"))

dt <- d[c("NS", "Sample", "StressGrM", "OSPANgr", "PRCw", "PRRw", "PUCw", "PURw")]

dLong <- dt%>%
  gather(key = "RewTrans", value = "Prob", PRCw:PURw)%>%
  group_by(Sample, StressGrM, RewTrans)%>%
  summarise(Proba = mean(Prob), ProbaSD = sd(Prob))%>%
  group_by()%>%
  unite("Group", c(Sample:StressGrM), sep = "_")

dLong <- AddDummyCol(dLong, "n")
dLong$n[dLong$Group=="Gambler_NotStressed"] <- length(d$NS[d$SampleC == -1 & d$StressGrM == "NotStressed"])
dLong$n[dLong$Group=="Gambler_Stressed"] <- length(d$NS[d$SampleC == -1 & d$StressGrM == "Stressed"])
dLong$n[dLong$Group=="HC_NotStressed"] <- length(d$NS[d$SampleC == 1 & d$StressGrM == "NotStressed"])
dLong$n[dLong$Group=="HC_Stressed"] <- length(d$NS[d$SampleC == 1 & d$StressGrM == "Stressed"])

dLong <- mutate(dLong, SE = ProbaSD/sqrt(n),
                ymin = Proba - multEB*SE, ymax = Proba + multEB*SE)

dLong$RewTrans[dLong$RewTrans == "PRCw"] <- "PRC"
dLong$RewTrans[dLong$RewTrans == "PRRw"] <- "PRR"
dLong$RewTrans[dLong$RewTrans == "PUCw"] <- "PUC"
dLong$RewTrans[dLong$RewTrans == "PURw"] <- "PUR"
dLong$RewTrans <- factor(dLong$RewTrans, levels = c("PRC", "PRR", "PUC", "PUR"))

##### Graphics
# dF
dPGU <- filter(dLong, Group == "Gambler_NotStressed")
dPGS <- filter(dLong, Group == "Gambler_Stressed")

dHCU <- filter(dLong, Group == "HC_NotStressed")
dHCS <- filter(dLong, Group == "HC_Stressed")

# Plots 
gPGU <- ClassicGraph(dPGU, dPGU$RewTrans, dPGU$Proba, dPGU$ymin, dPGU$ymax, Title = "Unstressed PG")
gPGS <- ClassicGraph(dPGS, dPGS$RewTrans, dPGS$Proba, dPGS$ymin, dPGS$ymax, Title = "Stressed PG")

gHCU <- ClassicGraph(dHCU, dHCU$RewTrans, dHCU$Proba, dHCU$ymin, dHCU$ymax, Title = "Unstressed HC")
gHCS <- ClassicGraph(dHCS, dHCS$RewTrans, dHCS$Proba, dHCS$ymin, dHCS$ymax, Title = "Stressed HC")

# Multiple plots in one
g <- plot_grid(gPGU, gPGS, gHCU, gHCS, ncol = 2, labels = c("A", "B", "C", "D"))
g
ggsave(paste0(Graphic_path, "ClassicGraph.tiff"), dpi = 300)
