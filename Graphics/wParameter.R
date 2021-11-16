remove(list = ls())

############################################ Parameter ############################################
source("MBMFStudy2_Initialization.R")
Datapath = "Raw_Data/"
Output_path = "Output/"
Graphic_path = "Graphics/Graph/"
multEB = 2

############################################# Frame ###############################################
d <- read.delim(paste0(Output_path,"dOKGamFE_Comp7P_OK_HCPG.txt"))

dt <- d[c("NS", "Sample", "StressGrM", "w")]
dt$StressGrM[dt$StressGrM == -1] <- "Stressed"
dt$StressGrM[dt$StressGrM == 1] <- "Not Stressed"

dg <- dt%>%
  unite("Group", Sample:StressGrM, sep = " ")%>%
  group_by(Group)%>%
  summarise(wM = mean(w), wSD = sd(w))

dg <- AddDummyCol(dg, "n")
dg$n[dg$Group=="Gambler Not Stressed"] <- length(d$NS[d$SampleC == -1 & d$StressGrM == 1])
dg$n[dg$Group=="Gambler Stressed"] <- length(d$NS[d$SampleC == -1 & d$StressGrM == -1])
dg$n[dg$Group=="HC Not Stressed"] <- length(d$NS[d$SampleC == 1 & d$StressGrM == 1])
dg$n[dg$Group=="HC Stressed"] <- length(d$NS[d$SampleC == 1 & d$StressGrM == -1])

dg <- mutate(dg, SE = wSD/sqrt(n), ymin = wM - multEB*SE, ymax = wM + multEB*SE)

dg$Group <- factor(dg$Group,
                   levels = c("Gambler Not Stressed", "Gambler Stressed", "HC Not Stressed", "HC Stressed"))

g <- ggplot(dg, aes(x = Group, y = wM, fill = Group)) +
  geom_bar(stat = "identity", color="black") +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2) +
  
  # Title
  # ggtitle(Title) +
  
  # Axis
  ylab("w-parameter") +
  # ylim(0, 1) +
  scale_y_continuous(limits=c(0.3, 0.6), oob = rescale_none) +
  
  # Theme
  theme_classic() +
  scale_fill_manual(values=c("blue3", "blue3", "gray80", "gray80")) +
  theme(legend.title = element_blank(), legend.position = "none") +
  theme(plot.title = element_text(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12))

ggsave(paste0(Graphic_path, "wParameterByGroup.tiff"), dpi=300)
g
