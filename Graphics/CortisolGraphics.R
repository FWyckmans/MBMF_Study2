remove(list = ls())

############################################ Parameter ############################################
source("MBMFStudy2_Initialization.R")
Datapath = "Raw_Data/"
Output_path = "Output/"
Graphic_path = "Graphics/Graph/"
multEB = 1

############################################# Frame ###############################################
d <- read.delim(paste0(Output_path,"dOKGamFE_Comp7P_OK_HCPG.txt"))

############################################ Graphics #############################################
##### Cortisol like Otto Ross 2013
# Prepare Frame
dGCB <- d%>%
  select(NS, Water, SampleC, CrtB1, CrtB2, CrtB3, CrtB4)%>%
  group_by(Water)%>%
  summarise(CrtB1M = mean(CrtB1), CrtB1sd = sd(CrtB1),
            CrtB2M = mean(CrtB2), CrtB2sd = sd(CrtB2),
            CrtB3M = mean(CrtB3), CrtB3sd = sd(CrtB3),
            CrtB4M = mean(CrtB4), CrtB4sd = sd(CrtB4))%>%
  unite("Baseline", CrtB1M:CrtB1sd, sep = "_")%>%
  unite("Before\nprocedure", CrtB2M:CrtB2sd, sep = "_")%>%
  unite("After\nprocedure", CrtB3M:CrtB3sd, sep = "_")%>%
  unite("After\nRL task", CrtB4M:CrtB4sd, sep = "_")%>%
  gather(key = "Time", value = "Mean_SD", Baseline:"After\nRL task")%>%
  separate("Mean_SD", c("LogCortisol", "sd"), sep = "_")

dGCB$LogCortisol <- as.numeric(dGCB$LogCortisol)
dGCB$sd <- as.numeric(dGCB$sd)

# Compute SE
dGCB <- AddDummyCol(dGCB, "n")

dGCB$n[dGCB$Water == 1] <- length(d$subjID[d$Water == 1])
dGCB$n[dGCB$Water == -1] <- length(d$subjID[d$Water == -1])

dGCB <- mutate(dGCB, SE = (sd/(sqrt(n))), EBmin = LogCortisol - multEB*SE, EBmax = LogCortisol + multEB*SE)
dGCB$SE[dGCB$Time == "Baseline"] <- NA
dGCB$EBmin[dGCB$Time == "Baseline"] <- NA
dGCB$EBmax[dGCB$Time == "Baseline"] <- NA

# Rename for convenience
dGCB$Water[dGCB$Water == 1] <- "WPT"
dGCB$Water[dGCB$Water == -1] <- "CPT"

# Factor time
dGCB$Time <- factor(dGCB$Time, levels = c("Baseline", "Before\nprocedure", "After\nprocedure", "After\nRL task"))

# Graphic
MainCortGraph <- ggplot(dGCB, aes(x = Time, y = LogCortisol, group = Water)) +
  geom_line(aes(colour = Water), size = 1) +
  geom_errorbar(aes(ymin = EBmin, ymax = EBmax, colour = Water), width = 0.2) +
  theme_classic() +
  ylab("Log Cortisol (nmol/l)") +
  theme(axis.title.x=element_blank(),
        axis.text.x = element_text(size = 8),
        legend.position = "none") +
  scale_color_manual(values=c("dodgerblue3","firebrick2"))+
  guides(col=guide_legend("Procedure"))
MainCortGraph
ggsave(paste0(Graphic_path, "CrtB1234.tiff"), dpi = 300)

################### Self-reported
##### Stress
dSSR <- d%>%
  select(NS, Water, SampleC, Stress2, Stress3)%>%
  group_by(Water)%>%
  summarise(BeforeM = mean(Stress2), Beforesd = sd(Stress2),
            AfterM = mean(Stress3), Aftersd = sd(Stress3))%>%
  unite("Before\nprocedure", BeforeM:Beforesd, sep = "_")%>%
  unite("After\nprocedure", AfterM:Aftersd, sep = "_")%>%
  gather(key = "Time", value = "Mean_SD", "Before\nprocedure":"After\nprocedure")%>%
  separate("Mean_SD", c("SRStress", "sd"), sep = "_")

dSSR$SRStress <- as.numeric(dSSR$SRStress)
dSSR$sd <- as.numeric(dSSR$sd)

# Compute SE
dSSR <- AddDummyCol(dSSR, "n")

dSSR$n[dSSR$Water == 1] <- length(d$subjID[d$Water == 1])
dSSR$n[dSSR$Water == -1] <- length(d$subjID[d$Water == -1])

dSSR <- mutate(dSSR, SE = (sd/(sqrt(n))), EBmin = SRStress - multEB*SE, EBmax = SRStress + multEB*SE)

#Re-order Time
dSSR$Time <- factor(dSSR$Time, levels = c("Before\nprocedure", "After\nprocedure"))

# Rename for convenience
dSSR$Water[dSSR$Water == 1] <- "WPT"
dSSR$Water[dSSR$Water == -1] <- "CPT"

# Graphic
SRSGraph <- ggplot(dSSR, aes(x = Time, y = SRStress, group = Water)) +
  geom_line(aes(colour = Water),size = 1) +
  geom_errorbar(aes(ymin = EBmin, ymax = EBmax, colour = Water), width = 0.2) +
  theme_classic() +
  ylab("Self-reported Stress (/10)") +
  theme(axis.title.x=element_blank(),
        axis.text.x = element_text(size = 8),
        legend.position = "none") +
  scale_color_manual(values=c("dodgerblue3","firebrick2"))+
  guides(col=guide_legend("Procedure"))
SRSGraph
ggsave(paste0(Graphic_path, "SRStress.tiff"), dpi = 300)

################### Self-reported
##### Pain
dSSR <- d%>%
  select(NS, Water, SampleC, Pain2, Pain3)%>%
  group_by(Water)%>%
  summarise(BeforeM = mean(Pain2), Beforesd = sd(Pain2),
            AfterM = mean(Pain3), Aftersd = sd(Pain3))%>%
  unite("Before\nprocedure", BeforeM:Beforesd, sep = "_")%>%
  unite("After\nprocedure", AfterM:Aftersd, sep = "_")%>%
  gather(key = "Time", value = "Mean_SD", "Before\nprocedure":"After\nprocedure")%>%
  separate("Mean_SD", c("SRPain", "sd"), sep = "_")

dSSR$SRPain <- as.numeric(dSSR$SRPain)
dSSR$sd <- as.numeric(dSSR$sd)

# Compute SE
dSSR <- AddDummyCol(dSSR, "n")

dSSR$n[dSSR$Water == 1] <- length(d$subjID[d$Water == 1])
dSSR$n[dSSR$Water == -1] <- length(d$subjID[d$Water == -1])

dSSR <- mutate(dSSR, SE = (sd/(sqrt(n))), EBmin = SRPain - multEB*SE, EBmax = SRPain + multEB*SE)

#Re-order Time
dSSR$Time <- factor(dSSR$Time, levels = c("Before\nprocedure", "After\nprocedure"))

# Rename for convenience
dSSR$Water[dSSR$Water == 1] <- "WPT"
dSSR$Water[dSSR$Water == -1] <- "CPT"

# Graphic
SRPGraph <- ggplot(dSSR, aes(x = Time, y = SRPain, group = Water)) +
  geom_line(aes(colour = Water),size = 1) +
  geom_errorbar(aes(ymin = EBmin, ymax = EBmax, colour = Water), width = 0.2) +
  theme_classic() +
  ylab("Self-reported Pain (/10)") +
  theme(axis.title.x=element_blank(),
        axis.text.x = element_text(size = 8),
        legend.position = "none") +
  scale_color_manual(values=c("dodgerblue3","firebrick2"))+
  guides(col=guide_legend("Procedure"))
SRPGraph
ggsave(paste0(Graphic_path, "SRPain.tiff"), dpi = 300)

##### Craving
dSSR <- d%>%
  filter(SampleC == -1)%>%
  select(NS, Water, SampleC, Craving2, Craving3)%>%
  group_by(Water)%>%
  summarise(BeforeM = mean(Craving2), Beforesd = sd(Craving2),
            AfterM = mean(Craving3), Aftersd = sd(Craving3))%>%
  unite("Before\nprocedure", BeforeM:Beforesd, sep = "_")%>%
  unite("After\nprocedure", AfterM:Aftersd, sep = "_")%>%
  gather(key = "Time", value = "Mean_SD", "Before\nprocedure":"After\nprocedure")%>%
  separate("Mean_SD", c("SRCraving", "sd"), sep = "_")

dSSR$SRCraving <- as.numeric(dSSR$SRCraving)
dSSR$sd <- as.numeric(dSSR$sd)

# Compute SE
dSSR <- AddDummyCol(dSSR, "n")

dSSR$n[dSSR$Water == 1] <- length(d$subjID[d$Water == 1])
dSSR$n[dSSR$Water == -1] <- length(d$subjID[d$Water == -1])

dSSR <- mutate(dSSR, SE = (sd/(sqrt(n))), EBmin = SRCraving - multEB*SE, EBmax = SRCraving + multEB*SE)

#Re-order Time
dSSR$Time <- factor(dSSR$Time, levels = c("Before\nprocedure", "After\nprocedure"))

# Rename for convenience
dSSR$Water[dSSR$Water == 1] <- "WPT"
dSSR$Water[dSSR$Water == -1] <- "CPT"

# Graphic
SRCGraph <- ggplot(dSSR, aes(x = Time, y = SRCraving, group = Water)) +
  geom_line(aes(colour = Water),size = 1) +
  geom_errorbar(aes(ymin = EBmin, ymax = EBmax, colour = Water), width = 0.2) +
  theme_classic() +
  ylab("Self-reported Craving (/10)") +
  theme(axis.title.x=element_blank(),
        axis.text.x = element_text(size = 8),
        legend.position = "none") +
  scale_color_manual(values=c("dodgerblue3","firebrick2"))+
  guides(col=guide_legend("Procedure"))
SRCGraph
ggsave(paste0(Graphic_path, "SRCraving.tiff"), dpi = 300)

###### MultipleGrid
# Mg <- plot_grid(MainCortGraph, SRSGraph, SRPGraph, SRCGraph, ncol = 2,
#                 labels = c("A", "B", "C", "D"))
# Mg
Mg <- ggarrange(MainCortGraph, SRSGraph, SRPGraph, SRCGraph, ncol = 2, nrow = 2,
                labels = c("A", "B", "C", "D"),
                common.legend = TRUE, legend="bottom")
Mg
ggsave(paste0(Graphic_path, "StressResponse.tiff"), dpi = 300)
