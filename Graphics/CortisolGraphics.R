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
  unite("T1", CrtB1M:CrtB1sd, sep = "_")%>%
  unite("T2", CrtB2M:CrtB2sd, sep = "_")%>%
  unite("T3", CrtB3M:CrtB3sd, sep = "_")%>%
  unite("T4", CrtB4M:CrtB4sd, sep = "_")%>%
  gather(key = "Time", value = "Mean_SD", T1:T4)%>%
  separate("Mean_SD", c("LogCortisol", "sd"), sep = "_")

dGCB$LogCortisol <- as.numeric(dGCB$LogCortisol)
dGCB$sd <- as.numeric(dGCB$sd)

# Compute SE
dGCB <- AddDummyCol(dGCB, "n")

dGCB$n[dGCB$Water == 1] <- length(d$subjID[d$Water == 1])
dGCB$n[dGCB$Water == -1] <- length(d$subjID[d$Water == -1])

dGCB <- mutate(dGCB, SE = (sd/(sqrt(n))), EBmin = LogCortisol - multEB*SE, EBmax = LogCortisol + multEB*SE)
dGCB$SE[dGCB$Time == "T1"] <- NA
dGCB$EBmin[dGCB$Time == "T1"] <- NA
dGCB$EBmax[dGCB$Time == "T1"] <- NA

# Rename for convenience
dGCB$Water[dGCB$Water == 1] <- "WPT"
dGCB$Water[dGCB$Water == -1] <- "CPT"

# Graphic
CortGraph <- ggplot(dGCB, aes(x = Time, y = LogCortisol, group = Water)) +
  geom_line(aes(colour = Water),size = 1) +
  geom_errorbar(aes(ymin = EBmin, ymax = EBmax, colour = Water), width = 0.2) +
  theme_classic() +
  ylab("Log Cortisol (nmol/ml)") +
  theme(axis.title.x=element_blank()) +
  guides(col=guide_legend("Procedure"))
CortGraph
ggsave(paste0(Graphic_path, "CrtB1234.tiff"), dpi = 300)

##### Cortisol mean(T1B_T2B) and mean(T3B_T4B)
# Prepare Frame
dGCBMd <- d%>%
  select(NS, Water, SampleC, CrtBM12, CrtBM34)%>%
  group_by(Water)%>%
  summarise(CrtBBeforePTM = mean(CrtBM12), CrtBBeforePTsd = sd(CrtBM12),
            CrtBAfterPTM = mean(CrtBM34), CrtBAfterPTsd = sd(CrtBM34))%>%
  unite("Before", CrtBBeforePTM:CrtBBeforePTsd, sep = "_")%>%
  unite("After", CrtBAfterPTM:CrtBAfterPTsd, sep = "_")%>%
  gather(key = "Time", value = "Mean_SD", Before:After)%>%
  separate("Mean_SD", c("LogCortisol", "sd"), sep = "_")

dGCBMd$LogCortisol <- as.numeric(dGCBMd$LogCortisol)
dGCBMd$sd <- as.numeric(dGCBMd$sd)

# Compute SE
dGCBMd <- AddDummyCol(dGCBMd, "n")

dGCBMd$n[dGCBMd$Water == 1] <- length(d$subjID[d$Water == 1])
dGCBMd$n[dGCBMd$Water == -1] <- length(d$subjID[d$Water == -1])

dGCBMd <- mutate(dGCBMd, SE = (sd/(sqrt(n))), EBmin = LogCortisol - multEB*SE, EBmax = LogCortisol + multEB*SE)
dGCBMd$SE[dGCBMd$Time == "Before"] <- NA
dGCBMd$EBmin[dGCBMd$Time == "Before"] <- NA
dGCBMd$EBmax[dGCBMd$Time == "Before"] <- NA

# Rename for convenience
dGCBMd$Water[dGCBMd$Water == 1] <- "WPT"
dGCBMd$Water[dGCBMd$Water == -1] <- "CPT"

#Re-order Time
dGCBMd$Time <- factor(dGCBMd$Time, levels = c("Before", "After"))

# Graphic
CortGraph <- ggplot(dGCBMd, aes(x = Time, y = LogCortisol, group = Water)) +
  geom_line(aes(colour = Water),size = 1) +
  geom_errorbar(aes(ymin = EBmin, ymax = EBmax, colour = Water), width = 0.2) +
  theme_classic() +
  ylab("Log Cortisol (nmol/ml)") +
  theme(axis.title.x=element_blank()) +
  guides(col=guide_legend("Procedure"))
CortGraph
ggsave(paste0(Graphic_path, "CrtBM1234.tiff"), dpi = 300)

##### Cortisol only T2 and T3
# Prepare Frame
dGCBM <- d%>%
  select(NS, Water, SampleC, CrtB32_2, dCrtB32)%>%
  group_by(Water)%>%
  summarise(CrtBBeforePTM = mean(CrtB32_2), CrtBBeforePTsd = sd(CrtB32_2),
            CrtBAfterPTM = mean(dCrtB32), CrtBAfterPTsd = sd(dCrtB32))%>%
  unite("Before", CrtBBeforePTM:CrtBBeforePTsd, sep = "_")%>%
  unite("After", CrtBAfterPTM:CrtBAfterPTsd, sep = "_")%>%
  gather(key = "Time", value = "Mean_SD", Before:After)%>%
  separate("Mean_SD", c("LogCortisol", "sd"), sep = "_")

dGCBM$LogCortisol <- as.numeric(dGCBM$LogCortisol)
dGCBM$sd <- as.numeric(dGCBM$sd)

# Compute SE
dGCBM <- AddDummyCol(dGCBM, "n")

dGCBM$n[dGCBM$Water == 1] <- length(d$subjID[d$Water == 1])
dGCBM$n[dGCBM$Water == -1] <- length(d$subjID[d$Water == -1])

dGCBM <- mutate(dGCBM, SE = (sd/(sqrt(n))), EBmin = LogCortisol - multEB*SE, EBmax = LogCortisol + multEB*SE)
dGCBM$SE[dGCBM$Time == "Before"] <- NA
dGCBM$EBmin[dGCBM$Time == "Before"] <- NA
dGCBM$EBmax[dGCBM$Time == "Before"] <- NA

# Rename for convenience
dGCBM$Water[dGCBM$Water == 1] <- "WPT"
dGCBM$Water[dGCBM$Water == -1] <- "CPT"

#Re-order Time
dGCBM$Time <- factor(dGCBM$Time, levels = c("Before", "After"))

# Graphic
CortGraph <- ggplot(dGCBM, aes(x = Time, y = LogCortisol, group = Water)) +
  geom_line(aes(colour = Water),size = 1) +
  geom_errorbar(aes(ymin = EBmin, ymax = EBmax, colour = Water), width = 0.2) +
  theme_classic() +
  ylab("Log Cortisol (nmol/ml)") +
  theme(axis.title.x=element_blank()) +
  guides(col=guide_legend("Procedure"))
CortGraph
ggsave(paste0(Graphic_path, "CrtB23.tiff"), dpi = 300)

##### Cortisol only T2, T3 and T4
# Prepare Frame
dGCBM234 <- d%>%
  mutate(dCrtB42 = Corti3-Corti2)%>%
  select(NS, Water, SampleC, CrtB32_2, dCrtB32, dCrtB42)%>%
  group_by(Water)%>%
  summarise(CrtBBeforePTM = mean(CrtB32_2), CrtBBeforePTsd = sd(CrtB32_2),
            CrtBAfterPTM = mean(dCrtB32), CrtBAfterPTsd = sd(dCrtB32),
            CrtBAfterDTM = mean(dCrtB42), CrtBAfterDTsd = sd(dCrtB42))%>%
  unite("BeforePT", CrtBBeforePTM:CrtBBeforePTsd, sep = "_")%>%
  unite("AfterPT", CrtBAfterPTM:CrtBAfterPTsd, sep = "_")%>%
  unite("AfterRL", CrtBAfterDTM:CrtBAfterDTsd, sep = "_")%>%
  gather(key = "Time", value = "Mean_SD", BeforePT:AfterRL)%>%
  separate("Mean_SD", c("LogCortisol", "sd"), sep = "_")

dGCBM234$LogCortisol <- as.numeric(dGCBM234$LogCortisol)
dGCBM234$sd <- as.numeric(dGCBM234$sd)

# Compute SE
dGCBM234 <- AddDummyCol(dGCBM234, "n")

dGCBM234$n[dGCBM234$Water == 1] <- length(d$subjID[d$Water == 1])
dGCBM234$n[dGCBM234$Water == -1] <- length(d$subjID[d$Water == -1])

dGCBM234 <- mutate(dGCBM234, SE = (sd/(sqrt(n))), EBmin = LogCortisol - multEB*SE, EBmax = LogCortisol + multEB*SE)
dGCBM234$SE[dGCBM234$Time == "BeforePT"] <- NA
dGCBM234$EBmin[dGCBM234$Time == "BeforePT"] <- NA
dGCBM234$EBmax[dGCBM234$Time == "BeforePT"] <- NA

# Rename for convenience
dGCBM234$Water[dGCBM234$Water == 1] <- "WPT"
dGCBM234$Water[dGCBM234$Water == -1] <- "CPT"

#Re-order Time
dGCBM234$Time <- factor(dGCBM234$Time, levels = c("BeforePT", "AfterPT", "AfterRL"))

# Graphic
CortGraph <- ggplot(dGCBM234, aes(x = Time, y = LogCortisol, group = Water)) +
  geom_line(aes(colour = Water),size = 1) +
  geom_errorbar(aes(ymin = EBmin, ymax = EBmax, colour = Water), width = 0.2) +
  theme_classic() +
  ylab("Log Cortisol (nmol/ml)") +
  theme(axis.title.x=element_blank()) +
  guides(col=guide_legend("Procedure"))
CortGraph
ggsave(paste0(Graphic_path, "CrtB234.tiff"), dpi = 300)

# Plot <- ggplot(d, aes(x = VI, y = VD)) +
#   geom_point() +
#   geom_smooth(method=lm, se=F) +
#   theme_classic() +
#   xlab(VIname) +
#   ylab(VDname) +
#   xlim(VImin, VImax) +
#   ylim(VDmin, VDmax) +
#   
#   geom_text(x = EqX, y = EqY, label = Eq)
# ggsave(NameSave, dpi=300)
# Plot

##### Cortisol mean(T1B_T2B) and mean(T3B_T4B) only HC
dHC <- filter(d, SampleC == 1)

# Prepare Frame
dGCBMd <- dHC%>%
  select(NS, Water, SampleC, CrtBM12, CrtBM34)%>%
  group_by(Water)%>%
  summarise(CrtBBeforePTM = mean(CrtBM12), CrtBBeforePTsd = sd(CrtBM12),
            CrtBAfterPTM = mean(CrtBM34), CrtBAfterPTsd = sd(CrtBM34))%>%
  unite("Before", CrtBBeforePTM:CrtBBeforePTsd, sep = "_")%>%
  unite("After", CrtBAfterPTM:CrtBAfterPTsd, sep = "_")%>%
  gather(key = "Time", value = "Mean_SD", Before:After)%>%
  separate("Mean_SD", c("LogCortisol", "sd"), sep = "_")

dGCBMd$LogCortisol <- as.numeric(dGCBMd$LogCortisol)
dGCBMd$sd <- as.numeric(dGCBMd$sd)

# Compute SE
dGCBMd <- AddDummyCol(dGCBMd, "n")

dGCBMd$n[dGCBMd$Water == 1] <- length(d$subjID[d$Water == 1])
dGCBMd$n[dGCBMd$Water == -1] <- length(d$subjID[d$Water == -1])

dGCBMd <- mutate(dGCBMd, SE = (sd/(sqrt(n))), EBmin = LogCortisol - multEB*SE, EBmax = LogCortisol + multEB*SE)
dGCBMd$SE[dGCBMd$Time == "Before"] <- NA
dGCBMd$EBmin[dGCBMd$Time == "Before"] <- NA
dGCBMd$EBmax[dGCBMd$Time == "Before"] <- NA

# Rename for convenience
dGCBMd$Water[dGCBMd$Water == 1] <- "WPT"
dGCBMd$Water[dGCBMd$Water == -1] <- "CPT"

#Re-order Time
dGCBMd$Time <- factor(dGCBMd$Time, levels = c("Before", "After"))

# Graphic
CortGraph <- ggplot(dGCBMd, aes(x = Time, y = LogCortisol, group = Water)) +
  geom_line(aes(colour = Water),size = 1) +
  geom_errorbar(aes(ymin = EBmin, ymax = EBmax, colour = Water), width = 0.2) +
  theme_classic() +
  ylab("Log Cortisol (nmol/ml)") +
  theme(axis.title.x=element_blank()) +
  guides(col=guide_legend("Procedure")) +
  labs(title = "HC")
CortGraph
ggsave(paste0(Graphic_path, "CrtBM1234HC.tiff"), dpi = 300)

##### Cortisol mean(T1B_T2B) and mean(T3B_T4B) only HC
dPG <- filter(d, SampleC == -1)

# Prepare Frame
dGCBMd <- dPG%>%
  select(NS, Water, SampleC, CrtBM12, CrtBM34)%>%
  group_by(Water)%>%
  summarise(CrtBBeforePTM = mean(CrtBM12), CrtBBeforePTsd = sd(CrtBM12),
            CrtBAfterPTM = mean(CrtBM34), CrtBAfterPTsd = sd(CrtBM34))%>%
  unite("Before", CrtBBeforePTM:CrtBBeforePTsd, sep = "_")%>%
  unite("After", CrtBAfterPTM:CrtBAfterPTsd, sep = "_")%>%
  gather(key = "Time", value = "Mean_SD", Before:After)%>%
  separate("Mean_SD", c("LogCortisol", "sd"), sep = "_")

dGCBMd$LogCortisol <- as.numeric(dGCBMd$LogCortisol)
dGCBMd$sd <- as.numeric(dGCBMd$sd)

# Compute SE
dGCBMd <- AddDummyCol(dGCBMd, "n")

dGCBMd$n[dGCBMd$Water == 1] <- length(d$subjID[d$Water == 1])
dGCBMd$n[dGCBMd$Water == -1] <- length(d$subjID[d$Water == -1])

dGCBMd <- mutate(dGCBMd, SE = (sd/(sqrt(n))), EBmin = LogCortisol - multEB*SE, EBmax = LogCortisol + multEB*SE)
dGCBMd$SE[dGCBMd$Time == "Before"] <- NA
dGCBMd$EBmin[dGCBMd$Time == "Before"] <- NA
dGCBMd$EBmax[dGCBMd$Time == "Before"] <- NA

# Rename for convenience
dGCBMd$Water[dGCBMd$Water == 1] <- "WPT"
dGCBMd$Water[dGCBMd$Water == -1] <- "CPT"

#Re-order Time
dGCBMd$Time <- factor(dGCBMd$Time, levels = c("Before", "After"))

# Graphic
CortGraph <- ggplot(dGCBMd, aes(x = Time, y = LogCortisol, group = Water)) +
  geom_line(aes(colour = Water),size = 1) +
  geom_errorbar(aes(ymin = EBmin, ymax = EBmax, colour = Water), width = 0.2) +
  theme_classic() +
  ylab("Log Cortisol (nmol/ml)") +
  theme(axis.title.x=element_blank()) +
  guides(col=guide_legend("Procedure")) +
  labs(title = "PG")
CortGraph
ggsave(paste0(Graphic_path, "CrtBM1234PG.tiff"), dpi = 300)
