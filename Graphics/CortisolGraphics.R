remove(list = ls())

############################################ Parameter ############################################
source("MBMFStudy2_Initialization.R")
Datapath = "Raw_Data/"
Output_path = "Output/"
multEB = 2

############################################# Frame ###############################################
d <- read.delim(paste0(Output_path,"dOKGamFE.txt"))

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

CortGraph <- ggplot(dGCB, aes(x = Time, y = LogCortisol, group = Water)) +
  geom_line(aes(colour = Water),size = 1) +
  geom_errorbar(aes(ymin = EBmin, ymax = EBmax, colour = Water), width = 0.2)
CortGraph

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