remove(list = ls())

############################################ Parameter ############################################
source("MBMFStudy2_Initialization.R")
Datapath = "Raw_Data/"
Output_path = "Output/"
Graphic_path = "Graphics/Graph/"
multEB = 2

############################################# Frame ###############################################
d <- read.delim(paste0(Output_path,"dOKGamFE_Comp7P_OK_HCPG.txt"))

dt <- d[c("NS", "Sample", "StressGrM", "w", "beta1", "beta2", "a1", "a2", "lambda", "pi", "MBv", "MFv")]
dt$StressGrM[dt$StressGrM == -1] <- "Stressed"
dt$StressGrM[dt$StressGrM == 1] <- "Not Stressed"

##### w parameter
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

dg$Group[dg$Group == "Gambler Not Stressed"] <- "PG NR"
dg$Group[dg$Group == "Gambler Stressed"] <- "PG R"

dg$Group[dg$Group == "HC Not Stressed"] <- "HC NR"
dg$Group[dg$Group == "HC Stressed"] <- "HC R"

dg$Group <- factor(dg$Group,
                   levels = c("PG R", "PG NR", "HC R", "HC NR"))

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

##### a1 parameter
dg <- dt%>%
  unite("Group", Sample:StressGrM, sep = " ")%>%
  group_by(Group)%>%
  summarise(a1M = mean(a1), a1SD = sd(a1))

dg <- AddDummyCol(dg, "n")
dg$n[dg$Group=="Gambler Not Stressed"] <- length(d$NS[d$SampleC == -1 & d$StressGrM == 1])
dg$n[dg$Group=="Gambler Stressed"] <- length(d$NS[d$SampleC == -1 & d$StressGrM == -1])
dg$n[dg$Group=="HC Not Stressed"] <- length(d$NS[d$SampleC == 1 & d$StressGrM == 1])
dg$n[dg$Group=="HC Stressed"] <- length(d$NS[d$SampleC == 1 & d$StressGrM == -1])

dg <- mutate(dg, SE = a1SD/sqrt(n), ymin = a1M - multEB*SE, ymax = a1M + multEB*SE)

dg$Group[dg$Group == "Gambler Not Stressed"] <- "PG NR"
dg$Group[dg$Group == "Gambler Stressed"] <- "PG R"

dg$Group[dg$Group == "HC Not Stressed"] <- "HC NR"
dg$Group[dg$Group == "HC Stressed"] <- "HC R"

dg$Group <- factor(dg$Group,
                   levels = c("PG R", "PG NR", "HC R", "HC NR"))

ga1 <- ggplot(dg, aes(x = Group, y = a1M, fill = Group)) +
  geom_bar(stat = "identity", color="black") +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2) +
  
  # Title
  # ggtitle(Title) +
  
  # Axis
  ylab("a1-parameter") +
  # ylim(0, 1) +
  # scale_y_continuous(limits=c(0.3, 0.6), oob = rescale_none) +
  
  # Theme
  theme_classic() +
  scale_fill_manual(values=c("blue3", "blue3", "gray80", "gray80")) +
  theme(legend.title = element_blank(), legend.position = "none") +
  theme(plot.title = element_text(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12))

ggsave(paste0(Graphic_path, "a1ParameterByGroup.tiff"), dpi=300)
ga1

##### a2 parameter
dg <- dt%>%
  unite("Group", Sample:StressGrM, sep = " ")%>%
  group_by(Group)%>%
  summarise(a2M = mean(a2), a2SD = sd(a2))

dg <- AddDummyCol(dg, "n")
dg$n[dg$Group=="Gambler Not Stressed"] <- length(d$NS[d$SampleC == -1 & d$StressGrM == 1])
dg$n[dg$Group=="Gambler Stressed"] <- length(d$NS[d$SampleC == -1 & d$StressGrM == -1])
dg$n[dg$Group=="HC Not Stressed"] <- length(d$NS[d$SampleC == 1 & d$StressGrM == 1])
dg$n[dg$Group=="HC Stressed"] <- length(d$NS[d$SampleC == 1 & d$StressGrM == -1])

dg <- mutate(dg, SE = a2SD/sqrt(n), ymin = a2M - multEB*SE, ymax = a2M + multEB*SE)

dg$Group[dg$Group == "Gambler Not Stressed"] <- "PG NR"
dg$Group[dg$Group == "Gambler Stressed"] <- "PG R"

dg$Group[dg$Group == "HC Not Stressed"] <- "HC NR"
dg$Group[dg$Group == "HC Stressed"] <- "HC R"

dg$Group <- factor(dg$Group,
                   levels = c("PG R", "PG NR", "HC R", "HC NR"))

ga2 <- ggplot(dg, aes(x = Group, y = a2M, fill = Group)) +
  geom_bar(stat = "identity", color="black") +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2) +
  
  # Title
  # ggtitle(Title) +
  
  # Axis
  ylab("a2-parameter") +
  # ylim(0, 1) +
  # scale_y_continuous(limits=c(0.3, 0.6), oob = rescale_none) +
  
  # Theme
  theme_classic() +
  scale_fill_manual(values=c("blue3", "blue3", "gray80", "gray80")) +
  theme(legend.title = element_blank(), legend.position = "none") +
  theme(plot.title = element_text(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12))

ggsave(paste0(Graphic_path, "a2ParameterByGroup.tiff"), dpi=300)
ga2

##### beta1 parameter
dg <- dt%>%
  unite("Group", Sample:StressGrM, sep = " ")%>%
  group_by(Group)%>%
  summarise(beta1M = mean(beta1), beta1SD = sd(beta1))

dg <- AddDummyCol(dg, "n")
dg$n[dg$Group=="Gambler Not Stressed"] <- length(d$NS[d$SampleC == -1 & d$StressGrM == 1])
dg$n[dg$Group=="Gambler Stressed"] <- length(d$NS[d$SampleC == -1 & d$StressGrM == -1])
dg$n[dg$Group=="HC Not Stressed"] <- length(d$NS[d$SampleC == 1 & d$StressGrM == 1])
dg$n[dg$Group=="HC Stressed"] <- length(d$NS[d$SampleC == 1 & d$StressGrM == -1])

dg <- mutate(dg, SE = beta1SD/sqrt(n), ymin = beta1M - multEB*SE, ymax = beta1M + multEB*SE)

dg$Group[dg$Group == "Gambler Not Stressed"] <- "PG NR"
dg$Group[dg$Group == "Gambler Stressed"] <- "PG R"

dg$Group[dg$Group == "HC Not Stressed"] <- "HC NR"
dg$Group[dg$Group == "HC Stressed"] <- "HC R"

dg$Group <- factor(dg$Group,
                   levels = c("PG R", "PG NR", "HC R", "HC NR"))

gbeta1 <- ggplot(dg, aes(x = Group, y = beta1M, fill = Group)) +
  geom_bar(stat = "identity", color="black") +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2) +
  
  # Title
  # ggtitle(Title) +
  
  # Axis
  ylab("beta1-parameter") +
  # ylim(0, 1) +
  # scale_y_continuous(limits=c(0.3, 0.6), oob = rescale_none) +
  
  # Theme
  theme_classic() +
  scale_fill_manual(values=c("blue3", "blue3", "gray80", "gray80")) +
  theme(legend.title = element_blank(), legend.position = "none") +
  theme(plot.title = element_text(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12))

ggsave(paste0(Graphic_path, "beta1ParameterByGroup.tiff"), dpi=300)
gbeta1

##### beta2 parameter
dg <- dt%>%
  unite("Group", Sample:StressGrM, sep = " ")%>%
  group_by(Group)%>%
  summarise(beta2M = mean(beta2), beta2SD = sd(beta2))

dg <- AddDummyCol(dg, "n")
dg$n[dg$Group=="Gambler Not Stressed"] <- length(d$NS[d$SampleC == -1 & d$StressGrM == 1])
dg$n[dg$Group=="Gambler Stressed"] <- length(d$NS[d$SampleC == -1 & d$StressGrM == -1])
dg$n[dg$Group=="HC Not Stressed"] <- length(d$NS[d$SampleC == 1 & d$StressGrM == 1])
dg$n[dg$Group=="HC Stressed"] <- length(d$NS[d$SampleC == 1 & d$StressGrM == -1])

dg <- mutate(dg, SE = beta2SD/sqrt(n), ymin = beta2M - multEB*SE, ymax = beta2M + multEB*SE)

dg$Group[dg$Group == "Gambler Not Stressed"] <- "PG NR"
dg$Group[dg$Group == "Gambler Stressed"] <- "PG R"

dg$Group[dg$Group == "HC Not Stressed"] <- "HC NR"
dg$Group[dg$Group == "HC Stressed"] <- "HC R"

dg$Group <- factor(dg$Group,
                   levels = c("PG R", "PG NR", "HC R", "HC NR"))

gbeta2 <- ggplot(dg, aes(x = Group, y = beta2M, fill = Group)) +
  geom_bar(stat = "identity", color="black") +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2) +
  
  # Title
  # ggtitle(Title) +
  
  # Axis
  ylab("beta2-parameter") +
  # ylim(0, 1) +
  # scale_y_continuous(limits=c(0.3, 0.6), oob = rescale_none) +
  
  # Theme
  theme_classic() +
  scale_fill_manual(values=c("blue3", "blue3", "gray80", "gray80")) +
  theme(legend.title = element_blank(), legend.position = "none") +
  theme(plot.title = element_text(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12))

ggsave(paste0(Graphic_path, "beta2ParameterByGroup.tiff"), dpi=300)
gbeta2

### pi parameter
dg <- dt%>%
  unite("Group", Sample:StressGrM, sep = " ")%>%
  group_by(Group)%>%
  summarise(piM = mean(pi), piSD = sd(pi))

dg <- AddDummyCol(dg, "n")
dg$n[dg$Group=="Gambler Not Stressed"] <- length(d$NS[d$SampleC == -1 & d$StressGrM == 1])
dg$n[dg$Group=="Gambler Stressed"] <- length(d$NS[d$SampleC == -1 & d$StressGrM == -1])
dg$n[dg$Group=="HC Not Stressed"] <- length(d$NS[d$SampleC == 1 & d$StressGrM == 1])
dg$n[dg$Group=="HC Stressed"] <- length(d$NS[d$SampleC == 1 & d$StressGrM == -1])

dg <- mutate(dg, SE = piSD/sqrt(n), ymin = piM - multEB*SE, ymax = piM + multEB*SE)

dg$Group[dg$Group == "Gambler Not Stressed"] <- "PG NR"
dg$Group[dg$Group == "Gambler Stressed"] <- "PG R"

dg$Group[dg$Group == "HC Not Stressed"] <- "HC NR"
dg$Group[dg$Group == "HC Stressed"] <- "HC R"

dg$Group <- factor(dg$Group,
                   levels = c("PG R", "PG NR", "HC R", "HC NR"))

gpi <- ggplot(dg, aes(x = Group, y = piM, fill = Group)) +
  geom_bar(stat = "identity", color="black") +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2) +
  
  # Title
  # ggtitle(Title) +
  
  # Axis
  ylab("pi-parameter") +
  # ylim(0, 1) +
  # scale_y_continuous(limits=c(0.3, 0.6), oob = rescale_none) +
  
  # Theme
  theme_classic() +
  scale_fill_manual(values=c("blue3", "blue3", "gray80", "gray80")) +
  theme(legend.title = element_blank(), legend.position = "none") +
  theme(plot.title = element_text(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12))

ggsave(paste0(Graphic_path, "piParameterByGroup.tiff"), dpi=300)
gpi

### lambda parameter
dg <- dt%>%
  unite("Group", Sample:StressGrM, sep = " ")%>%
  group_by(Group)%>%
  summarise(lambdaM = mean(lambda), lambdaSD = sd(lambda))

dg <- AddDummyCol(dg, "n")
dg$n[dg$Group=="Gambler Not Stressed"] <- length(d$NS[d$SampleC == -1 & d$StressGrM == 1])
dg$n[dg$Group=="Gambler Stressed"] <- length(d$NS[d$SampleC == -1 & d$StressGrM == -1])
dg$n[dg$Group=="HC Not Stressed"] <- length(d$NS[d$SampleC == 1 & d$StressGrM == 1])
dg$n[dg$Group=="HC Stressed"] <- length(d$NS[d$SampleC == 1 & d$StressGrM == -1])

dg <- mutate(dg, SE = lambdaSD/sqrt(n), ymin = lambdaM - multEB*SE, ymax = lambdaM + multEB*SE)

dg$Group[dg$Group == "Gambler Not Stressed"] <- "PG NR"
dg$Group[dg$Group == "Gambler Stressed"] <- "PG R"

dg$Group[dg$Group == "HC Not Stressed"] <- "HC NR"
dg$Group[dg$Group == "HC Stressed"] <- "HC R"

dg$Group <- factor(dg$Group,
                   levels = c("PG R", "PG NR", "HC R", "HC NR"))

glambda <- ggplot(dg, aes(x = Group, y = lambdaM, fill = Group)) +
  geom_bar(stat = "identity", color="black") +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2) +
  
  # Title
  # ggtitle(Title) +
  
  # Axis
  ylab("lambda-parameter") +
  # ylim(0, 1) +
  # scale_y_continuous(limits=c(0.3, 0.6), oob = rescale_none) +
  
  # Theme
  theme_classic() +
  scale_fill_manual(values=c("blue3", "blue3", "gray80", "gray80")) +
  theme(legend.title = element_blank(), legend.position = "none") +
  theme(plot.title = element_text(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12))

ggsave(paste0(Graphic_path, "lambdaParameterByGroup.tiff"), dpi=300)
glambda

### MBv parameter
dg <- dt%>%
  unite("Group", Sample:StressGrM, sep = " ")%>%
  group_by(Group)%>%
  summarise(MBvM = mean(MBv), MBvSD = sd(MBv))

dg <- AddDummyCol(dg, "n")
dg$n[dg$Group=="Gambler Not Stressed"] <- length(d$NS[d$SampleC == -1 & d$StressGrM == 1])
dg$n[dg$Group=="Gambler Stressed"] <- length(d$NS[d$SampleC == -1 & d$StressGrM == -1])
dg$n[dg$Group=="HC Not Stressed"] <- length(d$NS[d$SampleC == 1 & d$StressGrM == 1])
dg$n[dg$Group=="HC Stressed"] <- length(d$NS[d$SampleC == 1 & d$StressGrM == -1])

dg <- mutate(dg, SE = MBvSD/sqrt(n), ymin = MBvM - multEB*SE, ymax = MBvM + multEB*SE)

dg$Group[dg$Group == "Gambler Not Stressed"] <- "PG NR"
dg$Group[dg$Group == "Gambler Stressed"] <- "PG R"

dg$Group[dg$Group == "HC Not Stressed"] <- "HC NR"
dg$Group[dg$Group == "HC Stressed"] <- "HC R"

dg$Group <- factor(dg$Group,
                   levels = c("PG R", "PG NR", "HC R", "HC NR"))

gMBv <- ggplot(dg, aes(x = Group, y = MBvM, fill = Group)) +
  geom_bar(stat = "identity", color="black") +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2) +
  
  # Title
  # ggtitle(Title) +
  
  # Axis
  ylab("MB-parameter") +
  # ylim(0, 1) +
  # scale_y_continuous(limits=c(0.3, 0.6), oob = rescale_none) +
  
  # Theme
  theme_classic() +
  scale_fill_manual(values=c("blue3", "blue3", "gray80", "gray80")) +
  theme(legend.title = element_blank(), legend.position = "none") +
  theme(plot.title = element_text(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12))

ggsave(paste0(Graphic_path, "MBvParameterByGroup.tiff"), dpi=300)
gMBv

### MFv parameter
dg <- dt%>%
  unite("Group", Sample:StressGrM, sep = " ")%>%
  group_by(Group)%>%
  summarise(MFvM = mean(MFv), MFvSD = sd(MFv))

dg <- AddDummyCol(dg, "n")
dg$n[dg$Group=="Gambler Not Stressed"] <- length(d$NS[d$SampleC == -1 & d$StressGrM == 1])
dg$n[dg$Group=="Gambler Stressed"] <- length(d$NS[d$SampleC == -1 & d$StressGrM == -1])
dg$n[dg$Group=="HC Not Stressed"] <- length(d$NS[d$SampleC == 1 & d$StressGrM == 1])
dg$n[dg$Group=="HC Stressed"] <- length(d$NS[d$SampleC == 1 & d$StressGrM == -1])

dg <- mutate(dg, SE = MFvSD/sqrt(n), ymin = MFvM - multEB*SE, ymax = MFvM + multEB*SE)

dg$Group[dg$Group == "Gambler Not Stressed"] <- "PG NR"
dg$Group[dg$Group == "Gambler Stressed"] <- "PG R"

dg$Group[dg$Group == "HC Not Stressed"] <- "HC NR"
dg$Group[dg$Group == "HC Stressed"] <- "HC R"

dg$Group <- factor(dg$Group,
                   levels = c("PG R", "PG NR", "HC R", "HC NR"))

gMFv <- ggplot(dg, aes(x = Group, y = MFvM, fill = Group)) +
  geom_bar(stat = "identity", color="black") +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2) +
  
  # Title
  # ggtitle(Title) +
  
  # Axis
  ylab("MF-parameter") +
  # ylim(0, 1) +
  # scale_y_continuous(limits=c(0.3, 0.6), oob = rescale_none) +
  
  # Theme
  theme_classic() +
  scale_fill_manual(values=c("blue3", "blue3", "gray80", "gray80")) +
  theme(legend.title = element_blank(), legend.position = "none") +
  theme(plot.title = element_text(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12))

ggsave(paste0(Graphic_path, "MFvParameterByGroup.tiff"), dpi=300)
gMFv

###### MultipleGrid
Mg <- plot_grid(g, ga1, ga2, gbeta1, gbeta2, glambda, gpi, gMBv, gMFv, ncol = 3,
                labels = c()) # labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I"))
Mg
ggsave(paste0(Graphic_path, "GbyStrComputationalParameters.tiff"), dpi = 300)

Mg <- plot_grid(g, ga1, ga2, gbeta1, gbeta2, glambda, gpi, ncol = 3,
                labels = c()) # labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I"))
Mg
ggsave(paste0(Graphic_path, "GbyStrComputationalParameters.tiff"), dpi = 300)
