remove(list = ls())

############################################ Parameter ############################################
source("MBMFStudy2_Initialization.R")
Datapath = "Raw_Data/"
Output_path = "Output/"

dRL <- read.csv(paste0(Datapath, "DataFromORScript/choice_regress.dat"), sep="")

d <- read.delim(paste0(Output_path,"dTot.txt"))%>%
  filter(OKd == 1) %>%
  filter(OKCort == 1) %>%
  filter(Sample != "Alc") %>%
  filter(subjID != 250) %>%
  select(subjID, dCorti, SampleC, Raven, RavenXdCortM)

dRL <- dRL%>%
  filter(subj %in% d$subjID)

dRL <- AddDummyCol(dRL, c("dCorti", "SampleC", "Raven", "RavenXdCortM"))

dRLF <- data.frame()

for (i in unique(dRL$subj)) {
  print(i)
  dRLt <- filter(dRL, subj==i)
  # dt <- filter(d, subjID == 150)
  dRLt$dCorti <- d$dCorti[d$subjID == i]
  dRLt$SampleC <- d$SampleC[d$subjID == i]
  dRLt$Raven <- d$Raven[d$subjID == i]
  dRLt$RavenXdCortM <- d$RavenXdCortM[d$subjID == i]
  dRLF <- rbind(dRLF, dRLt)
}




LRM <- glmer(stay ~ (1+mn*common|subj) + dCorti*SampleC*Raven*mn*common,
             family = binomial, data = dRLF, control = glmerControl(optimizer = "bobyqa"),
             nAGQ = 1)
summary(LRM)
