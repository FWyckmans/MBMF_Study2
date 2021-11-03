remove(list = ls())

############################################ Parameter ############################################
source("MBMFStudy2_Initialization.R")
Datapath = "Raw_Data/"
Output_path = "Output/"

dRL <- read.csv(paste0(Datapath, "DataFromORScript/choice_regress.dat"), sep="")

d <- read.delim(paste0(Output_path,"dOKGamFE_Comp7P_OK_HCPG.txt"))

dRL <- dRL%>%
  filter(subj %in% d$subjID)

dRL <- AddDummyCol(dRL, c("dCorti", "SampleC", "Raven", "RavenXdCortM"))

dRLF <- data.frame()

for (i in unique(dRL$subj)) {
  print(i)
  dRLt <- filter(dRL, subj==i)
  dRLt$zdCorti <- d$zdCorti[d$subjID == i]
  dRLt$zdCortiM <- d$zdCortiM[d$subjID == i]
  dRLt$SampleC <- d$SampleC[d$subjID == i]
  dRLt$zRaven <- d$zRaven[d$subjID == i]
  dRLt$zOSPAN <- d$zOSPAN[d$subjID == i]
  dRLF <- rbind(dRLF, dRLt)
}

dRLFR <- dRLF%>%
  filter(mn == 1)

dRLFUR <- dRLF%>%
  filter(mn == -1)


########################################## Modelisation ###########################################
########## Total model
if("LRMtCorti_OK_HCPG.Rdata" %in% dir(paste0(Output_path, "Models/"))){
  LRM <- readRDS(paste0(Output_path, "Models/LRMtCorti_OK_HCPG.Rdata"))
} else {
  LRM <- glmer(stay ~ (1+mn*common|subj) + zdCorti*SampleC*zOSPAN*mn*common,
               family = binomial, data = dRLF, control = glmerControl(optimizer = "bobyqa"), nAGQ = 1)
  saveRDS(LRM, file=paste0(Output_path, "Models/LRMtCorti_OK_HCPG.Rdata"))
}

if("LRMRtCorti_OK_HCPG.Rdata" %in% dir(paste0(Output_path, "Models/"))){
  LRMR <- readRDS(paste0(Output_path, "Models/LRMRtCorti_OK_HCPG.Rdata"))
} else {
  LRMR <- glmer(stay ~ (1+common|subj) + zdCorti*SampleC*zOSPAN*common,
                family = binomial, data = dRLFR, control = glmerControl(optimizer = "bobyqa"), nAGQ = 1)
  saveRDS(LRMR, file=paste0(Output_path, "Models/LRMRtCorti_OK_HCPG.Rdata"))
}

if("LRMURtCorti_OK_HCPG.Rdata" %in% dir(paste0(Output_path, "Models/"))){
  LRMUR <- readRDS(paste0(Output_path, "Models/LRMURtCorti_OK_HCPG.Rdata"))
} else {
  LRMUR <- glmer(stay ~ (1+common|subj) + zdCorti*SampleC*zOSPAN*common,
                 family = binomial, data = dRLFUR, control = glmerControl(optimizer = "bobyqa"), nAGQ = 1)
  saveRDS(LRMUR, file=paste0(Output_path, "Models/LRMURtCorti_OK_HCPG.Rdata"))
}


########## Partial model
if("LRMpCorti_OK_HCPG.Rdata" %in% dir(paste0(Output_path, "Models/"))){
  LRMp <- readRDS(paste0(Output_path, "Models/LRMpCorti_OK_HCPG.Rdata"))
} else {
  LRMp <- glmer(stay ~ (1+mn*common|subj) +
                  mn + mn:common +
                  SampleC:mn + SampleC:mn:common +
                  zOSPAN:mn + zOSPAN:mn:common +
                  zdCorti:mn + zdCorti:mn:common +
                  
                  SampleC:zOSPAN:mn + SampleC:zOSPAN:mn:common +
                  SampleC:zdCorti:mn + SampleC:zdCorti:mn:common +
                  zdCorti:zOSPAN:mn + zdCorti:zOSPAN:mn:common +
                  
                  SampleC:zOSPAN:zdCorti:mn + SampleC:zOSPAN:zdCorti:mn:common,
                family = binomial, data = dRLF, control = glmerControl(optimizer = "bobyqa"), nAGQ = 1)
  saveRDS(LRMp, file=paste0(Output_path, "Models/LRMpCorti_OK_HCPG.Rdata"))
}

if("LRMRpCorti_OK_HCPG.Rdata" %in% dir(paste0(Output_path, "Models/"))){
  LRMRp <- readRDS(paste0(Output_path, "Models/LRMRpCorti_OK_HCPG.Rdata"))
} else {
  LRMRp <- glmer(stay ~ (1+common|subj) +
                   common +
                   SampleC:common +
                   zOSPAN:common +
                   zdCorti:common +
                   
                   SampleC:zOSPAN:common +
                   SampleC:zdCorti:common +
                   zdCorti:zOSPAN:common +
                   
                   SampleC:zOSPAN:zdCorti:common,
                 family = binomial, data = dRLFR, control = glmerControl(optimizer = "bobyqa"), nAGQ = 1)
  saveRDS(LRMRp, file=paste0(Output_path, "Models/LRMRpCorti_OK_HCPG.Rdata"))
}


if("LRMURpCorti_OK_HCPG.Rdata" %in% dir(paste0(Output_path, "Models/"))){
  LRMURp <- readRDS(paste0(Output_path, "Models/LRMURpCorti_OK_HCPG.Rdata"))
} else {
  LRMURp <- glmer(stay ~ (1+common|subj) +
                   common +
                   SampleC:common +
                   zOSPAN:common +
                   zdCorti:common +
                   
                   SampleC:zOSPAN:common +
                   SampleC:zdCorti:common +
                   zdCorti:zOSPAN:common +
                   
                   SampleC:zOSPAN:zdCorti:common,
                 family = binomial, data = dRLFR, control = glmerControl(optimizer = "bobyqa"), nAGQ = 1)
  saveRDS(LRMURp, file=paste0(Output_path, "Models/LRMURpCorti_OK_HCPG.Rdata"))
}

##### Reproduce w results
if("LRMrCorti_OK_HCPG.Rdata" %in% dir(paste0(Output_path, "Models/"))){
  LRMr <- readRDS(paste0(Output_path, "Models/LRMrCorti_OK_HCPG.Rdata"))
} else {
  LRMr <- glmer(stay ~ (1+mn*common|subj) +
                  mn + mn:common +
                  zdCorti:zOSPAN:mn + zdCorti:zOSPAN:mn:common +
                  
                  SampleC:zOSPAN:zdCorti:mn + SampleC:zOSPAN:zdCorti:mn:common,
                family = binomial, data = dRLF, control = glmerControl(optimizer = "bobyqa"), nAGQ = 1)
  saveRDS(LRMr, file=paste0(Output_path, "Models/LRMrCorti_OK_HCPG.Rdata"))
}

if("LRMRrCorti_OK_HCPG.Rdata" %in% dir(paste0(Output_path, "Models/"))){
  LRMRr <- readRDS(paste0(Output_path, "Models/LRMRrCorti_OK_HCPG.Rdata"))
} else {
  LRMRr <- glmer(stay ~ (1+common|subj) +
                   common +
                   zdCorti:zOSPAN:common +
                   SampleC:zOSPAN:zdCorti:common,
                 family = binomial, data = dRLFR, control = glmerControl(optimizer = "bobyqa"), nAGQ = 1)
  saveRDS(LRMRr, file=paste0(Output_path, "Models/LRMRrCorti_OK_HCPG.Rdata"))
}


if("LRMURrCorti_OK_HCPG.Rdata" %in% dir(paste0(Output_path, "Models/"))){
  LRMURr <- readRDS(paste0(Output_path, "Models/LRMURrCorti_OK_HCPG.Rdata"))
} else {
  LRMURr <- glmer(stay ~ (1+common|subj) +
                    common +
                    zdCorti:zOSPAN:common +
                    SampleC:zOSPAN:zdCorti:common,
                  family = binomial, data = dRLFUR, control = glmerControl(optimizer = "bobyqa"), nAGQ = 1)
  saveRDS(LRMURr, file=paste0(Output_path, "Models/LRMURrCorti_OK_HCPG.Rdata"))
}

########## Output
summary(LRM)
summary(LRMR)
summary(LRMUR)

summary(LRMp)
summary(LRMRp)
summary(LRMURp)

summary(LRMr)
summary(LRMRr)
summary(LRMURr)
