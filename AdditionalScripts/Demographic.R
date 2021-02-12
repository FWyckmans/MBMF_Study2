remove(list = ls())

############################################ Parameter ############################################
source("MBMFStudy2_1_Initialization.R")
Datapath = "Raw_Data/"
Output_path = "Output/"

############################################ Frame ################################################
d <- read.delim(paste0(Output_path,"dTot.txt"))%>%
  filter(OKd == 1)%>%
  select(subjID, Condition, Age, StudyLevel)

d <- AddDummyCol(d, "Sample")
d$Sample[d$Condition=="A_CPT"|d$Condition=="A_WPT"] <- "Alc"
d$Sample[d$Condition=="G_CPT"|d$Condition=="G_WPT"] <- "Gambler"
d$Sample[d$Condition=="HC_CPT"|d$Condition=="HC_WPT"] <- "HC"

d <- select(d, SubjID, Condition, Sample, Age, StudyLevel)

########################################### Descriptive ###########################################
########## By group and stressor
dBGS <- d%>%
  group_by(Condition)%>%
  summarise(MeanAge = mean(Age), SDAge = sd(Age), MedianAge = median(Age),
            MeanStudyLevel = mean(StudyLevel), SDStudyLevel = sd(StudyLevel), MedianStudyLevel = median(StudyLevel))

boxplot(Age ~ Condition, data = d)
boxplot(StudyLevel ~ Condition, data = d)

########## By group only
dBG <- d%>%
  group_by(Sample)%>%
  summarise(MeanAge = mean(Age), SDAge = sd(Age), MedianAge = median(Age),
            MeanStudyLevel = mean(StudyLevel), SDStudyLevel = sd(StudyLevel), MedianStudyLevel = median(StudyLevel))

boxplot(Age ~ Sample, data = d)
boxplot(StudyLevel ~ Sample, data = d)