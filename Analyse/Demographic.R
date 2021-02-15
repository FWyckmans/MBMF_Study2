remove(list = ls())

############################################ Parameter ############################################
source("MBMFStudy2_Initialization.R")
Datapath = "Raw_Data/"
Output_path = "Output/"

############################################ Frame ################################################
d <- read.delim(paste0(Output_path,"dTot.txt"))%>%
  filter(OKd == 1)%>%
  select(subjID, Condition, Sample, Age, StudyLevel)

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
