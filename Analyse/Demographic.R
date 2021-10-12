remove(list = ls())

############################################ Parameter ############################################
source("MBMFStudy2_Initialization.R")
Datapath = "Raw_Data/"
Output_path = "Output/"

############################################ Frame ################################################
d <- read.delim(paste0(Output_path,"dOKGamFE.txt"))

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

FromColNameToIndex(d, "StudyLevel")

dDescr <- NormalitySkewKurtosis(d,
                                VoI = FromColNameToIndex(d, c(AllCol$Demo, AllCol$Gamb, AllCol$Alc,
                                                              AllCol$Cog, AllCol$FR, AllCol$Perso,
                                                              AllCol$Computation, AllCol$ProbaM)),
                                Groups = "Sample", Format = "Long")

write_xlsx(dDescr, paste0(Output_path, "dDescrTot.xlsx"))
