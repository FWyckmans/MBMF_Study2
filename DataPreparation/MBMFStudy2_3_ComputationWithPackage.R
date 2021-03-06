remove(list = ls())

############################################ Parameter ############################################
source("MBMFStudy2_Initialization.R")
Datapath = "Raw_Data/"
Output_path = "Output/"
Test = 0
StartOver = 1 # 1 to start from scratch, 0 to start with only new participants
rstan_options(javascript = FALSE)

############################################# Frame ###############################################
d <- read.csv(paste0(Output_path, "ComputationsReady.txt"), sep="")
dOK <- read.delim(paste0(Output_path, "dTot.txt"))%>%
  filter(OKd == 1)

d <- filter(d, subjID %in% dOK$subjID)

if (Test != 0){
  d <- filter(d, subjID == Test)
}

if (StartOver == 0){
  ComputationParameter <- read.delim(paste0(Output_path, "ComputationParameter.txt"))
  d <- filter(d, !subjID %in% ComputationParameter$subjID)
}

########################################## Computations ###########################################
# output <- ts_par7(
#   data = "example", niter = 2000, nwarmup = 1000, nchain = 4, ncore = 4)

niter = 4000
nwarmup = niter/2
output <- ts_par7(data = d, niter = 4000, nwarmup = 2000, nchain = 4, ncore = 4)

########################################## Save Output ###########################################
dOutput <- output$allIndPars
if (StartOver == 0){
  dOutput <- rbind(ComputationParameter, output$allIndPars)
}

if (Test == 0){
  write.table(dOutput, paste0(Output_path, "ComputationParameter.txt"), col.names = T, row.names = F, sep = "\t", dec = ".")}
