remove(list = ls())

############################################ Parameter ############################################
source("MBMFStudy2_Initialization.R")
Datapath = "Raw_Data/"
Output_path = "Output/"
Test = 0
StartOver = 1 # 1 to start from scratch
# rstan_options(javascript = FALSE)

############################################# Frame ###############################################
d <- read.csv(paste0(Output_path, "ComputationsReady.txt"), sep="")
d <- read.csv(paste0(Output_path, "dCompReadyGamb.txt"), sep="")

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
niter = 6000
nwarmup = niter/2
output <- ts_par7(data = d, niter = 6000, nwarmup = 3000, nchain = 4, ncore = 4)

########################################## Save Output ###########################################
dOutput <- output$allIndPars

if (StartOver == 0){
  dOutput <- rbind(ComputationParameter, output$allIndPars)
}

if (Test == 0){
  write.table(dOutput, paste0(Output_path, "ComputationParameter7P_OK_HCPGAL.txt"), col.names = T, row.names = F, sep = "\t", dec = ".")
  saveRDS(output, file=paste0(Output_path, "Models/output7P_OK_HCPGAL.Rdata"))
  }

# Visually check convergence of the sampling chains (should look like 'hairy caterpillars')
plot(output, type = "trace")

# Check Rhat values (all Rhat values should be less than or equal to 1.1)
rhat(output)

# Plot the posterior distributions of the hyper-parameters (distributions should be unimodal)
plot(output)

# Show the WAIC and LOOIC model fit estimates
printFit(output)
