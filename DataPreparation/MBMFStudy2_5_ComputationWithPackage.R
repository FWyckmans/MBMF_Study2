remove(list = ls())

############################################ Parameter ############################################
source("MBMFStudy2_Initialization.R")
Datapath = "Raw_Data/"
Output_path = "Output/"
Test = 0
# rstan_options(javascript = FALSE)

############################################# Frame ###############################################
d <- read.csv(paste0(Output_path, "ComputationsReady.txt"), sep="")
# d <- read.csv(paste0(Output_path, "dCompReadyGamb.txt"), sep="")

dHCGamb <- read.delim(paste0(Output_path,"dOKGamFE_Comp7P_OK_HCPG.txt"))

d <- filter(d, subjID %in% dHCGamb$subjID)

d <- filter(d, Trial > 10)

if (Test != 0){
  d <- filter(d, subjID == Test)
}

########################################## Computations ###########################################
if ("output7P_OK_HCPG_AllTrials.Rdata" %in% dir(paste0(Output_path, "Models/"))){
  output = readRDS(file=paste0(Output_path, "Models/output7P_OK_HCPG_AllTrials.Rdata"))
} else {
  niter = 2000
  nwarmup = niter/2
  output <- ts_par7(data = d, niter = 2000, nwarmup = 1000, nchain = 4, ncore = 4)
}

########################################## Save Output ###########################################
dOutput <- output$allIndPars

if (Test == 0){
  write.table(dOutput, paste0(Output_path, "ComputationParameter7P_OK_HCPG.txt"),
              col.names = T, row.names = F, sep = "\t", dec = ".")
  saveRDS(output, file=paste0(Output_path, "Models/output7P_OK_HCPG.Rdata"))
  }

# Visually check convergence of the sampling chains (should look like 'hairy caterpillars')
plot(output, type = "trace")

# Check Rhat values (all Rhat values should be less than or equal to 1.1)
rhat(output)

# Plot the posterior distributions of the hyper-parameters (distributions should be unimodal)
plot(output)

# Show the WAIC and LOOIC model fit estimates
printFit(output)
