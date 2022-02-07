remove(list = ls())

############################################ Parameter ############################################
source("MBMFStudy2_Initialization.R")
Datapath = "Raw_Data/"
Output_path = "Output/"
# rstan_options(javascript = FALSE)

############################################# Frame ###############################################
d <- read.csv(paste0(Output_path, "ComputationsReady.txt"), sep="")
# d <- read.csv(paste0(Output_path, "dCompReadyGamb.txt"), sep="")

dHCGamb <- read.delim(paste0(Output_path,"dOKGamFE_Comp7P_OK_HCPG.txt"))

d <- filter(d, subjID %in% dHCGamb$subjID)

d1 <- data.frame()
d2 <- data.frame()

for (i in unique(d$subjID)) {
  dt <- filter(d, subjID == i)
  nT <- length(dt$Trial)
  Half <- nT/2
  cat("PS ", i, "  ", nT, " trials\n")
  dt1 <- filter(dt, Trial <= Half)
  dt2 <- filter(dt, Trial > Half)
  
  d1 <- rbind(d1, dt1)
  d2 <- rbind(d2, dt2)
}

########################################## Computations ###########################################
niter = 2000
nwarmup = niter/2

output1Half <- ts_par7(data = d1, niter = 2000, nwarmup = 1000, nchain = 4, ncore = 4)
dOutput1 <- output1Half$allIndPars
write.table(dOutput1, paste0(Output_path, "ComputationParameter7P_OK_HCPG_1stHalf.txt"),
            col.names = T, row.names = F, sep = "\t", dec = ".")
saveRDS(output1Half, file=paste0(Output_path, "Models/output7P_OK_HCPG_1stHalf.Rdata"))

output2Half <- ts_par7(data = d2, niter = 2000, nwarmup = 1000, nchain = 4, ncore = 4)
dOutput2 <- output2Half$allIndPars
write.table(dOutput2, paste0(Output_path, "ComputationParameter7P_OK_HCPG_2ndHalf.txt"),
            col.names = T, row.names = F, sep = "\t", dec = ".")
saveRDS(output2Half, file=paste0(Output_path, "Models/output7P_OK_HCPG_2ndHalf.Rdata"))

##### Check parameters
# Visually check convergence of the sampling chains (should look like 'hairy caterpillars')
plot(output1Half, type = "trace")
plot(output2Half, type = "trace")

# Check Rhat values (all Rhat values should be less than or equal to 1.1)
rhat(output1Half)
rhat(output2Half)

# Plot the posterior distributions of the hyper-parameters (distributions should be unimodal)
plot(output1Half)
plot(output2Half)

# Show the WAIC and LOOIC model fit estimates
printFit(output1Half)
printFit(output2Half)
