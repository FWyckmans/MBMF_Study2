remove(list = ls())

############################################ Parameter ############################################
Test = 0
Datapath = "Raw_Data/RLTaskData/"
Output_path = "Output/"
Playlist = "This is: Blondie by Spotify, Call me, Random"

############################################# Frame ###############################################
d <- read.csv(paste0(Output_path, "ComputationsReady.txt"), sep="")

if (Test != 0){
  d <- filter(d, subjID == Test)
}

########################################## Computations ###########################################
# output <- ts_par7(
#   data = "example", niter = 2000, nwarmup = 1000, nchain = 4, ncore = 4)

output <- ts_par7(
  data = d, niter = 2000, nwarmup = 1000, nchain = 4, ncore = 4)

########################################## Save Output ###########################################
dOutput <- output$allIndPars
write.table(dOutput, paste0(Output_path, "ComputationParameter.txt"), col.names = T, row.names = F, sep = "\t", dec = ".")
