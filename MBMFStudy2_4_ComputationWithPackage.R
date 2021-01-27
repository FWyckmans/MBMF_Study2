remove(list = ls())

############################################ Parameter ############################################
Test = 0
Way = 'Wyck'
Datapath = "Raw_Data/RLTaskData/"
Output_path = "Output/"
Playlist = "This is: Johnny Cash by Spotify, Ring of Fire, random"

############################################# Frame ###############################################
d <- read.csv(paste0(Output_path, "ComputationsReady.txt"), sep="")

########################################## Computations ###########################################
# output <- ts_par7(
#   data = "example", niter = 2000, nwarmup = 1000, nchain = 4, ncore = 4)

output <- ts_par7(
  data = d, niter = 2000, nwarmup = 1000, nchain = 4, ncore = 4)
