remove(list = ls())

############################################ Parameter ############################################
Test = 0
Way = 'Wyck'
Datapath = "Raw_Data/RLTaskData/"
Output_path = "Output/"
Playlist = "This is Elton John by Spotify, Benny and the Jets"

############################################ Frame ################################################
d <- read.csv(paste0(Output_path, "ComputationsReady.txt"), sep="")