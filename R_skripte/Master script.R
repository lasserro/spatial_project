########### ############################################### ####################
#######################MASTER SCRIPT############ ###############################
## ########## ########################## ######################### #############
######"This is why we wipe the floor with those stata guys."############### ####

library(eurostat)
library(dplyr)
library(rgdal)

####Data collection


#### I changed the code in the collection so it will try to load the data from a
#### folder "RData", if it doesn't succeed it will download the data and save it
#### there.
#### So just download the file just once, -> then load it each time.
#### I hope i got it right that this folder
#### will be ignored by git. Please share if it works, also please check the
#### folder in advance if it even exists.!!


source("./R_skripte/Data/data_download.R")


