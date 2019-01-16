########### ############################################### ####################
#######################MASTER SCRIPT############ ###############################
## ########## ########################## ######################### #############
######"This is why we wipe the floor with those stata guys."############### ####

library(eurostat)
library(dplyr)
library(rgdal)
library(spdep)

### Data collection

source("./R_skripte/Data Download.R") #European Regional Database


### Data transformation

# Define min <- 1 for minimal working dataset
# Define min <- 0 for maximal dataset

min<-1

#define Weightsmatrix for k-nearest neighbour

kn<-5

source("./R_skripte/Data Transformation.R") #ERD

rm(X_1,X_2,Y,drop,min,overseas,CV,GDP_ERD,POP_ERD,gdp2,gdp3,pop2,pop3)

### Tests

#source("./R_skripte/Spatial Tests.R") #ERD

### Regressions

source("./R_skripte/Regressions.R") #ERD
