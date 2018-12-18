########### ############################################### ####################
#######################MASTER SCRIPT############ ###############################
## ########## ########################## ######################### #############
######"This is why we wipe the floor with those stata guys."############### ####

library(eurostat)
library(dplyr)
library(rgdal)
library(spdep)

### Data collection

# source("./R_skripte/Data/data_download.R")   #EUROSTAT
source("./R_skripte/Data_Transformation/data_download ERD.R") #European Regional Database


### Data transformation

# Define min <- 1 for minimal working dataset
# Define min <- 0 for maximal dataset

min<-1

# source("./R_skripte/Data/data_transformation.R")   #EUROSTAT
source("./R_skripte/Data_Transformation/data_transformation_ERD.R") #ERD

### Linear Regression

# source("./R_skripte/Regressions/linear_regression.R")   #EUROSTAT
source("./R_skripte/Regressions/linear_regression_ERD.R") #ERD

### Combining Data with Shapefile (2013)
source("./R_skripte/Data_Transformation/combining_shapefiles_with_dataD.R")