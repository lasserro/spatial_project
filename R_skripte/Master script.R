########### ############################################### ####################
#######################MASTER SCRIPT############ ###############################
## ########## ########################## ######################### #############
######"This is why we wipe the floor with those stata guys."############### ####

library(eurostat)
library(dplyr)
library(rgdal)
library(spdep)
library(plm)
library(reshape2)
library(splm)

########################
### Data collection
########################
#Data used is European Regional Database, we used the years 1996 to 2015
# While the paper covers the years 1995-2007, we now have data up to 2015. We 
# have to exclude 1995 because we also have Croatia in our set, for which there
# is no data available for that year. 

source("./R_skripte/Data Download.R") #European Regional Database

### Data transformation
#In the following we have two specifications for the dataset, first a maximal set, 
# with all countries and periods which we used to assess which ones we will keep in the end. 
# The min-specification which we ended up using later on includes all countries from 
# the ERD, with few exceptions: 
#--Malta, Cyprys: stastical reasons (Nuts 2 = Nuts 3 = Nuts 1)
#--Norway, (we concentrate on EU)
#--CHECK

# Define min <- 1 for minimal working dataset
# Define min <- 0 for maximal dataset

min<-1

#define Weightsmatrix for k-nearest neighbour

kn<-5
#In this section we construct the inequality index, as well as the weight matrices we will use in the regressions. Overall, we prepare 
# the data for further analysis.


source("./R_skripte/Data Transformation.R") #ERD

#rm(X_1,X_2,Y,drop,min,overseas,CV,GDP_ERD,POP_ERD,gdp2,gdp3,pop2,pop3)

### Tests
#Define Weightslist
W.list <- W.list.k
# W.list <- W.list.inv

########################
#### Testing
########################

#We used morans I, as well as LISA, both suggeted that there is spatial dependence. Further testing follows in the regression part,
# after we estimated the different models

source("./R_skripte/Spatial Tests.R") #ERD

#########################
### Regressions
##########################
source("./R_skripte/Regressions.R") #ERD

#First we estimated an OLS model, which already rejected the inverted-U theory. Since morans I and LISA suggested that
# we use spatial models, we also estimated SAR and SEM models (after using a range of tests to decide which models to use). 
# We also tested for heteroskedasticity and normality and used a 2SLS model to produce robust results (which did not differ drastically)
# Furthermore, we tested the hypothesis that EU countries are already in the developed state, meaning that they would be on the 
# declining part of the inverted-U curve (what would lead to negative betas). Our results showed a positive realtionship between 
# growth and inequality, rejecting this hypothesis. 
# More detailled information on what we exactly did can be found in the file.
