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
library(sphet)

########################
### Data collection
########################
#Data used is European Regional Database, we used the years 1996 to 2015
# While the paper covers the years 1995-2007, we now have data up to 2015. We 
# have to exclude 1995 because we also have Croatia in our set, for which there
# is no data available for that year. 

source("./R_skripte/Data Download.R") #European Regional Database

########################
### Data Transformation
########################

# In the following we restict our dataset to all countries within the ERD with
# the following exceptions:
  #--Cyprys, Luxembuorg: stastical reasons (Nuts 2 = Nuts 3 = Nuts 1)
  #--Norway, (we concentrate on EU)
  #--overseas regions


# Here we can pre-specify the k-nearest neighbour count:
kn<-5



# In this section we further construct the inequality index. Overall, we prepare 
# the data for further analysis.

source("./R_skripte/Data Transformation.R") #ERD

#Define which Weightslist to use. K-nearest (W.list.k) or Inverse distance (W.list.inv) 
W.list <- W.list.k
# W.list <- W.list.inv

########################
#### Regressions & Testing
########################
# The following script is divided into 3 sections:
  # -- 1.) Classical Linear Model
  # -- 2.) Spatial Models
  # -- 3.) Testing the hypothesis of a linear relationship between inequality and GDP

########
### 1.) We estimate simply OLS for applying certain tests on special dependence

 # Morans I
 # LISA
 # Breush-Pagan (for Heteroskedasticity)
 # Jarque-Bera (for normality)
 # LM Tests (for spatial dependence)

########
### 2.) Estimating spatial models

# 2.1 ) SAR- Model
# 2.2 ) SEM- Model
# 2.3 ) SDM- Model
# 2.4 ) 2SLS estimation of SAR- Model

########
### 3.) Hypothesis testing of linear relationship

# 3.1 ) Classical Linear Model
# 3.2 ) SEM- Model

source("./R_skripte/Regressions.R") #ERD

#First we estimated an OLS model, which already rejected the inverted-U theory. Since morans I and LISA suggested that
# we use spatial models, we also estimated SAR and SEM models (after using a range of tests to decide which models to use). 
# We also tested for heteroskedasticity and normality and used a 2SLS model to produce robust results (which did not differ drastically)
# Furthermore, we tested the hypothesis that EU countries are already in the developed state, meaning that they would be on the 
# declining part of the inverted-U curve (what would lead to negative betas). Our results showed a positive realtionship between 
# growth and inequality, rejecting this hypothesis. 
# More detailled information on what we exactly did can be found in the file.
