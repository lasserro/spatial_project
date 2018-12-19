### Try to replicate paper exactly

library(eurostat)
library(dplyr)
library(rgdal)
library(spdep)

### Data collection


source("./R_skripte/Data_Transformation/data_download ERD.R") #European Regional Database

min<-2

  ###############################DEFINE#########################
  #nonEU<-list("AL","CH","EF","EU","IS","ME","MK","NO","TR","LI")
  drop<-c("NO","MT","CY")
  period<-c(1995:2008)
  overseas <- c("FRA1", "FRA2", "FRA3", "FRA4", "FRZZ", "FRA5", "PT20", "PT30", "PTZZ", "ES70", "ESZZ")
  
  ############################################################# 

source("./R_skripte/Data_Transformation/data_transformation_ERD.R") #ERD

### Linear Regression

# source("./R_skripte/Regressions/linear_regression.R")   #EUROSTAT
source("./R_skripte/Regressions/linear_regression_ERD.R") #ERD

## FIRST: Excluding regions where NUTS 2 = NUTS 3
  
zeros <- which(Y[,1]== 0)
  
Y_ex <- Y[-zeros,]
X_1ex <- X_1[-zeros,]
X_2ex <- X_2[-zeros,]

lmex <- lapply(1:k, function(i) lm(Y_ex[,i] ~ X_1ex[,i] + I(X_1ex[,i]^2) + X_2ex[,2]))
  
  for (i in 1:k) {
    names(lm)[i] <- paste("lm_", period[i], sep = "")
    names(lm[[1]]$coefficients) <- c("(Intercept)", "Beta", "Gamma", "Delta")
  }

sapply(lmex, coef)

## Second: using national average
Y_ex2<-Y

for (i in 1:length(Y[,1])) {
  
  if(Y[i,3] == 0){
    for (j in 1:length(Y[1,])) {
      Y_ex2[i,j] <- mean(Y[substr(rownames(Y),1,2)==substr(rownames(Y)[i],1,2),j])      
    }
  }
}

lmex2 <- lapply(1:k, function(i) lm(Y_ex2[,i] ~ X_1[,i] + I(X_1[,i]^2) + X_2[,2]))

sapply(lmex2, coef)

### Combining Data with Shapefile (2013)
#source("./R_skripte/Data_Transformation/combining_shapefiles_with_data.R")