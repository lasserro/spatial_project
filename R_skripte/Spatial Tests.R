
###Start with CLM:

#Define model
f1 <- Y ~ X_1 + I((X_1)^2) + X_2

#do regressions for each year seperately
lm <- lapply(1:k, function(i) lm(f1,data=shp_list[[i]]))

#and rename them conviniently
for (i in 1:k) {
  names(lm)[i] <- paste("lm_", period[i], sep = "")
  names(lm[[1]]$coefficients) <- c("(Intercept)", "Beta", "Gamma", "Delta")
}


### Compute Morans I of dependent variable
moran_Y <- matrix(ncol = 4, nrow = k)
dimnames(moran_Y) <- list(period,c("Moran's I", "Moran_p.value","Moran.mc","Moran.mc_p.value"))
for (i in 1:k) {
  f<- moran.test(shp_list[[i]]$Y, listw = W.list, alternative = "greater", randomisation = FALSE)
  moran_Y[i,1] <- f$estimate[1]
  moran_Y[i,2] <- f$p.value
  f<- moran.mc(shp_list[[i]]$Y, listw = W.list, alternative = "greater", nsim = 100)
  moran_Y[i,3] <- f$statistic
  moran_Y[i,4] <- f$p.value
}
  
### various Test-statistics on residuals
moran_res <- matrix(ncol = 6, nrow = k)
dimnames(moran_res) <- list(period,c("Moran's I", "Moran-p-value","Moran.mc","Moran.mc_p.value","Geary C","Geary_p.value"))

for (i in 1:k) {
  f <- moran.test(lm[[i]]$residuals, listw = W.list, alternative = "greater", randomisation = FALSE)
  moran_res[i,1] <- f$estimate[1]
  moran_res[i,2] <- f$p.value
  f <- moran.mc(lm[[i]]$residuals, listw = W.list, alternative = "greater", nsim = 100)
  moran_res[i,3] <- f$statistic
  moran_res[i,4] <- f$p.value
  f <- geary.test(lm[[i]]$residuals, listw = W.list, alternative = "greater")
  moran_res[i,5] <- f$estimate[1]
  moran_res[i,6] <- f$p.value
}

## and LISA...
lisa.test <- list()
#lisa.test <- matrix(ncol = 6, nrow = n_2)
#dimnames(lisa.test) <- list(period,c("LISA","Lisa_p.value"))
for (i in 1:k) {
  lisa.test[[i]] <- localmoran(lm[[k]]$residuals, listw = W.list, alternative = "greater")
}


### LM-tests
  
    
  ## Lag Structure
  # local.rob.LM <- matrix(ncol =4, nrow = 2)
  # tests <- c("lml", "lme", "rlml", "rlme")
  # dimnames(local.rob.LM) <- list(c("LM test", "p-value"), tests)
  # 
  # #ad slmtest:This tests are panel versions of the locally robust LM tests of Anselin et al. (1996), based on a pooling assumption: i.e., they do not allow for any kind of individual effect. Therefore it is advisable to employ a within transformation whenever individual effects cannot be ruled out. 
  # #Passt das für uns? was sind indivicual effects?
  # 
  # for (i in tests) {
  #   local.rob.LM[1, i] <- slmtest(f1, data = data.long, listw = W.list, test = i)$statistic
  #   local.rob.LM[2, i] <- slmtest(f1, data = data.long, listw = W.list, test = i)$p.value
  # }
  # round(local.rob.LM, digits = 4)
  # sign. values for rlml and rlme mean that we are likely dealing with spatial dependence in the lag and in the error term, as well
