### Regressions

## Classic Linear Model (CLM)

#Define model
f1 <- Y ~ X_1 + I((X_1)^2) + X_2

#do regressions for each year seperately
lm <- lapply(1:k, function(i) lm(f1,data=shp_list[[i]]))

#and rename them conviniently
for (i in 1:k) {
  names(lm)[i] <- paste("lm_", period[i], sep = "")
  names(lm[[i]]$coefficients) <- c("(Intercept)", "Beta", "Gamma", "Delta")
}

#### TESTS ####
# Test for heteroskedasticity - Breusch-Pagan-Test
library(AER)
bp_lm <- matrix(nrow = k, ncol = 2)
dimnames(bp_lm) <- list(period, c("Breusch-Pagan", "p-value"))
for (i in 1:k) {
  bp_lm[i,1] <- bptest(lm[[i]])$statistic
  bp_lm[i,2] <- bptest(lm[[i]])$p.value
}

# Test for normality - Jarque-Bera Test
library(tseries)
jb_lm <- matrix(nrow = k, ncol = 2)
dimnames(jb_lm) <- list(period, c("Jarque-Bera", "p-value"))
for (i in 1:k) {
  jb_lm[i,1] <- jarque.bera.test(lm[[i]]$residuals)$statistic
  jb_lm[i,2] <- jarque.bera.test(lm[[i]]$residuals)$p.value
}

## Test for Spatial Dependence
# LM-Tests
lm_tests <- matrix(nrow = k, ncol = 8)
dimnames(lm_tests) <- list(period, c("LMerr","p-value","LMlag","p-value","RLMerr","p-value","RLMlag","p-value"))
for (i in 1:k) {
  lm_tests[i,1] <- lm.LMtests(lm[[i]],listw = W.list,test="LMerr")$LMerr$statistic
  lm_tests[i,2] <- lm.LMtests(lm[[i]],listw = W.list,test="LMerr")$LMerr$p.value
  lm_tests[i,3] <- lm.LMtests(lm[[i]],listw = W.list,test="LMlag")$LMlag$statistic
  lm_tests[i,4] <- lm.LMtests(lm[[i]],listw = W.list,test="LMlag")$LMlag$p.value
  lm_tests[i,5] <- lm.LMtests(lm[[i]],listw = W.list,test="RLMerr")$RLMerr$statistic
  lm_tests[i,6] <- lm.LMtests(lm[[i]],listw = W.list,test="RLMerr")$RLMerr$p.value
  lm_tests[i,7] <- lm.LMtests(lm[[i]],listw = W.list,test="RLMlag")$RLMlag$statistic
  lm_tests[i,8] <- lm.LMtests(lm[[i]],listw = W.list,test="RLMlag")$RLMlag$p.value
}

## 3.1 How to access stuff:

# extract just coefficients
# sapply(lm, coef)

# if you need more info, get full summary call. now you can get whatever, like:
# summaries <- lapply(lm, summary)

# ...coefficents with p values:
# lapply(summaries, function(x) x$coefficients[, c(1,4)])

# ...or r-squared values
# sapply(summaries, function(x) c(r_sq = x$r.squared, 
# adj_r_sq = x$adj.r.squared))

## Spatial Regressions

##### SAR Model #####

  sar <- lapply(1:k, function(i) lagsarlm(f1, data=shp_list[[i]], W.list, tol.solve=1.0e-30))
  names(sar) <- period

##### SEM Model #####

  sem <-lapply(1:k, function(i) errorsarlm(f1, data=shp_list[[i]], W.list, tol.solve=1.0e-30))
  names(sem) <- period

##### SDM Model #####
  
sdm <- lapply(1:k, function(i) lagsarlm(f1, data=shp_list[[i]], type="mixed", W.list, tol.solve=1.0e-30))
names(sdm) <- period

#direct indirect and total effects

sar.impacts <- lapply(1:k, function(i) impacts(sar[[i]], listw = W.list))
sdm.impacts <- lapply(1:k, function(i) impacts(sdm[[i]], listw = W.list))
#sar.impacts <- lapply(1:k, function(i) impacts(sar[[i]], listw = W.list))
# machen impacts in einem error model sinn?


#SPLM

#extract data: 
data.long <- shp_list[[1]]@data
for (i in 2:k) {
  data.long <- rbind(data.long,shp_list[[i]]@data)
}
#add time column
data.long$time <- sort(rep(period,n_2))
data.long <- as.data.frame(data.long)
data.long <- data.long[,c(1,11,2:10)]

#The regression (i set it to pooling, funkt aber keine ahnung was es macht)
spml(f1, data = data.long, listw = W.list,
     model="pooling")
