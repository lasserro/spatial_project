###################################################################################
################################### REGRESSIONS ###################################
###################################################################################

########################## 1. Classic Linear Model (CLM) ##########################
##### 1.1. Estimation of CLM
# define model
f1 <- Y ~ X_1 + I((X_1)^2) + X_2

# do regressions for each year seperately
lm <- lapply(1:k, function(i) lm(f1,data=shp_list[[i]]))

# and rename them conviniently
for (i in 1:k) {
  names(lm)[i] <- paste("lm_", period[i], sep = "")
  names(lm[[i]]$coefficients) <- c("(Intercept)", "Beta", "Gamma", "Delta")
}

# Test for heteroskedasticity - Breusch-Pagan-Test
library(AER)
bp_lm <- matrix(nrow = k, ncol = 2)
dimnames(bp_lm) <- list(period, c("Breusch-Pagan", "p-value"))
for (i in 1:k) {
  bp_lm[i,1] <- bptest(lm[[i]])$statistic
  bp_lm[i,2] <- bptest(lm[[i]])$p.value
}
# (highly) significant values for each year meaning we are dealing with heteroscedasticity

# Test for normality - Jarque-Bera Test
library(tseries)
jb_lm <- matrix(nrow = k, ncol = 2)
dimnames(jb_lm) <- list(period, c("Jarque-Bera", "p-value"))
for (i in 1:k) {
  jb_lm[i,1] <- jarque.bera.test(lm[[i]]$residuals)$statistic
  jb_lm[i,2] <- jarque.bera.test(lm[[i]]$residuals)$p.value
}
# since we have significant values for each year, the normality assumption won't hold anymore --> we have to use
# other estimation methods (GMM), since OLS and ML wouldn't be efficient anymore

## Tests for Spatial Dependence
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
# interpretation of LM Tests !!

# Other Spatial Tests


################################ 2. Spatial Models ################################
##### 2.1. SAR Model
# estimating the model
sar <- lapply(1:k, function(i) lagsarlm(f1, data=shp_list[[i]], W.list, tol.solve=1.0e-30))
names(sar) <- period

# impacts (direct, indirect & total effect)
sar.impacts <- lapply(1:k, function(i) impacts(sar[[i]], listw = W.list))

# Test for heteroskedasticity - Breusch-Pagan-Test
bp_sar <- matrix(nrow = k, ncol = 2)
dimnames(bp_sar) <- list(period, c("Breusch-Pagan", "p-value"))
for (i in 1:k) {
  bp_sar[i,1] <- bptest.sarlm(sar[[i]])$statistic
  bp_sar[i,2] <- bptest.sarlm(sar[[i]])$p.value
}
# still (highly) significant values for each year meaning we are dealing with heteroscedasticity

# Test for normality - Jarque-Bera Test
jb_sar <- matrix(nrow = k, ncol = 2)
dimnames(jb_sar) <- list(period, c("Jarque-Bera", "p-value"))
for (i in 1:k) {
  jb_sar[i,1] <- jarque.bera.test(sar[[i]]$residuals)$statistic
  jb_sar[i,2] <- jarque.bera.test(sar[[i]]$residuals)$p.value
}
# pretty much the same as in the CLM case


##### 2.2. SEM Model
# estimating the model
sem <-lapply(1:k, function(i) errorsarlm(f1, data=shp_list[[i]], W.list, tol.solve=1.0e-30))
names(sem) <- period

# Test for heteroskedasticity - Breusch-Pagan-Test
bp_sem <- matrix(nrow = k, ncol = 2)
dimnames(bp_sem) <- list(period, c("Breusch-Pagan", "p-value"))
for (i in 1:k) {
  bp_sem[i,1] <- bptest.sarlm(sem[[i]])$statistic
  bp_sem[i,2] <- bptest.sarlm(sem[[i]])$p.value
}
# also significant values for each year

# Test for normality - Jarque-Bera Test
jb_sem <- matrix(nrow = k, ncol = 2)
dimnames(jb_sem) <- list(period, c("Jarque-Bera", "p-value"))
for (i in 1:k) {
  jb_sem[i,1] <- jarque.bera.test(sem[[i]]$residuals)$statistic
  jb_sem[i,2] <- jarque.bera.test(sem[[i]]$residuals)$p.value
}
# pretty much the same as in the CLM case


##### 2.3. SDM Model
# estimating the model
sdm <- lapply(1:k, function(i) lagsarlm(f1, data=shp_list[[i]], type="mixed", W.list, tol.solve=1.0e-30))
names(sdm) <- period

# impacts (direct, indirect & total effect)
sdm.impacts <- lapply(1:k, function(i) impacts(sdm[[i]], listw = W.list))

# Test for heteroskedasticity - Breusch-Pagan-Test
bp_sdm <- matrix(nrow = k, ncol = 2)
dimnames(bp_sdm) <- list(period, c("Breusch-Pagan", "p-value"))
for (i in 1:k) {
  bp_sdm[i,1] <- bptest.sarlm(sdm[[i]])$statistic
  bp_sdm[i,2] <- bptest.sarlm(sdm[[i]])$p.value
}
# also (highly) significant values for each year

# Test for normality - Jarque-Bera Test
jb_sdm <- matrix(nrow = k, ncol = 2)
dimnames(jb_sdm) <- list(period, c("Jarque-Bera", "p-value"))
for (i in 1:k) {
  jb_sdm[i,1] <- jarque.bera.test(sdm[[i]]$residuals)$statistic
  jb_sdm[i,2] <- jarque.bera.test(sdm[[i]]$residuals)$p.value
}
# pretty much the same as in the CLM case


##### 2.5. Spatial Panel Models

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

