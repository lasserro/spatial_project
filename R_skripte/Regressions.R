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
W<-as(as_dgRMatrix_listw(W.list), "CsparseMatrix") #drop 0
trMat<-trW(W, type="mult")
trMC<-trW(W, type="MC")
set.seed(12345)
sar.impacts <- lapply(1:k, function(i) impacts(sar[[i]], tr=trMat, R=100))
sar.impacts_summary <- lapply(1:k, function(i) summary(sar.impacts[[i]], zstats=TRUE, short=TRUE))

sar.impacts_output <- list()
imp <- matrix(nrow = 3, ncol = 6)
dimnames(imp) <- list(c("X_1","I((X_1)^2)","X_2"),c("Direct","Dir p-value","Indirect","Indir p-value","Total","Tot p-value"))
for (i in 1:k) {
  sar.impacts_output[[i]] <- imp
}
names(sar.impacts_output) <- period
for (i in 1:k) {
  sar.impacts_output[[i]][1:3,1] <- sar.impacts_summary[[i]]$res$direct
  sar.impacts_output[[i]][1:3,2] <- sar.impacts_summary[[i]]$pzmat[,1]
  sar.impacts_output[[i]][1:3,3] <- sar.impacts_summary[[i]]$res$indirect
  sar.impacts_output[[i]][1:3,4] <- sar.impacts_summary[[i]]$pzmat[,2]
  sar.impacts_output[[i]][1:3,5] <- sar.impacts_summary[[i]]$res$total
  sar.impacts_output[[i]][1:3,6] <- sar.impacts_summary[[i]]$pzmat[,3]
}

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

# Thresholds
sar_coef<- list()
for (i in 1:k) {
  sar_coef[[i]] <- sar[[i]]$coefficients
}
names(sar_coef) <- period

thres_sar <- matrix(nrow = k, ncol = 1)
for (i in 1:k) {
  thres_sar[i,1] <- sar_coef[[i]][2]/(-2*sar_coef[[i]][3])*1e+06
}
rownames(thres_sar) <- period

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

# Thresholds
sem_coef<- list()
for (i in 1:k) {
  sem_coef[[i]] <- sem[[i]]$coefficients
}
names(sem_coef) <- period

thres_sem <- matrix(nrow = k, ncol = 1)
for (i in 1:k) {
  thres_sem[i,1] <- sem_coef[[i]][2]/(-2*sem_coef[[i]][3])*1e+06
}
rownames(thres_sem) <- period

##### 2.3. SDM Model
# estimating the model
sdm <- lapply(1:k, function(i) lagsarlm(f1, data=shp_list[[i]], type="mixed", W.list, tol.solve=1.0e-30))
names(sdm) <- period

# impacts (direct, indirect & total effect)
sdm.impacts <- lapply(1:k, function(i) impacts(sdm[[i]], tr=trMat, R=100))
sdm.impacts_summary <- lapply(1:k, function(i) summary(sdm.impacts[[i]], zstats=TRUE, short=TRUE))
sdm.impacts_output <- list()
for (i in 1:k) {
  sdm.impacts_output[[i]] <- imp
}
names(sdm.impacts_output) <- period
for (i in 1:k) {
  sdm.impacts_output[[i]][1:3,1] <- sdm.impacts_summary[[i]]$res$direct
  sdm.impacts_output[[i]][1:3,2] <- sdm.impacts_summary[[i]]$pzmat[,1]
  sdm.impacts_output[[i]][1:3,3] <- sdm.impacts_summary[[i]]$res$indirect
  sdm.impacts_output[[i]][1:3,4] <- sdm.impacts_summary[[i]]$pzmat[,2]
  sdm.impacts_output[[i]][1:3,5] <- sdm.impacts_summary[[i]]$res$total
  sdm.impacts_output[[i]][1:3,6] <- sdm.impacts_summary[[i]]$pzmat[,3]
}


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

# Thresholds
sdm_coef<- list()
for (i in 1:k) {
  sdm_coef[[i]] <- sdm[[i]]$coefficients
}
names(sdm_coef) <- period

thres_sdm <- matrix(nrow = k, ncol = 1)
for (i in 1:k) {
  thres_sdm[i,1] <- sdm_coef[[i]][2]/(-2*sdm_coef[[i]][3])*1e+06
}
rownames(thres_sdm) <- period

# Comparison of models (Akaike Information Criterion)
AIC <- matrix(nrow = k, ncol = 4)
dimnames(AIC) <- list(period,c("CLM","SAR","SEM","SDM"))
for (i in 1:k) {
  AIC[i,1] <- AIC(lm[[i]],sar[[i]],sem[[i]],sdm[[i]])[1,2]
  AIC[i,2] <- AIC(lm[[i]],sar[[i]],sem[[i]],sdm[[i]])[2,2]
  AIC[i,3] <- AIC(lm[[i]],sar[[i]],sem[[i]],sdm[[i]])[3,2]
  AIC[i,4] <- AIC(lm[[i]],sar[[i]],sem[[i]],sdm[[i]])[4,2]
}


# ##### 2.4. Spatial Models with GMM
# #### 2.4.1. SAR Model
# sar_gm <- lapply(1:k, function(i) spreg(f1, data=shp_list[[i]], W.list, model = "lag", het = TRUE))
# names(sar_gm) <- period
# 
# #### 2.4.2. SEM Model
# sem_gm <- lapply(1:k, function(i) spreg(f1, data=shp_list[[i]], W.list, model = "error", het = TRUE))
# names(sem_gm) <- period

###### 2.4. 2SLS - SAR
id <- seq(1, nrow(shp_list[[1]]@data))
d <- distance(coord=coords, region.id = id, output = TRUE, type = "distance", 
              shape.name = "shapefile", region.id.name="id", firstline = TRUE,
              file.name = "Europe.GWT")
coldist <- read.gwt2dist(file = "Europe.GWT",  region.id = id, skip = 1)

sar.2stls.hac <- lapply(1:k, function(i) stslshac(f1, data=shp_list[[i]], listw=W.list, distance = coldist,
                                                  HAC = TRUE, type ="Epanechnikov" ))
names(sar.2stls.hac) <- period


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
