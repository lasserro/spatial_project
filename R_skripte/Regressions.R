### Regressions

## Classic Linear Model (CLM)

#Define model
f1 <- Y ~ X_1 + I((X_1)^2) + X_2

#do regressions for each year seperately
lm <- lapply(1:k, function(i) lm(f1,data=shp_list[[i]]))

#and rename them conviniently
for (i in 1:k) {
  names(lm)[i] <- paste("lm_", period[i], sep = "")
  names(lm[[1]]$coefficients) <- c("(Intercept)", "Beta", "Gamma", "Delta")
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

  sar <- lapply(1:k, function(i) lagsarlm(f1, data=shp_list[[i]], W.list.k, tol.solve=1.0e-30))
  names(sar) <- period

##### SEM Model #####

  sem <-lapply(1:k, function(i) errorsarlm(f1, data=shp_list[[i]], W.list.k, tol.solve=1.0e-30))
  names(sem) <- period

##### SDM Model #####
  
sdm <- lapply(1:k, function(i) lagsarlm(f1, data=shp_list[[i]], type="mixed", W.list.k, tol.solve=1.0e-30))
names(sdm) <- period

#direct indirect and total effects

sar.impacts <- lapply(1:k, function(i) impacts(sar[[i]], listw = W.list.k))
sdm.impacts <- lapply(1:k, function(i) impacts(sdm[[i]], listw = W.list.k))
#sar.impacts <- lapply(1:k, function(i) impacts(sar[[i]], listw = W.list.k))
# machen impacts in einem error model sinn?
