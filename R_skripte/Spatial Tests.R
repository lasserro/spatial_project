
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
  f<- moran.test(shp_list[[i]]$Y, listw = W.list.k, alternative = "greater", randomisation = FALSE)
  moran_Y[i,1] <- f$estimate[1]
  moran_Y[i,2] <- f$p.value
  f<- moran.mc(shp_list[[i]]$Y, listw = W.list.k, alternative = "greater", nsim = 100)
  moran_Y[i,3] <- f$statistic
  moran_Y[i,4] <- f$p.value
}
  
### various Test-statistics on residuals
moran_res <- matrix(ncol = 6, nrow = k)
dimnames(moran_res) <- list(period,c("Moran's I", "Moran-p-value","Moran.mc","Moran.mc_p.value","Geary C","Geary_p.value"))

for (i in 1:k) {
  f <- moran.test(lm[[i]]$residuals, listw = W.list.k, alternative = "greater", randomisation = FALSE)
  moran_res[i,1] <- f$estimate[1]
  moran_res[i,2] <- f$p.value
  f <- moran.mc(lm[[i]]$residuals, listw = W.list.k, alternative = "greater", nsim = 100)
  moran_res[i,3] <- f$statistic
  moran_res[i,4] <- f$p.value
  f <- geary.test(lm[[i]]$residuals, listw = W.list.k, alternative = "greater")
  moran_res[i,5] <- f$estimate[1]
  moran_res[i,6] <- f$p.value
}

## and LISA...
lisa.test <- list()
#lisa.test <- matrix(ncol = 6, nrow = n_2)
#dimnames(lisa.test) <- list(period,c("LISA","Lisa_p.value"))
for (i in 1:k) {
  lisa.test[[i]] <- localmoran(lm[[k]]$residuals, listw = W.list.k, alternative = "greater")
}

## and attempt to plot lisa

lisa<-lisa.test[[1]]
  
library(GISTools)
sids79.shading <- auto.shading(c(lisa[,1], -lisa[,1]),
                               cols=brewer.pal(5, "PRGn"))
choropleth(shp_list[[1]], lisa[,1], shading=sids79.shading, main="Disposable income in EU NUTS 2")
#locator(1)
choro.legend(-1423654, 11198209, sh=sids79.shading, cex=0.6, title="Local Moran's I")


