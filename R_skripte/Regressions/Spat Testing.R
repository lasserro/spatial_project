########## Spatial Panel ##########
## manipulate data for spatial regression/testing
library(plm)
library(reshape2)
library(splm)
X_1.long <- melt(X_1)
colnames(X_1.long) <- c("nuts_2", "time", "x_1")

df <- matrix(NA, n_2, k)
df[,seq(1:k)] <- X_2[,2]
colnames(df) <- period
rownames(df) <- pop2$nuts_2

X_2.long <- melt(df)
colnames(X_2.long) <- c("nuts_2", "time", "x_2")

Y.long <- melt(Y)
colnames(Y.long) <- c("nuts_2", "time", "y")

data.long <- cbind(Y.long, X_1.long[,3], X_2.long[,3])
colnames(data.long) <- c("nuts_2", "time", "y", "x_1", "x_2")

## Weight Matrix
coords <- coordinates(shp2)

k.near <- knearneigh(coords, k=5) #indexing neighbors based on k=5
k5 <- knn2nb(k.near) #creating neighborhood list based on the k(5) nearest neighbors
W.list.k <- nb2listw(k5, style = "W", zero.policy = FALSE) #creating a weights-list
W.mat <- listw2mat(W.list.k) #creates a weigths matrix

## model formula
f1 <- y ~ x_1 + I((x_1)^2) + x_2

## Lag Structure
local.rob.LM <- matrix(ncol =4, nrow = 2)
tests <- c("lml", "lme", "rlml", "rlme")
dimnames(local.rob.LM) <- list(c("LM test", "p-value"), tests)

#ad slmtest:This tests are panel versions of the locally robust LM tests of Anselin et al. (1996), based on a pooling assumption: i.e., they do not allow for any kind of individual effect. Therefore it is advisable to employ a within transformation whenever individual effects cannot be ruled out. 
#Passt das für uns? was sind indivicual effects?

for (i in tests) {
  local.rob.LM[1, i] <- slmtest(f1, data = data.long, listw = W.list.k, test = i)$statistic
  local.rob.LM[2, i] <- slmtest(f1, data = data.long, listw = W.list.k, test = i)$p.value
}
round(local.rob.LM, digits = 4)
# sign. values for rlml and rlme mean that we are likely dealing with spatial dependence in the lag and in the error term, as well

#### Let's see if there's spatial dependence in the error terms
tests <- matrix(ncol = 6, nrow = k)
dimnames(tests) <- list(period,c("Moran's I", "Moran-p-value","Geary C","Geary-p-value","LISA","Lisa-p-value"))

for (i in 1:k) {
  tests[i,1] <- moran.mc(lm[[i]]$residuals,W.list.k,999)$statistic
  tests[i,2] <- moran.mc(lm[[i]]$residuals,W.list.k,999)$p.value
  tests[i,3] <- geary.test(lm[[i]]$residuals, listw = W.list.k, alternative = "greater")$statistic
  tests[i,4] <- geary.test(lm[[i]]$residuals, listw = W.list.k, alternative = "greater")$p.value
 # tests[i,5] <- localmoran(lm[[i]]$residuals, listw = W.list.k, alternative = "greater")$statistic
  #tests[i,6] <- localmoran(lm[[i]]$residuals, listw = W.list.k, alternative = "greater")$p.value  
}

## Moransi

moransi <- list()
  
for (i in 1:length(period)) {
   f<- moran.test(data.long[data.long$time==period[i],3], listw = W.list.k, alternative = "greater", randomisation = FALSE)
   moransi[[i]] <-f$estimate
   f<-moran.mc(data.long[data.long$time==period[i],3], listw = W.list.k, alternative = "greater", nsim = 100)
      moransi[[i+length(period)]] <-summary(f)
}

# ## Hausmann-Test: RE or FE?
# # error and lags
# spherrlag <- sphtest(x=f1, data=data.long, listw=W.list.k, 
#                      spatial.model="sarar", method="ML")
# spherrlag
# # FUNZT NICHT!!!!


## Using combined testing since we are dealing with spatial errors
LM.bsk <- matrix(ncol =4, nrow = 2)
tests.bsk <- c("LM1", "LM2", "CLMlambda", "CLMmu")
dimnames(LM.bsk) <- list(c("LM test", "p-value"), tests.bsk)

for (i in tests.bsk) {
  LM.bsk[1, i] <- bsktest(f1, data = data.long, listw = W.list.k, test = i)$statistic
  LM.bsk[2, i] <- bsktest(f1, data = data.long, listw = W.list.k, test = i)$p.value
}
round(LM.bsk, digits = 4)

# "LM1"       H0 :  σ²=0 assuming λ=0 (Are there random effects,  assuming no spatial autocorr.?)
# "LM2"       H0 :  λ=0 assuming σ²=0 (Is there spatial corr., assuming no random effects?)
# "CLMlambda" H0 :  λ=0 (Is there spatial correlation given the possibility of RE?)
# "CLMmu"     H0 :  σ²=0 (Are there RE, given the possibility of spatial correlation?)


## Is there serial correlation on top? (C.2 funktioniert ebenfalls NICHT....gleicher Fehler wie bei Spat.Regression mit random effects)
LM.bsjk <- matrix(ncol = 3, nrow = 2)
tests.bsjk <- c("J", "C.1", "C.3")
dimnames(LM.bsjk) <- list(c("LM test", "p-value"), tests.bsjk)

for (i in tests.bsjk) {
  LM.bsjk[1, i] <- bsjktest(f1, data = data.long, listw = W.list.k, test = i)$statistic
  LM.bsjk[2, i] <- bsjktest(f1, data = data.long, listw = W.list.k, test = i)$p.value
}
round(LM.bsjk, digits = 4)

# "J"         H0 : λ=σ²=ρ=0 (RE, spatial or serial correlation non-zero?)
# "C.1"       H0 : λ=0 assumig ρ≠0 (serial correlation) and σ²>0 (RE)
# "C.2"       H0 : ρ=0 assuming λ≠0 (spatial correlation) and σ²>0
# "C.3"       H0 : σ²=0 assuming λ≠0 and ρ≠0


