### Spatial lag model
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

f1 <- y ~ x_1 + I((x_1)^2) + x_2
spml(f1, data = data.long, listw = W.list.k,
     model="random")




Y <- as.data.frame(Y)           

Y <- Y %>% mutate(nuts = row.names(Y))

shp2 <- shp2[shp2$NUTS_ID %in% Y$nuts, ]

shp <- merge(shp2, Y, all.x = FALSE, all.y = TRUE, by.x = "NUTS_ID", by.y = "nuts")


k1 <- knearneigh(coords, k=1) #indexing everyones closest neighbor
k1 <- knn2nb(k1) #transforming it into a neighborhood list
link.max <- max(unlist(nbdists(k1, coords=coords))) #choose maximal distance
link.max #printing max distance between neighbors



lm <- lapply(1:k, function(i) lm(Y[,i] ~ X_1[,i] + I(X_1[,i]^2) + X_2[,2]))

for (i in 1:k) {
  names(lm)[i] <- paste("lm_", period[i], sep = "")
  names(lm[[1]]$coefficients) <- c("(Intercept)", "Beta", "Gamma", "Delta")
}


Y <- as.data.frame(Y)           

Y <- Y %>% mutate(nuts = row.names(Y))


###############################################

y <- Y$"1996"

X12 <- X_1[,2]^2
X1 <- X_1[,2]
X2<-X_2[,2]
X0 <- rep(1,272)

X <- as.matrix(cbind(int=1,X1,X2,X12))

XX <- t(X) %*% X

XXi <- solve(XX)

beta <- solve(XX) %*% t(X) %*% y



#################### F) Testing for spatial autocorrelation ####################
# residuals
res <- y - X %*% beta

## Morans I
# calculating Morans I manually
y.mean <- mean(y)
MI.y.num <- (y - y.mean) %*% W.mat %*% (y - y.mean)
MI.y.den <- (y - y.mean) %*% (y - y.mean)
MI.y <- MI.y.num / MI.y.den
MI.y

# calculating Morans I with formula
moran.mc(shp$"1996", listw = W.list.k, alternative = "greater", nsim = 1000)
