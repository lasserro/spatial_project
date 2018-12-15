########################################################################################
############################## ASSIGNMENT 3 - Group 7 ##################################
########################################################################################
#--------- Valerie Buttler (01105984) ------------ Martin Dambauer (01151851) ---------#
#--------- Robert Lasser (01253423) -------------- Lorenz Reiter (01167961) -----------#
#----------------------------- Bruno Sagmeister (01352105) ----------------------------#
#--------------------------------------------------------------------------------------#

# loading data in environment
library(dplyr)
library(stringr)
setwd("./Assignments/assignment3")
load("./Data/data1.rda")
df <- data1 %>% select(IDb,pr80b,pr103b,lninvb,lndens.empb) %>%
  filter(str_detect(IDb, paste(c("AT", "DE", "IT", "FR", "ES", "PT"), collapse = '|')))

# loading shapefile
library(rgdal)
library(maps)
shp <- readOGR(dsn = "./data", layer ="EU27")
proj4string(shp) <- CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs")
proj4string(shp)


############## A) reduction of shapefile to the NUTS2 regions needed ##############
regions <- df$IDb
shp <- shp[shp$Id %in% regions, ]

######################### B) Distance Based Weight Matrix #########################
library(spdep)
coords <- coordinates(shp)

# calculating 1 nearest neighbor to ensure that everyone has at least 1 neighbor + transforming it into a neighborhood list
k1 <- knn2nb(knearneigh(coords, k = 1))
link.max <- max(unlist(nbdists(k1, coords = coords))) # max distance
link.max

# creating neighborhood list for a distance of at least the one calculated previously
distw <- dnearneigh(coords, 0, link.max, row.names = shp$Id)
W.mat <- nb2mat(distw, style = "W", zero.policy = FALSE)

# plot of the resulting neighbor relationships
W.list <- nb2listw(distw, style = "W", zero.policy = FALSE)
plot(shp)
plot(W.list$neighbours, coords, add=TRUE, col="green", cex=0.5)


####################### C) Calculation of the growth rate #######################
df <- df %>% mutate(growth = (pr103b - pr80b)/pr80b)
shp <- merge(shp, df, all.x = FALSE, all.y = TRUE, by.x = "Id", by.y = "IDb")


######################### D) Plot of productivity growth #########################
library(ggplot2)
library(ggthemes)
library(viridis)
map_eu <- fortify(shp, region = "Id")
df <- df %>% mutate(id = IDb) %>% select(-IDb)
map_eu <- map_eu %>% left_join(df,by = "id")

p <- ggplot() +
  geom_polygon(data=map_eu, aes(fill = growth, x = long, y = lat, group = group)) +
  coord_fixed(1.3)+
  geom_path(data = map_eu, aes(x = long, y = lat, group = group), color = "white", size = 0.1) +
  theme_map() + theme(legend.position = c(0.4,0)) +
  scale_fill_viridis(name="Growth Rate",
                     guide = guide_colorbar(direction="horizontal",
                                            barheight = unit(2, units= "mm"),
                                            barwidth = unit(30, units= "mm"),
                                            draw.ulim = F, title.position="top")) +
  labs(x = NULL, y = NULL, title = "Productivity Growth Rate", subtitle = "NUTS2")
p


##################### E) Solow-style mdodel - OLS estimates #####################
y <- df$growth
X <- data.matrix(df[,c("pr80b","lninvb","lndens.empb")])
X_t <- t(X)
XX <- X_t %*% X
beta <- solve(XX) %*% X_t %*% y


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
moran.mc(shp$growth, listw = W.list, alternative = "greater", nsim = 100)
# the results from Moran's I suggest that there is (most likely) spatial dependence (positive spatial autocorrelation)
# in the dependent variable y --> OLS estimates are biased and inconsistent

######################## G) Choosing appropriate model ########################
# From a theoretical point of view SDM would be the proper choice, since we do not have any biased estimates
# if the true DGP is SAR or SEM. In contrast, if we choose SAR or SEM but the true DGP is SDM, we will suffer
# from an omitted variable bias. SEM would suffer an additional bias as it does not account for the spatial lag in y.

library(latticeExtra)
library(RColorBrewer)
f1 <- growth ~ pr80b + lninvb + lndens.empb
durbin <- lagsarlm(formula = f1, listw = W.list, type="mixed", data=shp, tol.solve=1.0e-30)
summary(durbin, correlation=FALSE)

shp$res.durb <- residuals(durbin)
moran.mc(shp$res.durb, W.list, 999)

grps <- 10
brks <- quantile(shp$res.durb, 0:(grps-1)/(grps-1), na.rm=TRUE)
spplot(shp, "res.durb", at=brks, col.regions=rev(brewer.pal(grps, "RdBu")), col="black")
# residuals seems to be independent --> no spatial autocorrelation in the disturbance

# alternative 
W<-as(as_dgRMatrix_listw(W.list), "CsparseMatrix") #drop 0
trMat<-trW(W, type="mult")
set.seed(12345)
durbin.impacts2<-impacts(durbin, tr=trMat, R=100)
summary(durbin.impacts2, zstats=TRUE, short=TRUE)

# confidence intervals for direct, indirect and total effect
library(coda)
HPDinterval(durbin.impacts2, choice="direct")
HPDinterval(durbin.impacts2, choice ="indirect")
HPDinterval(durbin.impacts2, choice="total")
