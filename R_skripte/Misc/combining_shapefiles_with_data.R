
library(dplyr)
library(spdep)
library(rgdal)

################ this part is already covered in data_download.R ###############

#dir.create("data")
#temp <- tempfile(fileext = ".zip")
# download only when necessary, not every time you run the script because of the big
# file size

#download.file("http://ec.europa.eu/eurostat/cache/GISCO/distribution/v2/nuts/download/ref-nuts-2013-03m.shp.zip",temp)

#outDir<-"./Rdata/Shapefiles"
# now unzip the boundary data
#command from script doesnt work. alternative:
#zipF <- list.files(path = "./Rdata/Shapefiles", pattern = "*.zip", full.names = TRUE)
#sapply(zipF, function(x) unzip(x, exdir = outDir))
#file.remove(zipF)


################# begin here: Shapefiles #####################################

#read shapefiles
# let us choose projection WGS 84 (EPSG 4326) which is visible the file name 
# between the year (2013) and the level of the data (NUTS 2):
# shp2 <- readOGR(dsn = "./RData/Shapefiles", layer ="NUTS_RG_03M_2013_4326_LEVL_2") 
# shp3 <- readOGR(dsn = "./RData/Shapefiles", layer ="NUTS_RG_03M_2013_4326_LEVL_3") 

#plot(shp2)
#plot(shp3)

# overseas <- c("FRA1", "FRA2", "FRA3", "FRA4", "FRZZ", "FRA5", "PT20", "PT30", "PTZZ", "ES70", "ESZZ")
# # we can also exclude all oversea territories
shp2 <- shp2[! shp2$NUTS_ID %in% overseas, ]
shp3 <- shp3[! shp3$NUTS_ID %in% overseas, ]

# ich glaube wir brauchen im Endeffekt für die Analyse dann eh nur die shp2.
# die einzige info bzgl nuts_3 regionen, die in die regressionen eingehen ist,
# wie viele nuts3 in nuts2 regionen sind. der rest ist im CV abgebildet. aber
# vorerst egal

#_______________________________________________________________________________
###### zum shp verständnis: ###################### 

View(head(shp2@data))
table(shp2$NUTS_ID %in% pop2$nuts_2) # shp file has 44 more nuts2 regions
setdiff(shp2$NUTS_ID, pop2$nuts_2) # this is the data which does not overlap in 
# the shape file and the data shp2 auf unser datenset reduzieren
shp2 <- shp2[shp2$NUTS_ID %in% pop2$nuts_2, ]
# test
setdiff(shp2$NUTS_ID, pop2$nuts_2) # alright
 


######____________ first only for one year (2013): ____________________________

# merge data to shp:
shp13 <- merge(shp2, pop2[,c('nuts_2','2013')], all.x= F, all.y= T, by.x= 'NUTS_ID', by.y='nuts_2')
colnames(shp13@data)[6] <- 'Pop_2013'
shp13 <- merge(shp13, gdp2[,c('nuts_2','2013')], all.x= F, all.y= T, by.x= 'NUTS_ID', by.y='nuts_2')
colnames(shp13@data)[7] <- 'Gdp_2013'
shp13 <- merge(shp13, Y13, all.x=F, all.y=T, by.x='NUTS_ID', by.y='nuts_2')
colnames(shp13@data)[8] <- 'CV_2013'
coords <- coordinates(shp13)

#----- k-nearest Matrix ------

k.near <- knearneigh(coords, k=5)
#indexing neighbors based on k=5
k5 <- knn2nb(k.near)
#creating neighborhood list based on the k(5) nearest neighbors
W.list.k <- nb2listw(k5, style = 'W', zero.policy = F)
#creating a weights-list

plot(W.list.k, coords, add=T) # this shows us the problem of excluding so many countries. 

#_________________________erster plot versuch __________________________________

library(latticeExtra)
grps <- 10
brks <- quantile(shp13$'Gdp_2013', 0:(grps-1)/(grps-1), na.rm=TRUE)
p <- spplot(shp13, "Gdp_2013", at=brks, col.regions=rev(brewer.pal(grps, "RdBu")), col="transparent")
p + layer(sp.polygons(shp13))

#_______________________________________________________________________________
grps2 <- 10
brks2 <- quantile(shp13$'CV_2013', 0:(grps2-1)/(grps2-1), na.rm=TRUE)
p2 <- spplot(shp13, "CV_2013", at=brks2, col.regions=rev(brewer.pal(grps2, "RdBu")), col="transparent")
p2 + layer(sp.polygons(shp13))

#_______________________________________________________________________________
#################### CREATE LIST OF SHAPEFILES FOR ALL YEARS ###################
shp_list <- vector('list', 20)
Y0 <- as.data.frame(Y)
Y0$'nuts_2' <- rownames(Y0)
X_1df <- as.data.frame(X_1)
X_1df$'nuts_2' <- rownames(X_1df)
X_2df <- as.data.frame(X_2)
X_2df$'nuts_2' <- rownames(X_2df)


for (i in 1:20) {
  shp_list[i] <- merge(shp2, pop2[, c(4, I(i + 4))], all.x= F, all.y= T, by.x= 'NUTS_ID', by.y='nuts_2')
  colnames(shp_list[[i]]@data)[6] <- 'Pop'
  shp_list[[i]] <- merge(shp_list[[i]], gdp2[,c(4, I(i + 4))], all.x= F, all.y= T, by.x= 'NUTS_ID', by.y='nuts_2')
  colnames(shp_list[[i]]@data)[7] <- 'Gdp'
  shp_list[[i]] <- merge(shp_list[[i]], Y0[,c(i, 21)], all.x= F, all.y= T, by.x= 'NUTS_ID', by.y='nuts_2')
  colnames(shp_list[[i]]@data)[8] <- 'Y'
  shp_list[[i]] <- merge(shp_list[[i]], X_1df[,c(i, 21)], all.x= F, all.y= T, by.x= 'NUTS_ID', by.y='nuts_2')
  colnames(shp_list[[i]]@data)[9] <- 'X_1'
  shp_list[[i]] <- merge(shp_list[[i]], X_2df, all.x= F, all.y= T, by.x= 'NUTS_ID', by.y='nuts_2')
  colnames(shp_list[[i]]@data)[10] <- 'X_2'
   }
rm(Y0, X_1df, X_2df)
View(shp_list[[3]]) # for 1998

#########
shp_listx <- vector('list', 20)
Y0 <- as.data.frame(Y)
Y0$'nuts_2' <- rownames(Y0)
X_1df <- as.data.frame(X_1)
X_1df$'nuts_2' <- rownames(X_1df)
X_2df <- as.data.frame(X_2)
X_2df$'nuts_2' <- rownames(X_2df)


for (i in 1:20) {
  shp_listx[i] <- merge(shp2, pop2[, c(4, i + 4)], all.x= T, all.y= F, by.x= 'NUTS_ID', by.y='nuts_2')
  colnames(shp_listx[[i]]@data)[6] <- 'Pop'
  shp_listx[[i]] <- merge(shp_listx[[i]], gdp2[,c(4, i + 4)], all.x= T, all.y= F, by.x= 'NUTS_ID', by.y='nuts_2')
  colnames(shp_listx[[i]]@data)[7] <- 'Gdp'
  shp_listx[[i]] <- merge(shp_listx[[i]], Y0[,c(i, 21)], all.x= T, all.y= F, by.x= 'NUTS_ID', by.y='nuts_2')
  colnames(shp_listx[[i]]@data)[8] <- 'Y'
  shp_listx[[i]] <- merge(shp_listx[[i]], X_1df[,c(i, 21)], all.x= T, all.y= F, by.x= 'NUTS_ID', by.y='nuts_2')
  colnames(shp_listx[[i]]@data)[9] <- 'X_1'
  shp_listx[[i]] <- merge(shp_listx[[i]], X_2df, all.x= T, all.y= F, by.x= 'NUTS_ID', by.y='nuts_2')
  colnames(shp_listx[[i]]@data)[10] <- 'X_2'
}
rm(Y0, X_1df, X_2df)
View(shp_listx[[3]]) # for 1998

coords1 <- coordinates(shp_list[[1]])


#shp2x <- merge(shp2, nuts_2, all.x = FALSE, all.y = TRUE, by.x = "NUTS_ID", by.y = "geo_2")
#nuts_2x <-nuts_2 %>% group_by(time) %>% merge(., shp2, all.y = FALSE, all.x = TRUE, by.y = "NUTS_ID", by.x = "geo_2")




#nuts_2 <- nuts_2 %>% filter(time==2010)

#shp2x <- shp2[ shp$CNTR_CODE %in% com, ]

#nonEU<-list("AL","CH","EF","EU","IS","ME","MK","NO","TR","LI")
#drop<-c(nonEU,"DK","DE","FR","PL")


#unique(shp2$CNTR_CODE)
#unique(nuts_2$country)

#com <-intersect(unique(shp2$CNTR_CODE),
#          unique(nuts_2$country))


#overseas <- c("FRA1", "FRA2", "FRA3", "FRA4", "FRZZ", "FRA5", "PT20", "PT30", "PTZZ", "ES70", "ESZZ")

# we can also exclude all oversea territories
#shp <- shp[! shp$NUTS_ID %in% overseas, ]
