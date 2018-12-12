
library(dplyr)

library(rgdal)

#setwd("C:/Users/loren/Dropbox/Spatial/SpatialProjekt")

dir.create("data")



temp <- tempfile(fileext = ".zip")
# download only when necessary, not every time you run the script because of the big
# file size

#download.file("http://ec.europa.eu/eurostat/cache/GISCO/distribution/v2/nuts/download/ref-nuts-2013-03m.shp.zip",temp)

outDir<-"./data"
# now unzip the boundary data
#command from script doesnt work. alternative:
zipF <- list.files(path = "./data", pattern = "*.zip", full.names = TRUE)
sapply(zipF, function(x) unzip(x, exdir = outDir))
file.remove(zipF)

#read shapefiles
# let us choose projection WGS 84 (EPSG 4326) which is visible the file name 
# between the year (2013) and the level of the data (NUTS 2):
shp2 <- readOGR(dsn = "./data", layer ="NUTS_RG_03M_2013_4326_LEVL_2") 
shp3 <- readOGR(dsn = "./data", layer ="NUTS_RG_03M_2013_4326_LEVL_3") 

#plot(shp2)
#plot(shp3)

#overseas <- c("FRA1", "FRA2", "FRA3", "FRA4", "FRZZ", "FRA5", "PT20", "PT30", "PTZZ", "ES70", "ESZZ")
# we can also exclude all oversea territories
#shp <- shp[! shp$NUTS_ID %in% overseas, ]


shp2x <- merge(shp2, nuts_2, all.x = FALSE, all.y = TRUE, by.x = "NUTS_ID", by.y = "geo_2")


nuts_2x <-nuts_2 %>% group_by(time) %>% merge(., shp2, all.y = FALSE, all.x = TRUE, by.y = "NUTS_ID", by.x = "geo_2")




nuts_2 <- nuts_2 %>% filter(time==2010)

shp2x <- shp2[ shp$CNTR_CODE %in% com, ]

nonEU<-list("AL","CH","EF","EU","IS","ME","MK","NO","TR","LI")
drop<-c(nonEU,"DK","DE","FR","PL")


unique(shp2$CNTR_CODE)
unique(nuts_2$country)

com <-intersect(unique(shp2$CNTR_CODE),
          unique(nuts_2$country))


overseas <- c("FRA1", "FRA2", "FRA3", "FRA4", "FRZZ", "FRA5", "PT20", "PT30", "PTZZ", "ES70", "ESZZ")

# we can also exclude all oversea territories
shp <- shp[! shp$NUTS_ID %in% overseas, ]






