
############## Download ########################################################

### GDP

suppressWarnings(                                 
  try(load("./RData/GDP_Nuts3"),silent = TRUE))   ##try suppresses errors

if(!exists("GDP_Nuts3")){
  
GDP_Nuts3 <- get_eurostat('nama_10r_3gdp', time_format = "raw",
                          stringsAsFactors = FALSE)

save(GDP_Nuts3,file = "./RData/GDP_Nuts3")
}


### Population

suppressWarnings(
  try(load("./RData/Pop_Nuts3"),silent = TRUE))

if(!exists("Pop_Nuts3")){
  
Pop_Nuts3 <- get_eurostat('demo_r_pjanaggr3', time_format = "raw",
                          stringsAsFactors = FALSE)
  
save(Pop_Nuts3,file = "./RData/Pop_Nuts3")
}


### Shapefiles

suppressWarnings(                                 
  try(load("./RData/Shapefiles/shp2"),silent = TRUE))
suppressWarnings(                                 
  try(load("./RData/Shapefiles/shp3"),silent = TRUE))

if(!exists("shp2")){
  
temp <- tempfile(fileext = ".zip")
    
  download.file("http://ec.europa.eu/eurostat/cache/GISCO/distribution/v2/nuts/download/ref-nuts-2013-03m.shp.zip",
  temp)

outDir<-"./RData/Shapefiles"

#command from script doesn't work. alternative:
unzip(temp, exdir=outDir)
zipF <- list.files(path = "./RData/Shapefiles", pattern = "*.zip", full.names = TRUE)
suppressMessages(sapply(zipF, function(x) unzip(x, exdir = outDir)))

shp2 <- readOGR(dsn = "./RData/Shapefiles", layer ="NUTS_RG_03M_2013_4326_LEVL_2") 
shp3 <- readOGR(dsn = "./RData/Shapefiles", layer ="NUTS_RG_03M_2013_4326_LEVL_3") 

save(shp2,file = "./RData/Shapefiles/shp2") 
save(shp3,file = "./RData/Shapefiles/shp3") 

rm(outDir,temp,zipF)
}



