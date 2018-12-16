
############## Download ########################################################

dir.create("./RData")


### GDP (Achtung: total, nicht per capita) in 2005 prices

GDP_ERD <-read.table("./RData/NUTS3 GDP ERD fix.txt",
                     header=TRUE,
                     skip = 1,
                     sep = ";",
                     dec = ".",
                     stringsAsFactors = FALSE,
                     check.names=F,
                     colClasses=c("character", "character",
                                  rep("numeric", length(1980:2015)))
)

colnames(GDP_ERD)[1:2] <- c("nuts_level", "nuts_code")
GDP_ERD[GDP_ERD == "nuts 0"]<-0
GDP_ERD[GDP_ERD == "nuts 1"]<-1
GDP_ERD[GDP_ERD == "nuts 2"]<-2
GDP_ERD[GDP_ERD == "nuts 3"]<-3
GDP_ERD$nuts_code <- toupper(GDP_ERD$nuts_code)
GDP_ERD <- GDP_ERD[order(GDP_ERD$nuts_code),]
GDP_ERD <- GDP_ERD %>% mutate(country=substr(nuts_code, start = 1, stop = 2))
GDP_ERD <- GDP_ERD %>% mutate(nuts_2=substr(nuts_code, start = 1, stop = 4))

GDP_ERD <- GDP_ERD[,c(1,39,40,2,3:38)]
GDP_ERD[,-(1:4)] <- GDP_ERD[,-(1:4)]*1e+9
#GDP_ERD[GDP_ERD == ".NaN"] <-NA

### Population

POP_ERD <-read.table("./RData/NUTS3 Population ERD fix.txt",
                     header=TRUE,
                     skip = 1,
                     sep = ";",
                     dec = ".",
                     stringsAsFactors = FALSE,
                     check.names=F,
                     colClasses=c("character", "character",
                                  rep("numeric", length(1980:2015)))
)
colnames(POP_ERD)[1:2] <- c("nuts_level", "nuts_code")
POP_ERD[POP_ERD == "nuts 0"]<-0
POP_ERD[POP_ERD == "nuts 1"]<-1
POP_ERD[POP_ERD == "nuts 2"]<-2
POP_ERD[POP_ERD == "nuts 3"]<-3
POP_ERD$nuts_code <- toupper(POP_ERD$nuts_code)
POP_ERD <- POP_ERD[order(POP_ERD$nuts_code),]
POP_ERD <- POP_ERD %>% mutate(country=substr(nuts_code, start = 1, stop = 2))
POP_ERD <- POP_ERD %>% mutate(nuts_2=substr(nuts_code, start = 1, stop = 4))

POP_ERD <- POP_ERD[,c(1,39,40,2,3:38)]
#POP_ERD[POP_ERD == ".NaN"] <-NA
#POP_ERD[,-2,POP_ERD != NA] <- POP_ERD[,-2, POP_ERD != NA]*1000
POP_ERD[,-(1:4)] <- POP_ERD[,-(1:4)]*1000
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
