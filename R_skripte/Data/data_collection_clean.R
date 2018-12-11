library(eurostat)
library(dplyr)

if(set==max){
############################## DEFINE ########################
## Which countries to drop
nonEU<-list("AL","CH","EF","EU","IS","ME","MK","NO","TR","LI")
drop<-nonEU
## the time frame
period<-c(2000:2016)
# müssen wahrscheinlich 2016, wenn nicht auch 2015 droppen.
# siehe (table(gdp3$time))

## unit of GDP-measurement
measure="EUR_HAB"
##############################################################
}
if(set==min){
  ###############################DEFINE#########################
  nonEU<-list("AL","CH","EF","EU","IS","ME","MK","NO","TR","LI")
  drop<-c(nonEU,"DK","DE","FR","PL")
  period<-c(2004:2014)
  measure="EUR_HAB"
  ############################################################## 
} 
  
############## Download ########################################################

suppressWarnings(                                 
  try(load("./RData/GDP_Nuts3"),silent = TRUE))   ##try suppresses errors
  
suppressWarnings(
  try(load("./RData/Pop_Nuts3"),silent = TRUE))

if(exists(GDP_Nuts3)){
GDP_Nuts3 <- get_eurostat('nama_10r_3gdp', time_format = "raw",
                          stringsAsFactors = FALSE)
Pop_Nuts3 <- get_eurostat('demo_r_pjanaggr3', time_format = "raw",
                          stringsAsFactors = FALSE)

save(cn,file = "./RData/GDP_Nuts3")
save(cn,file = "./RData/Pop_Nuts3")
}

## Data Transformation

#1. Population: Nuts3
pop3 <- Pop_Nuts3 %>% mutate(country=substr(geo, start = 1, stop = 2)) %>%
                      filter(nchar(geo)==5,
                             sex=="T",
                             age=="TOTAL",
                             time %in% period,
                             !country %in% drop)

##2. Population: Nuts2

pop2 <- Pop_Nuts3 %>% mutate(country=substr(geo, start = 1, stop = 2)) %>%
                      filter(nchar(geo)==4,
                             sex=="T",
                             age=="TOTAL",
                             time %in% period,
                             !country %in% drop)

##3. GDP: Nuts3
gdp3 <- GDP_Nuts3 %>% mutate(country=substr(geo, start = 1, stop = 2)) %>%
                      filter(nchar(geo)==5,
                             unit == measure,
                             time %in% period,
                             !country %in% drop)
##4. GDP: Nuts2
gdp2 <- GDP_Nuts3 %>% mutate(country=substr(geo, start = 1, stop = 2)) %>% 
                      filter(nchar(geo)==4,
                             unit == measure,
                             time %in% period,
                             !country %in% drop)

#rm(drop,GDP_Nuts3,Pop_Nuts3, measure, period)



