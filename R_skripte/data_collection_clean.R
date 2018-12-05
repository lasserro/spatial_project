library(eurostat)
library(dplyr)

###############################DEFINE#########################
##Which countries to drop
drop<-list("AL","CH","DK","EF","EU","IS","ME","MK","NO","TR")
##the time frame
period<-c(2003:2016)
##unit of GDP-measurement
measure="EUR_HAB"
##############################################################




GDP_Nuts3 <- get_eurostat('nama_10r_3gdp', time_format = "raw",
                          stringsAsFactors = FALSE)
Pop_Nuts3 <- get_eurostat('demo_r_pjanaggr3', time_format = "raw",
                          stringsAsFactors = FALSE)

##Data Transformation
#1. Population: Nuts3


drop<-list("AL","CH","DK","EF","EU","IS","ME","MK","NO","TR")
period<-c(2003:2016)

pop3 <- Pop_Nuts3 %>% mutate(country=substr(geo, start = 1, stop = 2)) %>%
                      filter(time %in% period,
                      sex=="T",
                      age=="TOTAL",
                      nchar(geo)==5,
                      !country %in% drop)

##2. Population: Nuts2

pop2 <- Pop_Nuts3 %>% mutate(country=substr(geo, start = 1, stop = 2)) %>%
                      filter(time %in% period,
                             sex=="T",
                             age=="TOTAL",
                             nchar(geo)==4,
                             !country %in% drop)

##3. GDP: Nuts3
gdp3 <- GDP_Nuts3 %>% mutate(country=substr(geo, start = 1, stop = 2)) %>%
                      filter(nchar(geo)==5,
                             unit == measure,
                             time %in% period)
##4. GDP: Nuts2
gdp2 <- GDP_Nuts3 %>% mutate(country=substr(geo, start = 1, stop = 2)) %>% 
                      filter(nchar(geo)==4,
                             unit == measure,
                             time %in% period)

rm(drop,GDP_Nuts3,Pop_Nuts3, measure, period)
