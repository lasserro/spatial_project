#Achtung, dieses Skript generiert die Daten als 4 einzelne sets, das is eher
#zach, da wir sie dann eh wieder joinen. Daher würd ich eher Minimal working
#dataset II verwenden. (Stand 10.12.2018)

###############################################################################
########This is a minimal working set#######
########################################

library(eurostat)
library(dplyr)

###############################DEFINE#########################
##Which countries to drop
nonEU<-list("AL","CH","EF","EU","IS","ME","MK","NO","TR","LI")
drop<-c(nonEU,"DK","DE","FR","PL")
##the time frame
period<-c(2004:2014)

##unit of GDP-measurement
measure="EUR_HAB"
##############################################################

GDP_Nuts3 <- get_eurostat('nama_10r_3gdp', time_format = "raw",
                          stringsAsFactors = FALSE)
Pop_Nuts3 <- get_eurostat('demo_r_pjanaggr3', time_format = "raw",
                          stringsAsFactors = FALSE)

##Data Transformation

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

rm(drop,GDP_Nuts3,Pop_Nuts3, measure, period)

##join datasets


#only nuts_2
nuts_2<-inner_join(pop2,gdp2,by = c("geo", "time", "country"))

colnames(nuts_2)[colnames(nuts_2)=="values.y"]<-"gdp"
colnames(nuts_2)[colnames(nuts_2)=="values.x"]<-"pop"

nuts_2$unit.x<-NULL
nuts_2$sex<-NULL
nuts_2$age<-NULL
#nuts_2$unit.y<-NULL

#only nuts_3
nuts_3<-inner_join(pop3,gdp3,by = c("geo", "time", "country"))

colnames(nuts_3)[colnames(nuts_3)=="values.y"]<-"gdp"
colnames(nuts_3)[colnames(nuts_3)=="values.x"]<-"pop"

nuts_3$unit.x<-NULL
nuts_3$sex<-NULL
nuts_3$age<-NULL
#nuts_3$unit.y<-NULL

#both nuts 2&3
#df<-left_join(nuts_3, nuts_2, by = c("time","country","unit.y") )

#colnames(df)[colnames(df)=="geo.x"]<-"geo_3"
#colnames(df)[colnames(df)=="geo.y"]<-"geo_2"
#colnames(df)[colnames(df)=="geo.y"]<-"geo_2"
#colnames(df)[colnames(df)=="geo.y"]<-"geo_2"

