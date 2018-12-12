
if(min==0){
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
if(min==1){
  ###############################DEFINE#########################
  nonEU<-list("AL","CH","EF","EU","IS","ME","MK","NO","TR","LI")
  drop<-c(nonEU,"DK","DE","FR","PL")
  period<-c(2004:2014)
  measure="EUR_HAB"
  ############################################################## 
} 

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

rm(drop,GDP_Nuts3,Pop_Nuts3, measure, period,nonEU)

##join datasets

#First rename everything to avoid confusion later and drop unnecessary columns.

pop3$unit<-NULL
pop3$sex<-NULL
pop3$age<-NULL
colnames(pop3)[colnames(pop3)=="values"]<-"pop_3"
colnames(pop3)[colnames(pop3)=="geo"]<-"geo_3"

pop2$unit<-NULL
pop2$sex<-NULL
pop2$age<-NULL
colnames(pop2)[colnames(pop2)=="values"]<-"pop_2"
colnames(pop2)[colnames(pop2)=="geo"]<-"geo_2"

#gdp3$unit<-NULL
colnames(gdp3)[colnames(gdp3)=="values"]<-"gdp_3"
colnames(gdp3)[colnames(gdp3)=="geo"]<-"geo_3"

gdp2$unit<-NULL
colnames(gdp2)[colnames(gdp2)=="values"]<-"gdp_2"
colnames(gdp2)[colnames(gdp2)=="geo"]<-"geo_2"

##onto the joining, start with nuts 2 (gdp and pop)

nuts_2<-inner_join(pop2,gdp2,by = c("time", "country","geo_2"))

#joining nuts 3 (gdp and pop)
nuts_3<-inner_join(pop3,gdp3,by = c("time", "country","geo_3"))

#joining nuts 2 & 3
df<-nuts_3 %>%
  mutate(geo_2=substr(geo_3, start = 1, stop = 4)) %>% #needed as join-id
  left_join(nuts_2, nuts_3, by = c("time","country","geo_2") )


df<-df[,c(2,4,7,1,8,3,9,6,5)] # sort columns

rm(gdp2,gdp3,pop2,pop3)


#rm(drop,GDP_Nuts3,Pop_Nuts3, measure, period)

n<-length(pop2$geo)
k<-length(period)
