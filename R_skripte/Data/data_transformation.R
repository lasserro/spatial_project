### 1. Definitions

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

### 2. The Transformation

## Population: Nuts3
pop3 <- Pop_Nuts3 %>% mutate(country=substr(geo, start = 1, stop = 2)) %>%
  mutate(geo_2=substr(geo, start = 1, stop = 4)) %>%
    filter(nchar(geo)==5,
         sex=="T",
         age=="TOTAL",
         time %in% period,
         !country %in% drop)

## Population: Nuts2
pop2 <- Pop_Nuts3 %>% mutate(country=substr(geo, start = 1, stop = 2)) %>%
  filter(nchar(geo)==4,
         sex=="T",
         age=="TOTAL",
         time %in% period,
         !country %in% drop)

## GDP: Nuts3
gdp3 <- GDP_Nuts3 %>% mutate(country=substr(geo, start = 1, stop = 2)) %>%
  mutate(geo_2=substr(geo, start = 1, stop = 4)) %>%
  filter(nchar(geo)==5,
         unit == measure,
         time %in% period,
         !country %in% drop)

## GDP: Nuts2
gdp2 <- GDP_Nuts3 %>% mutate(country=substr(geo, start = 1, stop = 2)) %>% 
  filter(nchar(geo)==4,
         unit == measure,
         time %in% period,
         !country %in% drop)


### 3. Join datasets

## 3.1 Rename columns and drop unnecessary ones

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

## 3.2 Join nuts 2 (gdp and pop)

nuts_2 <- inner_join(pop2,gdp2,by = c("time", "country","geo_2"))

nuts_2 <- nuts_2[,c(2,4,1,3,5)]

## 3.3 Join nuts 3 (gdp and pop)

nuts_3 <- inner_join(pop3,gdp3,by = c("time", "country","geo_3"))

nuts_3 <- nuts_3[,c(2,4,1,3,6,5)]

## 3.4 Number of geo_3 in geo_2 (freq)

nrNuts3in2 <- as.data.frame(table(nuts_3$geo_2.x))
colnames(nrNuts3in2) <- c('geo_2', 'freq')
nrNuts3in2$geo_2 <- as.character(nrNuts3in2$geo_2)

nrNuts3in2$freq <- nrNuts3in2$freq/length(period)
nuts_2 <- left_join(nuts_2, nrNuts3in2, by = "geo_2")

rm(nrNuts3in2)
## 3.5 Join nuts 2 and 3 

df<-nuts_3 %>%
  mutate(geo_2=substr(geo_3, start = 1, stop = 4)) %>% #needed as join-id
  left_join(nuts_2, nuts_3, by = c("time","country","geo_2") )


df<-df[,c(1,2,7,3,8,4,9,5,6)] # sort columns



#rm(gdp2,gdp3,pop2,pop3)
#rm(drop,GDP_Nuts3,Pop_Nuts3, measure, period)
#rm(drop,GDP_Nuts3,Pop_Nuts3, measure, period,nonEU)

## 4. Further definitions (if needed later)

#n_2<-length(pop2$geo_2)
#k<-length(period)
#Countries<-rownames(table(gdp2$country))
#n_0<-length(Countries)
