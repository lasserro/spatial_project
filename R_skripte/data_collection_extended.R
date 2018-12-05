
#########################################################################
##############its a MESS, i will tidy it up, do not delete###############
#########################################################################



library(eurostat)
library(dplyr)

#EurostatTOC <- get_eurostat_toc()

##Get Data
#GDP_Nuts2 <- get_eurostat('nama_10r_2gdp', time_format = "raw")
GDP_Nuts3 <- get_eurostat('nama_10r_3gdp', time_format = "raw", , stringsAsFactors = FALSE)
#Pop_Nuts2 <- get_eurostat('demo_r_pjangroup', time_format = "raw")
Pop_Nuts3 <- get_eurostat('demo_r_pjanaggr3', time_format = "raw", stringsAsFactors = FALSE)

##Data Transformation
#1. Population:Nuts3
pop <- Pop_Nuts3 %>% filter(time >= 2000, sex=="T", age=="TOTAL", nchar(geo)==5)

#Check number of Observations in Nuts3regions per country per year:
#Get country-identifier CTR as column
popc <-pop %>% mutate(CTR=substr(geo, start = 1, stop = 2))
#create Matrix  ctr with years and number of observations per country per year (its messy, but it works (fast))
Countries<-rownames(table(popc$CTR))
n<-length(Countries)
k<-length(2000:2017)
ctr3<-matrix(NA,n,k)
rownames(ctr3)<-Countries
colnames(ctr3)<-2000:2017

for (i in 1:n) {
  t<-table(popc$time[popc$CTR==Countries[i]])
  ctr3[i,(k-length(t)+1):k]<-t
}
rm(t,n,k,popc,i)

##Same for Nuts_2
pop <- Pop_Nuts3 %>% filter(time >= 2000, sex=="T", age=="TOTAL", nchar(geo)==4)
popc <- pop %>% mutate(CTR=substr(geo, start = 1, stop = 2))
#create Matrix  ctr with years and number of observations per country per year (its messy, but it works (fast))
Countries<-rownames(table(popc$CTR))
n<-length(Countries)
k<-length(2000:2017)
ctr2<-matrix(NA,n,k)
rownames(ctr2)<-Countries
colnames(ctr2)<-2000:2017

for (i in 1:n) {
  t<-table(popc$time[popc$CTR==Countries[i]])
  ctr2[i,(k-length(t)+1):k]<-t
}
rm(t,n,k,popc,i)

##Compare countries from papers with our sample:
orgctr <- c("BE","DK","DE","EL","ES","FR","IE","IT","NL","LU","AT","PT","FI","SE","UK","CZ","HU","PL","SK","EE","LT","LV","SI","BG","RO")
orgctr <- sort(orgctr)
##Drop:
#Albania: not EU, and no data
#Switzerland: not EU, BUT PERFECT DATA (include?)
#Denmark: only data from 2007, include despite this? paper has it...
#EF: appearantly thats the EU as whole, only in Nuts2 set, hmmm, drop it!
#EU: detto?!
#Iceland: not EU
#Montenegro: not EU
#MK: The former Yugoslav Republic of Macedonia, not EU
#Norway: not EU
#Turkey: To EU or not to EU? Drop it


##Differences paper and our set
#Denmark: we dont have the data they do???
#Croatia: good Data, but in EU since 2013, so not in paper
#LU: here NUTS_3==Nuts_2 but is included in paper, lets keep it for now, but then we also have to use cyprus, no?
#Malta, cyprus
#Cyprus: , paper drops it for statistical reasons, maybe bcs: Nuts2==Nuts3?

drop<-list("AL","CH","DK","EF","EU","IS","ME","MK","NO","TR")
ctr3p <- ctr3[!rownames(ctr3) %in% drop,]
ctr2p <- ctr2[!rownames(ctr2) %in% drop,]


##Looking at the matrix now i suggest removing the years 2000,2001,2002. Then we have no more NAs. 
##BUT: One problem prevails, there are odd changes in the number of observations after certain years. This might be due to changes in the NUTS-classification (FU EU!). Only Nuts_3 regions have changed. Nuts_2 is unaffected.
#The changes are:
#DE, 2011
#FR, 2013
#PL, 2010

#GDP

##Data Transformation
#1. GDP:Nuts3

gdp <- GDP_Nuts3 %>% filter(nchar(geo)==5, unit == "MIO_EUR")

#Check number of Observations in Nuts3regions per country per year:
#Get country-identifier CTR as column
popc <-gdp %>% mutate(CTR=substr(geo, start = 1, stop = 2))
#create Matrix  ctr with years and number of observations per country per year (its messy, but it works (fast))
Countries<-rownames(table(popc$CTR))
n<-length(Countries)
k<-length(2000:2016)
ctr3<-matrix(NA,n,k)
rownames(ctr3)<-Countries
colnames(ctr3)<-2000:2016

for (i in 1:n) {
  t<-table(popc$time[popc$CTR==Countries[i]])
  ctr3[i,(k-length(t)+1):k]<-t
}
rm(t,n,k,popc,i)

##Same for Nuts_2
pop <- GDP_Nuts3 %>% filter(nchar(geo)==4, unit == "MIO_EUR")
popc <- pop %>% mutate(CTR=substr(geo, start = 1, stop = 2))
#create Matrix  ctr with years and number of observations per country per year (its messy, but it works (fast))
Countries<-rownames(table(popc$CTR))
n<-length(Countries)
k<-length(2000:2016)
ctr2<-matrix(NA,n,k)
rownames(ctr2)<-Countries
colnames(ctr2)<-2000:2016

for (i in 1:n) {
  t<-table(popc$time[popc$CTR==Countries[i]])
  ctr2[i,(k-length(t)+1):k]<-t
}
rm(t,n,k,popc,i)

##Compare countries from papers with our sample:
orgctr <- c("BE","DK","DE","EL","ES","FR","IE","IT","NL","LU","AT","PT","FI","SE","UK","CZ","HU","PL","SK","EE","LT","LV","SI","BG","RO")
orgctr <- sort(orgctr)
##Drop:
#Albania: not EU, and no data
#Switzerland: not EU, BUT PERFECT DATA (include?)
#Denmark: IS COOL HERE (not with population though)
#EU: appearantly thats the EU as whole, only in Nuts2 set, hmmm, drop it!
#Iceland: not EU
#Montenegro: not EU
#MK: The former Yugoslav Republic of Macedonia, not EU
#Norway: not EU

##Differences paper and our set
#Croatia: good Data, but in EU since 2013, so not in paper
#LU: here NUTS_3==Nuts_2 but is included in paper, lets keep it for now, but then we also have to use cyprus, no?
#Malta, cyprus
#Cyprus: , paper drops it for statistical reasons, maybe bcs: Nuts2==Nuts3?

drop<-list("AL","CH","EU","IS","ME","MK","NO")
ctr2g <- ctr2[!rownames(ctr2) %in% drop,]
ctr3g <- ctr3[!rownames(ctr3) %in% drop,]


##Compare this to population, we only have NAs in BELGIUM, still dropping 2000,2001,2002. =>to combine sets 
##BUT: Here we have NO changes in number i=of obs, very curious, could be a problem


##doesnt work: needs rework do not delete
Countries<-rownames(table(gdp2$country))
n<-length(Countries)
k<-length(period)
ctr2<-matrix(NA,2*n,k)
rownames(ctr2)<-c(Countries, Countries)
colnames(ctr2)<-period

for (i in 1:n) {
  t<-table(pop2$time[pop2$country==Countries[i]])
  ctr2[2*i-1,(k-length(t)+1):k]<-t
  t<-table(gdp2$time[gdp2$country==Countries[i]])
  ctr2[2*i,(k-length(t)+1):k]<-t
}

##Es scheint Deutschland ist auch ein Problem: wahrschienlich ist der Grund: Brandenburg wurde von 2003 bis 2011 in die NUTS-2-Regionen Brandenburg-Nordost und Brandenburg-Südwest geteilt, in der Absicht, nach der EU-Erweiterung vom 1. Mai 2004 wenigstens noch für den ärmeren Nordosten weiterhin EU-Fördergelder zugewiesen zu bekommen.
siehe: https://de.wikipedia.org/wiki/NUTS:DE#cite_note-1

pop2 <- Pop_Nuts3 %>% mutate(country=substr(geo, start = 1, stop = 2)) %>%
  filter(nchar(geo)==4,
         sex=="T",
         age=="TOTAL",
         time %in% period,
         country=="DE")

##4. GDP: Nuts2
gdp2 <- GDP_Nuts3 %>% mutate(country=substr(geo, start = 1, stop = 2)) %>% 
  filter(nchar(geo)==4,
         unit == measure,
         time %in% period,
         country=="DE")

cs<- pop2  %>%filter(time==2005)
c<- gdp2  %>%filter(time==2005)
