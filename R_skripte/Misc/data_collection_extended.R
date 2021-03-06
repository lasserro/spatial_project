
################################################################################
##############Extended Code for data collection#################################
################################################################################
#####I. Can we use nuts3-dataset for nuts2 regions##############################
######II. Compare matrices(to compare number of obs##############################
################################################################################
library(eurostat)
library(dplyr)

#EurostatTOC <- get_eurostat_toc()

##Get Data
# Das sind alle Daten die wir bis jetzt gefunden haben, GDP auf Nuts3 Ebene
#leider erst ab 2000, das komische is, das im paper die Zahlen bis 1995 
#zurückgehen, wo haben die die her???

# robert: die haben ein ganz anderes datenset gnommn. dieser cambridge shit oder
# so



GDP_Nuts2 <- get_eurostat('nama_10r_2gdp', time_format = "raw"
                          , stringsAsFactors = FALSE)
GDP_Nuts3 <- get_eurostat('nama_10r_3gdp', time_format = "raw"
                          , stringsAsFactors = FALSE)
Pop_Nuts2 <- get_eurostat('demo_r_pjangroup', time_format = "raw"
                          , stringsAsFactors = FALSE)
Pop_Nuts3 <- get_eurostat('demo_r_pjanaggr3', time_format = "raw"
                          , stringsAsFactors = FALSE)


###############################################################################
##I. Check if datasets for Nuts2 and Nuts3 are the same
###############################################################################
##1. For Population
nonEU<-list("AL","CH","EF","EU","IS","ME","MK","NO","TR","LI")
period<-c(2000:2017)

pop3 <- Pop_Nuts3 %>% mutate(country=substr(geo, start = 1, stop = 2)) %>%
  filter(nchar(geo)==4,
         sex=="T",
         age=="TOTAL"
         ,time %in% period
         ,!country %in% nonEU
  )
pop2 <- Pop_Nuts2 %>% mutate(country=substr(geo, start = 1, stop = 2)) %>%
  filter(nchar(geo)==4,
         sex=="T",
         age=="TOTAL"
         ,time %in% period
         ,!country %in% nonEU
  )

#now see what they have in common
com<-intersect(pop2$values,pop3$values)
#and recalculate them by excluding everything that is identical:
pop3uncommon <- Pop_Nuts3 %>% 
                          mutate(country=substr(geo, start = 1, stop = 2)) %>%
                          filter(nchar(geo)==4,
                          sex=="T",
                          age=="TOTAL"
                          ,time %in% period
                          ,!country %in% nonEU
                          ,!values %in% com
  )
pop2uncommon <- Pop_Nuts2 %>% 
                          mutate(country=substr(geo, start = 1, stop = 2)) %>%
                          filter(nchar(geo)==4,
                          sex=="T",
                          age=="TOTAL"
                          ,time %in% period
                          ,!country %in% nonEU
                          ,!values %in% com
  )
##Fazit: Comparing the two datasets the difference is minimal (max of 6, mostly
#1) for 60 observations, so it doesn't seem two make a difference if you use
#'demo_r_pjangroup' or 'demo_r_pjanaggr3' for the nuts2 regions. That is the
#case if you exclude NonEu Countries and for the period 2000-2017

##For GDP

nonEU<-list("AL","CH","EF","EU","IS","ME","MK","NO","TR","LI")

gdp3 <- GDP_Nuts3 %>% mutate(country=substr(geo, start = 1, stop = 2)) %>%
  filter(nchar(geo)==4,
         #unit == measure,
         !country %in% nonEU)

gdp2 <- GDP_Nuts2 %>% mutate(country=substr(geo, start = 1, stop = 2)) %>% 
  filter(nchar(geo)==4,
         #unit == measure,
         !country %in% nonEU,
         !unit %in% c("PPS_EU27_2019_HAB","PPS_HAB_EU27_2019")
         )

#do the same as with pop
com<-intersect(gdp2$values,gdp3$values)

gdp3uncommon <- GDP_Nuts3 %>%
                          mutate(country=substr(geo, start = 1, stop = 2)) %>%
                          filter(nchar(geo)==4,
                          #unit == measure,
                          !country %in% nonEU
                          ,!values %in% com)

gdp2uncommon <- GDP_Nuts2 %>% 
                          mutate(country=substr(geo, start = 1, stop = 2)) %>% 
                          filter(nchar(geo)==4,
                          #unit == measure,
                          !country %in% nonEU
                          ,!values %in% com
                          ,!unit %in% c("PPS_EU27_2019_HAB","PPS_HAB_EU27_2019")
         )
#They seem to be exactly the same. In the nuts2 dataset there are additional
#units, some measurements that excludes the UK. in your face UK, hrhr.

#Fazit: its super okay to just use the nuts2 values from the nuts3 dataset. Was
#all of that unnecessary? quite a bit...

rm(list=ls())

###############################################################################
#########II.####### What years/countries to use?###############################
###############################################################################

#For the years the maximal period is 2000-2016, as we only have data on GDP
#for those.
#We want to create a matrix, where we can easily compare data on populaton with 
#data on GDP, we start with the maximal data set. 
#That is the period 2000-2016
#and all EU-memberstates we have data on.
#There are 4 different units for GDP,
#i just use "EUR_HAB" for now: Euro per inhabitant, note that two of those units 
#have more observations, that might have implications for our dataset.

library(eurostat)
library(dplyr)

GDP_Nuts3 <- get_eurostat('nama_10r_3gdp', time_format = "raw"
                          , stringsAsFactors = FALSE)
Pop_Nuts3 <- get_eurostat('demo_r_pjanaggr3', time_format = "raw"
                          , stringsAsFactors = FALSE)

###############################DEFINE#########################
############################Max data set######################
#########################Max years, only EU###################
##Which countries to drop
nonEU<-list("AL","CH","EF","EU","IS","ME","MK","NO","TR","LI")
##the time frame
period<-c(2000:2016)
##unit of GDP
measure="EUR_HAB"
##############################################################

##1. Population: Nuts3
pop3 <- Pop_Nuts3 %>% mutate(country=substr(geo, start = 1, stop = 2)) %>%
  filter(nchar(geo)==5,
         sex=="T",
         age=="TOTAL",
         time %in% period,
         !country %in% nonEU)

##2. Population: Nuts2
pop2 <- Pop_Nuts3 %>% mutate(country=substr(geo, start = 1, stop = 2)) %>%
  filter(nchar(geo)==4,
         sex=="T",
         age=="TOTAL",
         time %in% period,
         !country %in% nonEU)

##3. GDP: Nuts3
gdp3 <- GDP_Nuts3 %>% mutate(country=substr(geo, start = 1, stop = 2)) %>%
  filter(nchar(geo)==5,
         unit == measure,
         time %in% period,
         !country %in% nonEU)

##4. GDP: Nuts2
gdp2 <- GDP_Nuts3 %>% mutate(country=substr(geo, start = 1, stop = 2)) %>% 
  filter(nchar(geo)==4,
         unit == measure,
         time %in% period,
         !country %in% nonEU)


##Matrix with number of observations for Pop and Gdp NUTS 2

Countries<-rownames(table(gdp2$country)) ##take countries directly from data
n<-length(Countries) 
k<-length(period)    #define n and k for easier reading
compare_nuts_2<-matrix(NA,2*n,k) #create matrix(2*n because we want gdp AND pop)
Country_pop <- sapply(Countries, function(x) paste(x, "_pop", sep = ""))
Country_gdp <- sapply(Countries, function(x) paste(x, "_gdp", sep = ""))
#Above is necessary to hav proper rownames (and distinguish between pop and gdp)
rownames(compare_nuts_2)<-sort(c(Country_pop, Country_gdp))
colnames(compare_nuts_2)<-period


##The loop for the matrix
for (i in (1:n)) { #once for each country
  
  t<-table(pop2$time[pop2$country==Countries[i]])
  #get table with numbers of obs for country i for each year
  
  tt<-rep(NA,k)
  #create empty vector with the same length as matrix rows
  
  for (j in 1:k) { #inner loop to place each numofobs to the right year
    if(period[j] %in% rownames(t)) #check if year j is in table t
    {tt[j]<-t[rownames(t)==period[j]]}#yes, put numofobs in the right spot of tt
    else 
    {tt[j]<-NA} #if not, put an NA there (actually unnecessary)
  }
  
  compare_nuts_2[2*i,]<-tt #put the vector in the right place in the matrix.
  #Note that 2*i only takes even rows (which are the pops) 2*i-1 only takes
  #odd rows, see below

  #Now the same for gdp  
  t<-table(gdp2$time[gdp2$country==Countries[i]])
  
  tt<-rep(NA,k)
  for (j in 1:k) {
    if(period[j] %in% rownames(t))
    {tt[j]<-t[rownames(t)==period[j]]}
    else
    {tt[j]<-NA}
  }
  
  compare_nuts_2[2*i-1,]<-tt
}
############Compare matrix for nuts3################

Countries<-rownames(table(gdp3$country))
n<-length(Countries)
k<-length(period)
compare_nuts_3<-matrix(NA,2*n,k)
Country_pop <- sapply(Countries, function(x) paste(x, "_pop", sep = ""))
Country_gdp <- sapply(Countries, function(x) paste(x, "_gdp", sep = ""))
rownames(compare_nuts_3)<-sort(c(Country_pop, Country_gdp))
colnames(compare_nuts_3)<-period


for (i in (1:n)) {

  t<-table(pop3$time[pop3$country==Countries[i]])
  
  tt<-rep(NA,k)
  for (j in 1:k) {
    if(period[j] %in% rownames(t))
    {tt[j]<-t[rownames(t)==period[j]]}
    else
    {tt[j]<-NA}
  }
  
  compare_nuts_3[2*i,]<-tt
  
  t<-table(gdp3$time[gdp3$country==Countries[i]])
  
  tt<-rep(NA,k)
  for (j in 1:k) {
    if(period[j] %in% rownames(t))
    {tt[j]<-t[rownames(t)==period[j]]}
    else
    {tt[j]<-NA}
  }
  
  compare_nuts_3[2*i-1,]<-tt
}
#########################################
########## A refined Comparison matrix that marks all nice countries
#########################################
compare_nuts_3_cool<-compare_nuts_3

for (i in 1:n) {  #for each country
  if(!NA %in% compare_nuts_3[2*i-1,]) #check if there are NAs in gdp
  {if(!0 %in% compare_nuts_3[2*i-1,]) #check if there are 0s in gdp
  {if(!NA %in% compare_nuts_3[2*i,])  #check if there are NAs in pop
  {if(!0 %in% compare_nuts_3[2*i,])   #check if there are 0s in pop
  {if(!FALSE %in% (compare_nuts_3[2*i-1,]==compare_nuts_3[2*i,])) 
                                #Check if pop and gdp have the same numofobs
  {if(length(unique(compare_nuts_3[2*i-1,]))==1)
                                #Check if gdp has same numofobs for each year
  {if(length(unique(compare_nuts_3[2*i,]))==1)
  {                             #Check if pop has same numofobs for each year
    compare_nuts_3_cool[2*i-1,]<-rep("cool",k) 
    compare_nuts_3_cool[2*i,]<-rep("cool",k)   #if nice, make it cool
  }}}}}}}
}

compare_nuts_2_cool<-compare_nuts_2

for (i in 1:n) {
  if(!NA %in% compare_nuts_2[2*i-1,])
  {if(!0 %in% compare_nuts_2[2*i-1,])
  {if(!NA %in% compare_nuts_2[2*i,])
  {if(!0 %in% compare_nuts_2[2*i,])
  {if(!FALSE %in% (compare_nuts_2[2*i-1,]==compare_nuts_2[2*i,]))
  {if(length(unique(compare_nuts_2[2*i-1,]))==1)
  {if(length(unique(compare_nuts_2[2*i,]))==1)
  {
    compare_nuts_2_cool[2*i-1,]<-rep("cool",k)
    compare_nuts_2_cool[2*i,]<-rep("cool",k)
  }}}}}}}
}

rm(nonEU,Countries,Country_gdp,Country_pop,i,j,k,n,period,measure,t,tt)
#now we can check those two matrices for problematic countries
#There seem to be a lot of changes between 2000 and 2002 especially for the
#nuts 3 regions, it looks like we should definitely drop 2000 as there are a lot
#of Nas. 
#I looked into the specific case of Germany and they changed their nuts regions
#exactly in the year where our data set gets mor observations, so that could 
#explain a lot, what to do??

#Brandenburg wurde von 2003 bis 2011 in die NUTS-2-Regionen Brandenburg-Nordost
#und Brandenburg-Südwest geteilt, in der Absicht, nach der EU-Erweiterung vom
#1. Mai 2004 wenigstens noch für den ärmeren Nordosten weiterhin EU-Fördergelder
#zugewiesen zu bekommen.
#siehe: https://de.wikipedia.org/wiki/NUTS:DE#cite_note-1

###############################################################################
##############List of problematic countries####################################
###############################################################################
######################NICHT MEHR AKTUELL#######################################
###############################################################################
#2000 will be ignored, lets drop it
#Countries with only 1 obs, will be ignored too, is dealt with in the paper
##########For nuts2:

#BE:Belgium, NAs 2000-2002

#DE:Germany, Change in numofobs in 2011, explanation see above

#DK: Denmark, NAs until 2006, and little obs otherwise, this ain't good,
#because its included in the paper

#FR:France: changes in number of obs

#HR: Croatia: little obs, Na in 2000

#SI:Slovenia: Nas 2000-2002, little obs

#UK:UK: changes in 2003


##########For nuts3:

#AT:Austria: NAs 2000-2003

#BE:Belgium, NAs 2000-2002, changes in 2004

#DE:Germany, Change in numofobs in 2011, explanation see above
#and 2007

#DK: Denmark, NAs until 2006, and little obs otherwise, this ain't good, because its included in the paper

#ES:Spain: changes in 2003

#FR:France: changes in number of obs, and difference between gdp and pop

#HR: Croatia: little obs, Na in 2000

#HU: Na in 2001

#LU:Luxemburg: Na in 2001

#NL:Netherlands: changes in 2003

#PL:Poland: Diff in obs and changes in 2010

#SI:Slovenia: Nas 2000-2002,

#SK:Slovakia: change in 2002

#UK:UK: changes in 2003

#####################################################################
#How to deal with that? We could for example drop 2000-2003 that would solve a lot, (but it hurts reeeaaaally bad)
#appearently there is a way to deal with changes in NUTS specifications, we should definetly look into that
#and i think we have to drop DK
###################################################################
##################Smaller data set#################################
###################################################################
##Which countries to drop
nonEU<-list("AL","CH","EF","EU","IS","ME","MK","NO","TR","LI")
##the time frame
period<-c(2000:2016)
##unit of GDP
measure="EUR_HAB"
###################################################################
####################ADD:###########################################
drop<- c(nonEU,"DK")
period <- (2005:2016)

############AAAAAAAAAAND recalculate our compare-Matrices##########




##1. Population: Nuts3
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

##Matrix with number of observations for Pop and Gdp NUTS 2
Countries<-rownames(table(gdp2$country))
n<-length(Countries)
k<-length(period)
compare_2<-matrix(NA,2*n,k)
rownames(compare_2)<-sort(c(Countries, Countries))
colnames(compare_2)<-period

for (i in 1:n) {
  t<-table(pop2$time[pop2$country==Countries[i]])
  compare_2[2*i-1,(k-length(t)+1):k]<-t
  t<-table(gdp2$time[gdp2$country==Countries[i]])
  compare_2[2*i,(k-length(t)+1):k]<-t
}
#and get rid of unproblematic countries:
#That is no 0, no NA, no difference between pop and gdp and no change in number
#of obs for different years
compare_2x<-compare_2
for (i in 1:(2*n-1)) {
  if((!(FALSE)  %in% (compare_2[i,]==compare_2[i+1,])) 
     && (!(NA)  %in% (compare_2[i,]==compare_2[i+1,]))
     && (!0 %in% compare_2[i,]) && (!NA %in% compare_2[i,])
     && length(unique(compare_3[i,]==1))
  ){
    compare_2x[c(i,i+1),]<-rep("cool",length(k))}
}

##Matrix with number of observations for Pop and Gdp NUTS 3
Countries<-rownames(table(gdp3$country))
n<-length(Countries)
k<-length(period)
compare_3<-matrix(NA,2*n,k)
rownames(compare_3)<-sort(c(Countries, Countries))
colnames(compare_3)<-period

for (i in 1:n) {
  t<-table(pop3$time[pop3$country==Countries[i]])
  compare_3[2*i-1,(k-length(t)+1):k]<-t
  t<-table(gdp3$time[gdp3$country==Countries[i]])
  compare_3[2*i,(k-length(t)+1):k]<-t
}
#and get rid of unproblematic countries:
#That is no 0, no NA, no difference between pop and gdp and no change in number
#of obs for different years
compare_3x<-compare_3
for (i in 1:(2*n-1)) {
  if((!(FALSE)  %in% (compare_3[i,]==compare_3[i+1,])) 
     && (!(NA)  %in% (compare_3[i,]==compare_3[i+1,]))
     && (!0 %in% compare_3[i,]) && (!NA %in% compare_3[i,])
     && length(unique(compare_3[i,]==1))
  ){
    compare_3x[c(i,i+1),]<-rep("cool",length(k))}
}

compare_2x
compare_3x

#############################################################
#########Prevailing Problems#################################
###################with smaller set##########################

##Nuts 2

#DE
#FR

##Nuts 3

#AT: NA in 2003
#BE:change in 2004
#DE:diffs and changes
#

NEEEEEEEVERMIND, That code produces NAS, WHAT THE FUCKKKK?????????!!!!!!!

############################################################################

#new try


##Compare countries from papers with our sample:
orgctr <- c("BE","DK","DE","EL","ES","FR","IE","IT","NL","LU","AT","PT","FI","SE","UK","CZ","HU","PL","SK","EE","LT","LV","SI","BG","RO")
orgctr <- sort(orgctr)


##1. Population: Nuts3
pop3 <- Pop_Nuts3 %>% mutate(country=substr(geo, start = 1, stop = 2)) %>%
  filter(nchar(geo)==5,
         sex=="T",
         age=="TOTAL",
         time %in% period,
         !country %in% nonEU)

##2. Population: Nuts2
pop2 <- Pop_Nuts3 %>% mutate(country=substr(geo, start = 1, stop = 2)) %>%
  filter(nchar(geo)==4,
         sex=="T",
         age=="TOTAL",
         time %in% period,
         !country %in% nonEU)

##3. GDP: Nuts3
gdp3 <- GDP_Nuts3 %>% mutate(country=substr(geo, start = 1, stop = 2)) %>%
  filter(nchar(geo)==5,
         unit == measure,
         time %in% period,
         !country %in% nonEU)

##4. GDP: Nuts2
gdp2 <- GDP_Nuts3 %>% mutate(country=substr(geo, start = 1, stop = 2)) %>% 
  filter(nchar(geo)==4,
         unit == measure,
         time %in% period,
         !country %in% nonEU)


#####################

library(tidyverse)

pop2x <- pop2 %>% group_by(country) %>% collect()


pop2x <- pop2 %>% separate(pop2,values,Countries,sep = country)


pop2x <-pop2 %>% group_by(country) %>% extract()

separate(data, col, into, sep = "[^[:alnum:]]+", remove = TRUE,
         convert = FALSE, extra = "warn", fill = "warn", ...)
  
  
  pull(values)
  
  
  group_by(country)







