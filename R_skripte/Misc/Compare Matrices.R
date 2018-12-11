#####################################################################
############Compare Matrices#########################################
####################10.12.2018#######################################

##Achtung, das Skript funktioniert nur mit einer alten Spezifikation.
#Dazu muessen pop2,pop3,gdp3,gdp2 getrennte dataframes sein. Deshalb 
#ist der Datengenerierungsprozess nochmal extra im Skript, kann aber
#leicht mithilfe der DEFINE section angepasst werden.

#BRUNO

library(eurostat)
library(dplyr)

###############################DEFINE#########################
##Which countries to drop
drop<-nonEU<-list("AL","CH","EF","EU","IS","ME","MK","NO","TR","LI")
#drop<-c(nonEU,"DK","DE","FR","PL")
##the time frame
period<-c(2004:2014)
#period<-c(2000:2016)
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







period<-(min(unique(pop2$time)):max(unique(pop2$time)))


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