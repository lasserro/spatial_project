#compare num of obs for single Countries
pop <- GDP_Nuts3 %>% filter(nchar(geo)==4, unit=="EUR_HAB")
pop <- pop %>% mutate(CTR=substr(geo, start = 1, stop = 2))
ATg <- pop %>% filter(CTR=="AT")

pop <- Pop_Nuts3 %>% filter(nchar(geo)==4, age=="TOTAL", sex=="T")
pop <- pop %>% mutate(CTR=substr(geo, start = 1, stop = 2))
ATp <- pop %>% filter(CTR=="AT")

#################################
##old loop for compare matrix, doesn't care for missing values in the middle
#what a bitch


for (i in 1:n) {
  t<-table(pop2$time[pop2$country==Countries[i]])
  compare_2[2*i-1,(k-length(t)+1):k]<-t
  t<-table(gdp2$time[gdp2$country==Countries[i]])
  compare_2[2*i,(k-length(t)+1):k]<-t
}

###

for (i in 1:n) {
  t<-table(pop3$time[pop3$country==Countries[i]])
  compare_3[2*i-1,(k-length(t)+1):k]<-t
  t<-table(gdp3$time[gdp3$country==Countries[i]])
  compare_3[2*i,(k-length(t)+1):k]<-t
}

#########################

###############################DEFINE#########################
############################Max data set######################
#########################Max years, only EU###################
##Which countries to drop
nonEU<-list("AL","CH","EF","EU","IS","ME","MK","NO","TR","LI")
##the time frame
period<-c(2000:2016)
##unit of GDP
measure="EUR_HAB"
#############################################################

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
Countries<-rownames(table(gdp2$country))
n<-length(Countries)
k<-length(period)
compare_2<-matrix(NA,2*n,k)
Country_pop <- sapply(Countries, function(x) paste(x, "_pop", sep = ""))
Country_gdp <- sapply(Countries, function(x) paste(x, "_gdp", sep = ""))
rownames(compare_2)<-sort(c(Country_pop, Country_gdp))
colnames(compare_2)<-period


for (i in 1:n) {
  t<-table(pop2$time[pop2$country==Countries[i]])
  
  c<-rep(NA,k)
  q=1
  for (j in 1:k) {
    
    if(TRUE %in% (colnames(compare_2)[j] %in% rownames(t)))
    {c[j]<-t[q]
    q=q+1}
  }
  
  compare_2[2*i,]<-c
  t<-table(gdp2$time[gdp2$country==Countries[i]])
  
  c<-rep(NA,k)
  q=1
  for (j in 1:k) {
    if(TRUE %in% (colnames(compare_2)[j] %in% rownames(t)))
    {c[j]<-t[q]
    q=q+1}
  } 
  
  compare_2[2*i-1,]<-c
}

########################other old compare matrices...##########################


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
Country_pop <- sapply(Countries, function(x) paste(x, "_pop", sep = ""))
Country_gdp <- sapply(Countries, function(x) paste(x, "_gdp", sep = ""))
rownames(compare_3)<-sort(c(Country_pop, Country_gdp))
colnames(compare_3)<-period

s


for (i in 1:n) {
  t<-table(pop3$time[pop3$country==Countries[i]])
  compare_x[2*i-1,(k-length(t)+1):k]<-t
  t<-table(gdp3$time[gdp3$country==Countries[i]])
  compare_x[2*i,(k-length(t)+1):k]<-t
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

###############################DEFINE#########################
############################Max data set######################
#########################Max years, only EU###################
##Which countries to drop
nonEU<-list("AL","CH","EF","EU","IS","ME","MK","NO","TR","LI")
##the time frame
period<-c(2000:2016)
##unit of GDP
measure="EUR_HAB"
#############################################################

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
Countries<-rownames(table(gdp2$country))
n<-length(Countries)
k<-length(period)
compare_2<-matrix(NA,2*n,k)
Country_pop <- sapply(Countries, function(x) paste(x, "_pop", sep = ""))
Country_gdp <- sapply(Countries, function(x) paste(x, "_gdp", sep = ""))
rownames(compare_2)<-sort(c(Country_pop, Country_gdp))
colnames(compare_2)<-period


for (i in 1:n) {
  t<-table(pop2$time[pop2$country==Countries[i]])
  
  c<-rep(NA,k)
  q=1
  for (j in 1:k) {
    
    if(TRUE %in% (colnames(compare_2)[j] %in% rownames(t)))
    {c[j]<-t[q]
    q=q+1}
  }
  
  compare_2[2*i,]<-c
  t<-table(gdp2$time[gdp2$country==Countries[i]])
  
  c<-rep(NA,k)
  q=1
  for (j in 1:k) {
    if(TRUE %in% (colnames(compare_2)[j] %in% rownames(t)))
    {c[j]<-t[q]
    q=q+1}
  } 
  
  compare_2[2*i-1,]<-c
}




########################


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
Country_pop <- sapply(Countries, function(x) paste(x, "_pop", sep = ""))
Country_gdp <- sapply(Countries, function(x) paste(x, "_gdp", sep = ""))
rownames(compare_3)<-sort(c(Country_pop, Country_gdp))
colnames(compare_3)<-period

s


for (i in 1:n) {
  t<-table(pop3$time[pop3$country==Countries[i]])
  compare_x[2*i-1,(k-length(t)+1):k]<-t
  t<-table(gdp3$time[gdp3$country==Countries[i]])
  compare_x[2*i,(k-length(t)+1):k]<-t
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

