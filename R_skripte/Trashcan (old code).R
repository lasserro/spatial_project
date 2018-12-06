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