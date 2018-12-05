#compare num of obs for single Countries
pop <- GDP_Nuts3 %>% filter(nchar(geo)==4, unit=="EUR_HAB")
pop <- pop %>% mutate(CTR=substr(geo, start = 1, stop = 2))
ATg <- pop %>% filter(CTR=="AT")

pop <- Pop_Nuts3 %>% filter(nchar(geo)==4, age=="TOTAL", sex=="T")
pop <- pop %>% mutate(CTR=substr(geo, start = 1, stop = 2))
ATp <- pop %>% filter(CTR=="AT")
