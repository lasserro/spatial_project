pop3_2003 <- Pop_Nuts3 %>% mutate(country=substr(geo, start = 1, stop = 2)) %>%
  filter(nchar(geo)==5,
         sex=="T",
         age=="TOTAL",
         time %in% 2003,
         !country %in% drop)

pop3_2016 <- Pop_Nuts3 %>% mutate(country=substr(geo, start = 1, stop = 2)) %>%
  filter(nchar(geo)==5,
         sex=="T",
         age=="TOTAL",
         time %in% 2016,
         !country %in% drop)

summary(pop3_2016$geo %in% pop3_2003$geo) # für 50 NUTS3 Regionen gabs 2003 keine daten zu population. welche?

pop3_2016$logic <- (pop3_2016$geo %in% pop3_2003$geo)

pop3_2016_F <- pop3_2016[pop3_2016$logic=='FALSE',] # das sind die NUTS3 regionen, die es 2003 nicht gab
