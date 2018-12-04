library(eurostat)
library(dplyr)
EurostatTOC <- get_eurostat_toc()


GDP_Nuts2 <- get_eurostat('nama_10r_2gdp', time_format = "raw")
GDP_Nuts3 <- get_eurostat('nama_10r_3gdp', time_format = "raw")
Pop_Nuts2 <- get_eurostat('demo_r_pjangroup', time_format = "raw")
Pop_Nuts3 <- get_eurostat('demo_r_pjanaggr3', time_format = "raw")


GDP_Nuts2$time <- as.numeric(GDP_Nuts2$time)
GDP_Nuts3$time <- as.numeric(GDP_Nuts3$time)
Pop_Nuts2$time <- as.numeric(Pop_Nuts2$time)
Pop_Nuts3$time <- as.numeric(Pop_Nuts3$time)

Pop_Nuts3_new <- Pop_Nuts3_new %>% filter(time >= 2000)



