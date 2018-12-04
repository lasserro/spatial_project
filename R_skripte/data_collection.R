library(eurostat)
EurostatTOC <- get_eurostat_toc()

df <- get_eurostat(c('nama_10r_2gdp', 'nama_10r_3gdp', 'demo_r_pjangroup', 
                   'demo_r_pjangrp3'), time_format = "raw")

GDP_Nuts2 <- get_eurostat('nama_10r_2gdp', time_format = "raw")
GDP_Nuts3 <- get_eurostat('nama_10r_3gdp', time_format = "raw")
Pop_Nuts2 <- get_eurostat('demo_r_pjangroup', time_format = "raw")
Pop_Nuts3 <- get_eurostat('demo_r_pjangrp3', time_format = "raw")
