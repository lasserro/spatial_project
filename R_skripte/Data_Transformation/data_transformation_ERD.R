### 1. Definitions

if(min==0){
  ############################## DEFINE ########################
  ## Which countries to drop
  #nonEU<-list("AL","CH","EF","EU","IS","ME","MK","NO","TR","LI")
  drop<-c()
  ## the time frame
  period<-c(1980:2015)
  overseas <- c()
  # müssen wahrscheinlich 2016, wenn nicht auch 2015 droppen.
  ##############################################################
}
if(min==1){
  ###############################DEFINE#########################
  #nonEU<-list("AL","CH","EF","EU","IS","ME","MK","NO","TR","LI")
  drop<-c("NO")
  period<-c(1995:2015)
  overseas <- c("FRA1", "FRA2", "FRA3", "FRA4", "FRZZ", "FRA5", "PT20", "PT30", "PTZZ", "ES70", "ESZZ")
  
  ############################################################## 
} 

### 2. The Transformation


## 2.01 Go home Norway, Go home NAs

charcols <- c("nuts_level", "country", "nuts_2", "nuts_code")

POP_ERD <- POP_ERD %>% select(charcols, paste(period)) %>%
  filter(!country %in% drop)  %>%
  filter(!nuts_2 %in% overseas) 

GDP_ERD <- GDP_ERD %>% select(charcols, paste(period)) %>%
  filter(!country %in% drop ) %>%
  filter(!nuts_2 %in% overseas)

############# ACHUTNG, der folgende code schmeisst NUTS 3 regionen raus, die 
# nicht in pop3 und gdp3 übereinstimmen (vom namen her), das betrifft 6 Stück.

# Auf diese 6 kann gut verzichtet werden. Das sind einerseits die komischen
# Nuts_codes, die nur aus Zahlen bestehen und andererseits 'extra-regios'
# aus Deutschland, die sowieso NA's sind 

# NA sind sie nicht. und das sind ca 1 mio Menschen,
# wär schon interessant was das is

GDP_ERD <- GDP_ERD %>%
  filter(!nuts_code %in% setdiff(GDP_ERD$nuts_code,POP_ERD$nuts_code))
POP_ERD <- POP_ERD %>%
  filter(!nuts_code %in% setdiff(POP_ERD$nuts_code,GDP_ERD$nuts_code))

#############################################################################
########Achtung, an diesem Punkt haben wir NAs für Ungarn
#############################################################################

## 2.02 Fill in neglected regions 

# The ERD leaves out regions if the nuts level above is the same.
# (e.g. AT13 & AT130 ). This takes the official NUTS 2013 classification and 
# fills in the missing values.


# get the official nuts 2013 classification

nuts_2013 <- read.table("./RData/NUTS2013 all.csv",
                        header = TRUE,
                        sep = "\t",
                        skip = 1,
                        comment.char = ";",
                        check.names = FALSE,
                        stringsAsFactors = FALSE)

colnames(nuts_2013) <- "code2013"

# get rid of all (useless) z, zz, zzz. NO IDEA what those are... -> same as we 
# kicked out in the previous section...

nuts_2013 <- nuts_2013 %>% mutate (zzzz = substring(code2013,3)) %>%
  filter(!zzzz %in% "Z",
         !zzzz %in% "ZZ",
         !zzzz %in% "ZZZ"
  ) %>% 
  select(-zzzz)

# add the wishlist to the actual dataframe
# GDP

GDP_ERD <- left_join(nuts_2013,GDP_ERD, by = c("code2013" = "nuts_code"))

# and fill in missing values accordingly

# hier wäre der ideale Punkt um Später diese nuts2=nuts3 werte in national averages
# zu transformieren...


for (i in 1:length(GDP_ERD$code2013)) {
  
  if(GDP_ERD[i,2] %in% NA)
  {GDP_ERD[i,-(1:2)] <- GDP_ERD[i-1,-(1:2)]
  GDP_ERD[i,2] <- as.numeric(GDP_ERD[i-1,2])+1
  GDP_ERD[i,4] <- substr(GDP_ERD[i,1],1,4)
  }
}

# POP

POP_ERD <- left_join(nuts_2013,POP_ERD, by = c("code2013" = "nuts_code"))

# and fill in missing values accordingly

for (i in 1:length(POP_ERD$code2013)) {
  
  if(POP_ERD[i,2] %in% NA)
  {POP_ERD[i,-(1:2)] <- POP_ERD[i-1,-(1:2)]
  POP_ERD[i,2] <- as.numeric(POP_ERD[i-1,2])+1
  POP_ERD[i,4] <- substr(POP_ERD[i,1],1,4)
  }
}

rm(nuts_2013)

## 2.1 Single dataframes for gdp,pop on level 2 & 3


pop2 <- POP_ERD %>% filter(nuts_level == 2)

pop3 <- POP_ERD %>% filter(nuts_level == 3)

gdp2 <- GDP_ERD %>% filter(nuts_level == 2)

gdp2[,-(1:4)] <- gdp2[,-(1:4)] / pop2[,-(1:4)]

gdp3 <- GDP_ERD %>% filter(nuts_level == 3)

gdp3[,-(1:4)] <- gdp3[,-(1:4)] / pop3[,-(1:4)]

## 2.2 Level 2 & 3 combined (if needed, note its absolut)

#gdp23 <- GDP_ERD %>% select(charcols, paste(period)) %>%
#  filter(!country %in% drop,
#         nuts_level == 2 |
#         nuts_level == 3
#         )

#pop23 <- POP_ERD %>% select(charcols, paste(period)) %>%
#  filter(!country %in% drop,
#         nuts_level == 2 |
#         nuts_level == 3
#         )

## 2.3 Define stuff for later use

n_0 <- length(table(pop2$country))
n_2 <- length(table(pop2$nuts_2))
k <- length(period)

rm(charcols)