### 1. Definitions

#In the following we have two specifications for the dataset, first a maximal set, 
# with all countries and periods which we used to assess which ones we will keep in the end. 
# The min-specification which we ended up using later on includes all countries from 
# the ERD, with few exceptions: 
#--Malta, Cyprys: stastical reasons (Nuts 2 = Nuts 3 = Nuts 1)
#--Norway, (we concentrate on EU)
#--CHECK

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
  period<-c(1996:2015)
  overseas <- c("FRA1", "FRA2", "FRA3", "FRA4", "FRZZ", "FRA5", "PT20", "PT30", "PTZZ", "ES70", "ESZZ")
  
  ############################################################## 
} 

### 2. The Transformation


## 2.01 Crop dataset to specification above

charcols <- c("nuts_level", "country", "nuts_2", "nuts_code")

POP_ERD <- POP_ERD %>% select(charcols, paste(period)) %>%
  filter(!country %in% drop)  %>%
  filter(!nuts_2 %in% overseas) 

GDP_ERD <- GDP_ERD %>% select(charcols, paste(period)) %>%
  filter(!country %in% drop ) %>%
  filter(!nuts_2 %in% overseas)

## At this point, our datasets for population and GDP include same regions except 
# for 6 regions in Germany which are being dropped in the following code.

GDP_ERD <- GDP_ERD %>%
  filter(!nuts_code %in% setdiff(GDP_ERD$nuts_code,POP_ERD$nuts_code))
POP_ERD <- POP_ERD %>%
  filter(!nuts_code %in% setdiff(POP_ERD$nuts_code,GDP_ERD$nuts_code))

#############################################################################
########Achtung, an diesem Punkt haben wir NAs für Kroatien 1995 (Krieg) drop?
#############################################################################

## 2.02 Fill in neglected regions 

# The ERD leaves out regions if the nuts level above is the same.
# (e.g. with AT13 & AT130, AT130 is not included).This nonsensical approach forces 
# to use a little hack: This takes the official NUTS 2013 classification and 
# fills in the missing values.
# 1. Join the datasets with the official NUTS specification
# 2. This creates missing values for all neglected regions in the ERD-database
# 3. Fill missing values with the regions that are a level above.


# get the official nuts 2013 classification

nuts_2013 <- read.table("./RData/NUTS2013 all.csv",
                        header = TRUE,
                        sep = "\t",
                        skip = 1,
                        comment.char = ";",
                        check.names = FALSE,
                        stringsAsFactors = FALSE)

colnames(nuts_2013) <- "code2013"

# get rid of all (useless) z, zz, zzz. These are extraregions not included in 
# the ERD

nuts_2013 <- nuts_2013 %>% mutate (extra = substring(code2013,3)) %>%
  filter(!extra %in% "Z",
         !extra %in% "ZZ",
         !extra %in% "ZZZ"
  ) %>% 
  select(-extra)

# add this "wishlist" to the actual dataframe
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


rm(charcols)

###### kick 4 regions
pop2 <- pop2 %>% filter(! nuts_2 %in% overseas)
pop3 <- pop3 %>% filter(! nuts_2 %in% overseas)
gdp2 <- gdp2 %>% filter(! nuts_2 %in% overseas)
gdp3 <- gdp3 %>% filter(! nuts_2 %in% overseas)


## 2.3 Define stuff for later use

n_0 <- length(table(pop2$country))
n_2 <- length(table(pop2$nuts_2))
k <- length(period)



