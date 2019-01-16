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
  drop<-c("NO","LU","CY")
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
#and remove overseas 

nuts_2013 <- nuts_2013 %>% mutate (extra = substring(code2013,3)) %>%
  filter(!extra %in% "Z",
         !extra %in% "ZZ",
         !extra %in% "ZZZ",
         !substr(code2013, start = 1, stop = 4) %in% overseas
  ) %>% 
  select(-extra)

# add this "wishlist" to the actual dataframe

## GDP

GDP_ERD <- left_join(nuts_2013,GDP_ERD, by = c("code2013" = "nuts_code"))

# and fill in missing values accordingly

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

## 2.1 Single dataframes for gdp,pop on level 2 & 3


pop2 <- POP_ERD %>% filter(nuts_level == 2)

pop3 <- POP_ERD %>% filter(nuts_level == 3)

gdp2 <- GDP_ERD %>% filter(nuts_level == 2)

gdp2[,-(1:4)] <- gdp2[,-(1:4)] / pop2[,-(1:4)]

gdp3 <- GDP_ERD %>% filter(nuts_level == 3)

gdp3[,-(1:4)] <- gdp3[,-(1:4)] / pop3[,-(1:4)]


rm(nuts_2013,charcols)

#Define:
#number of Nuts_2 regions in dataset:
n_2 <- length(table(pop2$nuts_2))
#number of observed years: 
k <- length(period)


### 1. Die Funktion für CV_w

#function to calculate the weighted coefficient of variation
##where
#gdp3.......vector of gdp per capita at nuts_3
#gdp2.......scalar of average gdp per capita at nuts_2
#pop3.......vector of population at nuts_3
#pop2.......scalar of population at nuts_2

CV <- function(gdp2=NA,gdp3=NA,pop2=NA,pop3=NA){
  
  t <- sapply(gdp3, function(x) (x-gdp2)^2)
  t <- sqrt(sum(t*(pop3/pop2)))/gdp2
  return(t)
}


### 2. Create variables for the regression


## 2.1 Y - The weighted coefficient of variation

Y<-matrix(NA,n_2,k)
colnames(Y)<-period
rownames(Y)<-unique(pop2$nuts_2)

for (j in 1:k) {
  
  for (i in 1:n_2) {
    
    gdp_2 <- gdp2[i,j+4]
    
    pop_2 <- pop2[i,j+4]
    
    gdp_3 <- gdp3 %>% filter(nuts_2 == gdp2[i,"nuts_2"])
    gdp_3 <- gdp_3[,j+4]
    
    pop_3 <- pop3 %>% filter(nuts_2 == gdp2[i,"nuts_2"])
    pop_3 <- pop_3[,j+4]
    
    Y[i,j] <- CV(gdp_2, gdp_3, pop_2, pop_3)
    
  }}

# If Nuts 2 == Nuts 3, the CV is 0.
# we use the same approach as the paper and substitute with the national average
# of interregional inequality

for (i in 1:length(Y[,1])) {
  
  if(Y[i,3] == 0){
    for (j in 1:length(Y[1,])) {
      Y[i,j] <- mean(Y[substr(rownames(Y),1,2)==substr(rownames(Y)[i],1,2),j])      
    }
  }
}

rm(gdp_2, gdp_3, pop_2, pop_3)

## 2.2 x_1: GDP per capita per for each nuts_2 region 

X_1 <- as.matrix(gdp2[-(1:4)])
rownames(X_1)<-unique(pop2$nuts_2)

## 2.3 x_2: number of nuts_3 region in each nuts_2 region

X_2<- POP_ERD %>%
  filter(nuts_level == 3) %>%
  filter(!nuts_2 %in% overseas) %>%
  group_by(nuts_2) %>% 
  summarise(count=n()) 

X_2 <- data.matrix(X_2)
rownames(X_2)<-unique(pop2$nuts_2)
#X_2 <-X_2[,-1]


### Combine with shapefiles

# reduce shape file and the data shp2 to our dataset
shp <- shp2[shp2$NUTS_ID %in% pop2$nuts_2, ]
# test
#setdiff(shp$NUTS_ID, pop2$nuts_2) # alright

#prepare data
shp_list <- vector('list', 20)
Y0 <- as.data.frame(Y)
Y0$'nuts_2' <- rownames(Y0)
X_1df <- as.data.frame(X_1)
X_1df$'nuts_2' <- rownames(X_1df)
X_2df <- as.data.frame(X_2)
X_2df$'nuts_2' <- rownames(X_2df)

#and merge
for (i in 1:20) {
  shp_list[i] <- merge(shp, pop2[, c(4, i+4)], all.x= T, all.y= F, by.x= 'NUTS_ID', by.y='nuts_2')
  colnames(shp_list[[i]]@data)[6] <- 'Pop'
  shp_list[[i]] <- merge(shp_list[[i]], gdp2[,c(4, i+4)], all.x= T, all.y= F, by.x= 'NUTS_ID', by.y='nuts_2')
  colnames(shp_list[[i]]@data)[7] <- 'Gdp'
  shp_list[[i]] <- merge(shp_list[[i]], Y0[,c(i, 21)], all.x= T, all.y= F, by.x= 'NUTS_ID', by.y='nuts_2')
  colnames(shp_list[[i]]@data)[8] <- 'Y'
  shp_list[[i]] <- merge(shp_list[[i]], X_1df[,c(i, 21)], all.x= T, all.y= F, by.x= 'NUTS_ID', by.y='nuts_2')
  colnames(shp_list[[i]]@data)[9] <- 'X_1'
  shp_list[[i]] <- merge(shp_list[[i]], X_2df, all.x= T, all.y= F, by.x= 'NUTS_ID', by.y='nuts_2')
  colnames(shp_list[[i]]@data)[10] <- 'X_2'
}
# The following warning is produced: implicit list embedding of S4 objects is deprecated.
# As the code seems to be working we ignored it.

rm(Y0, X_1df, X_2df)

#get coordinates from the final dataset (it is VERY important to this at this stage,
# if you do it with the shapefiles alone, it renders all results useless.)
coords <- coordinates(shp_list[[1]])

#and get weights lists and matrices 
k.near <- knearneigh(coords, k=kn) #indexing neighbors based on k=5
kn <- knn2nb(k.near) #creating neighborhood list based on the k(5) nearest neighbors
W.list.k <- nb2listw(kn, style = "W", zero.policy = FALSE) #creating a weights-list
W.mat <- listw2mat(W.list.k) #creates a weigths matrix
