### Try to replicate paper exactly

library(eurostat)
library(dplyr)
library(rgdal)
library(spdep)

### Data collection



############## Download ########################################################

dir.create("./RData")


### GDP (Achtung: total, nicht per capita) in 2005 prices

GDP_ERD <-read.table("./RData/NUTS3 GDP ERD fix.txt",
                     header=TRUE,
                     skip = 1,
                     sep = ";",
                     dec = ".",
                     stringsAsFactors = FALSE,
                     check.names=F,
                     colClasses=c("character", "character",
                                  rep("numeric", length(1980:2015))))


colnames(GDP_ERD)[1:2] <- c("nuts_level", "nuts_code")
GDP_ERD[GDP_ERD == "nuts 0"]<-0
GDP_ERD[GDP_ERD == "nuts 1"]<-1
GDP_ERD[GDP_ERD == "nuts 2"]<-2
GDP_ERD[GDP_ERD == "nuts 3"]<-3
GDP_ERD$nuts_code <- toupper(GDP_ERD$nuts_code)
GDP_ERD <- GDP_ERD[order(GDP_ERD$nuts_code),]
GDP_ERD <- GDP_ERD %>% mutate(country=substr(nuts_code, start = 1, stop = 2))
GDP_ERD <- GDP_ERD %>% mutate(nuts_2=substr(nuts_code, start = 1, stop = 4))

GDP_ERD <- GDP_ERD[,c(1,39,40,2,3:38)]
GDP_ERD[,-(1:4)] <- GDP_ERD[,-(1:4)]
#GDP_ERD[GDP_ERD == ".NaN"] <-NA

### Population

POP_ERD <-read.table("./RData/NUTS3 Population ERD fix.txt",
                     header=TRUE,
                     skip = 1,
                     sep = ";",
                     dec = ".",
                     stringsAsFactors = FALSE,
                     check.names=F,
                     colClasses=c("character", "character",
                                  rep("numeric", length(1980:2015))))

colnames(POP_ERD)[1:2] <- c("nuts_level", "nuts_code")
POP_ERD[POP_ERD == "nuts 0"]<-0
POP_ERD[POP_ERD == "nuts 1"]<-1
POP_ERD[POP_ERD == "nuts 2"]<-2
POP_ERD[POP_ERD == "nuts 3"]<-3
POP_ERD$nuts_code <- toupper(POP_ERD$nuts_code)
POP_ERD <- POP_ERD[order(POP_ERD$nuts_code),]
POP_ERD <- POP_ERD %>% mutate(country=substr(nuts_code, start = 1, stop = 2))
POP_ERD <- POP_ERD %>% mutate(nuts_2=substr(nuts_code, start = 1, stop = 4))

POP_ERD <- POP_ERD[,c(1,39,40,2,3:38)]
#POP_ERD[POP_ERD == ".NaN"] <-NA
#POP_ERD[,-2,POP_ERD != NA] <- POP_ERD[,-2, POP_ERD != NA]*1000
POP_ERD[,-(1:4)] <- POP_ERD[,-(1:4)]



###############################DEFINE#########################
#nonEU<-list("AL","CH","EF","EU","IS","ME","MK","NO","TR","LI")
#drop<-c("NO","MT","CY")
period<-c(1995:2008)
overseas <- c("FRA1", "FRA2", "FRA3", "FRA4", "FRZZ", "FRA5", "PT20", "PT30", "PTZZ", "ES70", "ESZZ")
#original paper:
orgctr <- c("BE","DK","DE","EL","ES","FR","IE","IT","NL","LU","AT","PT","FI","SE","UK","CZ","HU","PL","SK","EE","LT","LV","SI","BG","RO")
############################################################# 

### 2. The Transformation

charcols <- c("nuts_level", "country", "nuts_2", "nuts_code")

POP_ERD <- POP_ERD %>% select(charcols, paste(period)) %>%
  filter(country %in% orgctr)  %>%
  filter(!nuts_2 %in% overseas) 

GDP_ERD <- GDP_ERD %>% select(charcols, paste(period)) %>%
  filter(country %in% orgctr ) %>%
  filter(!nuts_2 %in% overseas)


GDP_ERD <- GDP_ERD %>%
  filter(!nuts_code %in% setdiff(GDP_ERD$nuts_code,POP_ERD$nuts_code))
POP_ERD <- POP_ERD %>%
  filter(!nuts_code %in% setdiff(POP_ERD$nuts_code,GDP_ERD$nuts_code))

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

nuts_2013 <- nuts_2013 %>% 
  mutate(country = substr(code2013,1,2)) %>%
  filter(country %in% orgctr) %>%
  select(-country)

# add the wishlist to the actual dataframe
# GDP

GDP_ERD <- left_join(nuts_2013,GDP_ERD, by = c("code2013" = "nuts_code"))

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

POP_ERD <- POP_ERD %>% filter(!code2013 %in% overseas)
GDP_ERD <- GDP_ERD %>% filter(!code2013 %in% overseas)

## 2.1 Single dataframes for gdp,pop on level 2 & 3


pop2 <- POP_ERD %>% filter(nuts_level == 2)

pop3 <- POP_ERD %>% filter(nuts_level == 3)

gdp2 <- GDP_ERD %>% filter(nuts_level == 2)

gdp2[,-(1:4)] <- gdp2[,-(1:4)] / pop2[,-(1:4)]

gdp3 <- GDP_ERD %>% filter(nuts_level == 3)

gdp3[,-(1:4)] <- gdp3[,-(1:4)] / pop3[,-(1:4)]


n_0 <- length(table(pop2$country))
n_2 <- length(table(pop2$nuts_2))
k <- length(period)

rm(charcols)


### Linear Regression

CV2 <- function(gdp2=NA,gdp3=NA,pop2=NA,pop3=NA){
  
  t <- sapply(gdp3, function(x) (x-gdp2)^2)
  t <- sqrt(sum(t*(pop3/pop2)))/gdp2
  return(t)
}

### 2. Abhängige und unabhängige Variablen

# Alle sind jeweils in einer Matrix, Zeilen sind geo_2, Spalten sind Jahre


## 2.1 y

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
    
    Y[i,j] <- CV2(gdp_2, gdp_3, pop_2, pop_3)
    
  }}

rm(gdp_2, gdp_3, pop_2, pop_3)

# extract CV_2013 from Y for shp13:
#Y13 <- as.data.frame(Y[,'2013'])
#Y13$nuts_2 <- rownames(Y13)

## 2.2 x_1

X_1 <- as.matrix(gdp2[-(1:4)])
rownames(X_1)<-unique(pop2$nuts_2)

## 2.3 x_2

X_2<- POP_ERD %>%
  filter(nuts_level == 3) %>%
  group_by(nuts_2) %>% 
  summarise(count=n())

X_2 <- X_2[!X_2$nuts_2 %in% overseas,]

X_2 <- data.matrix(X_2)
rownames(X_2)<-unique(pop2$nuts_2)

### 3. Die Regression

lm <- lapply(1:k, function(i) lm(Y[,i] ~ X_1[,i] + I(X_1[,i]^2) + X_2[,2]))

for (i in 1:k) {
  names(lm)[i] <- paste("lm_", period[i], sep = "")
  names(lm[[1]]$coefficients) <- c("(Intercept)", "Beta", "Gamma", "Delta")
}


## 3.1 How to access stuff:

# extract just coefficients
# sapply(lm, coef)

# if you need more info, get full summary call. now you can get whatever, like:
# summaries <- lapply(lm, summary)

# ...coefficents with p values:
# lapply(summaries, function(x) x$coefficients[, c(1,4)])

# ...or r-squared values
# sapply(summaries, function(x) c(r_sq = x$r.squared, 
# adj_r_sq = x$adj.r.squared))



## FIRST: Excluding regions where NUTS 2 = NUTS 3
  
zeros <- which(Y[,1]== 0)
  
Y_ex <- Y[-zeros,]
X_1ex <- X_1[-zeros,]
X_2ex <- X_2[-zeros,]

lmex <- lapply(1:k, function(i) lm(Y_ex[,i] ~ X_1ex[,i] + I(X_1ex[,i]^2) + X_2ex[,2]))
  
  for (i in 1:k) {
    names(lm)[i] <- paste("lm_", period[i], sep = "")
    names(lm[[1]]$coefficients) <- c("(Intercept)", "Beta", "Gamma", "Delta")
  }

sapply(lmex, coef)

## Second: using national average
Y_ex2<-Y

for (i in 1:length(Y[,1])) {
  
  if(Y[i,3] == 0){
    for (j in 1:length(Y[1,])) {
      Y_ex2[i,j] <- mean(Y[substr(rownames(Y),1,2)==substr(rownames(Y)[i],1,2),j])      
    }
  }
}

lmex2 <- lapply(1:k, function(i) lm(Y_ex2[,i] ~ X_1[,i] + I(X_1[,i]^2) + X_2[,2]))

sapply(lm, coef)
sapply(lmex, coef)
sapply(lmex2, coef)

length(unique(substr(row.names(Y),1,2)))

### Combining Data with Shapefile (2013)
#source("./R_skripte/Data_Transformation/combining_shapefiles_with_data.R")