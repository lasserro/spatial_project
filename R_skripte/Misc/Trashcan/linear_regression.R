# Das ist die lineare Regression, Seite 300. 

### 1. Die Funktion für CV_w

#function to calculate the weighted coefficient of variation
##where
#gdp3.......vector of gdp per capita at nuts_3
#gdp2.......scalar of average gdp per capita at nuts_2
#pop3.......vector of population at nuts_3
#pop2.......scalar of population at nuts_2


CV<-function(gdp2=NA,gdp3=NA,pop2=NA,pop3=NA){
  total<-0
  for (i in 1:length(gdp3)) {
    total<-total+
      ((gdp3[i]-gdp2[1])^2) *
      (pop3[i]/pop2[1])
  }
  total<-((total^(1/2))/(gdp2[1]))
  return(total)
}

### 2. Abhängige und unabhängige Variablen

# Alle sind jeweils in einer Matrix, Zeilen sind geo_2, Spalten sind Jahre


## 2.1 y

n <- length(unique(df$geo_2))
k <- length(unique(df$time))
Y<-matrix(NA,n,k)
colnames(Y)<-sort(unique(df$time))
rownames(Y)<-unique(df$geo_2)


for (i in 1:k) {
  t<-df %>%
    filter(time==sort(unique(df$time))[i]) %>%
    group_by(geo_2) %>%
    summarise(y=CV(
      gdp_2,
      gdp_3,
      pop_2,
      pop_3
    ))
  Y[,i] <- t(t[,2])
}


## 2.2 x_1

n <- length(unique(df$geo_2))
k <- length(unique(df$time))
X_1<-matrix(NA,n,k)
colnames(X_1)<-sort(unique(df$time))
rownames(X_1)<-unique(df$geo_2)

for (i in 1:k) {
  t<-nuts_2 %>%
    filter(time==sort(unique(df$time))[i]) %>%
    select(gdp_2)
  X_1[,i] <- t$gdp_2
}


## 2.3 x_2 (X_2 sollte jetzt stimmen)

n <- length(unique(df$geo_2))
k <- length(unique(df$time))
X_2<-matrix(NA,n,k)
colnames(X_2)<-sort(unique(df$time))
rownames(X_2)<-unique(df$geo_2)

for (i in 1:k) {
  t<-nuts_2 %>%
    filter(time==sort(unique(df$time))[i]) %>%
    select(freq)
  X_2[,i] <- t$freq
}

# alternative - sum nuts3 regions
# library(reshape2)
# X_2 <- df %>% group_by(geo_2, time) %>% summarise(n_geo3 = length(geo_3))
# X_2 <- acast(X_2, geo_2~time, value.var = "n_geo3")


### 3. Die Regression

lm <- lapply(1:k, function(i) lm(Y[,i] ~ X_1[,i] + I(X_1[,i]^2) + X_2[,i]))

for (i in 1:k) {
  names(lm)[i] <- paste("lm_", period[i], sep = "")
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

