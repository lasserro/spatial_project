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
Y13 <- as.data.frame(Y[,'2013'])
Y13$nuts_2 <- rownames(Y13)

## 2.2 x_1

X_1 <- as.matrix(gdp2[-(1:4)])
rownames(X_1)<-unique(pop2$nuts_2)

## 2.3 x_2

X_2<- POP_ERD %>%
  filter(nuts_level == 3) %>%
  group_by(nuts_2) %>% 
  summarise(count=n())

X_2 <- data.matrix(X_2)
rownames(X_2)<-unique(pop2$nuts_2)


# X_2 <- X_2[,2]
# X_2.1 <-matrix(X_2,n_2,k)
# colnames(X_2.1)<-period
# rownames(X_2.1)<-unique(pop2$nuts_2)

### 3. Die Regression

lm <- lapply(1:k, function(i) lm(Y[,i] ~ X_1[,i] + I(X_1[,i]^2) + X_2[,2]))

for (i in 1:k) {
  names(lm)[i] <- paste("lm_", period[i], sep = "")
  names(lm[[1]]$coefficients) <- c("(Intercept)", "Beta", "Gamma", "Delta")
}


#names(lm)[i]
#names(lm[1]$coefficients)

#lm


#<- lm(Y[,1] ~ X_1[,1] + I(X_1[,1]^2) + X_2[,2])



#names(fit$coefficients) <- c('bob','sally','sue'

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

