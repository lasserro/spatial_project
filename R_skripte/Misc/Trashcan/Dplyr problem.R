
###################################################################
#######WARUM ist xx nicht gleich x???????????!#####################
x<-df %>%
  filter(time=="2010" , geo_2==c("AT11","HR03"))
# group_by(geo_2) %>%
# group_by(geo_3)%>%

xx<-df %>%
  filter(time=="2010" , geo_2=="AT11"| geo_2=="HR03")
# group_by(geo_2) %>%
# group_by(geo_3)%>%

######################################################
#Linear regression model
###### y = \alpha + \beta x_1 + \gamma (x_2)^2 + \delta x_2 + \epsilon
#where
#y......vector of CVs
#x_1....vector of per capita GDP levels
#x_2....numbers of nuts 3 regions in each nuts 2 region
