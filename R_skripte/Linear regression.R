
#Das ist ein Anfang für die lineare Regression, Seite 300. Werd aber noch
#schaun, dass ichs für ein einzelnes Datenset schreib, dann is besser.



#function to calculate the coefficient of variation
##where
#GDP_3.......vector of gdp per capita at nuts_3
#GDPmean_2...scalar of average gdp per capita at nuts_2
#POP_3.......vector of population at nuts_3
#POP_2.......scalar of population at nuts_2

CV<-function(GDP_3,GDPmean_2,POP_3,POP_2){
  total<-0
  for (i in 1:length(GDP_3)) {
    total<-total+((GDP_3[i]-GDPmean_2)^2 * (POP_3[i]/POP_2))
  }
  return(total) 
}

x<-CV(GDP_3,GDPmean_2,POP_3,POP_2)


######################################################
#Linear regression model
###### y = \alpha + \beta x_1 + \gamma (x_2)^2 + \delta x_2 + \epsilon
#where
#y......vector of CVs
#x_1....vector of per capita GDP levels
#x_2....numbers of nuts 3 regions in each nuts 2 region

bal<-function(a,b){
  c<-a[5]+b[7]
  return(c)
}


xx<-nuts_3 %>%
  filter(country=="AT") %>%
#  select(pop) %>%
#  collect()
   summarise(blub = bal(pop,gdp))  

y %>% nuts_3 %>%
  filter(time=="2010",geo=="AT") %>%
  summarise()


y<-for (i in 1:length(unique(nuts_2$geo))) {
  CV(nuts_3$geo==nuts_2$geo[i]
    
    
  )
  
  
  
  
  
  CV(gdp3$values[gdp3$geo==unique(pop2$geo)[i]],
     gdp2$values[gdp2$geo==unique(pop2$geo)[i]],
     pop3$values[pop3$geo==unique(pop2$geo)[i]],
     pop2$values[pop2$geo==unique(pop2$geo)[i]]
     )
}

i<-1

gdp3$values[gdp3$geo==unique(pop2$geo)[i]]

unique(gdp3$geo==unique(pop2$geo)[i])
