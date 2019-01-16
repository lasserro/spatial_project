
#Versuch für AT11 und 2010, scheint zu funken
df %>%
  filter(time=="2010", geo_2=="AT11") %>%
  summarise(y=CV(
    gdp_2,
    gdp_3,
    pop_2,
    pop_3
  ))

#Versuch das Gleiche nachzubauen
pop2<-283697
pop3<-c(37526,148576,97595)
gdp2<-23600
gdp3<-c(19700,25800,21700)


((
  (gdp3[1]-gdp2[1])^2*(pop3[1]/pop2[1])+
    (gdp3[2]-gdp2[1])^2*(pop3[2]/pop2[1])+
    (gdp3[3]-gdp2[1])^2*(pop3[3]/pop2[1])
)^(1/2))/(gdp2[1])

#Scheint zu passen, aber die Funktion spuckt gerundete Werte aus! (ABER nicht
#mehr in summarise von dplyr, alles gut)


#Jetzt versuch mas mal für ein ganzes Jahr


#Das problem is wir muessten für geo2 und geo3 groupen...hm WORX!!!!!!!!!!


x<-df %>%
  filter(time=="2010", geo_2=="AT11" | geo_2=="HR03") %>%
  group_by(geo_2) %>%
  summarise(y=CV(
    gdp_2,
    gdp_3,
    pop_2,
    pop_3
  ))


#aber nur für zwei...


x<-df %>%
  filter(time=="2010") %>%
  group_by(geo_2) %>%
  summarise(y=CV(
    gdp_2,
    gdp_3,
    pop_2,
    pop_3
  ))


#scheint zu funktionieren, gibt einige 0s, z.B. für Wien (nuts2==nuts3),
#eh kloar? Problem?


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

