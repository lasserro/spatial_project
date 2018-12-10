To Dos:
  
1: warum gibts die komischen number of obs changes?
  #The changes are:
  #DE, 2011
  #FR, 2013
  #PL, 2010
  Und ist das überhaupt ein Problem?
  Und warum nur in Pop nicht in Gdp???!
  
  siehe Skript "missing_nuts3_regions"
  im schlimmsten Fall müssen wir max. 50 regionen skippen. dann müssen wir aber 
  wirklich mit k-nearest arbeiteten, da wir dadurch 'Löcher' erzeugen
  
2: Was ist das "statistische" Problem mit Zypern, malta aus dem paper?
   
       es kann sein, dass gleich wie bei Luxemburg (LU), Cypern (CY) wegen 
       der Tatsache, dass NUTS1=NUTS3 ist ein Problem darstellt.
  
3: Woher haben die die Daten für Dänemark?
  
  https://www.camecon.com/
  
4: Woher haben die die Daten für prä 2000 überhaupt?
  https://www.camecon.com/
  
5. Combine with shapefiles!
  
  
6. in den Nuts3 daten von Eurostat sind auch die Nuts2 daten drin. Sind diese ident mit dem EUrostat Nuts2 datenset?
  CHECK Yesss, the are (i think)
  
  
7. WTF is going on with the UK????! So much GDP (observations!)

   UK hat 174 Nuts3 regions, über 14 jahre kommen da schon >2000 observationen zusammen

8. Wir haben beim GDP 6 verschiedene units, which to use?

  MIO_EUR Millions of euro /Millions of
ECU 

MIO_PPS Millions of PPS (Purchasing Power Standard)

PPS_HAB Purchasing Power Standard per
inhabitant

PPS_HAB_EU Purchasing Power Standard per inhabitant
in percentage of the EU average

EUR_HAB Euro per inhabitant  # würde den nehmen, müssen aber checken, wie nicht-Euro Länder hier abgebildet sind. wurde schon umgerechnet von der landeswährung etc??

EUR_HAB_EU Euro per inhabitant in percentage of the EU
average 

source: https://ec.europa.eu/eurostat/cache/metadata/Annexes/reg_educ_esms_an1.pdf
