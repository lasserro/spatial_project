To Dos:
0: Anmerkung zum ERD datafile. Die aktuelle Fassung (16.12) hat 
  - vollständige Daten von 1995:2015 (dehalb wird das paper auch die verwendet haben)
  - zusätzlich NORWEGEN drin (weils halt da is)
  - es fehlen alle nuts3 die gleich sind wie nuts 2 und alle nuts 2 die gleich sind wie nuts 1
  - deshalb fehlen ZYPERN und LUXEMBURG ganz
  - 6 Regionen in Deutschland_3 sind falsch benannt, check it!
  - müssen die Daten generell mal druchsschaun, ob die Transformation da nix zerstört haben

Die Daten sind da, aber wir kriegen sie mit dem aktuellen code nicht rein. 
  
1: warum gibts die komischen number of obs changes?
  #The changes are:
  #DE, 2011
  #FR, 2013
  #PL, 2010
  #########Anmerkung, liste nicht aktuell
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

9. Check den Datentransformationsprozess
Vor allem auch column names ob die eh passen

10. CV Funktion verfeinern?
    
    Für die Fälle, in denen Nuts2=Nuts3 ist sollte unsere CV Funktion Y = 0 ausspucken
    summary(Y==0)
    
11. Datatransformation

    bei der erstellung der Shapefiles kommen noch extra einschränkungen (z.b !Overseas) rein,
    die unser datenset noch weiter einschränken, so das es nicht mehr mit der jetzingen linear 
    regression übereinstimmt. 
    Datenselektion im Transformationfile so weit einschränken, dass im shapefile nicht mehr gepfuscht
    werden muss?
    
