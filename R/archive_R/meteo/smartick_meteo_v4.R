######
###### Comparaison des données Météo MF et DSK

# 1. Introduction
### Alloue la librairie "set()" puis vérifie que c'est bien importé "getwd()", il pointe dessus!

## 1.1 XPS13
setwd("C:/3VG/MSH/lyme/Smartick17/pycitique/R/meteo")
getwd()

#Remove all objects
rm(list = ls() )
rm()

## 1.2 OP7570
setwd("E:/3VG/MSH/lyme/Smartick17/pycitique/R/meteo")
getwd()

# 2. Lab 1 : Importation de données darksky_moyennes_journalieres_maille_42_vg.csv et mf_moyennes_journalieres_42_vg.csv (issues de 42 stations météo de MF)

### Or, if .csv file, use this si données séparées par ";"
DSKdata <- read.csv("../../data/donnee_meteo_nationale_comparative/comparaison/darksky_moyennes_journalieres_maille_42_vg.csv", header = TRUE, sep = ";", dec = ".")

MFdata <- read.csv("../../data/donnee_meteo_nationale_comparative/comparaison/mf_moyennes_journalieres_42_vg.csv", header = TRUE, sep = ";", dec = ".")

## 2.1 Analyse du tableau de données

ls(DSKdata) ## liste les variables
ls(MFdata)


str(DSKdata) ##  a diagnostic function and an alternative to summary
str(MFdata)


summary(DSKdata)
summary(MFdata)



## 2.2 Analyse des Températures moyennes DSK vs MF (méthode Alice Favre)

### 2.2.1. Histogramme des Températures moyennes "temperature" pour MF et "temperature" pour DSK = (tempHigh + tempLow)/2 pour 42 stations Météo France

####Après avoir vérifié que le nombre d'obs n'est pas égal avec 
length(DSKdata$temperature)
length(MFdata$temperature)

##### et que l'étendue n'est pas la même
range(DSKdata$temperature, na.rm = TRUE)
range(MFdata$temperature, na.rm = TRUE)

#### on définit les breaks pour l'abscisse commune
BRt <- seq(from= -5, to= 30, by=2) ## tient compte des deux distributions

#### puis on fait l'histo en utilisant 

### freq=F => des fréquences relatives et pas des effectifs

hist(DSKdata$temperature, breaks = BRt,
     freq=F, # fréquences
     col="grey",
     main = "Mean Temperatures for 42 Météo France Synoptic Stations \n (France, july 2017 - february 2020), 1 191 days",
     ylab = "Densities",
     xlab = "Mean Temperatures (T°C)"
     )

### calcul des paramètres pour la fonction lines() à superposer à l'histo

HH2 <- hist(MFdata$temperature, breaks = BRt,  plot=F)
    
    #lines(HH2$mids, HH2$density, lwd = 2, col = "orange") ### courbe non lissée  ## SO
    lines(density(MFdata$temperature, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel

text(03, 0.07, paste("Darksky"), cex = 1.2,  col = "black")
text(02, 0.06, paste("Météo France"), cex = 1.2,  col = "blue")

## Si on voulait le nb d'enregistrements tudiés à la place du texte "Darksky" ou MF, il faudrait :
    ### text(03, 0.07, paste("N =", sum(complete.cases(DSKdata$temperature))), cex = 1.2,  col = "black")
    ### text(02, 0.06, paste("N =", sum(complete.cases(MFdata$temperature))), cex = 1.2,  col = "blue")

## Si on voulait les courbes à +1.5°C ou +2°C, il faudrait fabriquer et afficher :
    ### lines(density(MFdata$temperature_p15D, na.rm = TRUE), lwd = 2, col = "green") ### courbe liss?e, kernel
    ### lines(density(MFdata$temperature_p2D, na.rm = TRUE), lwd = 2, col = "blue") ### courbe liss?e, kernel
    

### 2.2.2. Test des distributions
## References :
## Patrick Royston (1982). An extension of Shapiro and Wilk's W test for normality to large samples. Applied Statistics, 31, 115–124. doi: 10.2307/2347973.

shapiro.test(DSKdata$temperature) # W = 0.97673, p-value = 6.398e-13 => p-value significative, l'échantillon ne suit pas une loi normale.

shapiro.test(MFdata$temperature) # W = 0.97696, p-value = 7.649e-13 => p-value significative, l'échantillon ne suit pas une loi normale.


### t.test entre les différentes températures
    t.test(MFdata$temperature,DSKdata$temperature) # t = -1.6278, df = 2379.9, p-value = 0.1037 => dommage, p-value non significative, les 2 échantillons ne sont pas significativement différents !
    
    # t.test(MFdata$temperature_p1D,DSKdata$temperature) ## SO
    # t.test(MFdata$temperature_p15D,DSKdata$temperature) ## SO
    # t.test(MFdata$temperature_p2D,DSKdata$temperature) ## SO
    
## References :
## Myles Hollander and Douglas A. Wolfe (1973), Nonparametric Statistical Methods. New York: John Wiley & Sons. Pages 115–120.

## http://www.sthda.com/english/wiki/kruskal-wallis-test-in-r
    
### Import des données pour le test kruskal.test() sur données MFdata$temperature, DSKdata$temperature in : KW_temp_maille_42.csv
    kwtempdata <- read.csv("../../data/donnee_meteo_nationale_comparative/comparaison/KW_temp_maille_42.csv", header = TRUE, sep = ";", dec = ".")
    summary(kwtempdata)
    
    kruskal.test(temp ~ type, data = kwtempdata) ## ~ signifie : "en fonction de"
    
    ## Kruskal-Wallis chi-squared = 2.5559, df = 1, p-value = 0.1099 => p-value non significative, les 2 échantillons ne sont pas significativement différents !
    
    
## 2.3. Analyse de l'humidité moyenne DSK vs MF (méthode Alice Favre)


### 2.3.1. Histogramme de l'humidité moyenne "humidite" pour MF et "humidity" pour DSK pour 42 stations Météo France
    
####Après avoir vérifié que le nombre d'obs n'est pas égal avec 
    length(DSKdata$humidity)
    length(MFdata$humidite)
    
##### et que l'étendue n'est pas la même
    range(DSKdata$humidity, na.rm = TRUE)
    range(MFdata$humidite, na.rm = TRUE)
    
#### on définit les breaks pour l'abscisse commune
    BRt <- seq(from= 45, to= 100, by=2) ## tient compte des deux distributions
    
#### puis on fait l'histo en utilisant 
    
### freq=F => des fréquences relatives et pas des effectifs
    
    hist(DSKdata$humidity, breaks = BRt,
         freq=F, # fréquences
         col="grey",
         main = "Mean Humidity for 42 Météo France Synoptic Stations \n (France, july 2017 - february 2020), 1 191 days",
         ylab = "Densities",
         xlab = "Mean Humidity (%)"
    )
    
### calcul des paramètres pour la fonction lines() à superposer à l'histo
    
    HH2 <- hist(MFdata$humidite, breaks = BRt,  plot=F)
    
#lines(HH2$mids, HH2$density, lwd = 2, col = "orange") ### courbe non lissée  ## SO
    lines(density(MFdata$humidite, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel
    
    text(60, 0.04, paste("Darksky"), cex = 1.2,  col = "black")
    text(55, 0.03, paste("Météo France"), cex = 1.2,  col = "blue")
    
## Si on voulait le nb d'enregistrements tudiés à la place du texte "Darksky" ou MF, il faudrait :
    ### text(03, 0.07, paste("N =", sum(complete.cases(DSKdata$humidity))), cex = 1.2,  col = "black")
    ### text(02, 0.06, paste("N =", sum(complete.cases(MFdata$humidite))), cex = 1.2,  col = "blue")
    
## Si on voulait les courbes à +1.5°C ou +2°C, il faudrait fabriquer et afficher :
    ### lines(density(MFdata$humidite_p15D, na.rm = TRUE), lwd = 2, col = "green") ### courbe liss?e, kernel
    ### lines(density(MFdata$humidite_p2D, na.rm = TRUE), lwd = 2, col = "blue") ### courbe liss?e, kernel
    
    
### 2.3.2. Test des distributions
## References :
## Patrick Royston (1982). An extension of Shapiro and Wilk's W test for normality to large samples. Applied Statistics, 31, 115–124. doi: 10.2307/2347973.
    
    shapiro.test(DSKdata$humidity) # W = 0.97679, p-value = 6.718e-13 => p-value significative, l'échantillon ne suit pas une loi normale.
    
    shapiro.test(MFdata$humidite) # W = 0.97952, p-value = 6.121e-12 => p-value significative, l'échantillon ne suit pas une loi normale.
    
    
### t.test entre les différentes températures
    t.test(MFdata$humidite,DSKdata$humidity) # t = 2.9604, df = 2370.3, p-value = 0.003103 => p-value significative, les 2 échantillons sont significativement différents !
    
    # t.test(MFdata$humidite_p1D,DSKdata$humidity) ## SO
    # t.test(MFdata$humidite_p15D,DSKdata$humidity) ## SO
    # t.test(MFdata$humidite_p2D,DSKdata$humidity) ## SO
    
## References :
## Myles Hollander and Douglas A. Wolfe (1973), Nonparametric Statistical Methods. New York: John Wiley & Sons. Pages 115–120.
    
## http://www.sthda.com/english/wiki/kruskal-wallis-test-in-r
    
### Import des données pour le test kruskal.test() sur données MFdata$humidite, DSKdata$humidity in : KW_temp_maille_42.csv
    kwhumdata <- read.csv("../../data/donnee_meteo_nationale_comparative/comparaison/KW_hum_maille_42.csv", header = TRUE, sep = ";", dec = ".")
    summary(kwhumdata)
    
    kruskal.test(temp ~ type, data = kwhumdata) ## ~ signifie : "en fonction de"
    
    ## Kruskal-Wallis chi-squared = 9.2389, df = 1, p-value = 0.002369 => p-value significative, les 2 échantillons sont significativement différents !
    

