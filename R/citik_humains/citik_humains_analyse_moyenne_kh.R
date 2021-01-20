######
###### Comparaison des données Météo MF et DSK smartick_meteo_v5

# Définition du répertoire de travail
getwd()
setwd("./")

# Importation de la donnée comparative météo-France (MF) et darksky (DSK)
# La donnée MF est issue de 42 stations synoptiques réparties sur le territoire nationale
# La donnée DSK est issue de la même coordonnée de station mais avec des extractoins issues des dépôt de darksky

dskdatavg<- read.csv("../../data/donnee_meteo_nationale_comparative/darksky/darksky_moyennes_journalieres_maille_42.csv", header = TRUE, sep = ",", dec = ".")
mfdatavgvg <- read.csv("../../data/donnee_meteo_nationale_comparative/meteoFrance/mf_moyennes_journalieres_maille_42.csv", header = TRUE, sep = ",", dec = ".")

## vérification des jeux de donnée
## liste les variables
ls(dskdatavg)
ls(mfdatavg)

##sommaire de la donnée et analyse primaire
summary(dskdatavg)
summary(mfdatavg)


## 2.2 Analyse des Températures moyennes DSK vs MF (méthode Alice Favre)

### 2.2.1. Histogramme des Températures moyennes "temperature" pour MF et "temperature" pour DSK = (tempHigh + tempLow)/2 pour 42 stations Météo France

####Après avoir vérifié que le nombre d'obs n'est pas égal avec 
length(dskdatavg$temperature)
length(na.omit(dskdatavg$temperature)) ## pour tester si NA
length(mfdatavg$temperature)
length(na.omit(mfdatavg$temperature)) ## pour tester si NA


##### et que l'étendue n'est pas la même
range(dskdatavg$temperature, na.rm = TRUE)
range(mfdatavg$temperature, na.rm = TRUE)

#### on définit les breaks pour l'abscisse commune
BRt <- seq(from= -5, to= 30, by=2) ## tient compte des deux distributions

#### puis on fait l'histo en utilisant 

### freq=F => des fréquences relatives et pas des effectifs

hist(dskdatavg$temperature, breaks = BRt,
     freq=F, # fréquences
     col="grey",
     main = "Mean Temperatures for 42 Météo France Synoptic Stations \n (France, july 2017 - february 2020), 1 191 days",
     ylab = "Densities",
     xlab = "Mean Temperatures (T°C)"
)

### calcul des paramètres pour la fonction lines() à superposer à l'histo

HH2 <- hist(mfdatavg$temperature, breaks = BRt,  plot=F)

#lines(HH2$mids, HH2$density, lwd = 2, col = "orange") ### courbe non lissée  ## SO
lines(density(mfdatavg$temperature, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel

text(03, 0.07, paste("Darksky"), cex = 1.2,  col = "black")
text(02, 0.06, paste("Météo France"), cex = 1.2,  col = "blue")

## Si on voulait le nb d'enregistrements tudiés à la place du texte "Darksky" ou MF, il faudrait :
### text(03, 0.07, paste("N =", sum(complete.cases(dskdatavg$temperature))), cex = 1.2,  col = "black")
### text(02, 0.06, paste("N =", sum(complete.cases(mfdatavg$temperature))), cex = 1.2,  col = "blue")

## Si on voulait les courbes à +1.5°C ou +2°C, il faudrait fabriquer et afficher :
### lines(density(mfdatavg$temperature_p15D, na.rm = TRUE), lwd = 2, col = "green") ### courbe liss?e, kernel
### lines(density(mfdatavg$temperature_p2D, na.rm = TRUE), lwd = 2, col = "blue") ### courbe liss?e, kernel


### 2.2.2. Test des distributions
## References :
## Patrick Royston (1982). An extension of Shapiro and Wilk's W test for normality to large samples. Applied Statistics, 31, 115–124. doi: 10.2307/2347973.

shapiro.test(dskdatavg$temperature) # W = 0.97673, p-value = 6.398e-13 => p-value significative, l'échantillon ne suit pas une loi normale.

shapiro.test(mfdatavg$temperature) # W = 0.97696, p-value = 7.649e-13 => p-value significative, l'échantillon ne suit pas une loi normale.


### t.test entre les différentes températures
t.test(mfdatavg$temperature,dskdatavg$temperature) # t = -1.6278, df = 2379.9, p-value = 0.1037 => dommage, p-value non significative, les 2 échantillons ne sont pas significativement différents !

# t.test(mfdatavg$temperature_p1D,dskdatavg$temperature) ## SO
# t.test(mfdatavg$temperature_p15D,dskdatavg$temperature) ## SO
# t.test(mfdatavg$temperature_p2D,dskdatavg$temperature) ## SO

## References :
## Myles Hollander and Douglas A. Wolfe (1973), Nonparametric Statistical Methods. New York: John Wiley & Sons. Pages 115–120.

## http://www.sthda.com/english/wiki/kruskal-wallis-test-in-r

### Import des données pour le test kruskal.test() sur données mfdatavg$temperature, dskdatavg$temperature in : KW_temp_maille_42.csv
kwtempdata <- read.csv("../../data/donnee_meteo_nationale_comparative/comparaison/KW_temp_maille_42.csv", header = TRUE, sep = ";", dec = ".")
summary(kwtempdata)

kruskal.test(temp ~ type, data = kwtempdata) ## ~ signifie : "en fonction de"

## Kruskal-Wallis chi-squared = 2.5559, df = 1, p-value = 0.1099 => p-value non significative, les 2 échantillons ne sont pas significativement différents !


## 2.3. Analyse de l'humidité moyenne DSK vs MF (méthode Alice Favre)


### 2.3.1. Histogramme de l'humidité moyenne "humidite" pour MF et "humidity" pour DSK pour 42 stations Météo France

####Après avoir vérifié que le nombre d'obs n'est pas égal avec 
length(dskdatavg$humidity)
length(na.omit(dskdatavg$humidity)) ## pour tester si NA
length(mfdatavg$humidite)
length(na.omit(mfdatavg$humidite)) ## pour tester si NA


##### et que l'étendue n'est pas la même
range(dskdatavg$humidity, na.rm = TRUE)
range(mfdatavg$humidite, na.rm = TRUE)

#### on définit les breaks pour l'abscisse commune
BRt <- seq(from= 45, to= 100, by=2) ## tient compte des deux distributions

#### puis on fait l'histo en utilisant 

### freq=F => des fréquences relatives et pas des effectifs

hist(dskdatavg$humidity, breaks = BRt,
     freq=F, # fréquences
     col="grey",
     main = "Mean Humidity for 42 Météo France Synoptic Stations \n (France, july 2017 - february 2020), 1 191 days",
     ylab = "Densities",
     xlab = "Mean Humidity (%)"
)

### calcul des paramètres pour la fonction lines() à superposer à l'histo

HH2 <- hist(mfdatavg$humidite, breaks = BRt,  plot=F)

#lines(HH2$mids, HH2$density, lwd = 2, col = "orange") ### courbe non lissée  ## SO
lines(density(mfdatavg$humidite, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel

text(60, 0.04, paste("Darksky"), cex = 1.2,  col = "black")
text(55, 0.03, paste("Météo France"), cex = 1.2,  col = "blue")

## Si on voulait le nb d'enregistrements tudiés à la place du texte "Darksky" ou MF, il faudrait :
### text(03, 0.07, paste("N =", sum(complete.cases(dskdatavg$humidity))), cex = 1.2,  col = "black")
### text(02, 0.06, paste("N =", sum(complete.cases(mfdatavg$humidite))), cex = 1.2,  col = "blue")

## Si on voulait les courbes à +1.5°C ou +2°C, il faudrait fabriquer et afficher :
### lines(density(mfdatavg$humidite_p15D, na.rm = TRUE), lwd = 2, col = "green") ### courbe liss?e, kernel
### lines(density(mfdatavg$humidite_p2D, na.rm = TRUE), lwd = 2, col = "blue") ### courbe liss?e, kernel


### 2.3.2. Test des distributions
## References :
## Patrick Royston (1982). An extension of Shapiro and Wilk's W test for normality to large samples. Applied Statistics, 31, 115–124. doi: 10.2307/2347973.

shapiro.test(dskdatavg$humidity) # W = 0.97679, p-value = 6.718e-13 => p-value significative, l'échantillon ne suit pas une loi normale.

shapiro.test(mfdatavg$humidite) # W = 0.97952, p-value = 6.121e-12 => p-value significative, l'échantillon ne suit pas une loi normale.


### t.test entre les différentes températures
t.test(mfdatavg$humidite,dskdatavg$humidity) # t = 2.9604, df = 2370.3, p-value = 0.003103 => p-value significative, les 2 échantillons sont significativement différents !

# t.test(mfdatavg$humidite_p1D,dskdatavg$humidity) ## SO
# t.test(mfdatavg$humidite_p15D,dskdatavg$humidity) ## SO
# t.test(mfdatavg$humidite_p2D,dskdatavg$humidity) ## SO

## References :
## Myles Hollander and Douglas A. Wolfe (1973), Nonparametric Statistical Methods. New York: John Wiley & Sons. Pages 115–120.

## http://www.sthda.com/english/wiki/kruskal-wallis-test-in-r

### Import des données pour le test kruskal.test() sur données mfdatavg$humidite, dskdatavg$humidity in : KW_temp_maille_42.csv
kwhumdata <- read.csv("../../data/donnee_meteo_nationale_comparative/comparaison/KW_hum_maille_42.csv", header = TRUE, sep = ";", dec = ".")
summary(kwhumdata)

kruskal.test(temp ~ type, data = kwhumdata) ## ~ signifie : "en fonction de"

## Kruskal-Wallis chi-squared = 9.2389, df = 1, p-value = 0.002369 => p-value significative, les 2 échantillons sont significativement différents !


## 2.4. Analyse du point de rosé moyen DSK vs MF (méthode Alice Favre)


### 2.4.1. Histogramme du point de rosé moyen "point_rose" pour MF et "dewpoint" pour DSK pour 42 stations Météo France

####Après avoir vérifié que le nombre d'obs n'est pas égal avec 
length(dskdatavg$dewpoint)
length(na.omit(dskdatavg$dewpoint)) ## pour tester si NA
length(mfdatavg$point_rose)
length(na.omit(mfdatavg$point_rose)) ## pour tester si NA


##### et que l'étendue n'est pas la même
range(dskdatavg$dewpoint, na.rm = TRUE)
range(mfdatavg$point_rose, na.rm = TRUE)

#### on définit les breaks pour l'abscisse commune
BRt <- seq(from= -15, to= 20, by=2) ## tient compte des deux distributions

#### puis on fait l'histo en utilisant 

### freq=F => des fréquences relatives et pas des effectifs

hist(dskdatavg$dewpoint, breaks = BRt,
     freq=F, # fréquences
     col="grey",
     main = "Mean dewpoint for 42 Météo France Synoptic Stations \n (France, july 2017 - february 2020), 1 191 days",
     ylab = "Densities",
     xlab = "Mean dewpoint (T°C)"
)

### calcul des paramètres pour la fonction lines() à superposer à l'histo

HH2 <- hist(mfdatavg$point_rose, breaks = BRt,  plot=F)

#lines(HH2$mids, HH2$density, lwd = 2, col = "orange") ### courbe non lissée  ## SO
lines(density(mfdatavg$point_rose, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel

text(-5, 0.05, paste("Darksky"), cex = 1.2,  col = "black")
text(-10, 0.03, paste("Météo France"), cex = 1.2,  col = "blue")

## Si on voulait le nb d'enregistrements étudiés à la place du texte "Darksky" ou MF, il faudrait :
### text(03, 0.07, paste("N =", sum(complete.cases(dskdatavg$dewpoint))), cex = 1.2,  col = "black")
### text(02, 0.06, paste("N =", sum(complete.cases(mfdatavg$point_rose))), cex = 1.2,  col = "blue")

## Si on voulait les courbes à +1.5°C ou +2°C, il faudrait fabriquer et afficher :
### lines(density(mfdatavg$point_rose_p15D, na.rm = TRUE), lwd = 2, col = "green") ### courbe liss?e, kernel
### lines(density(mfdatavg$point_rose_p2D, na.rm = TRUE), lwd = 2, col = "blue") ### courbe liss?e, kernel


### 2.4.2. Test des distributions
## References :
## Patrick Royston (1982). An extension of Shapiro and Wilk's W test for normality to large samples. Applied Statistics, 31, 115–124. doi: 10.2307/2347973.

shapiro.test(dskdatavg$dewpoint) # W = 0.97812, p-value = 1.924e-12 => p-value significative, l'échantillon ne suit pas une loi normale.

shapiro.test(mfdatavg$point_rose) # W = 0.97813, p-value = 1.932e-12 => p-value significative, l'échantillon ne suit pas une loi normale.


### t.test entre les différentes températures
t.test(mfdatavg$point_rose,dskdatavg$dewpoint) # t = 0.063932, df = 2378.9, p-value = 0.949 => p-value non significative, les 2 échantillons ne sont pas significativement différents !

# t.test(mfdatavg$point_rose_p1D,dskdatavg$dewpoint) ## SO
# t.test(mfdatavg$point_rose_p15D,dskdatavg$dewpoint) ## SO
# t.test(mfdatavg$point_rose_p2D,dskdatavg$dewpoint) ## SO

## References :
## Myles Hollander and Douglas A. Wolfe (1973), Nonparametric Statistical Methods. New York: John Wiley & Sons. Pages 115–120.

## http://www.sthda.com/english/wiki/kruskal-wallis-test-in-r

### Import des données pour le test kruskal.test() sur données mfdatavg$point_rose, dskdatavg$dewpoint

## création de deux sous-ensembles "subset"s" avec juste les variables à comparer et l'origine de l'éditeur 

d1 <- subset(dskdatavg, select = c("dewpoint", "origine")) ## création du subset venant de DSK http://larmarange.github.io/analyse-R/fusion-de-tables.html
dim(d1)
head(d1, 3)
d2 <- subset(mfdatavg, select = c("point_rose", "origine")) ## création du subset venant de MF
dim(d2)
head(d2, 3)

## pour utiliser la fusion "rbind" il faut que les champs aient le même nom (c'est le 1er qui pose problème)
names(d2)[1]
names(d2)[1]<-"dewpoint" ## renommage du 1er champ du tableau 2 https://forum.framasoft.org/viewtopic.php?p=75052
names(d2)[1]

kwptrosdata <- rbind(d1, d2) ## fusion des deux sous tableaux
head(kwptrosdata)

summary(kwptrosdata)

kruskal.test(dewpoint ~ origine, data = kwptrosdata) ## ~ signifie : "en fonction de"

## Kruskal-Wallis chi-squared = 0.0024133, df = 1, p-value = 0.9608 => p-value non significative, les 2 échantillons ne sont pas significativement différents !



## 2.5. Analyse de la pression moyenne au niveau de la mer DSK vs MF (méthode Alice Favre)


### 2.5.1. Histogramme de la pression moyenne au niveau de la mer "press_mer" pour MF et "pressure" pour DSK pour 42 stations Météo France

####Après avoir vérifié que le nombre d'obs n'est pas égal avec 
length(dskdatavg$pressure)
length(na.omit(dskdatavg$pressure))
length(mfdatavg$press_mer)
length(na.omit(mfdatavg$press_mer))

##### et que l'étendue n'est pas la même
range(dskdatavg$pressure, na.rm = TRUE)
range(mfdatavg$press_mer, na.rm = TRUE)

#### on définit les breaks pour l'abscisse commune
BRt <- seq(from= 980, to= 1045, by=2) ## tient compte des deux distributions

#### puis on fait l'histo en utilisant 

### freq=F => des fréquences relatives et pas des effectifs

hist(dskdatavg$pressure, breaks = BRt,
     freq=F, # fréquences
     col="grey",
     main = "Mean pressure for 42 Météo France Synoptic Stations \n (France, july 2017 - february 2020), 1 191 days",
     ylab = "Densities",
     xlab = "Mean pressure (hPa)"
)

### calcul des paramètres pour la fonction lines() à superposer à l'histo

HH2 <- hist(mfdatavg$press_mer, breaks = BRt,  plot=F)

#lines(HH2$mids, HH2$density, lwd = 2, col = "orange") ### courbe non lissée  ## SO
lines(density(mfdatavg$press_mer, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel

text(1000, 0.05, paste("Darksky"), cex = 1.2,  col = "black")
text(990, 0.03, paste("Météo France"), cex = 1.2,  col = "blue")

## Si on voulait le nb d'enregistrements étudiés à la place du texte "Darksky" ou MF, il faudrait :
### text(03, 0.07, paste("N =", sum(complete.cases(dskdatavg$pressure))), cex = 1.2,  col = "black")
### text(02, 0.06, paste("N =", sum(complete.cases(mfdatavg$press_mer))), cex = 1.2,  col = "blue")

## Si on voulait les courbes à +1.5°C ou +2°C, il faudrait fabriquer et afficher :
### lines(density(mfdatavg$press_mer_p15D, na.rm = TRUE), lwd = 2, col = "green") ### courbe liss?e, kernel
### lines(density(mfdatavg$press_mer_p2D, na.rm = TRUE), lwd = 2, col = "blue") ### courbe liss?e, kernel


### 2.5.2. Test des distributions
## References :
## Patrick Royston (1982). An extension of Shapiro and Wilk's W test for normality to large samples. Applied Statistics, 31, 115–124. doi: 10.2307/2347973.

shapiro.test(dskdatavg$pressure) # W = 0.98046, p-value = 6.908e-11 => p-value significative, l'échantillon ne suit pas une loi normale.

shapiro.test(mfdatavg$press_mer) # W = 0.9784, p-value = 2.414e-12 => p-value significative, l'échantillon ne suit pas une loi normale.


### t.test entre les différentes températures
t.test(mfdatavg$press_mer,dskdatavg$pressure) # t = -0.34466, df = 2233.7, p-value = 0.7304 => p-value non significative, les 2 échantillons ne sont pas significativement différents !

# t.test(mfdatavg$press_mer_p1D,dskdatavg$pressure) ## SO
# t.test(mfdatavg$press_mer_p15D,dskdatavg$pressure) ## SO
# t.test(mfdatavg$press_mer_p2D,dskdatavg$pressure) ## SO

## References :
## Myles Hollander and Douglas A. Wolfe (1973), Nonparametric Statistical Methods. New York: John Wiley & Sons. Pages 115–120.

## http://www.sthda.com/english/wiki/kruskal-wallis-test-in-r

### Import des données pour le test kruskal.test() sur données mfdatavg$press_mer, dskdatavg$pressure

## création de deux sous-ensembles "subset"s" avec juste les variables à comparer et l'origine de l'éditeur 

d1 <- subset(dskdatavg, select = c("pressure", "origine")) ## création du subset venant de DSK http://larmarange.github.io/analyse-R/fusion-de-tables.html
dim(d1)
d2 <- subset(mfdatavg, select = c("press_mer", "origine")) ## création du subset venant de MF
dim(d2)

## pour utiliser la fusion "rbind" il faut que les champs aient le même nom (c'est le 1er qui pose problème)
names(d2)[1]
names(d2)[1]<-"pressure" ## renommage du 1er champ du tableau 2 https://forum.framasoft.org/viewtopic.php?p=75052
names(d2)[1]

kwptrosdata <- rbind(d1, d2) ## fusion des deux sous tableaux
head(kwptrosdata)

summary(kwptrosdata)

kruskal.test(pressure ~ origine, data = kwptrosdata) ## ~ signifie : "en fonction de"

## Kruskal-Wallis chi-squared = 0.43237, df = 1, p-value = 0.5108 => p-value non significative, les 2 échantillons ne sont pas significativement différents !



## 2.6. Analyse de la vitesse moyenne du vent DSK vs MF (méthode Alice Favre)


### 2.6.1. Histogramme de la vitesse moyenne du vent "vvent" pour MF et "windspeed" pour DSK pour 42 stations Météo France

####Après avoir vérifié que le nombre d'obs n'est pas égal avec 
length(dskdatavg$windspeed)
length(na.omit(dskdatavg$windspeed)) ## pour tester si NA
length(mfdatavg$vvent)
length(na.omit(mfdatavg$vvent)) ## pour tester si NA

##### et que l'étendue n'est pas la même
range(dskdatavg$windspeed, na.rm = TRUE)
range(mfdatavg$vvent, na.rm = TRUE)

#### on définit les breaks pour l'abscisse commune
BRt <- seq(from= 0, to= 10, by=1) ## tient compte des deux distributions

#### puis on fait l'histo en utilisant 

### freq=F => des fréquences relatives et pas des effectifs

hist(dskdatavg$windspeed, breaks = BRt,
     freq=F, # fréquences
     col="grey",
     main = "Mean windspeed for 42 Météo France Synoptic Stations \n (France, july 2017 - february 2020), 1 191 days",
     ylab = "Densities",
     xlab = "Mean windspeed (m/s)"
)

### calcul des paramètres pour la fonction lines() à superposer à l'histo

HH2 <- hist(mfdatavg$vvent, breaks = BRt,  plot=F)

#lines(HH2$mids, HH2$density, lwd = 2, col = "orange") ### courbe non lissée  ## SO
lines(density(mfdatavg$vvent, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel

text(6, 0.20, paste("Darksky"), cex = 1.2,  col = "black")
text(6, 0.30, paste("Météo France"), cex = 1.2,  col = "blue")

## Si on voulait le nb d'enregistrements étudiés à la place du texte "Darksky" ou MF, il faudrait :
### text(03, 0.07, paste("N =", sum(complete.cases(dskdatavg$windspeed))), cex = 1.2,  col = "black")
### text(02, 0.06, paste("N =", sum(complete.cases(mfdatavg$vvent))), cex = 1.2,  col = "blue")

## Si on voulait les courbes à +1.5°C ou +2°C, il faudrait fabriquer et afficher :
### lines(density(mfdatavg$vvent_p15D, na.rm = TRUE), lwd = 2, col = "green") ### courbe liss?e, kernel
### lines(density(mfdatavg$vvent_p2D, na.rm = TRUE), lwd = 2, col = "blue") ### courbe liss?e, kernel


### 2.6.2. Test des distributions
## References :
## Patrick Royston (1982). An extension of Shapiro and Wilk's W test for normality to large samples. Applied Statistics, 31, 115–124. doi: 10.2307/2347973.

shapiro.test(dskdatavg$windspeed) # W = 0.93208, p-value < 2.2e-16 => p-value significative, l'échantillon ne suit pas une loi normale.

shapiro.test(mfdatavg$vvent) # W = 0.93244, p-value < 2.2e-16 => p-value significative, l'échantillon ne suit pas une loi normale.


### t.test entre les différentes températures
t.test(mfdatavg$vvent,dskdatavg$windspeed) # t = -0.73363, df = 2378.3, p-value = 0.4632 => p-value non significative, les 2 échantillons ne sont pas significativement différents !

# t.test(mfdatavg$vvent_p1D,dskdatavg$windspeed) ## SO
# t.test(mfdatavg$vvent_p15D,dskdatavg$windspeed) ## SO
# t.test(mfdatavg$vvent_p2D,dskdatavg$windspeed) ## SO

## References :
## Myles Hollander and Douglas A. Wolfe (1973), Nonparametric Statistical Methods. New York: John Wiley & Sons. Pages 115–120.

## http://www.sthda.com/english/wiki/kruskal-wallis-test-in-r

### Import des données pour le test kruskal.test() sur données mfdatavg$vvent, dskdatavg$windspeed

## création de deux sous-ensembles "subset"s" avec juste les variables à comparer et l'origine de l'éditeur 

d1 <- subset(dskdatavg, select = c("windspeed", "origine")) ## création du subset venant de DSK http://larmarange.github.io/analyse-R/fusion-de-tables.html
dim(d1)
d2 <- subset(mfdatavg, select = c("vvent", "origine")) ## création du subset venant de MF
dim(d2)

## pour utiliser la fusion "rbind" il faut que les champs aient le même nom (c'est le 1er qui pose problème)
names(d2)[1]
names(d2)[1]<-"windspeed" ## renommage du 1er champ du tableau 2 https://forum.framasoft.org/viewtopic.php?p=75052
names(d2)[1]

kwptrosdata <- rbind(d1, d2) ## fusion des deux sous tableaux
head(kwptrosdata)

summary(kwptrosdata)

kruskal.test(windspeed ~ origine, data = kwptrosdata) ## ~ signifie : "en fonction de"

## Kruskal-Wallis chi-squared = 0.36632, df = 1, p-value = 0.545 => p-value non significative, les 2 échantillons ne sont pas significativement différents !



## 2.7. Analyse de la visibilite moyenne DSK vs MF (méthode Alice Favre)


### 2.7.1. Histogramme de la visibilite moyenne "visibilite" pour MF et "visibility" pour DSK pour 42 stations Météo France

####Après avoir vérifié que le nombre d'obs n'est pas égal avec 
length(dskdatavg$visibility)
length(na.omit(dskdatavg$visibility)) ## pour tester si NA
length(mfdatavg$visibilite)
length(na.omit(mfdatavg$visibilite)) ## pour tester si NA

##### et que l'étendue n'est pas la même
range(dskdatavg$visibility, na.rm = TRUE)
range(mfdatavg$visibilite, na.rm = TRUE)

#### on définit les breaks pour l'abscisse commune
BRt <- seq(from= 5, to= 40, by=2) ## tient compte des deux distributions

#### puis on fait l'histo en utilisant 

### freq=F => des fréquences relatives et pas des effectifs

hist(dskdatavg$visibility, breaks = BRt,
     freq=F, # fréquences
     col="grey",
     main = "Mean visibility for 42 Météo France Synoptic Stations \n (France, july 2017 - february 2020), 1 191 days",
     ylab = "Densities",
     xlab = "Mean visibility (km)"
)

### calcul des paramètres pour la fonction lines() à superposer à l'histo

HH2 <- hist(mfdatavg$visibilite, breaks = BRt,  plot=F)

#lines(HH2$mids, HH2$density, lwd = 2, col = "orange") ### courbe non lissée  ## SO
lines(density(mfdatavg$visibilite, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel

text(17, 0.20, paste("Darksky"), cex = 1.2,  col = "black")
text(32, 0.102, paste("Météo France"), cex = 1.2,  col = "blue")

## Si on voulait le nb d'enregistrements étudiés à la place du texte "Darksky" ou MF, il faudrait :
### text(03, 0.07, paste("N =", sum(complete.cases(dskdatavg$visibility))), cex = 1.2,  col = "black")
### text(02, 0.06, paste("N =", sum(complete.cases(mfdatavg$visibilite))), cex = 1.2,  col = "blue")

## Si on voulait les courbes à +1.5°C ou +2°C, il faudrait fabriquer et afficher :
### lines(density(mfdatavg$visibilite_p15D, na.rm = TRUE), lwd = 2, col = "green") ### courbe liss?e, kernel
### lines(density(mfdatavg$visibilite_p2D, na.rm = TRUE), lwd = 2, col = "blue") ### courbe liss?e, kernel


### 2.7.2. Test des distributions
## References :
## Patrick Royston (1982). An extension of Shapiro and Wilk's W test for normality to large samples. Applied Statistics, 31, 115–124. doi: 10.2307/2347973.

shapiro.test(dskdatavg$visibility) # W = 0.93231, p-value < 2.2e-16 => p-value significative, l'échantillon ne suit pas une loi normale.

shapiro.test(mfdatavg$visibilite) # W = W = 0.97402, p-value = 8.433e-14 => p-value significative, l'échantillon ne suit pas une loi normale.


### t.test entre les différentes températures
t.test(mfdatavg$visibilite,dskdatavg$visibility) # t = 92.446, df = 1369.9, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents !

# t.test(mfdatavg$visibilite_p1D,dskdatavg$visibility) ## SO
# t.test(mfdatavg$visibilite_p15D,dskdatavg$visibility) ## SO
# t.test(mfdatavg$visibilite_p2D,dskdatavg$visibility) ## SO

## References :
## Myles Hollander and Douglas A. Wolfe (1973), Nonparametric Statistical Methods. New York: John Wiley & Sons. Pages 115–120.

## http://www.sthda.com/english/wiki/kruskal-wallis-test-in-r

### Import des données pour le test kruskal.test() sur données mfdatavg$visibilite, dskdatavg$visibility

## création de deux sous-ensembles "subset"s" avec juste les variables à comparer et l'origine de l'éditeur 

d1 <- subset(dskdatavg, select = c("visibility", "origine")) ## création du subset venant de DSK http://larmarange.github.io/analyse-R/fusion-de-tables.html
dim(d1)
d2 <- subset(mfdatavg, select = c("visibilite", "origine")) ## création du subset venant de MF
dim(d2)

## pour utiliser la fusion "rbind" il faut que les champs aient le même nom (c'est le 1er qui pose problème)
names(d2)[1]
names(d2)[1]<-"visibility" ## renommage du 1er champ du tableau 2 https://forum.framasoft.org/viewtopic.php?p=75052
names(d2)[1]

kwptrosdata <- rbind(d1, d2) ## fusion des deux sous tableaux
head(kwptrosdata)

summary(kwptrosdata)

kruskal.test(visibility ~ origine, data = kwptrosdata) ## ~ signifie : "en fonction de"

## Kruskal-Wallis chi-squared = 1746.4, df = 1, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents !

## 2.8. Analyse de la nebulosité moyenne DSK vs MF (méthode Alice Favre)


### 2.8.1. Histogramme de la nebulosité moyenne "nebulosite" pour MF et "cloudcover" pour DSK pour 42 stations Météo France

####Après avoir vérifié que le nombre d'obs n'est pas égal avec 
length(dskdatavg$cloudcover)
length(na.omit(dskdatavg$cloudcover)) ## pour tester si NA
length(mfdatavg$nebulosite)
length(na.omit(mfdatavg$nebulosite)) ## pour tester si NA

##### et que l'étendue n'est pas la même
range(dskdatavg$cloudcover, na.rm = TRUE)
range(mfdatavg$nebulosite, na.rm = TRUE)

#### on définit les breaks pour l'abscisse commune
BRt <- seq(from= 5, to= 100, by=2) ## tient compte des deux distributions

#### puis on fait l'histo en utilisant 

### freq=F => des fréquences relatives et pas des effectifs

hist(dskdatavg$cloudcover, breaks = BRt,
     freq=F, # fréquences
     col="grey",
     main = "Mean cloudcover for 42 Météo France Synoptic Stations \n (France, july 2017 - february 2020), 1 191 days",
     ylab = "Densities",
     xlab = "Mean cloudcover (%)"
)

### calcul des paramètres pour la fonction lines() à superposer à l'histo

HH2 <- hist(mfdatavg$nebulosite, breaks = BRt,  plot=F)

#lines(HH2$mids, HH2$density, lwd = 2, col = "orange") ### courbe non lissée  ## SO
lines(density(mfdatavg$nebulosite, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel

text(40, 0.015, paste("Darksky"), cex = 1.2,  col = "black")
text(55, 0.030, paste("Météo France"), cex = 1.2,  col = "blue")

## Si on voulait le nb d'enregistrements étudiés à la place du texte "Darksky" ou MF, il faudrait :
### text(03, 0.07, paste("N =", sum(complete.cases(dskdatavg$cloudcover))), cex = 1.2,  col = "black")
### text(02, 0.06, paste("N =", sum(complete.cases(mfdatavg$nebulosite))), cex = 1.2,  col = "blue")

## Si on voulait les courbes à +1.5°C ou +2°C, il faudrait fabriquer et afficher :
### lines(density(mfdatavg$nebulosite_p15D, na.rm = TRUE), lwd = 2, col = "green") ### courbe liss?e, kernel
### lines(density(mfdatavg$nebulosite_p2D, na.rm = TRUE), lwd = 2, col = "blue") ### courbe liss?e, kernel


### 2.8.2. Test des distributions
## References :
## Patrick Royston (1982). An extension of Shapiro and Wilk's W test for normality to large samples. Applied Statistics, 31, 115–124. doi: 10.2307/2347973.

shapiro.test(dskdatavg$cloudcover) # W = 0.90472, p-value < 2.2e-16 => p-value significative, l'échantillon ne suit pas une loi normale.

shapiro.test(mfdatavg$nebulosite) # W = 0.82479, p-value < 2.2e-16 => p-value significative, l'échantillon ne suit pas une loi normale.


### t.test entre les différentes températures
t.test(mfdatavg$nebulosite,dskdatavg$cloudcover) # t = 19.15, df = 2349.5, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents !

# t.test(mfdatavg$nebulosite_p1D,dskdatavg$cloudcover) ## SO
# t.test(mfdatavg$nebulosite_p15D,dskdatavg$cloudcover) ## SO
# t.test(mfdatavg$nebulosite_p2D,dskdatavg$cloudcover) ## SO

## References :
## Myles Hollander and Douglas A. Wolfe (1973), Nonparametric Statistical Methods. New York: John Wiley & Sons. Pages 115–120.

## http://www.sthda.com/english/wiki/kruskal-wallis-test-in-r

### Import des données pour le test kruskal.test() sur données mfdatavg$nebulosite, dskdatavg$cloudcover

## création de deux sous-ensembles "subset"s" avec juste les variables à comparer et l'origine de l'éditeur 

d1 <- subset(dskdatavg, select = c("cloudcover", "origine")) ## création du subset venant de DSK http://larmarange.github.io/analyse-R/fusion-de-tables.html
dim(d1)
d2 <- subset(mfdatavg, select = c("nebulosite", "origine")) ## création du subset venant de MF
dim(d2)

## pour utiliser la fusion "rbind" il faut que les champs aient le même nom (c'est le 1er qui pose problème)
names(d2)[1]
names(d2)[1]<-"cloudcover" ## renommage du 1er champ du tableau 2 https://forum.framasoft.org/viewtopic.php?p=75052
names(d2)[1]

kwptrosdata <- rbind(d1, d2) ## fusion des deux sous tableaux
head(kwptrosdata)

summary(kwptrosdata)

kruskal.test(cloudcover ~ origine, data = kwptrosdata) ## ~ signifie : "en fonction de"

## Kruskal-Wallis chi-squared = 474.52, df = 1, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents !


## 2.9. Analyse de la rafale_10min moyenne DSK vs MF (méthode Alice Favre)


### 2.9.1. Histogramme de la rafale_10min moyenne "rafale_10min" pour MF et "windgust" pour DSK pour 42 stations Météo France

####Après avoir vérifié que le nombre d'obs n'est pas égal avec 
length(dskdatavg$windgust)
length(na.omit(dskdatavg$windgust)) ## pour tester si NA
length(mfdatavg$rafale_10min)
length(na.omit(mfdatavg$rafale_10min)) ## pour tester si NA

##### et que l'étendue n'est pas la même
range(dskdatavg$windgust, na.rm = TRUE)
range(mfdatavg$rafale_10min, na.rm = TRUE)

#### on définit les breaks pour l'abscisse commune
BRt <- seq(from= 2, to= 25, by=1) ## tient compte des deux distributions

#### puis on fait l'histo en utilisant 

### freq=F => des fréquences relatives et pas des effectifs

hist(dskdatavg$windgust, breaks = BRt,
     freq=F, # fréquences
     col="grey",
     main = "Mean windgust for 42 Météo France Synoptic Stations \n (France, july 2017 - february 2020), 1 191 days",
     ylab = "Densities",
     xlab = "Mean windgust (m/s)"
)

### calcul des paramètres pour la fonction lines() à superposer à l'histo

HH2 <- hist(mfdatavg$rafale_10min, breaks = BRt,  plot=F)

#lines(HH2$mids, HH2$density, lwd = 2, col = "orange") ### courbe non lissée  ## SO
lines(density(mfdatavg$rafale_10min, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel

text(16, 0.05, paste("Darksky"), cex = 1.2,  col = "black")
text(13, 0.12, paste("Météo France"), cex = 1.2,  col = "blue")

## Si on voulait le nb d'enregistrements étudiés à la place du texte "Darksky" ou MF, il faudrait :
### text(03, 0.07, paste("N =", sum(complete.cases(dskdatavg$windgust))), cex = 1.2,  col = "black")
### text(02, 0.06, paste("N =", sum(complete.cases(mfdatavg$rafale_10min))), cex = 1.2,  col = "blue")

## Si on voulait les courbes à +1.5°C ou +2°C, il faudrait fabriquer et afficher :
### lines(density(mfdatavg$rafale_10min_p15D, na.rm = TRUE), lwd = 2, col = "green") ### courbe liss?e, kernel
### lines(density(mfdatavg$rafale_10min_p2D, na.rm = TRUE), lwd = 2, col = "blue") ### courbe liss?e, kernel


### 2.9.2. Test des distributions
## References :
## Patrick Royston (1982). An extension of Shapiro and Wilk's W test for normality to large samples. Applied Statistics, 31, 115–124. doi: 10.2307/2347973.

shapiro.test(dskdatavg$windgust) # W = 0.92466, p-value < 2.2e-16 => p-value significative, l'échantillon ne suit pas une loi normale.

shapiro.test(mfdatavg$rafale_10min) # W = 0.92202, p-value < 2.2e-16 => p-value significative, l'échantillon ne suit pas une loi normale.


### t.test entre les différentes températures
t.test(mfdatavg$rafale_10min,dskdatavg$windgust) # t = -29.089, df = 1644.3, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents !

# t.test(mfdatavg$rafale_10min_p1D,dskdatavg$windgust) ## SO
# t.test(mfdatavg$rafale_10min_p15D,dskdatavg$windgust) ## SO
# t.test(mfdatavg$rafale_10min_p2D,dskdatavg$windgust) ## SO

## References :
## Myles Hollander and Douglas A. Wolfe (1973), Nonparametric Statistical Methods. New York: John Wiley & Sons. Pages 115–120.

## http://www.sthda.com/english/wiki/kruskal-wallis-test-in-r

### Import des données pour le test kruskal.test() sur données mfdatavg$rafale_10min, dskdatavg$windgust

## création de deux sous-ensembles "subset"s" avec juste les variables à comparer et l'origine de l'éditeur 

d1 <- subset(dskdatavg, select = c("windgust", "origine")) ## création du subset venant de DSK http://larmarange.github.io/analyse-R/fusion-de-tables.html
dim(d1)
d2 <- subset(mfdatavg, select = c("rafale_10min", "origine")) ## création du subset venant de MF
dim(d2)

## pour utiliser la fusion "rbind" il faut que les champs aient le même nom (c'est le 1er qui pose problème)
names(d2)[1]
names(d2)[1]<-"windgust" ## renommage du 1er champ du tableau 2 https://forum.framasoft.org/viewtopic.php?p=75052
names(d2)[1]

kwptrosdata <- rbind(d1, d2) ## fusion des deux sous tableaux
head(kwptrosdata)

summary(kwptrosdata)

kruskal.test(windgust ~ origine, data = kwptrosdata) ## ~ signifie : "en fonction de"

## Kruskal-Wallis chi-squared = 742.22, df = 1, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents !


## 2.10. Analyse de la quantité moyenne de précipitation par heure DSK vs MF (méthode Alice Favre)


### 2.10.1. Histogramme de la precip_24h moyenne "precip_24h" pour MF et "precipintensity" pour DSK pour 42 stations Météo France

####Après avoir vérifié que le nombre d'obs n'est pas égal avec 
length(dskdatavg$precipintensity)
length(na.omit(dskdatavg$precipintensity)) ## pour tester si NA
length(mfdatavg$precip_24h)
length(na.omit(mfdatavg$precip_24h)) ## pour tester si NA

##### et que l'étendue n'est pas la même
range(dskdatavg$precipintensity, na.rm = TRUE)
range(mfdatavg$precip_24h, na.rm = TRUE)

#### on définit les breaks pour l'abscisse commune
BRt <- seq(from= -1, to= 16, by=0.1) ## tient compte des deux distributions

#### puis on fait l'histo en utilisant 

### freq=F => des fréquences relatives et pas des effectifs

hist(dskdatavg$precipintensity, breaks = BRt,
     freq=F, # fréquences
     col="grey",
     main = "Mean precipintensity for 42 Météo France Synoptic Stations \n (France, july 2017 - february 2020), 1 191 days",
     ylab = "Densities",
     xlab = "Mean precipintensity (mm/h)"
)

### calcul des paramètres pour la fonction lines() à superposer à l'histo

HH2 <- hist(mfdatavg$precip_24h, breaks = BRt,  plot=F)

#lines(HH2$mids, HH2$density, lwd = 2, col = "orange") ### courbe non lissée  ## SO
lines(density(mfdatavg$precip_24h, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel

text(3, 3, paste("Darksky"), cex = 1.2,  col = "black")
text(3, 1, paste("Météo France"), cex = 1.2,  col = "blue")

## Si on voulait le nb d'enregistrements étudiés à la place du texte "Darksky" ou MF, il faudrait :
### text(03, 0.07, paste("N =", sum(complete.cases(dskdatavg$precipintensity))), cex = 1.2,  col = "black")
### text(02, 0.06, paste("N =", sum(complete.cases(mfdatavg$precip_24h))), cex = 1.2,  col = "blue")

## Si on voulait les courbes à +1.5°C ou +2°C, il faudrait fabriquer et afficher :
### lines(density(mfdatavg$precip_24h_p15D, na.rm = TRUE), lwd = 2, col = "green") ### courbe liss?e, kernel
### lines(density(mfdatavg$precip_24h_p2D, na.rm = TRUE), lwd = 2, col = "blue") ### courbe liss?e, kernel


### 2.10.2. Test des distributions
## References :
## Patrick Royston (1982). An extension of Shapiro and Wilk's W test for normality to large samples. Applied Statistics, 31, 115–124. doi: 10.2307/2347973.

shapiro.test(dskdatavg$precipintensity) # W = 0.41001, p-value < 2.2e-16 => p-value significative, l'échantillon ne suit pas une loi normale.

shapiro.test(mfdatavg$precip_24h) # W = 0.80774, p-value < 2.2e-16 => p-value significative, l'échantillon ne suit pas une loi normale.


### t.test entre les différentes températures
t.test(mfdatavg$precip_24h,dskdatavg$precipintensity) # t = 28.767, df = 1199.6, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents !

# t.test(mfdatavg$precip_24h_p1D,dskdatavg$precipintensity) ## SO
# t.test(mfdatavg$precip_24h_p15D,dskdatavg$precipintensity) ## SO
# t.test(mfdatavg$precip_24h_p2D,dskdatavg$precipintensity) ## SO

## References :
## Myles Hollander and Douglas A. Wolfe (1973), Nonparametric Statistical Methods. New York: John Wiley & Sons. Pages 115–120.

## http://www.sthda.com/english/wiki/kruskal-wallis-test-in-r

### Import des données pour le test kruskal.test() sur données mfdatavg$precip_24h, dskdatavg$precipintensity

## création de deux sous-ensembles "subset"s" avec juste les variables à comparer et l'origine de l'éditeur 

d1 <- subset(dskdatavg, select = c("precipintensity", "origine")) ## création du subset venant de DSK http://larmarange.github.io/analyse-R/fusion-de-tables.html
dim(d1)
d2 <- subset(mfdatavg, select = c("precip_24h", "origine")) ## création du subset venant de MF
dim(d2)

## pour utiliser la fusion "rbind" il faut que les champs aient le même nom (c'est le 1er qui pose problème)
names(d2)[1]
names(d2)[1]<-"precipintensity" ## renommage du 1er champ du tableau 2 https://forum.framasoft.org/viewtopic.php?p=75052
names(d2)[1]

kwptrosdata <- rbind(d1, d2) ## fusion des deux sous tableaux
head(kwptrosdata)

summary(kwptrosdata)

kruskal.test(precipintensity ~ origine, data = kwptrosdata) ## ~ signifie : "en fonction de"

## Kruskal-Wallis chi-squared = 1128.4, df = 1, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents !

