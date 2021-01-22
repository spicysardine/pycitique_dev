######
###### Comparaison des données Météo MF et DSK smartick_meteo_v5

# Définition du répertoire de travail
getwd()
setwd("./")

# Importation de la donnée comparative météo-France (MF) et darksky (DSK)
# La donnée MF est issue de 42 stations synoptiques réparties sur le territoire nationale
# La donnée DSK est issue de la même coordonnée de station mais avec des extractoins issues des dépôt de darksky

dskdatavg<- read.csv("../../data/donnee_meteo_nationale_comparative/darksky/darksky_moyennes_journalieres_maille_42.csv", header = TRUE, sep = ",", dec = ".")
mfdatavg <- read.csv("../../data/donnee_meteo_nationale_comparative/meteoFrance/mf_moyennes_journalieres_maille_42.csv", header = TRUE, sep = ",", dec = ".")

## vérification des jeux de donnée
## liste les variables
ls(dskdatavg)
ls(mfdatavg)

##sommaire de la donnée et analyse primaire
summary(dskdatavg)
summary(mfdatavg)


############################## Analyse des Températures moyennes ########################################
# qqnorm(dskdatavg$temperature, pch = 1, frame = TRUE, ylab = "température moyenne DSK")
# qqline(dskdatavg$temperature, col = "steelblue", lwd = 2)
# 
# qqnorm(mfdatavg$temperature, pch = 1, frame = TRUE, ylab = "température moyenne MF")
# qqline(mfdatavg$temperature, col = "steelblue", lwd = 2)
qqPlot(dskdatavg$temperature, ylab = "température moyenne DSK")
qqPlot(mfdatavg$temperature, ylab = "température moyenne MF")
### Histogramme des Températures moyennes

#### Vérifications
length(dskdatavg$temperature)
length(na.omit(dskdatavg$temperature)) ## pour tester si NA
length(mfdatavg$temperature)
length(na.omit(mfdatavg$temperature)) ## pour tester si NA

##### vérification de l'étendue de la distribution
range(dskdatavg$temperature, na.rm = TRUE)
range(mfdatavg$temperature, na.rm = TRUE)

#### définition des intervalles
BRt <- seq(from= -5, to= 30, by=2) ## tient compte des deux distributions

### freq=F => des fréquences relatives et pas des effectifs
hist(dskdatavg$temperature, breaks = BRt,
     freq=F, # fréquences
     col="grey",
     main = "Moyenne des températures entre 1/1/2017 et 5/4/2020 sur 1191 jours",
     ylab = "densités",
     xlab = "Moyenne des temperatures (°C)"
)

### calcul des paramètres pour la fonction lines() à superposer à l'histo
HHt <- hist(mfdatavg$temperature, breaks = BRt,  plot=F)
lines(HHt$mids, HHt$density, lwd = 2, col = "green") ### courbe non lissée  ## SO
lines(density(mfdatavg$temperature, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel
## labels
text(03, 0.07, paste("Darksky"), cex = 1.2,  col = "black")
text(02, 0.06, paste("Météo France"), cex = 1.2,  col = "blue")

######### Test des distributions statistiques
## References :
## Patrick Royston (1982). An extension of Shapiro and Wilk's W test for normality to large samples. Applied Statistics, 31, 115–124. doi: 10.2307/2347973.
shapiro.test(dskdatavg$temperature) 
shapiro.test(mfdatavg$temperature) 

### t.test entre les différentes températures
t.test(mfdatavg$temperature,dskdatavg$temperature)

## References :
## Myles Hollander and Douglas A. Wolfe (1973), Nonparametric Statistical Methods. New York: John Wiley & Sons. Pages 115–120.
## http://www.sthda.com/english/wiki/kruskal-wallis-test-in-r
 
#### fabrication des données pour le test kruskal.test() sur données de températures des deux extractions
type <- "dsk"
dsktmp <- data.frame(temp=dskdatavg$temperature, type)

type <- "mf"
mftmp <- data.frame(temp=mfdatavg$temperature, type)

kwtempdata <- rbind(dsktmp,mftmp)

## Kruskal-Wallis 
kruskal.test(temp ~ type, data = kwtempdata) ## ~ signifie : "en fonction de"
summary(kwtempdata)
kwtempdata %>% kruskal_effsize(temp ~ type)

# Test de wilcoxon, plus adéquat pour deux groupes de comparaisons:
wilcox.test(temp ~ type, data = kwtempdata)



############################## Analyse de l'humidité moyenne DSK vs MF ############################## 

## TEST de normatlité graphique QQ plot
# qqplot(norm, dskdatavg$humidity, plot.it = TRUE)
# qqplot(norm, trunc(mfdatavg$humidite), plot.it = TRUE)
# 
# qqnorm(dskdatavg$humidity, pch = 1, frame = TRUE, ylab = "humidité DSK")
# qqline(dskdatavg$humidity, col = "steelblue", lwd = 2)
# 
# qqnorm(trunc(mfdatavg$humidite), pch = 1, frame = TRUE, ylab = "humidité MF")
# qqline(trunc(mfdatavg$humidite), col = "steelblue", lwd = 2)

qqPlot(dskdatavg$humidity, ylab = "humidité moyenne DSK")
qqPlot(mfdatavg$humidite, ylab = "humidité moyenne MF")

###Histogramme de l'humidité moyenne "humidite" pour MF et "humidity" pour DSK 

length(dskdatavg$humidity)
length(na.omit(dskdatavg$humidity)) ## pour tester si NA
length(mfdatavg$humidite)
length(na.omit(mfdatavg$humidite)) ## pour tester si NA

range(dskdatavg$humidity, na.rm = TRUE)
range(mfdatavg$humidite, na.rm = TRUE)

BRh <- seq(from= 0, to= 100, by=2) ## tient compte des deux distributions

hist(dskdatavg$humidity, breaks = BRh,
     freq=F,
     col="grey",
     main = "Moyenne de l'humidité entre 1/1/2017 et 5/4/2020 sur 1191 jours",
     ylab = "Densities",
     xlab = "Mean Humidity (%)"
)

HHh <- hist(trunc(mfdatavg$humidite), breaks = BRh,  plot=F)
lines(HHh$mids, HHh$density, lwd = 2, col = "orange") ### courbe non lissée  ## SO
lines(density(trunc(mfdatavg$humidite), na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel

text(60, 0.04, paste("Darksky"), cex = 1.2,  col = "black")
text(55, 0.03, paste("Météo France"), cex = 1.2,  col = "blue")

#DSK
shapiro.test(dskdatavg$humidity) 
#MF
shapiro.test(trunc(mfdatavg$humidite))

### t.test entre les différentes humidités
t.test(trunc(mfdatavg$humidite),dskdatavg$humidity) 

# construction du jeu de donnée pour le test Kurskal-Willis:
type <- "dsk"
dskhum <- data.frame(hum=dskdatavg$humidity, type)
type <- 'mf'
mfhum <- data.frame(hum=trunc(mfdatavg$humidite),type)
kwhumdata <- rbind(dskhum,mfhum)
kwhumdata
summary(kwhumdata)

?kruskal.test
kruskal.test(hum ~ type, data = kwhumdata) ## ~ signifie : "en fonction de"

# Test de l'effet de taille: résultat: effet= 0.00346 sur une échelle de 0 à 1, donc très petit.
kwhumdata %>% kruskal_effsize(hum ~ type)

# Test de wilcoxon, plus adéquat pour deux groupes de comparaisons:
wilcox.test(hum ~ type, data = kwhumdata)

######################################## Analyse du point de rosé moyen ############################## 


############# Histogramme du point de rosé moyen 

length(dskdatavg$dewpoint)
length(na.omit(dskdatavg$dewpoint)) ## pour tester si NA
length(mfdatavg$point_rose)
length(na.omit(mfdatavg$point_rose)) ## pour tester si NA

##### et que l'étendue n'est pas la même
range(dskdatavg$dewpoint, na.rm = TRUE)
range(mfdatavg$point_rose, na.rm = TRUE)

#### on définit les breaks pour l'abscisse commune
BRt <- seq(from= -15, to= 20, by=2) ## tient compte des deux distributions

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

### 2.4.2. Test des distributions

shapiro.test(dskdatavg$dewpoint) # W = 0.97812, p-value = 1.924e-12 => p-value significative, l'échantillon ne suit pas une loi normale.

shapiro.test(mfdatavg$point_rose) # W = 0.97813, p-value = 1.932e-12 => p-value significative, l'échantillon ne suit pas une loi normale.


### t.test entre les différentes températures
t.test(mfdatavg$point_rose,dskdatavg$dewpoint) # t = 0.063932, df = 2378.9, p-value = 0.949 => p-value non significative, les 2 échantillons ne sont pas significativement différents !

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

## Kruskal-Wallis chi-squared = 0.0024133, df = 1, p-value = 0.9608 => p-value non significative, les 2 échantillons ne sont pas significativement différents !
kruskal.test(dewpoint ~ origine, data = kwptrosdata) ## ~ signifie : "en fonction de"


##############################  Analyse de la pression moyenne au niveau de la mer ############################## 

############ Histogramme de la pression moyenne au niveau de la mer "press_mer" pour MF et "pressure" pour DSK 

length(dskdatavg$pressure)
length(na.omit(dskdatavg$pressure))
length(mfdatavg$press_mer)
length(na.omit(mfdatavg$press_mer))

range(dskdatavg$pressure, na.rm = TRUE)
range(mfdatavg$press_mer, na.rm = TRUE)

#### on définit les breaks pour l'abscisse commune
BRp <- seq(from= 980, to= 1045, by=2) ## tient compte des deux distributions

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

### 2.5.2. Test des distributions

shapiro.test(dskdatavg$pressure) # W = 0.98046, p-value = 6.908e-11 => p-value significative, l'échantillon ne suit pas une loi normale.

shapiro.test(mfdatavg$press_mer) # W = 0.9784, p-value = 2.414e-12 => p-value significative, l'échantillon ne suit pas une loi normale.

### t.test entre les différentes températures
t.test(mfdatavg$press_mer,dskdatavg$pressure) # t = -0.34466, df = 2233.7, p-value = 0.7304 => p-value non significative, les 2 échantillons ne sont pas significativement différents !

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

## Kruskal-Wallis chi-squared = 0.43237, df = 1, p-value = 0.5108 => p-value non significative, les 2 échantillons ne sont pas significativement différents !
kruskal.test(pressure ~ origine, data = kwptrosdata) ## ~ signifie : "en fonction de"

##############################  Analyse de la vitesse moyenne du vent ################################################# 

### 2.6.1. Histogramme de la vitesse moyenne du vent "vvent" pour MF et "windspeed" pour DSK 

length(dskdatavg$windspeed)
length(na.omit(dskdatavg$windspeed)) ## pour tester si NA
length(mfdatavg$vvent)
length(na.omit(mfdatavg$vvent)) ## pour tester si NA

range(dskdatavg$windspeed, na.rm = TRUE)
range(mfdatavg$vvent, na.rm = TRUE)

BRt <- seq(from= 0, to= 10, by=1) ## tient compte des deux distributions

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

### 2.6.2. Test des distributions

shapiro.test(dskdatavg$windspeed) # W = 0.93208, p-value < 2.2e-16 => p-value significative, l'échantillon ne suit pas une loi normale.

shapiro.test(mfdatavg$vvent) # W = 0.93244, p-value < 2.2e-16 => p-value significative, l'échantillon ne suit pas une loi normale.


### t.test entre les différentes températures
t.test(mfdatavg$vvent,dskdatavg$windspeed) # t = -0.73363, df = 2378.3, p-value = 0.4632 => p-value non significative, les 2 échantillons ne sont pas significativement différents !

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

## Kruskal-Wallis chi-squared = 0.36632, df = 1, p-value = 0.545 => p-value non significative, les 2 échantillons ne sont pas significativement différents !
kruskal.test(windspeed ~ origine, data = kwptrosdata) ## ~ signifie : "en fonction de"

##############################  Analyse de la visibilite moyenne ############################## 

################ Histogramme de la visibilite moyenne "visibilite" pour MF et "visibility" 

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


### 2.7.2. Test des distributions
## References :
## Patrick Royston (1982). An extension of Shapiro and Wilk's W test for normality to large samples. Applied Statistics, 31, 115–124. doi: 10.2307/2347973.

shapiro.test(dskdatavg$visibility) # W = 0.93231, p-value < 2.2e-16 => p-value significative, l'échantillon ne suit pas une loi normale.

shapiro.test(mfdatavg$visibilite) # W = W = 0.97402, p-value = 8.433e-14 => p-value significative, l'échantillon ne suit pas une loi normale.

### t.test entre les différentes températures
t.test(mfdatavg$visibilite,dskdatavg$visibility) # t = 92.446, df = 1369.9, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents !

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

##############################  Analyse de la nebulosité moyenne ############################## 

### 2.8.1. Histogramme de la nebulosité moyenne "nebulosite" pour MF et "cloudcover" pour DSK 

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

### 2.8.2. Test des distributions
## References :
## Patrick Royston (1982). An extension of Shapiro and Wilk's W test for normality to large samples. Applied Statistics, 31, 115–124. doi: 10.2307/2347973.

shapiro.test(dskdatavg$cloudcover) # W = 0.90472, p-value < 2.2e-16 => p-value significative, l'échantillon ne suit pas une loi normale.

shapiro.test(mfdatavg$nebulosite) # W = 0.82479, p-value < 2.2e-16 => p-value significative, l'échantillon ne suit pas une loi normale.


### t.test entre les différentes températures
t.test(mfdatavg$nebulosite,dskdatavg$cloudcover) # t = 19.15, df = 2349.5, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents !

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

## Kruskal-Wallis chi-squared = 474.52, df = 1, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents !
kruskal.test(cloudcover ~ origine, data = kwptrosdata) ## ~ signifie : "en fonction de"

##############################  Analyse de la rafale_10min moyenne############################## 


### 2.9.1. Histogramme de la rafale_10min moyenne "rafale_10min" pour MF et "windgust" pour DSK 

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

### 2.9.2. Test des distributions
## References :
## Patrick Royston (1982). An extension of Shapiro and Wilk's W test for normality to large samples. Applied Statistics, 31, 115–124. doi: 10.2307/2347973.

shapiro.test(dskdatavg$windgust) # W = 0.92466, p-value < 2.2e-16 => p-value significative, l'échantillon ne suit pas une loi normale.

shapiro.test(mfdatavg$rafale_10min) # W = 0.92202, p-value < 2.2e-16 => p-value significative, l'échantillon ne suit pas une loi normale.


### t.test entre les différentes températures
t.test(mfdatavg$rafale_10min,dskdatavg$windgust) # t = -29.089, df = 1644.3, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents !

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


##############################  Analyse de la quantité moyenne de précipitation par heure ############################## 


### 2.10.1. Histogramme de la precip_24h moyenne "precip_24h" pour MF et "precipintensity" pour DSK 

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

### 2.10.2. Test des distributions

shapiro.test(dskdatavg$precipintensity) # W = 0.41001, p-value < 2.2e-16 => p-value significative, l'échantillon ne suit pas une loi normale.

shapiro.test(mfdatavg$precip_24h) # W = 0.80774, p-value < 2.2e-16 => p-value significative, l'échantillon ne suit pas une loi normale.

### t.test entre les différentes températures
t.test(mfdatavg$precip_24h,dskdatavg$precipintensity) # t = 28.767, df = 1199.6, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents !


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

## Kruskal-Wallis chi-squared = 1128.4, df = 1, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents !
kruskal.test(precipintensity ~ origine, data = kwptrosdata) ## ~ signifie : "en fonction de"


