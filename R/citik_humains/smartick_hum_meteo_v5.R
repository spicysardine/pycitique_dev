######
###### Analyse des signalements et des données Météo DSK

# 1. Introduction
### Alloue la librairie "set()" puis vérifie que c'est bien importé "getwd()", il pointe dessus!

## 1.1 XPS13
setwd("C:/3VG/MSH/lyme/Smartick17/pycitique_miror/R")
getwd()

#Remove all objects
rm(list = ls() )
rm()

## 1.2 OP7570
setwd("E:/3VG/MSH/lyme/Smartick17/pycitique_miror/R")
getwd()

# 2. Lab 1 : Importation de données citik_humains_clean_weather_strict.csv

### Or, if .csv file, use this si données séparées par ","
### quote = "'" pour le caractère d’échappement
### stringAsFactpors pour voir les chaînes de caractères sans interprétation.
humdata <- read.csv("../../pycitique/data/donnee_signalements_avec_meteo_dsk/citik_humains_clean_weather_strict.csv", sep = ",", dec = ".", quote = "'", stringsAsFactors = FALSE)

DSKdata <- read.csv("../../pycitique/data/donnee_meteo_nationale_comparative/darksky/dsk_moy_journ_m700b.csv", sep = ",", dec = ".", quote = "'", stringsAsFactors = FALSE)

## 2.1 Analyse du tableau de données

ls(humdata) ## liste les variables
ls(DSKdata)


str(humdata) ##  a diagnostic function and an alternative to summary
str(DSKdata)


summary(humdata)
summary(DSKdata)



# 3. Analyse des Températures moyennes DSK vs MF (méthode Alice Favre)

## 3.1. Histogramme des Températures moyennes "temperature" pour les signalements et "temperature" pour DSK = (tempHigh + tempLow)/2 pour 700 points darksky

#### Après avoir :

#### vérifié que le nombre d'obs n'est pas égal entre les tableaux 
length(humdata$temperature)
length(na.omit(humdata$temperature)) ## pour tester si NA
length(DSKdata$temperature)
length(na.omit(DSKdata$temperature)) ## pour tester si NA

##### et que l'étendue n'est pas la même
range(humdata$temperature, na.rm = TRUE)
range(DSKdata$temperature, na.rm = TRUE)

### on calcule le nb de lignes des tableaux (pour l'habillage des histogrammes)
nr <- nrow(humdata)
nrdsk <- nrow(DSKdata)

#### on définit les breaks pour l'abscisse commune
BRt <- seq(from= -10, to= 35, by=2) ## tient compte des deux distributions

#### puis on fait l'histo en utilisant 

### freq=F => des fréquences relatives et pas des effectifs

hist(humdata$temperature, breaks = BRt,
     freq=F, # fréquences
     col="grey",
     main = "Average Temperatures on 14,657 reporting of ticks \n (France, july 2017 - february 2020), 1 191 days",
     ylab = "Densities",
     xlab = "Average Temperatures (T°C)"
     )

### calcul des paramètres pour la fonction lines() à superposer à l'histo

HH2 <- hist(DSKdata$temperature, breaks = BRt,  plot=F)
    
    #lines(HH2$mids, HH2$density, lwd = 2, col = "orange") ### courbe non lissée  ## SO
    lines(density(DSKdata$temperature, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel

text(10, 0.07, paste("Reporting"), cex = 1.2,  col = "black")
text(02, 0.06, paste("Random"), cex = 1.2,  col = "blue")
# text(8, .08, paste("N =",nr," Reporting" ), col = "black")

## Si on voulait le nb d'enregistrements tudiés à la place du texte "Darksky" ou MF, il faudrait :
    ### text(03, 0.07, paste("N =", sum(complete.cases(humdata$temperature))), cex = 1.2,  col = "black")
    ### text(02, 0.06, paste("N =", sum(complete.cases(DSKdata$temperature))), cex = 1.2,  col = "blue")

## Si on voulait les courbes à +1.5°C ou +2°C, il faudrait fabriquer et afficher :
    ### lines(density(DSKdata$temperature_p15D, na.rm = TRUE), lwd = 2, col = "green") ### courbe liss?e, kernel
    ### lines(density(DSKdata$temperature_p2D, na.rm = TRUE), lwd = 2, col = "blue") ### courbe liss?e, kernel



## 3.2. Test des distributions


### 3.2.1. Test si distribution normale

## References :
## Patrick Royston (1982). An extension of Shapiro and Wilk's W test for normality to large samples. Applied Statistics, 31, 115–124. doi: 10.2307/2347973.

### Echantillonnage de humdata$temperature pour avoir une taille d'échantillon < 5 000

# data[sample(1:nrow(data),n),] # pour faire un sous tableau échantillonné http://forums.cirad.fr/logiciel-r/viewtopic.php?t=6118

shapiro.test(humdata[sample(1:nrow(humdata),nrdsk),"temperature"]) # pour échantillonner une seule variable du data frame
        # ici n = nrdsk le nb de lignes du DF des 700 données météo
    # W = 0.99248, p-value = 1.002e-05 => p-value significative, l'échantillon ne suit pas une loi normale.

# shapiro.test(humdata$temperature) # uniquement si nrow < 5 000 (pas le cas ici !)

shapiro.test(DSKdata$temperature) # W = 0.98104, p-value = 2.268e-11 => p-value significative, l'échantillon ne suit pas une loi normale.


### t.test entre les différentes températures
t.test(DSKdata$temperature,humdata$temperature) # t = -29.987, df = 1297.1, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents (mais ne suivent pas une loi normale) !

# t.test(DSKdata$temperature_p1D,humdata$temperature) ## SO
# t.test(DSKdata$temperature_p15D,humdata$temperature) ## SO
# t.test(DSKdata$temperature_p2D,humdata$temperature) ## SO



### 3.2.2. Test si distribution non normale

## References :
## Myles Hollander and Douglas A. Wolfe (1973), Nonparametric Statistical Methods. New York: John Wiley & Sons. Pages 115–120.

## http://www.sthda.com/english/wiki/kruskal-wallis-test-in-r

### Import des données pour le test kruskal.test() sur données DSKdata$temperature, humdata$temperature in : KW_temp_maille_42.csv

# /!\ méthode qui crée des tableaux intermediaires ! Juste en exemple, car on saute au subset à partir de maintenant !

# kwtempdata <- read.csv("../../data/donnee_meteo_nationale_comparative/comparaison/KW_temp_maille_42.csv", header = TRUE, sep = ";", dec = ".")
# summary(kwtempdata)

# kruskal.test(temp ~ type, data = kwtempdata) ## ~ signifie : "en fonction de"

## Kruskal-Wallis chi-squared = 2.5559, df = 1, p-value = 0.1099 => p-value non significative, les 2 échantillons ne sont pas significativement différents !
### Import des données pour le test kruskal.test() sur données MFdata$point_rose, DSKdata$dewpoint

## création de deux sous-ensembles "subsets" avec juste les variables à comparer et l'origine de l'éditeur 
    ### ajout d'une colonne à droite contenant 1 var quali (si val num x$new=10:12) pour indiquer l'origine des données
    ### si ajout à gauche <cbind(10:12,x)> http://forums.cirad.fr/logiciel-R/viewtopic.php?t=9737
humdata$origine="hum"
d1 <- subset(humdata, select = c("temperature", "origine")) ## création du subset venant de humdata http://larmarange.github.io/analyse-R/fusion-de-tables.html
dim(d1)
head(d1, 3)
d2 <- subset(DSKdata, select = c("temperature", "origine")) ## création du subset venant de DSK
dim(d2)
head(d2, 3)

## pour utiliser la fusion "rbind" il faut que les champs aient le même nom (ici pas de problème)
#names(d2)[1]
#names(d2)[1]<-"dewpoint" ## renommage du 1er champ du tableau 2 https://forum.framasoft.org/viewtopic.php?p=75052
#names(d2)[1]

kwtempdata <- rbind(d1, d2) ## fusion des deux sous tableaux
head(kwtempdata)

summary(kwtempdata)

kruskal.test(temperature ~ origine, data = kwtempdata) ## ~ signifie : "en fonction de"

## Kruskal-Wallis chi-squared = 847.21, df = 1, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents !

summary(humdata$temperature)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    #  -3.90   14.74   17.97   17.58   20.89   32.77      74 
summary(DSKdata$temperature)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    # -5.320   6.745  11.065  11.858  17.225  28.260  


### 3.2.3. Calcul d'un IC via le test de Wilcoxon sur la médiane (cf. section 3.2 de Poinsot et C:\3VG\MSH\Lyme\Smartick17\data\TCD7\R\IC_foret_AL_CLC5.R)

### Comme distribution non normale

wilcox.test(humdata$temperature,
            conf.int=TRUE
            )$conf.int ## cette ligne pour demander l'IC 

### [1] 17.69999 17.85501
### attr(,"conf.level")
### [1] 0.95

### NO Warning messages:

### Soit un IC à 95% = [17.7 ; 17.9] autour de la /!\ médiane /!\


wilcox.test(DSKdata$temperature,
            conf.int=TRUE
)$conf.int ## cette ligne pour demander l'IC 

### [1] 11.39496 12.19495
### attr(,"conf.level")
### [1] 0.95

### Soit un IC à 95% = [11.4 ; 12.2] autour de la /!\ médiane /!\


# 4. Analyse des températures maximales quotidiennes DSK vs données humaines (méthode Alice Favre)

## 4.1. Histogramme des températures maximales quotidiennes "temperaturehigh" pour les signalements et "temperaturehigh" pour DSK pour 700 points darksky

#### Après avoir :

#### vérifié que le nombre d'obs n'est pas égal entre les tableaux 
length(humdata$temperaturehigh)
length(na.omit(humdata$temperaturehigh)) ## pour tester si NA
length(DSKdata$temperaturehigh)
length(na.omit(DSKdata$temperaturehigh)) ## pour tester si NA

##### et que l'étendue n'est pas la même
range(humdata$temperaturehigh, na.rm = TRUE)
range(DSKdata$temperaturehigh, na.rm = TRUE)

### on calcule le nb de lignes des tableaux (pour l'habillage des histogrammes)
nr <- nrow(humdata)
nrdsk <- nrow(DSKdata)

#### on définit les breaks pour l'abscisse commune
BRt <- seq(from= -10, to= 50, by=2) ## tient compte des deux distributions

#### puis on fait l'histo en utilisant 

### freq=F => des fréquences relatives et pas des effectifs

hist(humdata$temperaturehigh, breaks = BRt,
     freq=F, # fréquences
     col="grey",
     main = "Average Daily High Temperature\n on 14,657 reporting of ticks \n (France, july 2017 - february 2020), 1 191 days",
     ylab = "Densities",
     xlab = "Average High Temperatures (T°C)"
)

### calcul des paramètres pour la fonction lines() à superposer à l'histo

HH2 <- hist(DSKdata$temperaturehigh, breaks = BRt,  plot=F)

#lines(HH2$mids, HH2$density, lwd = 2, col = "orange") ### courbe non lissée  ## SO
lines(density(DSKdata$temperaturehigh, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel

text(12, 0.06, paste("Reporting"), cex = 1.2,  col = "black")
text(01, 0.04, paste("Random"), cex = 1.2,  col = "blue")
# text(8, .08, paste("N =",nr," Reporting" ), col = "black")

## Si on voulait le nb d'enregistrements tudiés à la place du texte "Darksky" ou MF, il faudrait :
### text(03, 0.07, paste("N =", sum(complete.cases(humdata$temperaturehigh))), cex = 1.2,  col = "black")
### text(02, 0.06, paste("N =", sum(complete.cases(DSKdata$temperaturehigh))), cex = 1.2,  col = "blue")

## Si on voulait les courbes à +1.5°C ou +2°C, il faudrait fabriquer et afficher :
### lines(density(DSKdata$temperaturehigh_p15D, na.rm = TRUE), lwd = 2, col = "green") ### courbe liss?e, kernel
### lines(density(DSKdata$temperaturehigh_p2D, na.rm = TRUE), lwd = 2, col = "blue") ### courbe liss?e, kernel
    

### 4.2. Test des distributions

### 4.2.1. Test si distribution normale


## References :
## Patrick Royston (1982). An extension of Shapiro and Wilk's W test for normality to large samples. Applied Statistics, 31, 115–124. doi: 10.2307/2347973.

### Echantillonnage de humdata$temperaturehigh pour avoir une taille d'échantillon < 5 000

# data[sample(1:nrow(data),n),] # pour faire un sous tableau échantillonné http://forums.cirad.fr/logiciel-r/viewtopic.php?t=6118

shapiro.test(humdata[sample(1:nrow(humdata),nrdsk),"temperature"]) # pour échantillonner une seule variable du data frame
# ici n = nrdsk le nb de lignes du DF des 700 données météo
# W = 0.98675, p-value = 6.996e-09 => p-value significative, l'échantillon ne suit pas une loi normale.

# shapiro.test(humdata$temperaturehigh) # uniquement si nrow < 5 000 (pas le cas ici !)

shapiro.test(DSKdata$temperaturehigh) # W = 0.97665, p-value = 6.028e-13 => p-value significative, l'échantillon ne suit pas une loi normale.


### t.test entre les différentes températures
t.test(DSKdata$temperaturehigh,humdata$temperaturehigh) # t = -29.621, df = 1296.3, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents (mais ne suivent pas une loi normale) !

# t.test(DSKdata$temperaturehigh_p1D,humdata$temperaturehigh) ## SO
# t.test(DSKdata$temperaturehigh_p15D,humdata$temperaturehigh) ## SO
# t.test(DSKdata$temperaturehigh_p2D,humdata$temperaturehigh) ## SO



### 4.2. Test si distribution non normale


### Import des données pour le test kruskal.test() sur données MFdata$point_rose, DSKdata$dewpoint

## création de deux sous-ensembles "subsets" avec juste les variables à comparer et l'origine de l'éditeur 
### ajout d'une colonne à droite contenant 1 var quali (si val num x$new=10:12) pour indiquer l'origine des données
### si ajout à gauche <cbind(10:12,x)> http://forums.cirad.fr/logiciel-R/viewtopic.php?t=9737
humdata$origine="hum"
d1 <- subset(humdata, select = c("temperaturehigh", "origine")) ## création du subset venant de humdata http://larmarange.github.io/analyse-R/fusion-de-tables.html
dim(d1)
head(d1, 3)
d2 <- subset(DSKdata, select = c("temperaturehigh", "origine")) ## création du subset venant de DSK
dim(d2)
head(d2, 3)

## pour utiliser la fusion "rbind" il faut que les champs aient le même nom (ici pas de problème)
#names(d2)[1]
#names(d2)[1]<-"dewpoint" ## renommage du 1er champ du tableau 2 https://forum.framasoft.org/viewtopic.php?p=75052
#names(d2)[1]

kwtempdata <- rbind(d1, d2) ## fusion des deux sous tableaux
head(kwtempdata)

summary(kwtempdata)

kruskal.test(temperaturehigh ~ origine, data = kwtempdata) ## ~ signifie : "en fonction de"

## Kruskal-Wallis chi-squared = 826.75, df = 1, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents !

summary(humdata$temperaturehigh)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# -0.13   19.84   23.49   23.19   27.00   43.27       8
summary(DSKdata$temperaturehigh)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -1.14   10.37   15.66   16.58   22.77   36.20   


### 4.3. Calcul d'un IC via le test de Wilcoxon sur la médiane (cf. section 3.2 de Poinsot et C:\3VG\MSH\Lyme\Smartick17\data\TCD7\R\IC_foret_AL_CLC5.R)

### Comme distribution non normale

wilcox.test(humdata$temperaturehigh,
            conf.int=TRUE
)$conf.int ## cette ligne pour demander l'IC 

### [1] 23.26005 23.44494
### attr(,"conf.level")
### [1] 0.95

### NO Warning messages:

### Soit un IC à 95% = [23.3 ; 23.4] autour de la /!\ médiane /!\


wilcox.test(DSKdata$temperaturehigh,
            conf.int=TRUE
)$conf.int ## cette ligne pour demander l'IC 

### [1] 16.02999 16.94501
### attr(,"conf.level")
### [1] 0.95

### Soit un IC à 95% = [16.3 ; 16.9] autour de la /!\ médiane /!\

    
# 5. Analyse de l'humidité relative DSK vs données humaines (méthode Alice Favre)

## 5.1. Histogramme de l'humidité relative "humidity" pour les signalements et "humidity" pour DSK pour 700 points darksky

#### Après avoir :

#### vérifié que le nombre d'obs n'est pas égal entre les tableaux 
length(humdata$humidity)
length(na.omit(humdata$humidity)) ## pour tester si NA
length(DSKdata$humidity)
length(na.omit(DSKdata$humidity)) ## pour tester si NA

##### et que l'étendue n'est pas la même
range(humdata$humidity, na.rm = TRUE)
range(DSKdata$humidity, na.rm = TRUE)

### on calcule le nb de lignes des tableaux (pour l'habillage des histogrammes)
nr <- nrow(humdata)
nrdsk <- nrow(DSKdata)

#### on définit les breaks pour l'abscisse commune
BRt <- seq(from= 20, to= 100, by=2) ## tient compte des deux distributions

#### puis on fait l'histo en utilisant 

### freq=F => des fréquences relatives et pas des effectifs

hist(humdata$humidity, breaks = BRt,
     freq=F, # fréquences
     col="grey",
     main = "Average Humidity on 14,657 reporting of ticks \n (France, july 2017 - february 2020), 1 191 days",
     ylab = "Densities",
     xlab = "Average Humidity (%)"
)

### calcul des paramètres pour la fonction lines() à superposer à l'histo

HH2 <- hist(DSKdata$humidity*100, breaks = BRt,  plot=F)


#lines(HH2$mids, HH2$density, lwd = 2, col = "orange") ### courbe non lissée  ## SO
lines(density(DSKdata$humidity*100, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel et création d'un champ humidité en % /!\

text(35, 0.015, paste("Reporting"), cex = 1.2,  col = "black")
text(45, 0.03, paste("Random"), cex = 1.2,  col = "blue")
# text(8, .08, paste("N =",nr," Reporting" ), col = "black")

## Si on voulait le nb d'enregistrements tudiés à la place du texte "Darksky" ou MF, il faudrait :
### text(03, 0.07, paste("N =", sum(complete.cases(humdata$humidity))), cex = 1.2,  col = "black")
### text(02, 0.06, paste("N =", sum(complete.cases(DSKdata$humidity))), cex = 1.2,  col = "blue")

## Si on voulait les courbes à +1.5°C ou +2°C, il faudrait fabriquer et afficher :
### lines(density(DSKdata$humidity_p15D, na.rm = TRUE), lwd = 2, col = "green") ### courbe liss?e, kernel
### lines(density(DSKdata$humidity_p2D, na.rm = TRUE), lwd = 2, col = "blue") ### courbe liss?e, kernel


### 5.2. Test des distributions

### 5.2.1. Test si distribution normale


## References :
## Patrick Royston (1982). An extension of Shapiro and Wilk's W test for normality to large samples. Applied Statistics, 31, 115–124. doi: 10.2307/2347973.

### Echantillonnage de humdata$humidity pour avoir une taille d'échantillon < 5 000

# data[sample(1:nrow(data),n),] # pour faire un sous tableau échantillonné http://forums.cirad.fr/logiciel-r/viewtopic.php?t=6118

shapiro.test(humdata[sample(1:nrow(humdata),nrdsk),"temperature"]) # pour échantillonner une seule variable du data frame
# ici n = nrdsk le nb de lignes du DF des 700 données météo
# W = 0.98601, p-value = 3.196e-09 => p-value significative, l'échantillon ne suit pas une loi normale.

# shapiro.test(humdata$humidity) # uniquement si nrow < 5 000 (pas le cas ici !)

shapiro.test(DSKdata$humidity) # W = 0.98061, p-value = 1.55e-11 => p-value significative, l'échantillon ne suit pas une loi normale.


### t.test entre les différentes températures
t.test(DSKdata$humidity*100,humdata$humidity) # t = 12.431, df = 1492.5, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents (mais ne suivent pas une loi normale) !

# t.test(DSKdata$humidity_p1D,humdata$humidity) ## SO
# t.test(DSKdata$humidity_p15D,humdata$humidity) ## SO
# t.test(DSKdata$humidity_p2D,humdata$humidity) ## SO



### 5.2. Test si distribution non normale


### Import des données pour le test kruskal.test() sur données MFdata$point_rose, DSKdata$dewpoint

## création de deux sous-ensembles "subsets" avec juste les variables à comparer et l'origine de l'éditeur 
### ajout d'une colonne à droite contenant 1 var quali (si val num x$new=10:12) pour indiquer l'origine des données
### si ajout à gauche <cbind(10:12,x)> http://forums.cirad.fr/logiciel-R/viewtopic.php?t=9737
humdata$origine="hum"
d1 <- subset(humdata, select = c("humidity", "origine")) ## création du subset venant de humdata http://larmarange.github.io/analyse-R/fusion-de-tables.html
dim(d1)
head(d1, 3)
DSKdata$humid100=DSKdata$humidity*100 ## création d'un champ humidité en %
d2 <- subset(DSKdata, select = c("humid100", "origine")) ## création du subset venant de DSK
dim(d2)
head(d2, 3)

## pour utiliser la fusion "rbind" il faut que les champs aient le même nom (c'est le cas ici)
names(d1)[1]
names(d1)[1]<-"humid100" ## renommage du 1er champ du tableau 1 https://forum.framasoft.org/viewtopic.php?p=75052
names(d1)[1]

kwhumidata <- rbind(d1, d2) ## fusion des deux sous tableaux
head(kwhumidata)

summary(kwhumidata)

kruskal.test(humid100 ~ origine, data = kwhumidata) ## ~ signifie : "en fonction de"

## Kruskal-Wallis chi-squared = 120.72, df = 1, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents !

summary(humdata$humidity)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 22.00   64.00   72.00   71.51   79.00  100.00       6 
summary(DSKdata$humid100)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 48.00   68.00   76.00   74.97   82.00   93.00   


### 5.3. Calcul d'un IC via le test de Wilcoxon sur la médiane (cf. section 3.2 de Poinsot et C:\3VG\MSH\Lyme\Smartick17\data\TCD7\R\IC_foret_AL_CLC5.R)

### Comme distribution non normale

wilcox.test(humdata$humidity,
            conf.int=TRUE
)$conf.int ## cette ligne pour demander l'IC 

### [1] 71.50000 71.99999
### attr(,"conf.level")
### [1] 0.95

### NO Warning messages:

### Soit un IC à 95% = [71.5 ; 72.0] autour de la /!\ médiane /!\


wilcox.test(DSKdata$humid100,
            conf.int=TRUE
)$conf.int ## cette ligne pour demander l'IC 

### [1] 74.50005 75.99997
### attr(,"conf.level")
### [1] 0.95

### Soit un IC à 95% = [74.5 ; 76.0] autour de la /!\ médiane /!\



    
    
    
    
    
    
    
    
    
