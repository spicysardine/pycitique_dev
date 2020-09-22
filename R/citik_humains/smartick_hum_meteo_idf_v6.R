######
###### Analyse des signalements et des données Météo DSK (non plus moyennées mais DSK brutes) pour l'Île-de-France

# 1. Introduction
### Alloue la librairie "set()" puis vérifie que c'est bien importé "getwd()", il pointe dessus!

## 1.1 XPS13
setwd("C:/3VG/MSH/lyme/Smartick17/pycitique_miror/R")
getwd()

#Remove all objects
rm(list = ls() )
rm()

## 1.2 OP7570
setwd("D:/3VG/MSH/lyme/Smartick17/pycitique_miror/R")
getwd()

#Remove all objects
rm(list = ls() )
rm()

# 2. Lab 1 : Importation de données citik_humains_clean_weather_strict.csv

### Or, if .csv file, use this si données séparées par ","
### quote = "'" pour le caractère d’échappement
### stringAsFactpors pour voir les chaînes de caractères sans interprétation.
humdata <- read.csv("../../pycitique/data/donnee_signalements_avec_meteo_dsk/citik_humains_clean_weather_strict.csv", sep = ",", dec = ".", quote = "'", stringsAsFactors = FALSE)

DSKdata <- read.csv("../../pycitique/data/donnee_meteo_nationale_comparative/darksky/darksky_donnee_brute_700_def3.csv", sep = ",", dec = ".", quote = "'", stringsAsFactors = FALSE)

### Création des subset pour l'IDF

humdataIDF <- subset(humdata, humdata$departement_code == "75" |  humdata$departement_code == "77" |  humdata$departement_code == "78" |  humdata$departement_code == "91" |  humdata$departement_code == "92" | humdata$departement_code == "93" |  humdata$departement_code == "94" | humdata$departement_code == "95")

DSKdataIDF <- subset(DSKdata, DSKdata$departement_code == "75" |  DSKdata$departement_code == "77" |  DSKdata$departement_code == "78" |  DSKdata$departement_code == "91" |  DSKdata$departement_code == "92" | DSKdata$departement_code == "93" |  DSKdata$departement_code == "94" | DSKdata$departement_code == "95")


## 2.1 Analyse du tableau de données

ls(humdataIDF) ## liste les variables
ls(DSKdataIDF)


str(humdataIDF) ##  a diagnostic function and an alternative to summary
str(DSKdataIDF)


summary(humdataIDF)
summary(DSKdataIDF)



# 3. Analyse des Températures moyennes DSK vs signalements (méthode Alice Favre)

## 3.1. Histogramme des Températures moyennes "temperature" pour les signalements et "temperature" pour DSK = (tempHigh + tempLow)/2 pour 700 points darksky

#### Après avoir :

#### vérifié que le nombre d'obs n'est pas égal entre les tableaux 
length(humdataIDF$temperature)
length(na.omit(humdataIDF$temperature)) ## pour tester si NA

length(DSKdataIDF$temperature)
length(na.omit(DSKdataIDF$temperature)) ## pour tester si NA

##### et que l'étendue n'est pas la même
range(humdataIDF$temperature, na.rm = TRUE)
range(DSKdataIDF$temperature, na.rm = TRUE)

### on calcule le nb de lignes des tableaux (pour l'habillage des histogrammes)
nr <- nrow(humdataIDF)
nrdsk <- nrow(DSKdataIDF)

#### on définit les breaks pour l'abscisse commune
BRt <- seq(from= -10, to= 35, by=2) ## tient compte des deux distributions

#### puis on fait l'histo en utilisant 

### freq=F => des fréquences relatives et pas des effectifs

hist(humdataIDF$temperature, breaks = BRt,
     freq=F, # fréquences
     col="grey",
     main = "Average Temperatures on 1,746 reporting of ticks \n (Île-de-France, july 2017 - april 2020), 995 days",
     ylab = "Densities",
     xlab = "Average Temperatures (T°C)"
     )

### calcul des paramètres pour la fonction lines() à superposer à l'histo

HH2 <- hist(DSKdataIDF$temperature, breaks = BRt,  plot=F)
    
    #lines(HH2$mids, HH2$density, lwd = 2, col = "orange") ### courbe non lissée  ## SO
    lines(density(DSKdataIDF$temperature, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel

text(10, 0.07, paste("Reporting"), cex = 1.2,  col = "black")
text(02, 0.06, paste("Random"), cex = 1.2,  col = "blue")
# text(8, .08, paste("N =",nr," Reporting" ), col = "black")

## Si on voulait le nb d'enregistrements tudiés à la place du texte "Darksky" ou MF, il faudrait :
    ### text(03, 0.07, paste("N =", sum(complete.cases(humdataIDF$temperature))), cex = 1.2,  col = "black")
    ### text(02, 0.06, paste("N =", sum(complete.cases(DSKdataIDF$temperature))), cex = 1.2,  col = "blue")

## Si on voulait les courbes à +1.5°C ou +2°C, il faudrait fabriquer et afficher :
    ### lines(density(DSKdataIDF$temperature_p15D, na.rm = TRUE), lwd = 2, col = "green") ### courbe lissée, kernel
    ### lines(density(DSKdataIDF$temperature_p2D, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel



## 3.2. Test des distributions


### 3.2.1. Test si distribution normale

## References :
## Patrick Royston (1982). An extension of Shapiro and Wilk's W test for normality to large samples. Applied Statistics, 31, 115–124. doi: 10.2307/2347973.

### Echantillonnage de humdataIDF$temperature pour avoir une taille d'échantillon < 5 000

# data[sample(1:nrow(data),n),] # pour faire un sous tableau échantillonné http://forums.cirad.fr/logiciel-r/viewtopic.php?t=6118

shapiro.test(humdataIDF[sample(1:nrow(humdataIDF),30),"temperature"]) # pour échantillonner une seule variable du data frame
    # plus le n augmente, plus la p-value devient significative (si < 5 000 !)
    # 
    # W = 0.99248, p-value = 1.002e-05 => p-value significative, l'échantillon ne suit pas une loi normale.

# shapiro.test(humdataIDF$temperature) # uniquement si nrow < 5 000 (pas le cas ici !)

shapiro.test(DSKdataIDF[sample(1:nrow(DSKdataIDF),300),"temperature"]) 
    # plus le n augmente, plus la p-value devient significative (si < 5 000 !)
    #  
    # W = 0.98104, p-value = 2.268e-11 => p-value significative, l'échantillon ne suit pas une loi normale.


### t.test entre les différentes températures
t.test(DSKdataIDF$temperature,humdataIDF$temperature) # t = -49.71, df = 2629.9, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents (mais ne suivent pas une loi normale) !

# t.test(DSKdataIDF$temperature_p1D,humdataIDF$temperature) ## SO
# t.test(DSKdataIDF$temperature_p15D,humdataIDF$temperature) ## SO
# t.test(DSKdataIDF$temperature_p2D,humdataIDF$temperature) ## SO



### 3.2.2. Test si distribution non normale

## References :
## Myles Hollander and Douglas A. Wolfe (1973), Nonparametric Statistical Methods. New York: John Wiley & Sons. Pages 115–120.

## http://www.sthda.com/english/wiki/kruskal-wallis-test-in-r

### Import des données pour le test kruskal.test() sur données DSKdataIDF$temperature, humdataIDF$temperature in : KW_temp_maille_42.csv (? à vérifier !)

# /!\ méthode qui crée des tableaux intermediaires ! Juste en exemple, car on saute au subset à partir de maintenant !

# kwtempdata <- read.csv("../../data/donnee_meteo_nationale_comparative/comparaison/KW_temp_maille_42.csv", header = TRUE, sep = ";", dec = ".")
# summary(kwtempdata)

# kruskal.test(temp ~ type, data = kwtempdata) ## ~ signifie : "en fonction de"

## Kruskal-Wallis chi-squared = 2.5559, df = 1, p-value = 0.1099 => p-value non significative, les 2 échantillons ne sont pas significativement différents !
### Import des données pour le test kruskal.test() sur données DSKdataIDF$temperature, humdataIDF$temperature

## création de deux sous-ensembles "subsets" avec juste les variables à comparer et l'origine de l'éditeur 
    ### ajout d'une colonne à droite contenant 1 var quali (si val num x$new=10:12) pour indiquer l'origine des données
    ### si ajout à gauche <cbind(10:12,x)> http://forums.cirad.fr/logiciel-R/viewtopic.php?t=9737
humdataIDF$origine="hum"
d1 <- subset(humdataIDF, select = c("temperature", "origine")) ## création du subset venant de humdataIDF http://larmarange.github.io/analyse-R/fusion-de-tables.html
dim(d1)
head(d1, 3)
d2 <- subset(DSKdataIDF, select = c("temperature", "origine")) ## création du subset venant de DSK
dim(d2)
head(d2, 3)

## pour utiliser la fusion "rbind" il faut que les champs aient le même nom (ici pas de problème)
#names(d2)[1]
#names(d2)[1]<-"temperature" ## renommage du 1er champ du tableau 2 https://forum.framasoft.org/viewtopic.php?p=75052
#names(d2)[1]

kwtempdata <- rbind(d1, d2) ## fusion des deux sous tableaux
head(kwtempdata)

summary(kwtempdata)

kruskal.test(temperature ~ origine, data = kwtempdata) ## ~ signifie : "en fonction de"

## Kruskal-Wallis chi-squared = 1325.8, df = 1, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents !

summary(humdataIDF$temperature)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    # -1.28   15.72   18.57   18.21   21.41   31.30  
summary(DSKdataIDF$temperature)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    # -7.46    7.02   11.62   12.20   17.33   32.22 


### 3.2.3. Calcul d'un IC via le test de Wilcoxon sur la médiane (cf. section 3.2 de Poinsot et C:\3VG\MSH\Lyme\Smartick17\data\TCD7\R\IC_foret_AL_CLC5.R)

### Comme distribution non normale

wilcox.test(humdataIDF$temperature,
            conf.int=TRUE
            )$conf.int ## cette ligne pour demander l'IC 

### [1] 18.20501 18.64001
### attr(,"conf.level")
### [1] 0.95

### NO Warning messages:

### Soit un IC à 95% = [18.2 ; 18.6] autour de la /!\ médiane /!\


wilcox.test(DSKdataIDF$temperature,
            conf.int=TRUE
)$conf.int ## cette ligne pour demander l'IC 

### [1] 12.01508 12.23747
### attr(,"conf.level")
### [1] 0.95

### Soit un IC à 95% = [12.0 ; 12.2] autour de la /!\ médiane /!\




# 4. Analyse des températures maximales quotidiennes DSK vs données humaines (méthode Alice Favre)

## 4.1. Histogramme des températures maximales quotidiennes "temperaturehigh" pour les signalements et "temperaturehigh" pour DSK pour 700 points darksky

#### Après avoir :

#### vérifié que le nombre d'obs n'est pas égal entre les tableaux 
length(humdataIDF$temperaturehigh)
length(na.omit(humdataIDF$temperaturehigh)) ## pour tester si NA
length(DSKdataIDF$temperaturehigh)
length(na.omit(DSKdataIDF$temperaturehigh)) ## pour tester si NA

##### et que l'étendue n'est pas la même
range(humdataIDF$temperaturehigh, na.rm = TRUE)
range(DSKdataIDF$temperaturehigh, na.rm = TRUE)

### on calcule le nb de lignes des tableaux (pour l'habillage des histogrammes)
nr <- nrow(humdataIDF)
nrdsk <- nrow(DSKdataIDF)

#### on définit les breaks pour l'abscisse commune
BRt <- seq(from= -5, to= 45, by=2) ## tient compte des deux distributions

#### puis on fait l'histo en utilisant 

### freq=F => des fréquences relatives et pas des effectifs

hist(humdataIDF$temperaturehigh, breaks = BRt,
     freq=F, # fréquences
     col="grey",
     main = "Average Daily High Temperature\n on 1,746 reporting of ticks \n (Île-de-France, july 2017 - april 2020), 995 days",
     ylab = "Densities",
     xlab = "Average High Temperatures (T?C)"
)

### calcul des paramètres pour la fonction lines() à superposer à l'histo

HH2 <- hist(DSKdataIDF$temperaturehigh, breaks = BRt,  plot=F)

#lines(HH2$mids, HH2$density, lwd = 2, col = "orange") ### courbe non lissée  ## SO
lines(density(DSKdataIDF$temperaturehigh, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel

text(12, 0.06, paste("Reporting"), cex = 1.2,  col = "black")
text(01, 0.04, paste("Random"), cex = 1.2,  col = "blue")
# text(8, .08, paste("N =",nr," Reporting" ), col = "black")

## Si on voulait le nb d'enregistrements tudiés à la place du texte "Darksky" ou MF, il faudrait :
### text(03, 0.07, paste("N =", sum(complete.cases(humdataIDF$temperaturehigh))), cex = 1.2,  col = "black")
### text(02, 0.06, paste("N =", sum(complete.cases(DSKdataIDF$temperaturehigh))), cex = 1.2,  col = "blue")

## Si on voulait les courbes à +1.5°C ou +2°C, il faudrait fabriquer et afficher :
### lines(density(DSKdataIDF$temperaturehigh_p15D, na.rm = TRUE), lwd = 2, col = "green") ### courbe lissée, kernel
### lines(density(DSKdataIDF$temperaturehigh_p2D, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel
    

### 4.2. Test des distributions

### 4.2.1. Test si distribution normale


## References :
## Patrick Royston (1982). An extension of Shapiro and Wilk's W test for normality to large samples. Applied Statistics, 31, 115–124. doi: 10.2307/2347973.

### Echantillonnage de humdataIDF$temperaturehigh pour avoir une taille d'échantillon < 5 000

# data[sample(1:nrow(data),n),] # pour faire un sous tableau échantillonné http://forums.cirad.fr/logiciel-r/viewtopic.php?t=6118

shapiro.test(humdataIDF[sample(1:nrow(humdataIDF), 300),"temperaturehigh"]) # pour échantillonner une seule variable du data frame
# plus le n augmente, plus la p-value devient significative (si < 5 000 !)

# data:  humdataIDF[sample(1:nrow(humdataIDF), 30), "temperaturehigh"]
# W = 0.95724, p-value = 0.2628 => p-value non significative, l'échantillon suit une loi normale.

# data:  humdataIDF[sample(1:nrow(humdataIDF), 300), "temperaturehigh"]
# W = 0.98633, p-value = 0.006058 => p-value significative, l'échantillon ne suit pas une loi normale (diffère, ce coup ci. Mais pas ? tous les coups !).

# shapiro.test(humdataIDF$temperaturehigh) # uniquement si nrow < 5 000 (pas le cas ici !)

shapiro.test(DSKdataIDF[sample(1:nrow(DSKdataIDF),300),"temperaturehigh"])
# data:  DSKdataIDF[sample(1:nrow(DSKdataIDF), 30), "temperaturehigh"]
# W = 0.95467, p-value = 0.2251 => p-value non significative, l'échantillon suit une loi normale.

# data:  DSKdataIDF[sample(1:nrow(DSKdataIDF), 300), "temperaturehigh"]
# W = 0.98381, p-value = 0.001825 => p-value significative, l'échantillon ne suit pas une loi normale (diffère, ce coup ci. Mais peut-?tre pas ? tous les coups !).




### t.test entre les différentes températures maximales
t.test(DSKdataIDF$temperaturehigh,humdataIDF$temperaturehigh) # t = -49.221, df = 2668.8, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents (mais ne suivent pas une loi normale) !

# t.test(DSKdataIDF$temperaturehigh_p1D,humdataIDF$temperaturehigh) ## SO
# t.test(DSKdataIDF$temperaturehigh_p15D,humdataIDF$temperaturehigh) ## SO
# t.test(DSKdataIDF$temperaturehigh_p2D,humdataIDF$temperaturehigh) ## SO



### 4.2.2. Test si distribution non normale


### Import des données pour le test kruskal.test() sur données DSKdataIDF$temperaturehigh, humdataIDF$temperaturehigh

## création de deux sous-ensembles "subsets" avec juste les variables à comparer et l'origine de l'éditeur 
### ajout d'une colonne à droite contenant 1 var quali (si val num x$new=10:12) pour indiquer l'origine des données
### si ajout à gauche <cbind(10:12,x)> http://forums.cirad.fr/logiciel-R/viewtopic.php?t=9737
humdataIDF$origine="hum"
d1 <- subset(humdataIDF, select = c("temperaturehigh", "origine")) ## création du subset venant de humdataIDF http://larmarange.github.io/analyse-R/fusion-de-tables.html
dim(d1)
head(d1, 3)
d2 <- subset(DSKdataIDF, select = c("temperaturehigh", "origine")) ## création du subset venant de DSK
dim(d2)
head(d2, 3)

## pour utiliser la fusion "rbind" il faut que les champs aient le même nom (ici pas de problème)
#names(d2)[1]
#names(d2)[1]<-"temperaturehigh" ## renommage du 1er champ du tableau 2 https://forum.framasoft.org/viewtopic.php?p=75052
#names(d2)[1]

kwtemphighdata <- rbind(d1, d2) ## fusion des deux sous tableaux
head(kwtemphighdata)

summary(kwtemphighdata)

kruskal.test(temperaturehigh ~ origine, data = kwtemphighdata) ## ~ signifie : "en fonction de"

## Kruskal-Wallis chi-squared = 1269.2, df = 1, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents !

summary(humdataIDF$temperaturehigh)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -0.13   20.49   23.93   23.53   27.15   41.44 
summary(DSKdataIDF$temperaturehigh)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -3.26   10.29   15.73   16.57   22.82   42.21 


### 4.3. Calcul d'un IC via le test de Wilcoxon sur la médiane (cf. section 3.2 de Poinsot et C:\3VG\MSH\Lyme\Smartick17\data\TCD7\R\IC_foret_AL_CLC5.R)

### Comme distribution non normale

wilcox.test(humdataIDF$temperaturehigh,
            conf.int=TRUE
)$conf.int ## cette ligne pour demander l'IC 

### [1] 23.45497 23.95493
### attr(,"conf.level")
### [1] 0.95

### NO Warning messages:

### Soit un IC à 95% = [23.5 ; 24.0] autour de la /!\ médiane /!\


wilcox.test(DSKdataIDF$temperaturehigh,
            conf.int=TRUE
)$conf.int ## cette ligne pour demander l'IC 

### [1] 16.31005 16.57004
### attr(,"conf.level")
### [1] 0.95

### Soit un IC à 95% = [16.3 ; 16.6] autour de la /!\ médiane /!\



    
# 5. Analyse de l'humidité relative DSK vs données humaines (méthode Alice Favre)

## 5.1. Histogramme de l'humidité relative "humidity" pour les signalements et "humidity" pour DSK pour 700 points darksky

#### Après avoir :

#### vérifié que le nombre d'obs n'est pas égal entre les tableaux 
length(humdataIDF$humidity)
length(na.omit(humdataIDF$humidity)) ## pour tester si NA
length(DSKdataIDF$humidity)
length(na.omit(DSKdataIDF$humidity)) ## pour tester si NA

##### et que l'étendue n'est pas la même
range(humdataIDF$humidity, na.rm = TRUE)
range(DSKdataIDF$humidity, na.rm = TRUE)

### on calcule le nb de lignes des tableaux (pour l'habillage des histogrammes)
nr <- nrow(humdataIDF)
nrdsk <- nrow(DSKdataIDF)

#### on définit les breaks pour l'abscisse commune
BRt <- seq(from= 20, to= 100, by=2) ## tient compte des deux distributions

#### puis on fait l'histo en utilisant 

### freq=F => des fréquences relatives et pas des effectifs

hist(humdataIDF$humidity, breaks = BRt,
     freq=F, # fréquences
     col="grey",
     main = "Average Humidity on 1,746 reporting of ticks \n (Île-de-France, july 2017 - april 2020), 995 days",
     ylab = "Densities",
     xlab = "Average Humidity (%)"
)

### calcul des paramètres pour la fonction lines() à superposer à l'histo

### /!\ ici x100 pour compenser la différence d'unité /!\ 

HH2 <- hist(DSKdataIDF$humidity*100, breaks = BRt,  plot=F)


#lines(HH2$mids, HH2$density, lwd = 2, col = "orange") ### courbe non lissée  ## SO

### /!\ ici x100 pour compenser la différence d'unité /!\ 

lines(density(DSKdataIDF$humidity*100, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel et création d'un champ humidité en % /!\

text(35, 0.015, paste("Reporting"), cex = 1.2,  col = "black")
text(45, 0.03, paste("Random"), cex = 1.2,  col = "blue")
# text(8, .08, paste("N =",nr," Reporting" ), col = "black")

## Si on voulait le nb d'enregistrements tudiés à la place du texte "Darksky" ou MF, il faudrait :
### text(03, 0.07, paste("N =", sum(complete.cases(humdataIDF$humidity))), cex = 1.2,  col = "black")
### text(02, 0.06, paste("N =", sum(complete.cases(DSKdataIDF$humidity))), cex = 1.2,  col = "blue")

## Si on voulait les courbes à +1.5°C ou +2°C, il faudrait fabriquer et afficher :
### lines(density(DSKdataIDF$humidity_p15D, na.rm = TRUE), lwd = 2, col = "green") ### courbe lissée, kernel
### lines(density(DSKdataIDF$humidity_p2D, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel


### 5.2. Test des distributions

### 5.2.1. Test si distribution normale


## References :
## Patrick Royston (1982). An extension of Shapiro and Wilk's W test for normality to large samples. Applied Statistics, 31, 115–124. doi: 10.2307/2347973.

### Echantillonnage de humdataIDF$humidity pour avoir une taille d'échantillon < 5 000

# data[sample(1:nrow(data),n),] # pour faire un sous tableau échantillonné http://forums.cirad.fr/logiciel-r/viewtopic.php?t=6118

shapiro.test(humdataIDF[sample(1:nrow(humdataIDF),300),"humidity"]) # pour échantillonner une seule variable du data frame
# plus le n augmente, plus la p-value devient significative (si < 5 000 !)

# data:  humdataIDF[sample(1:nrow(humdataIDF), 30), "humidity"]
# W = 0.96216, p-value = 0.3513 => p-value non significative, l'échantillon suit une loi normale.

# data:  humdataIDF[sample(1:nrow(humdataIDF), 300), "humidity"]
# W = 0.99362, p-value = 0.2353 => p-value non significative, l'échantillon suit une loi normale.(diffère, ce coup ci. Mais pas à tous les coups !).

# shapiro.test(humdataIDF$humidity) # uniquement si nrow < 5 000 (pas le cas ici !)

shapiro.test(DSKdataIDF[sample(1:nrow(DSKdataIDF),30),"humidity"])
# data:  DSKdataIDF[sample(1:nrow(DSKdataIDF), 30), "humidity"]
# W = 0.94822, p-value = 0.1514 => p-value non significative, l'échantillon suit une loi normale.

# data:  DSKdataIDF[sample(1:nrow(DSKdataIDF), 300), "humidity"]
# W = 0.97556, p-value = 5.373e-05 => p-value significative, l'échantillon ne suit pas une loi normale (diffère, ce coup ci. Mais peut-?tre pas ? tous les coups !).


### t.test entre les différentes humidités relatives
t.test(DSKdataIDF$humidity*100,humdataIDF$humidity) # t = 20.727, df = 2332.2, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents (mais ne suivent pas une loi normale) !

# t.test(DSKdataIDF$humidity_p1D,humdataIDF$humidity) ## SO
# t.test(DSKdataIDF$humidity_p15D,humdataIDF$humidity) ## SO
# t.test(DSKdataIDF$humidity_p2D,humdataIDF$humidity) ## SO



### 5.2.2. Test si distribution non normale


### Import des données pour le test kruskal.test() sur données DSKdataIDF$humidity, humdataIDF$humidity

## création de deux sous-ensembles "subsets" avec juste les variables à comparer et l'origine de l'éditeur 
### ajout d'une colonne à droite contenant 1 var quali (si val num x$new=10:12) pour indiquer l'origine des données
### si ajout à gauche <cbind(10:12,x)> http://forums.cirad.fr/logiciel-R/viewtopic.php?t=9737
humdataIDF$origine="hum"
d1 <- subset(humdataIDF, select = c("humidity", "origine")) ## création du subset venant de humdataIDF http://larmarange.github.io/analyse-R/fusion-de-tables.html
dim(d1)
head(d1, 3)
DSKdataIDF$humid100=DSKdataIDF$humidity*100 ## création d'un champ humidité en %
### /!\ ici x100 pour compenser la différence d'unité /!\ 
d2 <- subset(DSKdataIDF, select = c("humid100", "origine")) ## création du subset venant de DSK
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

## Kruskal-Wallis chi-squared = 385.49, df = 1, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents !

summary(humdataIDF$humidity)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 34.00   62.00   68.00   68.95   76.00   96.00  
summary(DSKdataIDF$humid100)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 28.00   66.00   76.00   74.29   84.00   99.00   


### 5.3. Calcul d'un IC via le test de Wilcoxon sur la médiane (cf. section 3.2 de Poinsot et C:\3VG\MSH\Lyme\Smartick17\data\TCD7\R\IC_foret_AL_CLC5.R)

### Comme distribution non normale

wilcox.test(humdataIDF$humidity,
            conf.int=TRUE
)$conf.int ## cette ligne pour demander l'IC 

### [1] 68.49994 69.49992
### attr(,"conf.level")
### [1] 0.95

### NO Warning messages:

### Soit un IC à 95% = [68.5 ; 69.5] autour de la /!\ médiane /!\


wilcox.test(DSKdataIDF$humid100,
            conf.int=TRUE
)$conf.int ## cette ligne pour demander l'IC 

### [1] 74.50006 75.00000
### attr(,"conf.level")
### [1] 0.95

### Soit un IC à 95% = [74.5 ; 75.0] autour de la /!\ médiane /!\




# 6. Analyse des points de rosée DSK vs données humaines (méthode Alice Favre)

## 6.1. Histogramme des points de rosée "dewpoint" pour les signalements et "dewpoint" pour DSK pour 700 points darksky

#### Après avoir :

#### vérifié que le nombre d'obs n'est pas égal entre les tableaux 
length(humdataIDF$dewpoint)
length(na.omit(humdataIDF$dewpoint)) ## pour tester si NA
length(DSKdataIDF$dewpoint)
length(na.omit(DSKdataIDF$dewpoint)) ## pour tester si NA

##### et que l'étendue n'est pas la même
range(humdataIDF$dewpoint, na.rm = TRUE)
range(DSKdataIDF$dewpoint, na.rm = TRUE)

### on calcule le nb de lignes des tableaux (pour l'habillage des histogrammes)
nr <- nrow(humdataIDF)
nrdsk <- nrow(DSKdataIDF)

#### on définit les breaks pour l'abscisse commune
BRt <- seq(from= -15, to= 25, by=1) ## tient compte des deux distributions

#### puis on fait l'histo en utilisant 

### freq=F => des fréquences relatives et pas des effectifs

hist(humdataIDF$dewpoint, breaks = BRt,
     freq=F, # fréquences
     col="grey",
     main = "Average dewpoint on 1,746 reporting of ticks \n (Île-de-France, july 2017 - april 2020), 995 days",
     ylab = "Densities",
     xlab = "Average dewpoint (Td°C)"
)

### calcul des paramètres pour la fonction lines() à superposer à l'histo

HH2 <- hist(DSKdataIDF$dewpoint, breaks = BRt,  plot=F)

#lines(HH2$mids, HH2$density, lwd = 2, col = "orange") ### courbe non lissée  ## SO
lines(density(DSKdataIDF$dewpoint, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel

text(5, 0.08, paste("Reporting"), cex = 1.2,  col = "black")
text(-5, 0.04, paste("Random"), cex = 1.2,  col = "blue")
# text(8, .08, paste("N =",nr," Reporting" ), col = "black")

## Si on voulait le nb d'enregistrements tudiés à la place du texte "Darksky" ou MF, il faudrait :
### text(03, 0.07, paste("N =", sum(complete.cases(humdataIDF$dewpoint))), cex = 1.2,  col = "black")
### text(02, 0.06, paste("N =", sum(complete.cases(DSKdataIDF$dewpoint))), cex = 1.2,  col = "blue")

## Si on voulait les courbes à +1.5°C ou +2°C, il faudrait fabriquer et afficher :
### lines(density(DSKdataIDF$dewpoint_p15D, na.rm = TRUE), lwd = 2, col = "green") ### courbe lissée, kernel
### lines(density(DSKdataIDF$dewpoint_p2D, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel


### 6.2. Test des distributions

### 6.2.1. Test si distribution normale


## References :
## Patrick Royston (1982). An extension of Shapiro and Wilk's W test for normality to large samples. Applied Statistics, 31, 115–124. doi: 10.2307/2347973.

### Echantillonnage de humdataIDF$dewpoint pour avoir une taille d'échantillon < 5 000

# data[sample(1:nrow(data),n),] # pour faire un sous tableau échantillonné http://forums.cirad.fr/logiciel-r/viewtopic.php?t=6118

shapiro.test(humdataIDF[sample(1:nrow(humdataIDF),300),"dewpoint"]) # pour échantillonner une seule variable du data frame
# plus le n augmente, plus la p-value devient significative (si < 5 000 !)

# data:  humdataIDF[sample(1:nrow(humdataIDF), 30), "dewpoint"]
# W = 0.93913, p-value = 0.08619 => p-value non significative, l'échantillon suit une loi normale.

# data:  humdataIDF[sample(1:nrow(humdataIDF), 300), "dewpoint"]
# W = 0.96781, p-value = 3.093e-06 => p-value significative, l'échantillon ne suit pas une loi normale.(diffère, ce coup ci. Mais pas à tous les coups !).

# shapiro.test(humdataIDF$dewpoint) # uniquement si nrow < 5 000 (pas le cas ici !)

shapiro.test(DSKdataIDF[sample(1:nrow(DSKdataIDF),300),"dewpoint"])
# data:  DSKdataIDF[sample(1:nrow(DSKdataIDF), 30), "dewpoint"]
# W = 0.94835, p-value = 0.1527 => p-value non significative, l'échantillon suit une loi normale.

# data:  DSKdataIDF[sample(1:nrow(DSKdataIDF), 300), "dewpoint"]
# W = 0.98946, p-value = 0.02882 => p-value significative, l'échantillon ne suit pas une loi normale (diffère, ce coup ci. Mais peut-être pas ? tous les coups !).



### t.test entre les différents points de rosée
t.test(DSKdataIDF$dewpoint,humdataIDF$dewpoint) # t = -45.279, df = 2585.6, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents (mais ne suivent pas une loi normale) !

# t.test(DSKdataIDF$dewpoint_p1D,humdataIDF$dewpoint) ## SO
# t.test(DSKdataIDF$dewpoint_p15D,humdataIDF$dewpoint) ## SO
# t.test(DSKdataIDF$dewpoint_p2D,humdataIDF$dewpoint) ## SO



### 6.2. Test si distribution non normale


### Import des données pour le test kruskal.test() sur données DSKdataIDF$dewpoint, humdataIDF$dewpoint

## création de deux sous-ensembles "subsets" avec juste les variables à comparer et l'origine de l'éditeur 
### ajout d'une colonne à droite contenant 1 var quali (si val num x$new=10:12) pour indiquer l'origine des données
### si ajout à gauche <cbind(10:12,x)> http://forums.cirad.fr/logiciel-R/viewtopic.php?t=9737
humdataIDF$origine="hum"
d1 <- subset(humdataIDF, select = c("dewpoint", "origine")) ## création du subset venant de humdataIDF http://larmarange.github.io/analyse-R/fusion-de-tables.html
dim(d1)
head(d1, 3)
d2 <- subset(DSKdataIDF, select = c("dewpoint", "origine")) ## création du subset venant de DSK
dim(d2)
head(d2, 3)

## pour utiliser la fusion "rbind" il faut que les champs aient le même nom (ici pas de problème)
#names(d2)[1]
#names(d2)[1]<-"dewpoint" ## renommage du 1er champ du tableau 2 https://forum.framasoft.org/viewtopic.php?p=75052
#names(d2)[1]

kwdewdata <- rbind(d1, d2) ## fusion des deux sous tableaux
head(kwdewdata)

summary(kwdewdata)

kruskal.test(dewpoint ~ origine, data = kwdewdata) ## ~ signifie : "en fonction de"

## Kruskal-Wallis chi-squared = 1184.9, df = 1, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents !

summary(humdataIDF$dewpoint)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -5.400   9.385  12.015  11.690  14.828  22.180
summary(DSKdataIDF$dewpoint)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# -13.790   3.260   7.180   6.978  11.190  22.320  


### 6.3. Calcul d'un IC via le test de Wilcoxon sur la médiane (cf. section 3.2 de Poinsot et C:\3VG\MSH\Lyme\Smartick17\data\TCD7\R\IC_foret_AL_CLC5.R)

### Comme distribution non normale

wilcox.test(humdataIDF$dewpoint,
            conf.int=TRUE
)$conf.int ## cette ligne pour demander l'IC 

### [1] 11.71995 12.08501
### attr(,"conf.level")
### [1] 0.95

### NO Warning messages:

### Soit un IC à 95% = [11.7 ; 12.1] autour de la /!\ médiane /!\


wilcox.test(DSKdataIDF$dewpoint,
            conf.int=TRUE
)$conf.int ## cette ligne pour demander l'IC 

### [1] 7.045029 7.229974
### attr(,"conf.level")
### [1] 0.95

### Soit un IC à 95% = [7.1 ; 7.2] autour de la /!\ médiane /!\


# 7. Analyse des pressions atmosphériques DSK vs données humaines (méthode Alice Favre)

## 7.1. Histogramme des pressions atmosphériques "pressure" pour les signalements et "pressure" pour DSK pour 700 points darksky

#### Après avoir :

#### vérifié que le nombre d'obs n'est pas égal entre les tableaux 
length(humdataIDF$pressure)
length(na.omit(humdataIDF$pressure)) ## pour tester si NA
length(DSKdataIDF$pressure)
length(na.omit(DSKdataIDF$pressure)) ## pour tester si NA

##### et que l'étendue n'est pas la même
range(humdataIDF$pressure, na.rm = TRUE)
range(DSKdataIDF$pressure, na.rm = TRUE)

### on calcule le nb de lignes des tableaux (pour l'habillage des histogrammes)
nr <- nrow(humdataIDF)
nrdsk <- nrow(DSKdataIDF)

#### on définit les breaks pour l'abscisse commune
BRt <- seq(from= 980, to= 1050, by=5) ## tient compte des deux distributions

#### puis on fait l'histo en utilisant 

### freq=F => des fréquences relatives et pas des effectifs

hist(humdataIDF$pressure, breaks = BRt,
     freq=F, # fréquences
     col="grey",
     main = "Average pressure on 1,746 reporting of ticks \n (Île-de-France, july 2017 - april 2020), 995 days",
     ylab = "Densities",
     xlab = "Average pressure (hPa)"
)

### calcul des paramètres pour la fonction lines() à superposer à l'histo

HH2 <- hist(DSKdataIDF$pressure, breaks = BRt,  plot=F)

#lines(HH2$mids, HH2$density, lwd = 2, col = "orange") ### courbe non lissée  ## SO
lines(density(DSKdataIDF$pressure, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel

text(1000, 0.05, paste("Reporting"), cex = 1.2,  col = "black")
text(990, 0.03, paste("Random"), cex = 1.2,  col = "blue")
# text(8, .08, paste("N =",nr," Reporting" ), col = "black")

## Si on voulait le nb d'enregistrements tudiés à la place du texte "Darksky" ou MF, il faudrait :
### text(03, 0.07, paste("N =", sum(complete.cases(humdataIDF$pressure))), cex = 1.2,  col = "black")
### text(02, 0.06, paste("N =", sum(complete.cases(DSKdataIDF$pressure))), cex = 1.2,  col = "blue")

## Si on voulait les courbes à +1.5°C ou +2°C, il faudrait fabriquer et afficher :
### lines(density(DSKdataIDF$pressure_p15D, na.rm = TRUE), lwd = 2, col = "green") ### courbe lissée, kernel
### lines(density(DSKdataIDF$pressure_p2D, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel


### 7.2. Test des distributions

### 7.2.1. Test si distribution normale


## References :
## Patrick Royston (1982). An extension of Shapiro and Wilk's W test for normality to large samples. Applied Statistics, 31, 115–124. doi: 10.2307/2347973.

### Echantillonnage de humdataIDF$pressure pour avoir une taille d'échantillon < 5 000

# data[sample(1:nrow(data),n),] # pour faire un sous tableau échantillonné http://forums.cirad.fr/logiciel-r/viewtopic.php?t=6118

shapiro.test(humdataIDF[sample(1:nrow(humdataIDF),300),"pressure"]) # pour échantillonner une seule variable du data frame
# plus le n augmente, plus la p-value devient significative (si < 5 000 !)

# data:  humdataIDF[sample(1:nrow(humdataIDF), 300), "pressure"]
# W = 0.94206, p-value = 0.1989 => p-value non significative, l'échantillon suit une loi normale.

# data:  humdataIDF[sample(1:nrow(humdataIDF), 300), "pressure"]
# W = 0.9807, p-value = 0.006653 => p-value significative, l'échantillon ne suit pas une loi normale.(diffère, ce coup ci. Mais pas à tous les coups !).

# shapiro.test(humdataIDF$pressure) # uniquement si nrow < 5 000 (pas le cas ici !)

shapiro.test(DSKdataIDF[sample(1:nrow(DSKdataIDF),300),"pressure"])
# data:  DSKdataIDF[sample(1:nrow(DSKdataIDF), 30), "pressure"]
# W = 0.97022, p-value = 0.6506 => p-value non significative, l'échantillon suit une loi normale.

# data:  DSKdataIDF[sample(1:nrow(DSKdataIDF), 300), "pressure"]
# W = 0.99185, p-value = 0.1509 => p-value non significative, l'échantillon suit une loi normale.
# W = 0.98638, p-value = 0.01023 => p-value significative, l'échantillon ne suit pas une loi normale (diffère, ce coup ci. Mais peut-être pas ? tous les coups !).




### t.test entre les différentes pressions atmosphériques
t.test(DSKdataIDF$pressure,humdataIDF$pressure) # t = 0.069962, df = 1685.1, p-value = 0.9442 => p-value non significative, les 2 échantillons NE sont PAS significativement différents (mais ne suivent pas toujours une loi normale) !

# t.test(DSKdataIDF$pressure_p1D,humdataIDF$pressure) ## SO
# t.test(DSKdataIDF$pressure_p15D,humdataIDF$pressure) ## SO
# t.test(DSKdataIDF$pressure_p2D,humdataIDF$pressure) ## SO



### 7.2. Test si distribution non normale


### Import des données pour le test kruskal.test() sur données DSKdataIDF$pressure, humdataIDF$pressure

## création de deux sous-ensembles "subsets" avec juste les variables à comparer et l'origine de l'éditeur 
### ajout d'une colonne à droite contenant 1 var quali (si val num x$new=10:12) pour indiquer l'origine des données
### si ajout à gauche <cbind(10:12,x)> http://forums.cirad.fr/logiciel-R/viewtopic.php?t=9737
humdataIDF$origine="hum"
d1 <- subset(humdataIDF, select = c("pressure", "origine")) ## création du subset venant de humdataIDF http://larmarange.github.io/analyse-R/fusion-de-tables.html
dim(d1)
head(d1, 3)
d2 <- subset(DSKdataIDF, select = c("pressure", "origine")) ## création du subset venant de DSK
dim(d2)
head(d2, 3)

## pour utiliser la fusion "rbind" il faut que les champs aient le même nom (ici pas de problème)
#names(d2)[1]
#names(d2)[1]<-"pressure" ## renommage du 1er champ du tableau 2 https://forum.framasoft.org/viewtopic.php?p=75052
#names(d2)[1]

kwpresdata <- rbind(d1, d2) ## fusion des deux sous tableaux
head(kwpresdata)

summary(kwpresdata)

kruskal.test(pressure ~ origine, data = kwpresdata) ## ~ signifie : "en fonction de"

## Kruskal-Wallis chi-squared = 0.84824, df = 1, p-value = 0.3571 => p-value NON significative, les 2 échantillons ne sont pas significativement différents !

summary(humdataIDF$pressure)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 985.6  1013.4  1016.9  1016.9  1021.2  1045.8     553 
summary(DSKdataIDF$pressure)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
# 977.1  1011.3  1017.4  1016.9  1022.8  1046.0    182   


### 7.3. Calcul d'un IC via le test de Wilcoxon sur la médiane (cf. section 3.2 de Poinsot et C:\3VG\MSH\Lyme\Smartick17\data\TCD7\R\IC_foret_AL_CLC5.R)

### Comme distribution non normale

wilcox.test(humdataIDF$pressure,
            conf.int=TRUE
)$conf.int ## cette ligne pour demander l'IC 

### [1] 1016.80 1017.45
### attr(,"conf.level")
### [1] 0.95

### NO Warning messages:

### Soit un IC à 95% = [1016.8 ; 1017.5] autour de la /!\ médiane /!\


wilcox.test(DSKdataIDF$pressure,
            conf.int=TRUE
)$conf.int ## cette ligne pour demander l'IC 

### [1] 1017.0 1017.3
### attr(,"conf.level")
### [1] 0.95

### Soit un IC à 95% = [1017.0 ; 1017.3] autour de la /!\ médiane /!\



# 8. Analyse des vitesse du vent DSK vs données humaines (méthode Alice Favre)

## 8.1. Histogramme des vitesse du vent "windspeed" pour les signalements et "windspeed" pour DSK pour 700 points darksky

#### Après avoir :

#### vérifié que le nombre d'obs n'est pas égal entre les tableaux 
length(humdataIDF$windspeed)
length(na.omit(humdataIDF$windspeed)) ## pour tester si NA
length(DSKdataIDF$windspeed)
length(na.omit(DSKdataIDF$windspeed)) ## pour tester si NA

##### et que l'étendue n'est pas la même
range(humdataIDF$windspeed, na.rm = TRUE)
range(DSKdataIDF$windspeed, na.rm = TRUE)

### on calcule le nb de lignes des tableaux (pour l'habillage des histogrammes)
nr <- nrow(humdataIDF)
nrdsk <- nrow(DSKdataIDF)

#### on définit les breaks pour l'abscisse commune
BRt <- seq(from= 0, to= 13, by=1) ## tient compte des deux distributions

#### puis on fait l'histo en utilisant 

### freq=F => des fréquences relatives et pas des effectifs

hist(humdataIDF$windspeed, breaks = BRt,
     freq=F, # fréquences
     col="grey",
     main = "Average windspeed on 1,746 reporting of ticks \n (Île-de-France, july 2017 - april 2020), 995 days",
     ylab = "Densities",
     xlab = "Average windspeed (m/s)"
)

### calcul des paramètres pour la fonction lines() à superposer à l'histo

HH2 <- hist(DSKdataIDF$windspeed, breaks = BRt,  plot=F)

#lines(HH2$mids, HH2$density, lwd = 2, col = "orange") ### courbe non lissée  ## SO
lines(density(DSKdataIDF$windspeed, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel

text(6, 0.25, paste("Reporting"), cex = 1.2,  col = "black")
text(7, 0.10, paste("Random"), cex = 1.2,  col = "blue")
# text(8, .08, paste("N =",nr," Reporting" ), col = "black")

## Si on voulait le nb d'enregistrements tudiés à la place du texte "Darksky" ou MF, il faudrait :
### text(03, 0.07, paste("N =", sum(complete.cases(humdataIDF$windspeed))), cex = 1.2,  col = "black")
### text(02, 0.06, paste("N =", sum(complete.cases(DSKdataIDF$windspeed))), cex = 1.2,  col = "blue")

## Si on voulait les courbes à +1.5°C ou +2°C, il faudrait fabriquer et afficher :
### lines(density(DSKdataIDF$windspeed_p15D, na.rm = TRUE), lwd = 2, col = "green") ### courbe lissée, kernel
### lines(density(DSKdataIDF$windspeed_p2D, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel


### 8.2. Test des distributions

### 8.2.1. Test si distribution normale


## References :
## Patrick Royston (1982). An extension of Shapiro and Wilk's W test for normality to large samples. Applied Statistics, 31, 115–124. doi: 10.2307/2347973.

### Echantillonnage de humdataIDF$windspeed pour avoir une taille d'échantillon < 5 000

# data[sample(1:nrow(data),n),] # pour faire un sous tableau échantillonné http://forums.cirad.fr/logiciel-r/viewtopic.php?t=6118

shapiro.test(humdataIDF[sample(1:nrow(humdataIDF),300),"windspeed"]) # pour échantillonner une seule variable du data frame
# plus le n augmente, plus la p-value devient significative (si < 5 000 !)

# data:  humdataIDF[sample(1:nrow(humdataIDF), 30), "windspeed"]
# W = 0.96739, p-value = 0.4705 => p-value non significative, l'échantillon suit une loi normale.

# data:  humdataIDF[sample(1:nrow(humdataIDF), 300), "windspeed"]
# W = 0.88591, p-value = 3.449e-14 => p-value significative, l'échantillon ne suit pas une loi normale.(diffère, ce coup ci. Mais pas à tous les coups !).

# shapiro.test(humdataIDF$windspeed) # uniquement si nrow < 5 000 (pas le cas ici !)

shapiro.test(DSKdataIDF[sample(1:nrow(DSKdataIDF),300),"windspeed"])
# data:  DSKdataIDF[sample(1:nrow(DSKdataIDF), 30), "windspeed"]
# W = 0.97797, p-value = 0.7694 => p-value non significative, l'échantillon suit une loi normale.

# data:  DSKdataIDF[sample(1:nrow(DSKdataIDF), 300), "windspeed"]
# W = 0.92803, p-value = 7.352e-11 => p-value significative, l'échantillon ne suit pas une loi normale (diffère, ce coup ci. Mais peut-être pas ? tous les coups !).



### t.test entre les différents vitesse du vent
t.test(DSKdataIDF$windspeed,humdataIDF$windspeed) # t = 22.018, df = 2861.2, p-value < 2.2e-16-16 => p-value significative, les 2 échantillons sont significativement différents (mais ne suivent pas une loi normale) !

# t.test(DSKdataIDF$windspeed_p1D,humdataIDF$windspeed) ## SO
# t.test(DSKdataIDF$windspeed_p15D,humdataIDF$windspeed) ## SO
# t.test(DSKdataIDF$windspeed_p2D,humdataIDF$windspeed) ## SO



### 8.2. Test si distribution non normale


### Import des données pour le test kruskal.test() sur données DSKdataIDF$windspeed, humdataIDF$windspeed

## création de deux sous-ensembles "subsets" avec juste les variables à comparer et l'origine de l'éditeur 
### ajout d'une colonne à droite contenant 1 var quali (si val num x$new=10:12) pour indiquer l'origine des données
### si ajout à gauche <cbind(10:12,x)> http://forums.cirad.fr/logiciel-R/viewtopic.php?t=9737
humdataIDF$origine="hum"
d1 <- subset(humdataIDF, select = c("windspeed", "origine")) ## création du subset venant de humdataIDF http://larmarange.github.io/analyse-R/fusion-de-tables.html
dim(d1)
head(d1, 3)
d2 <- subset(DSKdataIDF, select = c("windspeed", "origine")) ## création du subset venant de DSK
dim(d2)
head(d2, 3)

## pour utiliser la fusion "rbind" il faut que les champs aient le même nom (ici pas de problème)
#names(d2)[1]
#names(d2)[1]<-"windspeed" ## renommage du 1er champ du tableau 2 https://forum.framasoft.org/viewtopic.php?p=75052
#names(d2)[1]

kwwindspeeddata <- rbind(d1, d2) ## fusion des deux sous tableaux
head(kwwindspeeddata)

summary(kwwindspeeddata)

kruskal.test(windspeed ~ origine, data = kwwindspeeddata) ## ~ signifie : "en fonction de"

## Kruskal-Wallis chi-squared = 191.95, df = 1, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents !

summary(humdataIDF$windspeed)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.220   1.903   2.605   2.700   3.320  10.820 
summary(DSKdataIDF$windspeed)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.070   2.060   3.040   3.369   4.290  12.540   


### 8.3. Calcul d'un IC via le test de Wilcoxon sur la médiane (cf. section 3.2 de Poinsot et C:\3VG\MSH\Lyme\Smartick17\data\TCD7\R\IC_foret_AL_CLC5.R)

### Comme distribution non normale

wilcox.test(humdataIDF$windspeed,
            conf.int=TRUE
)$conf.int ## cette ligne pour demander l'IC 

### [1] 2.580069 2.679997
### attr(,"conf.level")
### [1] 0.95

### NO Warning messages:

### Soit un IC à 95% = [2.6 ; 2.7] autour de la /!\ médiane /!\


wilcox.test(DSKdataIDF$windspeed,
            conf.int=TRUE
)$conf.int ## cette ligne pour demander l'IC 

### [1] 3.165049 3.220022
### attr(,"conf.level")
### [1] 0.95

### Soit un IC à 95% = [3.16 ; 3.22] autour de la /!\ médiane /!\




# 9. Analyse des visibilités DSK vs données humaines (méthode Alice Favre)

## 9.1. Histogramme des visibilités "visibility" pour les signalements et "visibility" pour DSK pour 700 points darksky

#### Après avoir :

#### vérifié que le nombre d'obs n'est pas égal entre les tableaux 
length(humdataIDF$visibility)
length(na.omit(humdataIDF$visibility)) ## pour tester si NA
length(DSKdataIDF$visibility)
length(na.omit(DSKdataIDF$visibility)) ## pour tester si NA

##### et que l'étendue n'est pas la même
range(humdataIDF$visibility, na.rm = TRUE)
range(DSKdataIDF$visibility, na.rm = TRUE)

### on calcule le nb de lignes des tableaux (pour l'habillage des histogrammes)
nr <- nrow(humdataIDF)
nrdsk <- nrow(DSKdataIDF)

#### on définit les breaks pour l'abscisse commune
BRt <- seq(from= 0, to= 20, by=1) ## tient compte des deux distributions

#### puis on fait l'histo en utilisant 

### freq=F => des fréquences relatives et pas des effectifs

hist(humdataIDF$visibility, breaks = BRt,
     freq=F, # fréquences
     col="grey",
     main = "Average visibility on 1,746 reporting of ticks \n (Île-de-France, july 2017 - april 2020), 995 days",
     ylab = "Densities",
     xlab = "Average visibility (km)"
)

### calcul des paramètres pour la fonction lines() à superposer à l'histo

HH2 <- hist(DSKdataIDF$visibility, breaks = BRt,  plot=F)

#lines(HH2$mids, HH2$density, lwd = 2, col = "orange") ### courbe non lissée  ## SO
lines(density(DSKdataIDF$visibility, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel

text(7, 0.20, paste("Reporting"), cex = 1.2,  col = "black")
text(5, 0.10, paste("Random"), cex = 1.2,  col = "blue")
# text(8, .08, paste("N =",nr," Reporting" ), col = "black")

## Si on voulait le nb d'enregistrements tudiés à la place du texte "Darksky" ou MF, il faudrait :
### text(03, 0.07, paste("N =", sum(complete.cases(humdataIDF$visibility))), cex = 1.2,  col = "black")
### text(02, 0.06, paste("N =", sum(complete.cases(DSKdataIDF$visibility))), cex = 1.2,  col = "blue")

## Si on voulait les courbes à +1.5°C ou +2°C, il faudrait fabriquer et afficher :
### lines(density(DSKdataIDF$visibility_p15D, na.rm = TRUE), lwd = 2, col = "green") ### courbe lissée, kernel
### lines(density(DSKdataIDF$visibility_p2D, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel


### 9.2. Test des distributions

### 9.2.1. Test si distribution normale


## References :
## Patrick Royston (1982). An extension of Shapiro and Wilk's W test for normality to large samples. Applied Statistics, 31, 115–124. doi: 10.2307/2347973.

### Echantillonnage de humdataIDF$visibility pour avoir une taille d'échantillon < 5 000

# data[sample(1:nrow(data),n),] # pour faire un sous tableau échantillonné http://forums.cirad.fr/logiciel-r/viewtopic.php?t=6118

shapiro.test(humdataIDF[sample(1:nrow(humdataIDF),30),"visibility"]) # pour échantillonner une seule variable du data frame
# plus le n augmente, plus la p-value devient significative (si < 5 000 !)

# data:  humdataIDF[sample(1:nrow(humdataIDF), 30), "visibility"]
# W = 0.80348, p-value = 7.591e-05 => p-value significative, l'échantillon ne suit pas une loi normale.(diffère tt le temps)



shapiro.test(DSKdataIDF[sample(1:nrow(DSKdataIDF),30),"visibility"])
# data:  DSKdataIDF[sample(1:nrow(DSKdataIDF), 30), "visibility"]
# 0.71497, p-value = 2.596e-06 => p-value significative, l'échantillon ne suit pas une loi normale.(diffère tt le temps)



### t.test entre les différents visibilités
t.test(DSKdataIDF$visibility,humdataIDF$visibility) # t = 13.063, df = 2155.2, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents (mais ne suivent pas une loi normale) !

# t.test(DSKdataIDF$visibility_p1D,humdataIDF$visibility) ## SO
# t.test(DSKdataIDF$visibility_p15D,humdataIDF$visibility) ## SO
# t.test(DSKdataIDF$visibility_p2D,humdataIDF$visibility) ## SO



### 9.2. Test si distribution non normale


### Import des données pour le test kruskal.test() sur données DSKdataIDF$visibility, humdataIDF$visibility

## création de deux sous-ensembles "subsets" avec juste les variables à comparer et l'origine de l'éditeur 
### ajout d'une colonne à droite contenant 1 var quali (si val num x$new=10:12) pour indiquer l'origine des données
### si ajout à gauche <cbind(10:12,x)> http://forums.cirad.fr/logiciel-R/viewtopic.php?t=9737
humdataIDF$origine="hum"
d1 <- subset(humdataIDF, select = c("visibility", "origine")) ## création du subset venant de humdataIDF http://larmarange.github.io/analyse-R/fusion-de-tables.html
dim(d1)
head(d1, 3)
d2 <- subset(DSKdataIDF, select = c("visibility", "origine")) ## création du subset venant de DSK
dim(d2)
head(d2, 3)

## pour utiliser la fusion "rbind" il faut que les champs aient le même nom (ici pas de problème)
#names(d2)[1]
#names(d2)[1]<-"visibility" ## renommage du 1er champ du tableau 2 https://forum.framasoft.org/viewtopic.php?p=75052
#names(d2)[1]

kwvisidata <- rbind(d1, d2) ## fusion des deux sous tableaux
head(kwvisidata)

summary(kwvisidata)

kruskal.test(visibility ~ origine, data = kwvisidata) ## ~ signifie : "en fonction de"

## Kruskal-Wallis chi-squared = 109.58, df = 1, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents !

summary(humdataIDF$visibility)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 2.558   9.873  10.003  11.675  16.093  16.093       1
summary(DSKdataIDF$visibility)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.188   9.927  14.364  12.791  16.093  16.093      24   


### 9.3. Calcul d'un IC via le test de Wilcoxon sur la médiane (cf. section 3.2 de Poinsot et C:\3VG\MSH\Lyme\Smartick17\data\TCD7\R\IC_foret_AL_CLC5.R)

### Comme distribution non normale

wilcox.test(humdataIDF$visibility,
            conf.int=TRUE
)$conf.int ## cette ligne pour demander l'IC 

### [1] 11.84047 12.43256
### attr(,"conf.level")
### [1] 0.95

### NO Warning messages:

### Soit un IC à 95% = [11.8 ; 12.4] autour de la /!\ médiane /!\


wilcox.test(DSKdataIDF$visibility,
            conf.int=TRUE
)$conf.int ## cette ligne pour demander l'IC 

### [1] 12.93801 12.96847
### attr(,"conf.level")
### [1] 0.95

### Soit un IC à 95% = [12.9 ; 13.0] autour de la /!\ médiane /!\


# 10. Analyse du couvert nuageux DSK vs données humaines (méthode Alice Favre)

## 10.1. Histogramme du couvert nuageux "cloudcover" pour les signalements et "cloudcover" pour DSK pour 700 points darksky

#### Après avoir :

#### vérifié que le nombre d'obs n'est pas égal entre les tableaux 
length(humdataIDF$cloudcover)
length(na.omit(humdataIDF$cloudcover)) ## pour tester si NA
length(DSKdataIDF$cloudcover)
length(na.omit(DSKdataIDF$cloudcover)) ## pour tester si NA

##### et que l'étendue n'est pas la même
range(humdataIDF$cloudcover, na.rm = TRUE)
range(DSKdataIDF$cloudcover, na.rm = TRUE)

### on calcule le nb de lignes des tableaux (pour l'habillage des histogrammes)
nr <- nrow(humdataIDF)
nrdsk <- nrow(DSKdataIDF)

#### on définit les breaks pour l'abscisse commune
BRt <- seq(from= 0, to= 100, by=2) ## tient compte des deux distributions

#### puis on fait l'histo en utilisant 

### freq=F => des fréquences relatives et pas des effectifs

hist(humdataIDF$cloudcover, breaks = BRt,
     freq=F, # fréquences
     col="grey",
     main = "Average cloudcover on 1,746 reporting of ticks \n (Île-de-France, july 2017 - april 2020), 995 days",
     ylab = "Densities",
     xlab = "Average cloudcover (%)"
)

### calcul des paramètres pour la fonction lines() à superposer à l'histo

### /!\ ici x100 pour compenser la différence d'unité /!\ 

HH2 <- hist(DSKdataIDF$cloudcover*100, breaks = BRt,  plot=F)


#lines(HH2$mids, HH2$density, lwd = 2, col = "orange") ### courbe non lissée  ## SO

### /!\ ici x100 pour compenser la différence d'unité /!\ 

lines(density(DSKdataIDF$cloudcover*100, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel et création d'un champ humidité en % /!\

text(20, 0.012, paste("Reporting"), cex = 1.2,  col = "black")
text(40, 0.017, paste("Random"), cex = 1.2,  col = "blue")
# text(8, .08, paste("N =",nr," Reporting" ), col = "black")

## Si on voulait le nb d'enregistrements tudiés à la place du texte "Darksky" ou MF, il faudrait :
### text(03, 0.07, paste("N =", sum(complete.cases(humdataIDF$cloudcover))), cex = 1.2,  col = "black")
### text(02, 0.06, paste("N =", sum(complete.cases(DSKdataIDF$cloudcover))), cex = 1.2,  col = "blue")

## Si on voulait les courbes à +1.5°C ou +2°C, il faudrait fabriquer et afficher :
### lines(density(DSKdataIDF$cloudcover_p15D, na.rm = TRUE), lwd = 2, col = "green") ### courbe lissée, kernel
### lines(density(DSKdataIDF$cloudcover_p2D, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel


### 10.2. Test des distributions

### 10.2.1. Test si distribution normale


## References :
## Patrick Royston (1982). An extension of Shapiro and Wilk's W test for normality to large samples. Applied Statistics, 31, 115–124. doi: 10.2307/2347973.

### Echantillonnage de humdataIDF$cloudcover pour avoir une taille d'échantillon < 5 000

# data[sample(1:nrow(data),n),] # pour faire un sous tableau échantillonné http://forums.cirad.fr/logiciel-r/viewtopic.php?t=6118

shapiro.test(humdataIDF[sample(1:nrow(humdataIDF),300),"cloudcover"]) # pour échantillonner une seule variable du data frame
# plus le n augmente, plus la p-value devient significative (si < 5 000 !)

# data:  humdataIDF[sample(1:nrow(humdataIDF), 30), "cloudcover"]
# W = 0.94019, p-value = 0.1014 => p-value non significative, l'échantillon suit une loi normale (1 coup sur 5)

# data:  humdataIDF[sample(1:nrow(humdataIDF), 300), "cloudcover"]
# W = 0.92222, p-value = 6.559e-11 => p-value significative, l'échantillon ne suit pas une loi normale.(diffère, ce coup ci. Mais pas à tous les coups !).

# shapiro.test(humdataIDF$cloudcover) # uniquement si nrow < 5 000 (pas le cas ici !)

shapiro.test(DSKdataIDF[sample(1:nrow(DSKdataIDF),30),"cloudcover"])
# data:  DSKdataIDF[sample(1:nrow(DSKdataIDF), 30), "cloudcover"]
# W = 0.93678, p-value = 0.07452 => p-value non significative, l'échantillon suit une loi normale (1 coup sur 5 ou 6 !)

# data:  DSKdataIDF[sample(1:nrow(DSKdataIDF), 300), "cloudcover"]
# W = 0.9194, p-value = 2.697e-11 => p-value significative, l'échantillon ne suit pas une loi normale (diffère, ce coup ci. Mais peut-être pas ? tous les coups !).

### t.test entre les différentes humidités relatives
t.test(DSKdataIDF$cloudcover*100,humdataIDF$cloudcover) # t = 8.8713, df = 2033.6, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents (mais ne suivent pas une loi normale) !

# t.test(DSKdataIDF$cloudcover_p1D,humdataIDF$cloudcover) ## SO
# t.test(DSKdataIDF$cloudcover_p15D,humdataIDF$cloudcover) ## SO
# t.test(DSKdataIDF$cloudcover_p2D,humdataIDF$cloudcover) ## SO



### 10.2.2. Test si distribution non normale


### Import des données pour le test kruskal.test() sur données DSKdataIDF$cloudcover, humdataIDF$cloudcover

## création de deux sous-ensembles "subsets" avec juste les variables à comparer et l'origine de l'éditeur 
### ajout d'une colonne à droite contenant 1 var quali (si val num x$new=10:12) pour indiquer l'origine des données
### si ajout à gauche <cbind(10:12,x)> http://forums.cirad.fr/logiciel-R/viewtopic.php?t=9737
humdataIDF$origine="hum"
d1 <- subset(humdataIDF, select = c("cloudcover", "origine")) ## création du subset venant de humdataIDF http://larmarange.github.io/analyse-R/fusion-de-tables.html
dim(d1)
head(d1, 3)
DSKdataIDF$cldcov100=DSKdataIDF$cloudcover*100 ## création d'un champ humidité en %
### /!\ ici x100 pour compenser la différence d'unité /!\ 
d2 <- subset(DSKdataIDF, select = c("cldcov100", "origine")) ## création du subset venant de DSK
dim(d2)
head(d2, 3)

## pour utiliser la fusion "rbind" il faut que les champs aient le même nom (c'est le cas ici)
names(d1)[1]
names(d1)[1]<-"cldcov100" ## renommage du 1er champ du tableau 1 https://forum.framasoft.org/viewtopic.php?p=75052
names(d1)[1]

kwcldcovdata <- rbind(d1, d2) ## fusion des deux sous tableaux
head(kwcldcovdata)

summary(kwcldcovdata)

kruskal.test(cldcov100 ~ origine, data = kwcldcovdata) ## ~ signifie : "en fonction de"

## Kruskal-Wallis chi-squared = 140.87, df = 1, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents !

summary(humdataIDF$cloudcover)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.00   44.00   66.00   59.32   78.00  100.00     169 
summary(DSKdataIDF$cldcov100)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.0    48.0    74.0    65.2    88.0   100.0     992


### 10.3. Calcul d'un IC via le test de Wilcoxon sur la médiane (cf. section 3.2 de Poinsot et C:\3VG\MSH\Lyme\Smartick17\data\TCD7\R\IC_foret_AL_CLC5.R)

### Comme distribution non normale

wilcox.test(humdataIDF$cloudcover,
            conf.int=TRUE
)$conf.int ## cette ligne pour demander l'IC 

### [1] 60.99997 63.50005
### attr(,"conf.level")
### [1] 0.95

### NO Warning messages:

### Soit un IC à 95% = [61.0 ; 63.5] autour de la /!\ médiane /!\


wilcox.test(DSKdataIDF$cldcov100,
            conf.int=TRUE
)$conf.int ## cette ligne pour demander l'IC 

### [1] 68.99995 69.99994
### attr(,"conf.level")
### [1] 0.95

### Soit un IC à 95% = [68.0 ; 69.0] autour de la /!\ médiane /!\




# 11. Analyse des vitesses des rafales DSK vs données humaines (méthode Alice Favre)

## 11.1. Histogramme des vitesses des rafales "windgust" pour les signalements et "windgust" pour DSK pour 700 points darksky

#### Après avoir :

#### vérifié que le nombre d'obs n'est pas égal entre les tableaux 
length(humdataIDF$windgust)
length(na.omit(humdataIDF$windgust)) ## pour tester si NA
length(DSKdataIDF$windgust)
length(na.omit(DSKdataIDF$windgust)) ## pour tester si NA

##### et que l'étendue n'est pas la même
range(humdataIDF$windgust, na.rm = TRUE)
range(DSKdataIDF$windgust, na.rm = TRUE)

### on calcule le nb de lignes des tableaux (pour l'habillage des histogrammes)
nr <- nrow(humdataIDF)
nrdsk <- nrow(DSKdataIDF)

#### on définit les breaks pour l'abscisse commune
BRt <- seq(from= 0, to= 30, by=1) ## tient compte des deux distributions

#### puis on fait l'histo en utilisant 

### freq=F => des fréquences relatives et pas des effectifs

hist(humdataIDF$windgust, breaks = BRt,
     freq=F, # fréquences
     col="grey",
     main = "Average windgust on 1,746 reporting of ticks \n (Île-de-France, july 2017 - april 2020), 995 days",
     ylab = "Densities",
     xlab = "Average windgust (m/s)"
)

### calcul des paramètres pour la fonction lines() à superposer à l'histo

HH2 <- hist(DSKdataIDF$windgust, breaks = BRt,  plot=F)

#lines(HH2$mids, HH2$density, lwd = 2, col = "orange") ### courbe non lissée  ## SO
lines(density(DSKdataIDF$windgust, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel

text(15, 0.08, paste("Reporting"), cex = 1.2,  col = "black")
text(20, 0.04, paste("Random"), cex = 1.2,  col = "blue")
# text(8, .08, paste("N =",nr," Reporting" ), col = "black")

## Si on voulait le nb d'enregistrements tudiés à la place du texte "Darksky" ou MF, il faudrait :
### text(03, 0.07, paste("N =", sum(complete.cases(humdataIDF$windgust))), cex = 1.2,  col = "black")
### text(02, 0.06, paste("N =", sum(complete.cases(DSKdataIDF$windgust))), cex = 1.2,  col = "blue")

## Si on voulait les courbes à +1.5°C ou +2°C, il faudrait fabriquer et afficher :
### lines(density(DSKdataIDF$windgust_p15D, na.rm = TRUE), lwd = 2, col = "green") ### courbe lissée, kernel
### lines(density(DSKdataIDF$windgust_p2D, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel


### 11.2. Test des distributions

### 11.2.1. Test si distribution normale


## References :
## Patrick Royston (1982). An extension of Shapiro and Wilk's W test for normality to large samples. Applied Statistics, 31, 115–124. doi: 10.2307/2347973.

### Echantillonnage de humdataIDF$windgust pour avoir une taille d'échantillon < 5 000

# data[sample(1:nrow(data),n),] # pour faire un sous tableau échantillonné http://forums.cirad.fr/logiciel-r/viewtopic.php?t=6118

shapiro.test(humdataIDF[sample(1:nrow(humdataIDF),300),"windgust"]) # pour échantillonner une seule variable du data frame
# plus le n augmente, plus la p-value devient significative (si < 5 000 !)

# data:  humdataIDF[sample(1:nrow(humdataIDF), 30), "windgust"]
# W = 0.92905, p-value = 0.148 => p-value non significative, l'échantillon suit une loi normale (9 coup sur 10)

# data:  humdataIDF[sample(1:nrow(humdataIDF), 300), "windgust"]
# W = 0.92058, p-value = 3.773e-09 => p-value significative, l'échantillon ne suit pas une loi normale.(diffère, 10 fois sur 10 !).

# shapiro.test(humdataIDF$windgust) # uniquement si nrow < 5 000 (pas le cas ici !)

shapiro.test(DSKdataIDF[sample(1:nrow(DSKdataIDF),300),"windgust"])
# data:  DSKdataIDF[sample(1:nrow(DSKdataIDF), 30), "windgust"]
# W = 0.92553, p-value = 0.06855 => p-value non significative, l'échantillon suit une loi normale (1 coup sur 2 ou 3 !)

# data:  DSKdataIDF[sample(1:nrow(DSKdataIDF), 300), "windgust"]
# W = 0.94478, p-value = 1.519e-08 => p-value significative, l'échantillon ne suit pas une loi normale (diffère, , 10 fois sur 10 !).


### t.test entre les différents vitesses des rafales
t.test(DSKdataIDF$windgust,humdataIDF$windgust) # t = 21.011, df = 1666.3, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents (mais ne suivent pas une loi normale) !

# t.test(DSKdataIDF$windgust_p1D,humdataIDF$windgust) ## SO
# t.test(DSKdataIDF$windgust_p15D,humdataIDF$windgust) ## SO
# t.test(DSKdataIDF$windgust_p2D,humdataIDF$windgust) ## SO



### 11.2. Test si distribution non normale


### Import des données pour le test kruskal.test() sur données DSKdataIDF$windgust, humdataIDF$windgust

## création de deux sous-ensembles "subsets" avec juste les variables à comparer et l'origine de l'éditeur 
### ajout d'une colonne à droite contenant 1 var quali (si val num x$new=10:12) pour indiquer l'origine des données
### si ajout à gauche <cbind(10:12,x)> http://forums.cirad.fr/logiciel-R/viewtopic.php?t=9737
humdataIDF$origine="hum"
d1 <- subset(humdataIDF, select = c("windgust", "origine")) ## création du subset venant de humdataIDF http://larmarange.github.io/analyse-R/fusion-de-tables.html
dim(d1)
head(d1, 3)
d2 <- subset(DSKdataIDF, select = c("windgust", "origine")) ## création du subset venant de DSK
dim(d2)
head(d2, 3)

## pour utiliser la fusion "rbind" il faut que les champs aient le même nom (ici pas de problème)
#names(d2)[1]
#names(d2)[1]<-"windgust" ## renommage du 1er champ du tableau 2 https://forum.framasoft.org/viewtopic.php?p=75052
#names(d2)[1]

kwwingustdata <- rbind(d1, d2) ## fusion des deux sous tableaux
head(kwwingustdata)

summary(kwwingustdata)

kruskal.test(windgust ~ origine, data = kwwingustdata) ## ~ signifie : "en fonction de"

## Kruskal-Wallis chi-squared = 224.31, df = 1, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents !

summary(humdataIDF$windgust)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.500   4.450   6.190   6.806   8.710  27.580     541
summary(DSKdataIDF$windgust)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.180   5.080   8.180   8.943  12.370  28.820    1787   


### 11.3. Calcul d'un IC via le test de Wilcoxon sur la médiane (cf. section 3.2 de Poinsot et C:\3VG\MSH\Lyme\Smartick17\data\TCD7\R\IC_foret_AL_CLC5.R)

### Comme distribution non normale

wilcox.test(humdataIDF$windgust,
            conf.int=TRUE
)$conf.int ## cette ligne pour demander l'IC 

### [1] 6.319970 6.695041
### attr(,"conf.level")
### [1] 0.95

### NO Warning messages:

### Soit un IC à 95% = [6.3 ; 6.7] autour de la /!\ médiane /!\


wilcox.test(DSKdataIDF$windgust,
            conf.int=TRUE
)$conf.int ## cette ligne pour demander l'IC 

### [1] 8.644979 8.809992
### attr(,"conf.level")
### [1] 0.95

### Soit un IC à 95% = [8.7 ; 8.8] autour de la /!\ médiane /!\



# 12. Analyse de l'indice de rayonnement ultra-violet nuageux DSK vs données humaines (méthode Alice Favre)

## 12.1. Histogramme de l'indice de rayonnement ultra-violet nuageux "uvindex" pour les signalements et "uvindex" pour DSK pour 700 points darksky

#### Après avoir :

#### vérifié que le nombre d'obs n'est pas égal entre les tableaux 
length(humdataIDF$uvindex)
length(na.omit(humdataIDF$uvindex)) ## pour tester si NA
length(DSKdataIDF$uvindex)
length(na.omit(DSKdataIDF$uvindex)) ## pour tester si NA

##### et que l'étendue n'est pas la même
range(humdataIDF$uvindex, na.rm = TRUE)
range(DSKdataIDF$uvindex, na.rm = TRUE)

### on calcule le nb de lignes des tableaux (pour l'habillage des histogrammes)
nr <- nrow(humdataIDF)
nrdsk <- nrow(DSKdataIDF)

#### on définit les breaks pour l'abscisse commune
BRt <- seq(from= 0, to= 10, by=1) ## tient compte des deux distributions

#### puis on fait l'histo en utilisant 

### freq=F => des fréquences relatives et pas des effectifs

hist(humdataIDF$uvindex, breaks = BRt,
     freq=F, # fréquences
     col="grey",
     main = "Average uvindex on 1,746 reporting of ticks \n (Île-de-France, july 2017 - april 2020), 995 days",
     ylab = "Densities",
     xlab = "Average uvindex"
)

### calcul des paramètres pour la fonction lines() à superposer à l'histo


HH2 <- hist(DSKdataIDF$uvindex, breaks = BRt,  plot=F)


#lines(HH2$mids, HH2$density, lwd = 2, col = "orange") ### courbe non lissée  ## SO


#lines(density(DSKdataIDF$uvindex, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel et création d'un champ humidité en % /!\
#lines(density(DSKdataIDF$uvindex, na.rm = TRUE, bw = "nrd"), lwd = 2, col = 3)
#lines(density(DSKdataIDF$uvindex, na.rm = TRUE, bw = "ucv"), lwd = 2, col = 4)
lines(density(DSKdataIDF$uvindex, na.rm = TRUE, bw = "bcv"), lwd = 2, col = "blue") # bw.bcv(x, nb = 1000, lower = 0.1 * hmax, upper = hmax, tol = 0.1 * lower)
    # see Details @ https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/bandwidth 
    # here => bw.ucv and bw.bcv implement unbiased and biased cross-validation respectively.
#lines(density(DSKdataIDF$uvindex, na.rm = TRUE, bw = "SJ-ste"), lwd = 2, col = 6)
#lines(density(DSKdataIDF$uvindex, na.rm = TRUE, bw = "SJ-dpi"), lwd = 2, col = 7)


text(8, 0.17, paste("Reporting"), cex = 1.2,  col = "black")
text(1.5, 0.075, paste("Random"), cex = 1.2,  col = "blue")
# text(8, .08, paste("N =",nr," Reporting" ), col = "black")

## Si on voulait le nb d'enregistrements tudiés à la place du texte "Darksky" ou MF, il faudrait :
### text(03, 0.07, paste("N =", sum(complete.cases(humdataIDF$uvindex))), cex = 1.2,  col = "black")
### text(02, 0.06, paste("N =", sum(complete.cases(DSKdataIDF$uvindex))), cex = 1.2,  col = "blue")

## Si on voulait les courbes à +1.5°C ou +2°C, il faudrait fabriquer et afficher :
### lines(density(DSKdataIDF$uvindex_p15D, na.rm = TRUE), lwd = 2, col = "green") ### courbe lissée, kernel
### lines(density(DSKdataIDF$uvindex_p2D, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel


### 12.2. Test des distributions

### 12.2.1. Test si distribution normale


## References :
## Patrick Royston (1982). An extension of Shapiro and Wilk's W test for normality to large samples. Applied Statistics, 31, 115–124. doi: 10.2307/2347973.

### Echantillonnage de humdataIDF$uvindex pour avoir une taille d'échantillon < 5 000

# data[sample(1:nrow(data),n),] # pour faire un sous tableau échantillonné http://forums.cirad.fr/logiciel-r/viewtopic.php?t=6118

shapiro.test(humdataIDF[sample(1:nrow(humdataIDF),300),"uvindex"]) # pour échantillonner une seule variable du data frame
# plus le n augmente, plus la p-value devient significative (si < 5 000 !)

# data:  humdataIDF[sample(1:nrow(humdataIDF), 30), "uvindex"]
# W = 0.96756, p-value = 0.517 => p-value non significative, l'échantillon suit une loi normale (1 coup sur 2)

# data:  humdataIDF[sample(1:nrow(humdataIDF), 300), "uvindex"]
# W = 0.94576, p-value = 7.721e-09 => p-value significative, l'échantillon ne suit pas une loi normale.(diffère, 10 fois sur 10 !).

# shapiro.test(humdataIDF$uvindex) # uniquement si nrow < 5 000 (pas le cas ici !)

shapiro.test(DSKdataIDF[sample(1:nrow(DSKdataIDF),300),"uvindex"])
# data:  DSKdataIDF[sample(1:nrow(DSKdataIDF), 30), "uvindex"]
# W = W = 0.83849, p-value = 0.0003593 => p-value significative, l'échantillon ne suit pas une loi normale.(diffère, 10 fois sur 10 !).

# data:  DSKdataIDF[sample(1:nrow(DSKdataIDF), 300), "uvindex"]
# W = 0.89373, p-value = 2.522e-13 => p-value significative, l'échantillon ne suit pas une loi normale (diffère, , 10 fois sur 10 !).

### t.test entre les différentes humidités relatives
t.test(DSKdataIDF$uvindex*10,humdataIDF$uvindex) # t = 151.44, df = 17012, p-value < 2.2e-16, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents (mais ne suivent pas une loi normale) !

# t.test(DSKdataIDF$uvindex_p1D,humdataIDF$uvindex) ## SO
# t.test(DSKdataIDF$uvindex_p15D,humdataIDF$uvindex) ## SO
# t.test(DSKdataIDF$uvindex_p2D,humdataIDF$uvindex) ## SO



### 12.2.2. Test si distribution non normale


### Import des données pour le test kruskal.test() sur données DSKdataIDF$uvindex, humdataIDF$uvindex

## création de deux sous-ensembles "subsets" avec juste les variables à comparer et l'origine de l'éditeur 
### ajout d'une colonne à droite contenant 1 var quali (si val num x$new=10:12) pour indiquer l'origine des données
### si ajout à gauche <cbind(10:12,x)> http://forums.cirad.fr/logiciel-R/viewtopic.php?t=9737
humdataIDF$origine="hum"
d1 <- subset(humdataIDF, select = c("uvindex", "origine")) ## création du subset venant de humdataIDF http://larmarange.github.io/analyse-R/fusion-de-tables.html
dim(d1)
head(d1, 3)

d2 <- subset(DSKdataIDF, select = c("uvindex", "origine")) ## création du subset venant de DSK
dim(d2)
head(d2, 3)

## pour utiliser la fusion "rbind" il faut que les champs aient le même nom (c'est le cas ici)
names(d1)[1]
names(d1)[1]<-"uvindex" ## renommage du 1er champ du tableau 1 https://forum.framasoft.org/viewtopic.php?p=75052
names(d1)[1]

kwuvindexdata <- rbind(d1, d2) ## fusion des deux sous tableaux
head(kwuvindexdata)

summary(kwuvindexdata)

kruskal.test(uvindex ~ origine, data = kwuvindexdata) ## ~ signifie : "en fonction de"

## Kruskal-Wallis chi-squared = 1783.2, df = 1, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents !

summary(humdataIDF$uvindex)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.00    5.00    6.00    5.55    7.00    9.00      73 
summary(DSKdataIDF$uvindex)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.000   1.000   3.000   3.129   5.000   9.000     338   


### 12.3. Calcul d'un IC via le test de Wilcoxon sur la médiane (cf. section 3.2 de Poinsot et C:\3VG\MSH\Lyme\Smartick17\data\TCD7\R\IC_foret_AL_CLC5.R)

### Comme distribution non normale

wilcox.test(humdataIDF$uvindex,
            conf.int=TRUE
)$conf.int ## cette ligne pour demander l'IC 

### [1] 5.500009 5.500037
### attr(,"conf.level")
### [1] 0.95

### NO Warning messages:

### Soit un IC à 95% = [5.500009 ; 5.500037] autour de la /!\ médiane /!\


wilcox.test(DSKdataIDF$uvindex,
            conf.int=TRUE
)$conf.int ## cette ligne pour demander l'IC 

### [1] 3.000029 3.000049
### attr(,"conf.level")
### [1] 0.95

### Soit un IC à 95% = [3.00003 ; 3.00005] autour de la /!\ médiane /!\

