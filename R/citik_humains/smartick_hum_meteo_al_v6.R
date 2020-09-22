######
###### Analyse des signalements et des données Météo DSK (non plus moyennées mais DSK brutes) pour l'Alsace-Lorraine

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

humdataAL <- subset(humdata, humdata$departement_code == "54" |  humdata$departement_code == "55" |  humdata$departement_code == "57" |  humdata$departement_code == "88" |  humdata$departement_code == "67" | humdata$departement_code == "68")

DSKdataAL <- subset(DSKdata, DSKdata$departement_code == "54" |  DSKdata$departement_code == "55" |  DSKdata$departement_code == "57" |  DSKdata$departement_code == "88" |  DSKdata$departement_code == "67" | DSKdata$departement_code == "68")


## 2.1 Analyse du tableau de données

ls(humdataAL) ## liste les variables
ls(DSKdataAL)


str(humdataAL) ##  a diagnostic function and an alternative to summary
str(DSKdataAL)


summary(humdataAL)
summary(DSKdataAL)



# 3. Analyse des Températures moyennes DSK vs signalements (méthode Alice Favre)

## 3.1. Histogramme des Températures moyennes "temperature" pour les signalements et "temperature" pour DSK = (tempHigh + tempLow)/2 pour 700 points darksky

#### Après avoir :

#### vérifié que le nombre d'obs n'est pas égal entre les tableaux 
length(humdataAL$temperature)
length(na.omit(humdataAL$temperature)) ## pour tester si NA

length(DSKdataAL$temperature)
length(na.omit(DSKdataAL$temperature)) ## pour tester si NA

##### et que l'étendue n'est pas la même
range(humdataAL$temperature, na.rm = TRUE)
range(DSKdataAL$temperature, na.rm = TRUE)

### on calcule le nb de lignes des tableaux (pour l'habillage des histogrammes)
nr <- nrow(humdataAL)
nrdsk <- nrow(DSKdataAL)

#### on définit les breaks pour l'abscisse commune
BRt <- seq(from= -15, to= 35, by=2) ## tient compte des deux distributions

#### puis on fait l'histo en utilisant 

### freq=F => des fréquences relatives et pas des effectifs

hist(humdataAL$temperature, breaks = BRt,
     freq=F, # fréquences
     col="grey",
     main = "Average Temperatures on 2,761 reporting of ticks \n (Alsace-Lorraine, july 2017 - april 2020), 995 days",
     ylab = "Densities",
     xlab = "Average Temperatures (T°C)"
     )

### calcul des paramètres pour la fonction lines() à superposer à l'histo

HH2 <- hist(DSKdataAL$temperature, breaks = BRt,  plot=F)
    
    #lines(HH2$mids, HH2$density, lwd = 2, col = "orange") ### courbe non lissée  ## SO
    lines(density(DSKdataAL$temperature, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel

text(10, 0.07, paste("Reporting"), cex = 1.2,  col = "black")
text(02, 0.06, paste("Random"), cex = 1.2,  col = "blue")
# text(8, .08, paste("N =",nr," Reporting" ), col = "black")

## Si on voulait le nb d'enregistrements tudiés à la place du texte "Darksky" ou MF, il faudrait :
    ### text(03, 0.07, paste("N =", sum(complete.cases(humdataAL$temperature))), cex = 1.2,  col = "black")
    ### text(02, 0.06, paste("N =", sum(complete.cases(DSKdataAL$temperature))), cex = 1.2,  col = "blue")

## Si on voulait les courbes à +1.5°C ou +2°C, il faudrait fabriquer et afficher :
    ### lines(density(DSKdataAL$temperature_p15D, na.rm = TRUE), lwd = 2, col = "green") ### courbe lissée, kernel
    ### lines(density(DSKdataAL$temperature_p2D, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel



## 3.2. Test des distributions


### 3.2.1. Test si distribution normale

## References :
## Patrick Royston (1982). An extension of Shapiro and Wilk's W test for normality to large samples. Applied Statistics, 31, 115–124. doi: 10.2307/2347973.

### Echantillonnage de humdataAL$temperature pour avoir une taille d'échantillon < 5 000

# data[sample(1:nrow(data),n),] # pour faire un sous tableau échantillonné http://forums.cirad.fr/logiciel-r/viewtopic.php?t=6118

shapiro.test(humdataAL[sample(1:nrow(humdataAL),30),"temperature"]) # pour échantillonner une seule variable du data frame
    # plus le n augmente, plus la p-value devient significative (si < 5 000 !)
    # 
    # W = 0.91116, p-value = 0.01832 => p-value significative, l'échantillon ne suit pas une loi normale (variable avec 30 ! seulement 1/5).

# shapiro.test(humdataAL$temperature) # uniquement si nrow < 5 000 (pas le cas ici !)

shapiro.test(DSKdataAL[sample(1:nrow(DSKdataAL),30),"temperature"]) 
    # plus le n augmente, plus la p-value devient significative (si < 5 000 !)
    #  
    # W = 0.92036, p-value = 0.0274 => p-value significative, l'échantillon ne suit pas une loi normale (variable avec 30 ! seulement 1/5).


### t.test entre les différentes températures
t.test(DSKdataAL$temperature,humdataAL$temperature) # t = -67.634, df = 3606.1, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents (mais ne suivent pas une loi normale) !

# t.test(DSKdataAL$temperature_p1D,humdataAL$temperature) ## SO
# t.test(DSKdataAL$temperature_p15D,humdataAL$temperature) ## SO
# t.test(DSKdataAL$temperature_p2D,humdataAL$temperature) ## SO



### 3.2.2. Test si distribution non normale

## References :
## Myles Hollander and Douglas A. Wolfe (1973), Nonparametric Statistical Methods. New York: John Wiley & Sons. Pages 115–120.

## http://www.sthda.com/english/wiki/kruskal-wallis-test-in-r

### Import des données pour le test kruskal.test() sur données DSKdataAL$temperature, humdataAL$temperature in : KW_temp_maille_42.csv (? à vérifier !)

# /!\ méthode qui crée des tableaux intermediaires ! Juste en exemple, car on saute au subset à partir de maintenant !

# kwtempdata <- read.csv("../../data/donnee_meteo_nationale_comparative/comparaison/KW_temp_maille_42.csv", header = TRUE, sep = ";", dec = ".")
# summary(kwtempdata)

# kruskal.test(temp ~ type, data = kwtempdata) ## ~ signifie : "en fonction de"

## Kruskal-Wallis chi-squared = 2.5559, df = 1, p-value = 0.1099 => p-value non significative, les 2 échantillons ne sont pas significativement différents !
### Import des données pour le test kruskal.test() sur données DSKdataAL$temperature, humdataAL$temperature

## création de deux sous-ensembles "subsets" avec juste les variables à comparer et l'origine de l'éditeur 
    ### ajout d'une colonne à droite contenant 1 var quali (si val num x$new=10:12) pour indiquer l'origine des données
    ### si ajout à gauche <cbind(10:12,x)> http://forums.cirad.fr/logiciel-R/viewtopic.php?t=9737
humdataAL$origine="hum"
d1 <- subset(humdataAL, select = c("temperature", "origine")) ## création du subset venant de humdataAL http://larmarange.github.io/analyse-R/fusion-de-tables.html
dim(d1)
head(d1, 3)
d2 <- subset(DSKdataAL, select = c("temperature", "origine")) ## création du subset venant de DSK
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

## Kruskal-Wallis chi-squared = 2199.3, df = 1, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents !

summary(humdataAL$temperature)
    #  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # -0.84   14.56   17.90   17.48   20.97   29.26      11 
summary(DSKdataAL$temperature)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    # -10.34    4.97   10.48   10.94   16.52   31.30  


### 3.2.3. Calcul d'un IC via le test de Wilcoxon sur la médiane (cf. section 3.2 de Poinsot et C:\3VG\MSH\Lyme\Smartick17\data\TCD7\R\IC_foret_AL_CLC5.R)

### Comme distribution non normale

wilcox.test(humdataAL$temperature,
            conf.int=TRUE
            )$conf.int ## cette ligne pour demander l'IC 

### [1] 17.56001 17.92007
### attr(,"conf.level")
### [1] 0.95

### NO Warning messages:

### Soit un IC à 95% = [17.6 ; 17.96] autour de la /!\ médiane /!\


wilcox.test(DSKdataAL$temperature,
            conf.int=TRUE
)$conf.int ## cette ligne pour demander l'IC 

### [1] 10.76497 10.91003
### attr(,"conf.level")
### [1] 0.95

### Soit un IC à 95% = [10.8 ; 10.9] autour de la /!\ médiane /!\




# 4. Analyse des températures maximales quotidiennes DSK vs données humaines (méthode Alice Favre)

## 4.1. Histogramme des températures maximales quotidiennes "temperaturehigh" pour les signalements et "temperaturehigh" pour DSK pour 700 points darksky

#### Après avoir :

#### vérifié que le nombre d'obs n'est pas égal entre les tableaux 
length(humdataAL$temperaturehigh)
length(na.omit(humdataAL$temperaturehigh)) ## pour tester si NA
length(DSKdataAL$temperaturehigh)
length(na.omit(DSKdataAL$temperaturehigh)) ## pour tester si NA

##### et que l'étendue n'est pas la même
range(humdataAL$temperaturehigh, na.rm = TRUE)
range(DSKdataAL$temperaturehigh, na.rm = TRUE)

### on calcule le nb de lignes des tableaux (pour l'habillage des histogrammes)
nr <- nrow(humdataAL)
nrdsk <- nrow(DSKdataAL)

#### on définit les breaks pour l'abscisse commune
BRt <- seq(from= -10, to= 40, by=2) ## tient compte des deux distributions

#### puis on fait l'histo en utilisant 

### freq=F => des fréquences relatives et pas des effectifs

hist(humdataAL$temperaturehigh, breaks = BRt,
     freq=F, # fréquences
     col="grey",
     main = "Average Daily High Temperature\n on 2,761 reporting of ticks \n (Alsace-Lorraine, july 2017 - april 2020), 995 days",
     ylab = "Densities",
     xlab = "Average High Temperatures (T°C)"
)

### calcul des paramètres pour la fonction lines() à superposer à l'histo

HH2 <- hist(DSKdataAL$temperaturehigh, breaks = BRt,  plot=F)

#lines(HH2$mids, HH2$density, lwd = 2, col = "orange") ### courbe non lissée  ## SO
lines(density(DSKdataAL$temperaturehigh, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel

text(12, 0.06, paste("Reporting"), cex = 1.2,  col = "black")
text(01, 0.04, paste("Random"), cex = 1.2,  col = "blue")
# text(8, .08, paste("N =",nr," Reporting" ), col = "black")

## Si on voulait le nb d'enregistrements tudiés à la place du texte "Darksky" ou MF, il faudrait :
### text(03, 0.07, paste("N =", sum(complete.cases(humdataAL$temperaturehigh))), cex = 1.2,  col = "black")
### text(02, 0.06, paste("N =", sum(complete.cases(DSKdataAL$temperaturehigh))), cex = 1.2,  col = "blue")

## Si on voulait les courbes à +1.5°C ou +2°C, il faudrait fabriquer et afficher :
### lines(density(DSKdataAL$temperaturehigh_p15D, na.rm = TRUE), lwd = 2, col = "green") ### courbe lissée, kernel
### lines(density(DSKdataAL$temperaturehigh_p2D, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel
    

### 4.2. Test des distributions

### 4.2.1. Test si distribution normale


## References :
## Patrick Royston (1982). An extension of Shapiro and Wilk's W test for normality to large samples. Applied Statistics, 31, 115–124. doi: 10.2307/2347973.

### Echantillonnage de humdataAL$temperaturehigh pour avoir une taille d'échantillon < 5 000

# data[sample(1:nrow(data),n),] # pour faire un sous tableau échantillonné http://forums.cirad.fr/logiciel-r/viewtopic.php?t=6118

shapiro.test(humdataAL[sample(1:nrow(humdataAL), 300),"temperaturehigh"]) # pour échantillonner une seule variable du data frame
# plus le n augmente, plus la p-value devient significative (si < 5 000 !)

# data:  humdataAL[sample(1:nrow(humdataAL), 30), "temperaturehigh"]
# W = 0.9826, p-value = 0.8896 => p-value non significative, l'échantillon suit une loi normale (4/5 avec 30).

# data:  humdataAL[sample(1:nrow(humdataAL), 300), "temperaturehigh"]
# W = 0.98633, p-value = 0.006058 => p-value significative, l'échantillon ne suit pas une loi normale (diffère, ce coup ci. Mais pas ? tous les coups !).

# shapiro.test(humdataAL$temperaturehigh) # uniquement si nrow < 5 000 (pas le cas ici !)

shapiro.test(DSKdataAL[sample(1:nrow(DSKdataAL),30),"temperaturehigh"])
# data:  DSKdataAL[sample(1:nrow(DSKdataAL), 30), "temperaturehigh"]
# W = 0.95467, p-value = 0.2251 => p-value non significative, l'échantillon suit une loi normale (signif 1/8 avec n=30).

# data:  DSKdataAL[sample(1:nrow(DSKdataAL), 300), "temperaturehigh"]
# W = 0.98381, p-value = 0.001825 => p-value significative, l'échantillon ne suit pas une loi normale (diffère, ce coup ci. Mais peut-?tre pas ? tous les coups !).




### t.test entre les différentes températures maximales
t.test(DSKdataAL$temperaturehigh,humdataAL$temperaturehigh) # t = -69.041, df = 3689.1, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents (mais ne suivent pas une loi normale) !

# t.test(DSKdataAL$temperaturehigh_p1D,humdataAL$temperaturehigh) ## SO
# t.test(DSKdataAL$temperaturehigh_p15D,humdataAL$temperaturehigh) ## SO
# t.test(DSKdataAL$temperaturehigh_p2D,humdataAL$temperaturehigh) ## SO



### 4.2.2. Test si distribution non normale


### Import des données pour le test kruskal.test() sur données DSKdataAL$temperaturehigh, humdataAL$temperaturehigh

## création de deux sous-ensembles "subsets" avec juste les variables à comparer et l'origine de l'éditeur 
### ajout d'une colonne à droite contenant 1 var quali (si val num x$new=10:12) pour indiquer l'origine des données
### si ajout à gauche <cbind(10:12,x)> http://forums.cirad.fr/logiciel-R/viewtopic.php?t=9737
humdataAL$origine="hum"
d1 <- subset(humdataAL, select = c("temperaturehigh", "origine")) ## création du subset venant de humdataAL http://larmarange.github.io/analyse-R/fusion-de-tables.html
dim(d1)
head(d1, 3)
d2 <- subset(DSKdataAL, select = c("temperaturehigh", "origine")) ## création du subset venant de DSK
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

## Kruskal-Wallis chi-squared = 2155, df = 1, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents !

summary(humdataAL$temperaturehigh)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.11   20.18   23.94   23.29   27.16   37.31 
summary(DSKdataAL$temperaturehigh)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# -6.91    8.16   14.75   15.51   22.41   39.34      10  


### 4.3. Calcul d'un IC via le test de Wilcoxon sur la médiane (cf. section 3.2 de Poinsot et C:\3VG\MSH\Lyme\Smartick17\data\TCD7\R\IC_foret_AL_CLC5.R)

### Comme distribution non normale

wilcox.test(humdataAL$temperaturehigh,
            conf.int=TRUE
)$conf.int ## cette ligne pour demander l'IC 

### [1] 23.38505 23.80005
### attr(,"conf.level")
### [1] 0.95

### NO Warning messages:

### Soit un IC à 95% = [23.4 ; 23.8] autour de la /!\ médiane /!\


wilcox.test(DSKdataAL$temperaturehigh,
            conf.int=TRUE
)$conf.int ## cette ligne pour demander l'IC 

### [1] 15.29998 15.47002
### attr(,"conf.level")
### [1] 0.95

### Soit un IC à 95% = [15.3 ; 15.5] autour de la /!\ médiane /!\



    
# 5. Analyse de l'humidité relative DSK vs données humaines (méthode Alice Favre)

## 5.1. Histogramme de l'humidité relative "humidity" pour les signalements et "humidity" pour DSK pour 700 points darksky

#### Après avoir :

#### vérifié que le nombre d'obs n'est pas égal entre les tableaux 
length(humdataAL$humidity)
length(na.omit(humdataAL$humidity)) ## pour tester si NA
length(DSKdataAL$humidity)
length(na.omit(DSKdataAL$humidity)) ## pour tester si NA

##### et que l'étendue n'est pas la même
range(humdataAL$humidity, na.rm = TRUE)
range(DSKdataAL$humidity, na.rm = TRUE)

### on calcule le nb de lignes des tableaux (pour l'habillage des histogrammes)
nr <- nrow(humdataAL)
nrdsk <- nrow(DSKdataAL)

#### on définit les breaks pour l'abscisse commune
BRt <- seq(from= 20, to= 100, by=2) ## tient compte des deux distributions

#### puis on fait l'histo en utilisant 

### freq=F => des fréquences relatives et pas des effectifs

hist(humdataAL$humidity, breaks = BRt,
     freq=F, # fréquences
     col="grey",
     main = "Average Humidity on 2,761 reporting of ticks \n (Alsace-Lorraine, july 2017 - april 2020), 995 days",
     ylab = "Densities",
     xlab = "Average Humidity (%)"
)

### calcul des paramètres pour la fonction lines() à superposer à l'histo

### /!\ ici x100 pour compenser la différence d'unité /!\ 

HH2 <- hist(DSKdataAL$humidity*100, breaks = BRt,  plot=F)


#lines(HH2$mids, HH2$density, lwd = 2, col = "orange") ### courbe non lissée  ## SO

### /!\ ici x100 pour compenser la différence d'unité /!\ 

lines(density(DSKdataAL$humidity*100, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel et création d'un champ humidité en % /!\

text(35, 0.015, paste("Reporting"), cex = 1.2,  col = "black")
text(45, 0.03, paste("Random"), cex = 1.2,  col = "blue")
# text(8, .08, paste("N =",nr," Reporting" ), col = "black")

## Si on voulait le nb d'enregistrements tudiés à la place du texte "Darksky" ou MF, il faudrait :
### text(03, 0.07, paste("N =", sum(complete.cases(humdataAL$humidity))), cex = 1.2,  col = "black")
### text(02, 0.06, paste("N =", sum(complete.cases(DSKdataAL$humidity))), cex = 1.2,  col = "blue")

## Si on voulait les courbes à +1.5°C ou +2°C, il faudrait fabriquer et afficher :
### lines(density(DSKdataAL$humidity_p15D, na.rm = TRUE), lwd = 2, col = "green") ### courbe lissée, kernel
### lines(density(DSKdataAL$humidity_p2D, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel


### 5.2. Test des distributions

### 5.2.1. Test si distribution normale


## References :
## Patrick Royston (1982). An extension of Shapiro and Wilk's W test for normality to large samples. Applied Statistics, 31, 115–124. doi: 10.2307/2347973.

### Echantillonnage de humdataAL$humidity pour avoir une taille d'échantillon < 5 000

# data[sample(1:nrow(data),n),] # pour faire un sous tableau échantillonné http://forums.cirad.fr/logiciel-r/viewtopic.php?t=6118

shapiro.test(humdataAL[sample(1:nrow(humdataAL),300),"humidity"]) # pour échantillonner une seule variable du data frame
# plus le n augmente, plus la p-value devient significative (si < 5 000 !)

# data:  humdataAL[sample(1:nrow(humdataAL), 30), "humidity"]
# W = 0.96216, p-value = 0.3513 => p-value non significative, l'échantillon suit une loi normale.

# data:  humdataAL[sample(1:nrow(humdataAL), 300), "humidity"]
# W = 0.99362, p-value = 0.2353 => p-value non significative, l'échantillon suit une loi normale.(5/6 non signif !).

# shapiro.test(humdataAL$humidity) # uniquement si nrow < 5 000 (pas le cas ici !)

shapiro.test(DSKdataAL[sample(1:nrow(DSKdataAL),30),"humidity"])
# data:  DSKdataAL[sample(1:nrow(DSKdataAL), 30), "humidity"]
# W = 0.94822, p-value = 0.1514 => p-value non significative, l'échantillon suit une loi normale (10/10 non signif !).

# data:  DSKdataAL[sample(1:nrow(DSKdataAL), 300), "humidity"]
# W = 0.97556, p-value = 5.373e-05 => p-value significative, l'échantillon ne suit pas une loi normale (diffère, ce coup ci. Mais peut-?tre pas ? tous les coups !).


### t.test entre les différentes humidités relatives
t.test(DSKdataAL$humidity*100,humdataAL$humidity) # t = 20.061, df = 3287, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents (mais ne suivent pas une loi normale) !

# t.test(DSKdataAL$humidity_p1D,humdataAL$humidity) ## SO
# t.test(DSKdataAL$humidity_p15D,humdataAL$humidity) ## SO
# t.test(DSKdataAL$humidity_p2D,humdataAL$humidity) ## SO



### 5.2.2. Test si distribution non normale


### Import des données pour le test kruskal.test() sur données DSKdataAL$humidity, humdataAL$humidity

## création de deux sous-ensembles "subsets" avec juste les variables à comparer et l'origine de l'éditeur 
### ajout d'une colonne à droite contenant 1 var quali (si val num x$new=10:12) pour indiquer l'origine des données
### si ajout à gauche <cbind(10:12,x)> http://forums.cirad.fr/logiciel-R/viewtopic.php?t=9737
humdataAL$origine="hum"
d1 <- subset(humdataAL, select = c("humidity", "origine")) ## création du subset venant de humdataAL http://larmarange.github.io/analyse-R/fusion-de-tables.html
dim(d1)
head(d1, 3)
DSKdataAL$humid100=DSKdataAL$humidity*100 ## création d'un champ humidité en %
### /!\ ici x100 pour compenser la différence d'unité /!\ 
d2 <- subset(DSKdataAL, select = c("humid100", "origine")) ## création du subset venant de DSK
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

## Kruskal-Wallis chi-squared = 367.42, df = 1, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents !

summary(humdataAL$humidity)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 32.00   64.00   71.00   70.61   78.00  100.00  
summary(DSKdataAL$humid100)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 27.00   66.00   76.00   74.78   84.00  100.00      15  


### 5.3. Calcul d'un IC via le test de Wilcoxon sur la médiane (cf. section 3.2 de Poinsot et C:\3VG\MSH\Lyme\Smartick17\data\TCD7\R\IC_foret_AL_CLC5.R)

### Comme distribution non normale

wilcox.test(humdataAL$humidity,
            conf.int=TRUE
)$conf.int ## cette ligne pour demander l'IC 

### [1] 70.49998 71.00003
### attr(,"conf.level")
### [1] 0.95

### NO Warning messages:

### Soit un IC à 95% = [70.5 ; 71.0] autour de la /!\ médiane /!\


wilcox.test(DSKdataAL$humid100,
            conf.int=TRUE
)$conf.int ## cette ligne pour demander l'IC 

### [1] 75.00009 75.49996
### attr(,"conf.level")
### [1] 0.95

### Soit un IC à 95% = [75.0 ; 75.5] autour de la /!\ médiane /!\




# 6. Analyse des points de rosée DSK vs données humaines (méthode Alice Favre)

## 6.1. Histogramme des points de rosée "dewpoint" pour les signalements et "dewpoint" pour DSK pour 700 points darksky

#### Après avoir :

#### vérifié que le nombre d'obs n'est pas égal entre les tableaux 
length(humdataAL$dewpoint)
length(na.omit(humdataAL$dewpoint)) ## pour tester si NA
length(DSKdataAL$dewpoint)
length(na.omit(DSKdataAL$dewpoint)) ## pour tester si NA

##### et que l'étendue n'est pas la même
range(humdataAL$dewpoint, na.rm = TRUE)
range(DSKdataAL$dewpoint, na.rm = TRUE)

### on calcule le nb de lignes des tableaux (pour l'habillage des histogrammes)
nr <- nrow(humdataAL)
nrdsk <- nrow(DSKdataAL)

#### on définit les breaks pour l'abscisse commune
BRt <- seq(from= -20, to= 25, by=1) ## tient compte des deux distributions

#### puis on fait l'histo en utilisant 

### freq=F => des fréquences relatives et pas des effectifs

hist(humdataAL$dewpoint, breaks = BRt,
     freq=F, # fréquences
     col="grey",
     main = "Average dewpoint on 2,761 reporting of ticks \n (Alsace-Lorraine, july 2017 - april 2020), 995 days",
     ylab = "Densities",
     xlab = "Average dewpoint (Td°C)"
)

### calcul des paramètres pour la fonction lines() à superposer à l'histo

HH2 <- hist(DSKdataAL$dewpoint, breaks = BRt,  plot=F)

#lines(HH2$mids, HH2$density, lwd = 2, col = "orange") ### courbe non lissée  ## SO
lines(density(DSKdataAL$dewpoint, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel

text(5, 0.08, paste("Reporting"), cex = 1.2,  col = "black")
text(-5, 0.04, paste("Random"), cex = 1.2,  col = "blue")
# text(8, .08, paste("N =",nr," Reporting" ), col = "black")

## Si on voulait le nb d'enregistrements tudiés à la place du texte "Darksky" ou MF, il faudrait :
### text(03, 0.07, paste("N =", sum(complete.cases(humdataAL$dewpoint))), cex = 1.2,  col = "black")
### text(02, 0.06, paste("N =", sum(complete.cases(DSKdataAL$dewpoint))), cex = 1.2,  col = "blue")

## Si on voulait les courbes à +1.5°C ou +2°C, il faudrait fabriquer et afficher :
### lines(density(DSKdataAL$dewpoint_p15D, na.rm = TRUE), lwd = 2, col = "green") ### courbe lissée, kernel
### lines(density(DSKdataAL$dewpoint_p2D, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel


### 6.2. Test des distributions

### 6.2.1. Test si distribution normale


## References :
## Patrick Royston (1982). An extension of Shapiro and Wilk's W test for normality to large samples. Applied Statistics, 31, 115–124. doi: 10.2307/2347973.

### Echantillonnage de humdataAL$dewpoint pour avoir une taille d'échantillon < 5 000

# data[sample(1:nrow(data),n),] # pour faire un sous tableau échantillonné http://forums.cirad.fr/logiciel-r/viewtopic.php?t=6118

shapiro.test(humdataAL[sample(1:nrow(humdataAL),"300"),"dewpoint"]) # pour échantillonner une seule variable du data frame
# plus le n augmente, plus la p-value devient significative (si < 5 000 !)

# data:  humdataAL[sample(1:nrow(humdataAL), 30), "dewpoint"]
# W = 0.9464, p-value = 0.1353 => p-value non significative, l'échantillon suit une loi normale (signif 2/3 quand même).

# data:  humdataAL[sample(1:nrow(humdataAL), 300), "dewpoint"]
# W = 0.96781, p-value = 3.093e-06 => p-value significative, l'échantillon ne suit pas une loi normale.(tjrs signif !).

# shapiro.test(humdataAL$dewpoint) # uniquement si nrow < 5 000 (pas le cas ici !)

shapiro.test(DSKdataAL[sample(1:nrow(DSKdataAL),30),"dewpoint"])
# data:  DSKdataAL[sample(1:nrow(DSKdataAL), 30), "dewpoint"]
# W = 0.94835, p-value = 0.1527 => p-value non significative, l'échantillon suit une loi normale.

# data:  DSKdataAL[sample(1:nrow(DSKdataAL), 300), "dewpoint"]
# W = 0.98946, p-value = 0.02882 => p-value significative, l'échantillon ne suit pas une loi normale (diffère, ce coup ci. Mais peut-être pas ? tous les coups !).



### t.test entre les différents points de rosée
t.test(DSKdataAL$dewpoint,humdataAL$dewpoint) # t = -62.408, df = 3541, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents (mais ne suivent pas une loi normale) !

# t.test(DSKdataAL$dewpoint_p1D,humdataAL$dewpoint) ## SO
# t.test(DSKdataAL$dewpoint_p15D,humdataAL$dewpoint) ## SO
# t.test(DSKdataAL$dewpoint_p2D,humdataAL$dewpoint) ## SO



### 6.2. Test si distribution non normale


### Import des données pour le test kruskal.test() sur données DSKdataAL$dewpoint, humdataAL$dewpoint

## création de deux sous-ensembles "subsets" avec juste les variables à comparer et l'origine de l'éditeur 
### ajout d'une colonne à droite contenant 1 var quali (si val num x$new=10:12) pour indiquer l'origine des données
### si ajout à gauche <cbind(10:12,x)> http://forums.cirad.fr/logiciel-R/viewtopic.php?t=9737
humdataAL$origine="hum"
d1 <- subset(humdataAL, select = c("dewpoint", "origine")) ## création du subset venant de humdataAL http://larmarange.github.io/analyse-R/fusion-de-tables.html
dim(d1)
head(d1, 3)
d2 <- subset(DSKdataAL, select = c("dewpoint", "origine")) ## création du subset venant de DSK
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

## Kruskal-Wallis chi-squared = 2110.7, df = 1, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents !

summary(humdataAL$dewpoint)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -11.03    8.93   12.01   11.34   14.64   20.27
summary(DSKdataAL$dewpoint)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# -17.910   1.350   5.990   5.838  10.610  21.560      15   


### 6.3. Calcul d'un IC via le test de Wilcoxon sur la médiane (cf. section 3.2 de Poinsot et C:\3VG\MSH\Lyme\Smartick17\data\TCD7\R\IC_foret_AL_CLC5.R)

### Comme distribution non normale

wilcox.test(humdataAL$dewpoint,
            conf.int=TRUE
)$conf.int ## cette ligne pour demander l'IC 

### [1] 11.49996 11.83002
### attr(,"conf.level")
### [1] 0.95

### NO Warning messages:

### Soit un IC à 95% = [11.5 ; 11.8] autour de la /!\ médiane /!\


wilcox.test(DSKdataAL$dewpoint,
            conf.int=TRUE
)$conf.int ## cette ligne pour demander l'IC 

### [1] 5.894959 6.019979
### attr(,"conf.level")
### [1] 0.95

### Soit un IC à 95% = [5.9 ; 6.0] autour de la /!\ médiane /!\


# 7. Analyse des pressions atmosphériques DSK vs données humaines (méthode Alice Favre)

## 7.1. Histogramme des pressions atmosphériques "pressure" pour les signalements et "pressure" pour DSK pour 700 points darksky

#### Après avoir :

#### vérifié que le nombre d'obs n'est pas égal entre les tableaux 
length(humdataAL$pressure)
length(na.omit(humdataAL$pressure)) ## pour tester si NA
length(DSKdataAL$pressure)
length(na.omit(DSKdataAL$pressure)) ## pour tester si NA

##### et que l'étendue n'est pas la même
range(humdataAL$pressure, na.rm = TRUE)
range(DSKdataAL$pressure, na.rm = TRUE)

### on calcule le nb de lignes des tableaux (pour l'habillage des histogrammes)
nr <- nrow(humdataAL)
nrdsk <- nrow(DSKdataAL)

#### on définit les breaks pour l'abscisse commune
BRt <- seq(from= 980, to= 1050, by=5) ## tient compte des deux distributions

#### puis on fait l'histo en utilisant 

### freq=F => des fréquences relatives et pas des effectifs

hist(humdataAL$pressure, breaks = BRt,
     freq=F, # fréquences
     col="grey",
     main = "Average pressure on 2,761 reporting of ticks \n (Alsace-Lorraine, july 2017 - april 2020), 995 days",
     ylab = "Densities",
     xlab = "Average pressure (hPa)"
)

### calcul des paramètres pour la fonction lines() à superposer à l'histo

HH2 <- hist(DSKdataAL$pressure, breaks = BRt,  plot=F)

#lines(HH2$mids, HH2$density, lwd = 2, col = "orange") ### courbe non lissée  ## SO
lines(density(DSKdataAL$pressure, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel

text(1000, 0.05, paste("Reporting"), cex = 1.2,  col = "black")
text(990, 0.03, paste("Random"), cex = 1.2,  col = "blue")
# text(8, .08, paste("N =",nr," Reporting" ), col = "black")

## Si on voulait le nb d'enregistrements tudiés à la place du texte "Darksky" ou MF, il faudrait :
### text(03, 0.07, paste("N =", sum(complete.cases(humdataAL$pressure))), cex = 1.2,  col = "black")
### text(02, 0.06, paste("N =", sum(complete.cases(DSKdataAL$pressure))), cex = 1.2,  col = "blue")

## Si on voulait les courbes à +1.5°C ou +2°C, il faudrait fabriquer et afficher :
### lines(density(DSKdataAL$pressure_p15D, na.rm = TRUE), lwd = 2, col = "green") ### courbe lissée, kernel
### lines(density(DSKdataAL$pressure_p2D, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel


### 7.2. Test des distributions

### 7.2.1. Test si distribution normale


## References :
## Patrick Royston (1982). An extension of Shapiro and Wilk's W test for normality to large samples. Applied Statistics, 31, 115–124. doi: 10.2307/2347973.

### Echantillonnage de humdataAL$pressure pour avoir une taille d'échantillon < 5 000

# data[sample(1:nrow(data),n),] # pour faire un sous tableau échantillonné http://forums.cirad.fr/logiciel-r/viewtopic.php?t=6118

shapiro.test(humdataAL[sample(1:nrow(humdataAL),30),"pressure"]) # pour échantillonner une seule variable du data frame
# plus le n augmente, plus la p-value devient significative (si < 5 000 !)

# data:  humdataAL[sample(1:nrow(humdataAL), 30), "pressure"]
# W = 0.94206, p-value = 0.1989 => p-value non significative, l'échantillon suit une loi normale.

# data:  humdataAL[sample(1:nrow(humdataAL), 300), "pressure"]
# W = 0.9807, p-value = 0.006653 => p-value significative, l'échantillon ne suit pas une loi normale.(diffère, ce coup ci. Mais pas à tous les coups !).

# shapiro.test(humdataAL$pressure) # uniquement si nrow < 5 000 (pas le cas ici !)

shapiro.test(DSKdataAL[sample(1:nrow(DSKdataAL),30),"pressure"])
# data:  DSKdataAL[sample(1:nrow(DSKdataAL), 30), "pressure"]
# W = 0.97022, p-value = 0.6506 => p-value non significative, l'échantillon suit une loi normale.

# data:  DSKdataAL[sample(1:nrow(DSKdataAL), 300), "pressure"]
# W = 0.99185, p-value = 0.1509 => p-value non significative, l'échantillon suit une loi normale.
# W = 0.98638, p-value = 0.01023 => p-value significative, l'échantillon ne suit pas une loi normale (diffère, ce coup ci. Mais peut-être pas ? tous les coups !).




### t.test entre les différentes pressions atmosphériques
t.test(DSKdataAL$pressure,humdataAL$pressure) # t = 5.4359, df = 2032.3, p-value = 6.106e-08 => p-value significative, les 2 échantillons sont significativement différents (mais ne suivent pas toujours une loi normale) !

# t.test(DSKdataAL$pressure_p1D,humdataAL$pressure) ## SO
# t.test(DSKdataAL$pressure_p15D,humdataAL$pressure) ## SO
# t.test(DSKdataAL$pressure_p2D,humdataAL$pressure) ## SO



### 7.2. Test si distribution non normale


### Import des données pour le test kruskal.test() sur données DSKdataAL$pressure, humdataAL$pressure

## création de deux sous-ensembles "subsets" avec juste les variables à comparer et l'origine de l'éditeur 
### ajout d'une colonne à droite contenant 1 var quali (si val num x$new=10:12) pour indiquer l'origine des données
### si ajout à gauche <cbind(10:12,x)> http://forums.cirad.fr/logiciel-R/viewtopic.php?t=9737
humdataAL$origine="hum"
d1 <- subset(humdataAL, select = c("pressure", "origine")) ## création du subset venant de humdataAL http://larmarange.github.io/analyse-R/fusion-de-tables.html
dim(d1)
head(d1, 3)
d2 <- subset(DSKdataAL, select = c("pressure", "origine")) ## création du subset venant de DSK
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

## Kruskal-Wallis chi-squared = 29.14, df = 1, p-value = 6.733e-08 => p-value significative, les 2 échantillons sont significativement différents !

summary(humdataAL$pressure)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 993.5  1013.1  1016.9  1016.7  1020.7  1045.2    1067 
summary(DSKdataAL$pressure)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
# 979.5  1012.5  1017.9  1017.5  1022.9  1045.4    5260   


### 7.3. Calcul d'un IC via le test de Wilcoxon sur la médiane (cf. section 3.2 de Poinsot et C:\3VG\MSH\Lyme\Smartick17\data\TCD7\R\IC_foret_AL_CLC5.R)

### Comme distribution non normale

wilcox.test(humdataAL$pressure,
            conf.int=TRUE
)$conf.int ## cette ligne pour demander l'IC 

### [1] 1016.5 1017.1
### attr(,"conf.level")
### [1] 0.95

### NO Warning messages:

### Soit un IC à 95% = [1016.5 ; 1017.1] autour de la /!\ médiane /!\


wilcox.test(DSKdataAL$pressure,
            conf.int=TRUE
)$conf.int ## cette ligne pour demander l'IC 

### [1] 1017.6 1017.8
### attr(,"conf.level")
### [1] 0.95

### Soit un IC à 95% = [1017.6 ; 1017.8] autour de la /!\ médiane /!\



# 8. Analyse des vitesse du vent DSK vs données humaines (méthode Alice Favre)

## 8.1. Histogramme des vitesse du vent "windspeed" pour les signalements et "windspeed" pour DSK pour 700 points darksky

#### Après avoir :

#### vérifié que le nombre d'obs n'est pas égal entre les tableaux 
length(humdataAL$windspeed)
length(na.omit(humdataAL$windspeed)) ## pour tester si NA
length(DSKdataAL$windspeed)
length(na.omit(DSKdataAL$windspeed)) ## pour tester si NA

##### et que l'étendue n'est pas la même
range(humdataAL$windspeed, na.rm = TRUE)
range(DSKdataAL$windspeed, na.rm = TRUE)

### on calcule le nb de lignes des tableaux (pour l'habillage des histogrammes)
nr <- nrow(humdataAL)
nrdsk <- nrow(DSKdataAL)

#### on définit les breaks pour l'abscisse commune
BRt <- seq(from= 0, to= 17, by=1) ## tient compte des deux distributions

#### puis on fait l'histo en utilisant 

### freq=F => des fréquences relatives et pas des effectifs

hist(humdataAL$windspeed, breaks = BRt,
     freq=F, # fréquences
     col="grey",
     main = "Average windspeed on 2,761 reporting of ticks \n (Alsace-Lorraine, july 2017 - april 2020), 995 days",
     ylab = "Densities",
     xlab = "Average windspeed (m/s)"
)

### calcul des paramètres pour la fonction lines() à superposer à l'histo

HH2 <- hist(DSKdataAL$windspeed, breaks = BRt,  plot=F)

#lines(HH2$mids, HH2$density, lwd = 2, col = "orange") ### courbe non lissée  ## SO
lines(density(DSKdataAL$windspeed, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel

text(6, 0.25, paste("Reporting"), cex = 1.2,  col = "black")
text(7, 0.10, paste("Random"), cex = 1.2,  col = "blue")
# text(8, .08, paste("N =",nr," Reporting" ), col = "black")

## Si on voulait le nb d'enregistrements tudiés à la place du texte "Darksky" ou MF, il faudrait :
### text(03, 0.07, paste("N =", sum(complete.cases(humdataAL$windspeed))), cex = 1.2,  col = "black")
### text(02, 0.06, paste("N =", sum(complete.cases(DSKdataAL$windspeed))), cex = 1.2,  col = "blue")

## Si on voulait les courbes à +1.5°C ou +2°C, il faudrait fabriquer et afficher :
### lines(density(DSKdataAL$windspeed_p15D, na.rm = TRUE), lwd = 2, col = "green") ### courbe lissée, kernel
### lines(density(DSKdataAL$windspeed_p2D, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel


### 8.2. Test des distributions

### 8.2.1. Test si distribution normale


## References :
## Patrick Royston (1982). An extension of Shapiro and Wilk's W test for normality to large samples. Applied Statistics, 31, 115–124. doi: 10.2307/2347973.

### Echantillonnage de humdataAL$windspeed pour avoir une taille d'échantillon < 5 000

# data[sample(1:nrow(data),n),] # pour faire un sous tableau échantillonné http://forums.cirad.fr/logiciel-r/viewtopic.php?t=6118

shapiro.test(humdataAL[sample(1:nrow(humdataAL),30),"windspeed"]) # pour échantillonner une seule variable du data frame
# plus le n augmente, plus la p-value devient significative (si < 5 000 !)

# data:  humdataAL[sample(1:nrow(humdataAL), 30), "windspeed"]
# W = 0.83025, p-value = 0.0002459 => p-value significative, l'échantillon ne suit pas une loi normale (2/3 signif).

# data:  humdataAL[sample(1:nrow(humdataAL), 300), "windspeed"]
# W = 0.88591, p-value = 3.449e-14 => p-value significative, l'échantillon ne suit pas une loi normale.(diffère, ce coup ci. Mais pas à tous les coups !).

# shapiro.test(humdataAL$windspeed) # uniquement si nrow < 5 000 (pas le cas ici !)

shapiro.test(DSKdataAL[sample(1:nrow(DSKdataAL),30),"windspeed"])
# data:  DSKdataAL[sample(1:nrow(DSKdataAL), 30), "windspeed"]
# W = 0.97797, p-value = 0.7694 => p-value significative, l'échantillon ne suit pas une loi normale (8/9 signif).

# data:  DSKdataAL[sample(1:nrow(DSKdataAL), 300), "windspeed"]
# W = 0.92803, p-value = 7.352e-11 => p-value significative, l'échantillon ne suit pas une loi normale (diffère, ce coup ci. Mais peut-être pas ? tous les coups !).



### t.test entre les différents vitesse du vent
t.test(DSKdataAL$windspeed,humdataAL$windspeed) # t = 23.488, df = 3823.6, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents (mais ne suivent pas une loi normale) !

# t.test(DSKdataAL$windspeed_p1D,humdataAL$windspeed) ## SO
# t.test(DSKdataAL$windspeed_p15D,humdataAL$windspeed) ## SO
# t.test(DSKdataAL$windspeed_p2D,humdataAL$windspeed) ## SO



### 8.2. Test si distribution non normale


### Import des données pour le test kruskal.test() sur données DSKdataAL$windspeed, humdataAL$windspeed

## création de deux sous-ensembles "subsets" avec juste les variables à comparer et l'origine de l'éditeur 
### ajout d'une colonne à droite contenant 1 var quali (si val num x$new=10:12) pour indiquer l'origine des données
### si ajout à gauche <cbind(10:12,x)> http://forums.cirad.fr/logiciel-R/viewtopic.php?t=9737
humdataAL$origine="hum"
d1 <- subset(humdataAL, select = c("windspeed", "origine")) ## création du subset venant de humdataAL http://larmarange.github.io/analyse-R/fusion-de-tables.html
dim(d1)
head(d1, 3)
d2 <- subset(DSKdataAL, select = c("windspeed", "origine")) ## création du subset venant de DSK
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

## Kruskal-Wallis chi-squared = 115.24, df = 1, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents !

summary(humdataAL$windspeed)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.170   1.800   2.320   2.499   2.990  10.010       2 
summary(DSKdataAL$windspeed)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.000   1.740   2.570   3.012   3.820  16.470      41    


### 8.3. Calcul d'un IC via le test de Wilcoxon sur la médiane (cf. section 3.2 de Poinsot et C:\3VG\MSH\Lyme\Smartick17\data\TCD7\R\IC_foret_AL_CLC5.R)

### Comme distribution non normale

wilcox.test(humdataAL$windspeed,
            conf.int=TRUE
)$conf.int ## cette ligne pour demander l'IC 

### [1] 2.360068 2.434982
### attr(,"conf.level")
### [1] 0.95

### NO Warning messages:

### Soit un IC à 95% = [2.36 ; 2.43] autour de la /!\ médiane /!\


wilcox.test(DSKdataAL$windspeed,
            conf.int=TRUE
)$conf.int ## cette ligne pour demander l'IC 

### [1] 2.765046 2.799989
### attr(,"conf.level")
### [1] 0.95

### Soit un IC à 95% = [2.76 ; 2.79] autour de la /!\ médiane /!\




# 9. Analyse des visibilités DSK vs données humaines (méthode Alice Favre)

## 9.1. Histogramme des visibilités "visibility" pour les signalements et "visibility" pour DSK pour 700 points darksky

#### Après avoir :

#### vérifié que le nombre d'obs n'est pas égal entre les tableaux 
length(humdataAL$visibility)
length(na.omit(humdataAL$visibility)) ## pour tester si NA
length(DSKdataAL$visibility)
length(na.omit(DSKdataAL$visibility)) ## pour tester si NA

##### et que l'étendue n'est pas la même
range(humdataAL$visibility, na.rm = TRUE)
range(DSKdataAL$visibility, na.rm = TRUE)

### on calcule le nb de lignes des tableaux (pour l'habillage des histogrammes)
nr <- nrow(humdataAL)
nrdsk <- nrow(DSKdataAL)

#### on définit les breaks pour l'abscisse commune
BRt <- seq(from= 0, to= 20, by=1) ## tient compte des deux distributions

#### puis on fait l'histo en utilisant 

### freq=F => des fréquences relatives et pas des effectifs

hist(humdataAL$visibility, breaks = BRt,
     freq=F, # fréquences
     col="grey",
     main = "Average visibility on 2,761 reporting of ticks \n (Alsace-Lorraine, july 2017 - april 2020), 995 days",
     ylab = "Densities",
     xlab = "Average visibility (km)"
)

### calcul des paramètres pour la fonction lines() à superposer à l'histo

HH2 <- hist(DSKdataAL$visibility, breaks = BRt,  plot=F)

#lines(HH2$mids, HH2$density, lwd = 2, col = "orange") ### courbe non lissée  ## SO
lines(density(DSKdataAL$visibility, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel

text(7, 0.20, paste("Reporting"), cex = 1.2,  col = "black")
text(5, 0.10, paste("Random"), cex = 1.2,  col = "blue")
# text(8, .08, paste("N =",nr," Reporting" ), col = "black")

## Si on voulait le nb d'enregistrements tudiés à la place du texte "Darksky" ou MF, il faudrait :
### text(03, 0.07, paste("N =", sum(complete.cases(humdataAL$visibility))), cex = 1.2,  col = "black")
### text(02, 0.06, paste("N =", sum(complete.cases(DSKdataAL$visibility))), cex = 1.2,  col = "blue")

## Si on voulait les courbes à +1.5°C ou +2°C, il faudrait fabriquer et afficher :
### lines(density(DSKdataAL$visibility_p15D, na.rm = TRUE), lwd = 2, col = "green") ### courbe lissée, kernel
### lines(density(DSKdataAL$visibility_p2D, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel


### 9.2. Test des distributions

### 9.2.1. Test si distribution normale


## References :
## Patrick Royston (1982). An extension of Shapiro and Wilk's W test for normality to large samples. Applied Statistics, 31, 115–124. doi: 10.2307/2347973.

### Echantillonnage de humdataAL$visibility pour avoir une taille d'échantillon < 5 000

# data[sample(1:nrow(data),n),] # pour faire un sous tableau échantillonné http://forums.cirad.fr/logiciel-r/viewtopic.php?t=6118

shapiro.test(humdataAL[sample(1:nrow(humdataAL),30),"visibility"]) # pour échantillonner une seule variable du data frame
# plus le n augmente, plus la p-value devient significative (si < 5 000 !)

# data:  humdataAL[sample(1:nrow(humdataAL), 30), "visibility"]
# W = 0.80348, p-value = 7.591e-05 => p-value significative, l'échantillon ne suit pas une loi normale.(diffère tt le temps)



shapiro.test(DSKdataAL[sample(1:nrow(DSKdataAL),30),"visibility"])
# data:  DSKdataAL[sample(1:nrow(DSKdataAL), 30), "visibility"]
# 0.71497, p-value = 2.596e-06 => p-value significative, l'échantillon ne suit pas une loi normale.(diffère tt le temps)



### t.test entre les différents visibilités
t.test(DSKdataAL$visibility,humdataAL$visibility) # t = 17.13, df = 3232.5, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents (mais ne suivent pas une loi normale) !

# t.test(DSKdataAL$visibility_p1D,humdataAL$visibility) ## SO
# t.test(DSKdataAL$visibility_p15D,humdataAL$visibility) ## SO
# t.test(DSKdataAL$visibility_p2D,humdataAL$visibility) ## SO



### 9.2. Test si distribution non normale


### Import des données pour le test kruskal.test() sur données DSKdataAL$visibility, humdataAL$visibility

## création de deux sous-ensembles "subsets" avec juste les variables à comparer et l'origine de l'éditeur 
### ajout d'une colonne à droite contenant 1 var quali (si val num x$new=10:12) pour indiquer l'origine des données
### si ajout à gauche <cbind(10:12,x)> http://forums.cirad.fr/logiciel-R/viewtopic.php?t=9737
humdataAL$origine="hum"
d1 <- subset(humdataAL, select = c("visibility", "origine")) ## création du subset venant de humdataAL http://larmarange.github.io/analyse-R/fusion-de-tables.html
dim(d1)
head(d1, 3)
d2 <- subset(DSKdataAL, select = c("visibility", "origine")) ## création du subset venant de DSK
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

## Kruskal-Wallis chi-squared = 159.92, df = 1, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents !

summary(humdataAL$visibility)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 2.703   9.935  10.003  11.657  15.756  16.093      28
summary(DSKdataAL$visibility)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.185   9.941  13.568  12.659  16.093  16.093    1418   


### 9.3. Calcul d'un IC via le test de Wilcoxon sur la médiane (cf. section 3.2 de Poinsot et C:\3VG\MSH\Lyme\Smartick17\data\TCD7\R\IC_foret_AL_CLC5.R)

### Comme distribution non normale

wilcox.test(humdataAL$visibility,
            conf.int=TRUE
)$conf.int ## cette ligne pour demander l'IC 

### [1] 11.50101 12.17454
### attr(,"conf.level")
### [1] 0.95

### NO Warning messages:

### Soit un IC à 95% = [11.5 ; 12.2] autour de la /!\ médiane /!\


wilcox.test(DSKdataAL$visibility,
            conf.int=TRUE
)$conf.int ## cette ligne pour demander l'IC 

### [1] 12.86356 12.89644
### attr(,"conf.level")
### [1] 0.95

### Soit un IC à 95% = [12.86 ; 12.90] autour de la /!\ médiane /!\


# 10. Analyse du couvert nuageux DSK vs données humaines (méthode Alice Favre)

## 10.1. Histogramme du couvert nuageux "cloudcover" pour les signalements et "cloudcover" pour DSK pour 700 points darksky

#### Après avoir :

#### vérifié que le nombre d'obs n'est pas égal entre les tableaux 
length(humdataAL$cloudcover)
length(na.omit(humdataAL$cloudcover)) ## pour tester si NA
length(DSKdataAL$cloudcover)
length(na.omit(DSKdataAL$cloudcover)) ## pour tester si NA

##### et que l'étendue n'est pas la même
range(humdataAL$cloudcover, na.rm = TRUE)
range(DSKdataAL$cloudcover, na.rm = TRUE)

### on calcule le nb de lignes des tableaux (pour l'habillage des histogrammes)
nr <- nrow(humdataAL)
nrdsk <- nrow(DSKdataAL)

#### on définit les breaks pour l'abscisse commune
BRt <- seq(from= 0, to= 100, by=2) ## tient compte des deux distributions

#### puis on fait l'histo en utilisant 

### freq=F => des fréquences relatives et pas des effectifs

hist(humdataAL$cloudcover, breaks = BRt,
     freq=F, # fréquences
     col="grey",
     main = "Average cloudcover on 2,761 reporting of ticks \n (Alsace-Lorraine, july 2017 - april 2020), 995 days",
     ylab = "Densities",
     xlab = "Average cloudcover (%)"
)

### calcul des paramètres pour la fonction lines() à superposer à l'histo

### /!\ ici x100 pour compenser la différence d'unité /!\ 

HH2 <- hist(DSKdataAL$cloudcover*100, breaks = BRt,  plot=F)


#lines(HH2$mids, HH2$density, lwd = 2, col = "orange") ### courbe non lissée  ## SO

### /!\ ici x100 pour compenser la différence d'unité /!\ 

lines(density(DSKdataAL$cloudcover*100, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel et création d'un champ humidité en % /!\

text(20, 0.012, paste("Reporting"), cex = 1.2,  col = "black")
text(40, 0.017, paste("Random"), cex = 1.2,  col = "blue")
# text(8, .08, paste("N =",nr," Reporting" ), col = "black")

## Si on voulait le nb d'enregistrements tudiés à la place du texte "Darksky" ou MF, il faudrait :
### text(03, 0.07, paste("N =", sum(complete.cases(humdataAL$cloudcover))), cex = 1.2,  col = "black")
### text(02, 0.06, paste("N =", sum(complete.cases(DSKdataAL$cloudcover))), cex = 1.2,  col = "blue")

## Si on voulait les courbes à +1.5°C ou +2°C, il faudrait fabriquer et afficher :
### lines(density(DSKdataAL$cloudcover_p15D, na.rm = TRUE), lwd = 2, col = "green") ### courbe lissée, kernel
### lines(density(DSKdataAL$cloudcover_p2D, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel


### 10.2. Test des distributions

### 10.2.1. Test si distribution normale


## References :
## Patrick Royston (1982). An extension of Shapiro and Wilk's W test for normality to large samples. Applied Statistics, 31, 115–124. doi: 10.2307/2347973.

### Echantillonnage de humdataAL$cloudcover pour avoir une taille d'échantillon < 5 000

# data[sample(1:nrow(data),n),] # pour faire un sous tableau échantillonné http://forums.cirad.fr/logiciel-r/viewtopic.php?t=6118

shapiro.test(humdataAL[sample(1:nrow(humdataAL),30),"cloudcover"]) # pour échantillonner une seule variable du data frame
# plus le n augmente, plus la p-value devient significative (si < 5 000 !)

# data:  humdataAL[sample(1:nrow(humdataAL), 30), "cloudcover"]
# W = 0.94019, p-value = 0.1014 => p-value non significative, l'échantillon suit une loi normale (5 coup sur 7)

# data:  humdataAL[sample(1:nrow(humdataAL), 300), "cloudcover"]
# W = 0.92222, p-value = 6.559e-11 => p-value significative, l'échantillon ne suit pas une loi normale.(diffère, ce coup ci. Mais pas à tous les coups !).

# shapiro.test(humdataAL$cloudcover) # uniquement si nrow < 5 000 (pas le cas ici !)

shapiro.test(DSKdataAL[sample(1:nrow(DSKdataAL),30),"cloudcover"])
# data:  DSKdataAL[sample(1:nrow(DSKdataAL), 30), "cloudcover"]
# W = 0.86705, p-value = 0.002106 => p-value significative, l'échantillon ne suit pas une loi normale (4 coup sur 5 !)

# data:  DSKdataAL[sample(1:nrow(DSKdataAL), 300), "cloudcover"]
# W = 0.9194, p-value = 2.697e-11 => p-value significative, l'échantillon ne suit pas une loi normale.

### t.test entre les différentes humidités relatives
t.test(DSKdataAL$cloudcover*100,humdataAL$cloudcover) # t = 8.5776, df = 2474.3, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents (mais ne suivent pas une loi normale) !

# t.test(DSKdataAL$cloudcover_p1D,humdataAL$cloudcover) ## SO
# t.test(DSKdataAL$cloudcover_p15D,humdataAL$cloudcover) ## SO
# t.test(DSKdataAL$cloudcover_p2D,humdataAL$cloudcover) ## SO



### 10.2.2. Test si distribution non normale


### Import des données pour le test kruskal.test() sur données DSKdataAL$cloudcover, humdataAL$cloudcover

## création de deux sous-ensembles "subsets" avec juste les variables à comparer et l'origine de l'éditeur 
### ajout d'une colonne à droite contenant 1 var quali (si val num x$new=10:12) pour indiquer l'origine des données
### si ajout à gauche <cbind(10:12,x)> http://forums.cirad.fr/logiciel-R/viewtopic.php?t=9737
humdataAL$origine="hum"
d1 <- subset(humdataAL, select = c("cloudcover", "origine")) ## création du subset venant de humdataAL http://larmarange.github.io/analyse-R/fusion-de-tables.html
dim(d1)
head(d1, 3)
DSKdataAL$cldcov100=DSKdataAL$cloudcover*100 ## création d'un champ humidité en %
### /!\ ici x100 pour compenser la différence d'unité /!\ 
d2 <- subset(DSKdataAL, select = c("cldcov100", "origine")) ## création du subset venant de DSK
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

## Kruskal-Wallis chi-squared = 140.33, df = 1, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents !

summary(humdataAL$cloudcover)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.00   41.00   65.00   59.43   81.00  100.00     592
summary(DSKdataAL$cldcov100)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.0    48.0    74.0    65.2    88.0   100.0     992


### 10.3. Calcul d'un IC via le test de Wilcoxon sur la médiane (cf. section 3.2 de Poinsot et C:\3VG\MSH\Lyme\Smartick17\data\TCD7\R\IC_foret_AL_CLC5.R)

### Comme distribution non normale

wilcox.test(humdataAL$cloudcover,
            conf.int=TRUE
)$conf.int ## cette ligne pour demander l'IC 

### [1] 61.49996 63.99999
### attr(,"conf.level")
### [1] 0.95

### NO Warning messages:

### Soit un IC à 95% = [61.5 ; 64.0] autour de la /!\ médiane /!\


wilcox.test(DSKdataAL$cldcov100,
            conf.int=TRUE
)$conf.int ## cette ligne pour demander l'IC 

### [1] 68.49993 69.00005
### attr(,"conf.level")
### [1] 0.95

### Soit un IC à 95% = [68.5 ; 69.0] autour de la /!\ médiane /!\




# 11. Analyse des vitesses des rafales DSK vs données humaines (méthode Alice Favre)

## 11.1. Histogramme des vitesses des rafales "windgust" pour les signalements et "windgust" pour DSK pour 700 points darksky

#### Après avoir :

#### vérifié que le nombre d'obs n'est pas égal entre les tableaux 
length(humdataAL$windgust)
length(na.omit(humdataAL$windgust)) ## pour tester si NA
length(DSKdataAL$windgust)
length(na.omit(DSKdataAL$windgust)) ## pour tester si NA

##### et que l'étendue n'est pas la même
range(humdataAL$windgust, na.rm = TRUE)
range(DSKdataAL$windgust, na.rm = TRUE)

### on calcule le nb de lignes des tableaux (pour l'habillage des histogrammes)
nr <- nrow(humdataAL)
nrdsk <- nrow(DSKdataAL)

#### on définit les breaks pour l'abscisse commune
BRt <- seq(from= 0, to= 35, by=1) ## tient compte des deux distributions

#### puis on fait l'histo en utilisant 

### freq=F => des fréquences relatives et pas des effectifs

hist(humdataAL$windgust, breaks = BRt,
     freq=F, # fréquences
     col="grey",
     main = "Average windgust on 2,761 reporting of ticks \n (Alsace-Lorraine, july 2017 - april 2020), 995 days",
     ylab = "Densities",
     xlab = "Average windgust (m/s)"
)

### calcul des paramètres pour la fonction lines() à superposer à l'histo

HH2 <- hist(DSKdataAL$windgust, breaks = BRt,  plot=F)

#lines(HH2$mids, HH2$density, lwd = 2, col = "orange") ### courbe non lissée  ## SO
lines(density(DSKdataAL$windgust, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel

text(15, 0.08, paste("Reporting"), cex = 1.2,  col = "black")
text(20, 0.04, paste("Random"), cex = 1.2,  col = "blue")
# text(8, .08, paste("N =",nr," Reporting" ), col = "black")

## Si on voulait le nb d'enregistrements tudiés à la place du texte "Darksky" ou MF, il faudrait :
### text(03, 0.07, paste("N =", sum(complete.cases(humdataAL$windgust))), cex = 1.2,  col = "black")
### text(02, 0.06, paste("N =", sum(complete.cases(DSKdataAL$windgust))), cex = 1.2,  col = "blue")

## Si on voulait les courbes à +1.5°C ou +2°C, il faudrait fabriquer et afficher :
### lines(density(DSKdataAL$windgust_p15D, na.rm = TRUE), lwd = 2, col = "green") ### courbe lissée, kernel
### lines(density(DSKdataAL$windgust_p2D, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel


### 11.2. Test des distributions

### 11.2.1. Test si distribution normale


## References :
## Patrick Royston (1982). An extension of Shapiro and Wilk's W test for normality to large samples. Applied Statistics, 31, 115–124. doi: 10.2307/2347973.

### Echantillonnage de humdataAL$windgust pour avoir une taille d'échantillon < 5 000

# data[sample(1:nrow(data),n),] # pour faire un sous tableau échantillonné http://forums.cirad.fr/logiciel-r/viewtopic.php?t=6118

shapiro.test(humdataAL[sample(1:nrow(humdataAL),300),"windgust"]) # pour échantillonner une seule variable du data frame
# plus le n augmente, plus la p-value devient significative (si < 5 000 !)

# data:  humdataAL[sample(1:nrow(humdataAL), 30), "windgust"]
# W = 0.92905, p-value = 0.148 => p-value non significative, l'échantillon suit une loi normale (5 coup sur 10)

# data:  humdataAL[sample(1:nrow(humdataAL), 300), "windgust"]
# W = 0.92058, p-value = 3.773e-09 => p-value significative, l'échantillon ne suit pas une loi normale.(diffère, 4 fois sur 4 !).

# shapiro.test(humdataAL$windgust) # uniquement si nrow < 5 000 (pas le cas ici !)

shapiro.test(DSKdataAL[sample(1:nrow(DSKdataAL),300),"windgust"])
# data:  DSKdataAL[sample(1:nrow(DSKdataAL), 30), "windgust"]
# W = 0.92553, p-value = 0.06855 => p-value non significative, l'échantillon suit une loi normale (1 coup sur 2 ou 3 !)

# data:  DSKdataAL[sample(1:nrow(DSKdataAL), 300), "windgust"]
# W = 0.94478, p-value = 1.519e-08 => p-value significative, l'échantillon ne suit pas une loi normale (diffère, , 5 fois sur 5 !).


### t.test entre les différents vitesses des rafales
t.test(DSKdataAL$windgust,humdataAL$windgust) # t = 21.791, df = 1942, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents (mais ne suivent pas une loi normale) !

# t.test(DSKdataAL$windgust_p1D,humdataAL$windgust) ## SO
# t.test(DSKdataAL$windgust_p15D,humdataAL$windgust) ## SO
# t.test(DSKdataAL$windgust_p2D,humdataAL$windgust) ## SO



### 11.2. Test si distribution non normale


### Import des données pour le test kruskal.test() sur données DSKdataAL$windgust, humdataAL$windgust

## création de deux sous-ensembles "subsets" avec juste les variables à comparer et l'origine de l'éditeur 
### ajout d'une colonne à droite contenant 1 var quali (si val num x$new=10:12) pour indiquer l'origine des données
### si ajout à gauche <cbind(10:12,x)> http://forums.cirad.fr/logiciel-R/viewtopic.php?t=9737
humdataAL$origine="hum"
d1 <- subset(humdataAL, select = c("windgust", "origine")) ## création du subset venant de humdataAL http://larmarange.github.io/analyse-R/fusion-de-tables.html
dim(d1)
head(d1, 3)
d2 <- subset(DSKdataAL, select = c("windgust", "origine")) ## création du subset venant de DSK
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

## Kruskal-Wallis chi-squared = 201.79, df = 1, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents !

summary(humdataAL$windgust)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.950   4.300   6.160   6.819   8.560  24.060    1146
summary(DSKdataAL$windgust)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.00    4.67    7.75    8.78   12.10   32.11    8026   


### 11.3. Calcul d'un IC via le test de Wilcoxon sur la médiane (cf. section 3.2 de Poinsot et C:\3VG\MSH\Lyme\Smartick17\data\TCD7\R\IC_foret_AL_CLC5.R)

### Comme distribution non normale

wilcox.test(humdataAL$windgust,
            conf.int=TRUE
)$conf.int ## cette ligne pour demander l'IC 

### [1] 6.295014 6.625049
### attr(,"conf.level")
### [1] 0.95

### NO Warning messages:

### Soit un IC à 95% = [6.3 ; 6.6] autour de la /!\ médiane /!\


wilcox.test(DSKdataAL$windgust,
            conf.int=TRUE
)$conf.int ## cette ligne pour demander l'IC 

### [1] 8.384964 8.495025
### attr(,"conf.level")
### [1] 0.95

### Soit un IC à 95% = [8.4 ; 8.5] autour de la /!\ médiane /!\



# 12. Analyse de l'indice de rayonnement ultra-violet nuageux DSK vs données humaines (méthode Alice Favre)

## 12.1. Histogramme de l'indice de rayonnement ultra-violet nuageux "uvindex" pour les signalements et "uvindex" pour DSK pour 700 points darksky

#### Après avoir :

#### vérifié que le nombre d'obs n'est pas égal entre les tableaux 
length(humdataAL$uvindex)
length(na.omit(humdataAL$uvindex)) ## pour tester si NA
length(DSKdataAL$uvindex)
length(na.omit(DSKdataAL$uvindex)) ## pour tester si NA

##### et que l'étendue n'est pas la même
range(humdataAL$uvindex, na.rm = TRUE)
range(DSKdataAL$uvindex, na.rm = TRUE)

### on calcule le nb de lignes des tableaux (pour l'habillage des histogrammes)
nr <- nrow(humdataAL)
nrdsk <- nrow(DSKdataAL)

#### on définit les breaks pour l'abscisse commune
BRt <- seq(from= 0, to= 10, by=1) ## tient compte des deux distributions

#### puis on fait l'histo en utilisant 

### freq=F => des fréquences relatives et pas des effectifs

hist(humdataAL$uvindex, breaks = BRt,
     freq=F, # fréquences
     col="grey",
     main = "Average uvindex on 2,761 reporting of ticks \n (Alsace-Lorraine, july 2017 - april 2020), 995 days",
     ylab = "Densities",
     xlab = "Average uvindex"
)

### calcul des paramètres pour la fonction lines() à superposer à l'histo


HH2 <- hist(DSKdataAL$uvindex, breaks = BRt,  plot=F)


#lines(HH2$mids, HH2$density, lwd = 2, col = "orange") ### courbe non lissée  ## SO


#lines(density(DSKdataAL$uvindex, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel et création d'un champ humidité en % /!\
#lines(density(DSKdataAL$uvindex, na.rm = TRUE, bw = "nrd"), lwd = 2, col = 3)
#lines(density(DSKdataAL$uvindex, na.rm = TRUE, bw = "ucv"), lwd = 2, col = 4)
lines(density(DSKdataAL$uvindex, na.rm = TRUE, bw = "bcv"), lwd = 2, col = "blue") # bw.bcv(x, nb = 1000, lower = 0.1 * hmax, upper = hmax, tol = 0.1 * lower)
    # see Details @ https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/bandwidth 
    # here => bw.ucv and bw.bcv implement unbiased and biased cross-validation respectively.
#lines(density(DSKdataAL$uvindex, na.rm = TRUE, bw = "SJ-ste"), lwd = 2, col = 6)
#lines(density(DSKdataAL$uvindex, na.rm = TRUE, bw = "SJ-dpi"), lwd = 2, col = 7)


text(8, 0.17, paste("Reporting"), cex = 1.2,  col = "black")
text(1.5, 0.075, paste("Random"), cex = 1.2,  col = "blue")
# text(8, .08, paste("N =",nr," Reporting" ), col = "black")

## Si on voulait le nb d'enregistrements tudiés à la place du texte "Darksky" ou MF, il faudrait :
### text(03, 0.07, paste("N =", sum(complete.cases(humdataAL$uvindex))), cex = 1.2,  col = "black")
### text(02, 0.06, paste("N =", sum(complete.cases(DSKdataAL$uvindex))), cex = 1.2,  col = "blue")

## Si on voulait les courbes à +1.5°C ou +2°C, il faudrait fabriquer et afficher :
### lines(density(DSKdataAL$uvindex_p15D, na.rm = TRUE), lwd = 2, col = "green") ### courbe lissée, kernel
### lines(density(DSKdataAL$uvindex_p2D, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel


### 12.2. Test des distributions

### 12.2.1. Test si distribution normale


## References :
## Patrick Royston (1982). An extension of Shapiro and Wilk's W test for normality to large samples. Applied Statistics, 31, 115–124. doi: 10.2307/2347973.

### Echantillonnage de humdataAL$uvindex pour avoir une taille d'échantillon < 5 000

# data[sample(1:nrow(data),n),] # pour faire un sous tableau échantillonné http://forums.cirad.fr/logiciel-r/viewtopic.php?t=6118

shapiro.test(humdataAL[sample(1:nrow(humdataAL),30),"uvindex"]) # pour échantillonner une seule variable du data frame
# plus le n augmente, plus la p-value devient significative (si < 5 000 !)

# data:  humdataAL[sample(1:nrow(humdataAL), 30), "uvindex"]
# W = 0.90544, p-value = 0.01786 => p-value significative, l'échantillon ne suit pas une loi normale. (4 coup sur 5)

# data:  humdataAL[sample(1:nrow(humdataAL), 300), "uvindex"]
# W = 0.94576, p-value = 7.721e-09 => p-value significative, l'échantillon ne suit pas une loi normale.(diffère, 10 fois sur 10 !).

# shapiro.test(humdataAL$uvindex) # uniquement si nrow < 5 000 (pas le cas ici !)

shapiro.test(DSKdataAL[sample(1:nrow(DSKdataAL),30),"uvindex"])
# data:  DSKdataAL[sample(1:nrow(DSKdataAL), 30), "uvindex"]
# W = W = 0.83849, p-value = 0.0003593 => p-value significative, l'échantillon ne suit pas une loi normale.(diffère, 10 fois sur 10 !).

# data:  DSKdataAL[sample(1:nrow(DSKdataAL), 300), "uvindex"]
# W = 0.89373, p-value = 2.522e-13 => p-value significative, l'échantillon ne suit pas une loi normale (diffère, , 10 fois sur 10 !).

### t.test entre les différentes humidités relatives
t.test(DSKdataAL$uvindex*10,humdataAL$uvindex) # t = 220.94, df = 42214, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents (mais ne suivent pas une loi normale) !

# t.test(DSKdataAL$uvindex_p1D,humdataAL$uvindex) ## SO
# t.test(DSKdataAL$uvindex_p15D,humdataAL$uvindex) ## SO
# t.test(DSKdataAL$uvindex_p2D,humdataAL$uvindex) ## SO



### 12.2.2. Test si distribution non normale


### Import des données pour le test kruskal.test() sur données DSKdataAL$uvindex, humdataAL$uvindex

## création de deux sous-ensembles "subsets" avec juste les variables à comparer et l'origine de l'éditeur 
### ajout d'une colonne à droite contenant 1 var quali (si val num x$new=10:12) pour indiquer l'origine des données
### si ajout à gauche <cbind(10:12,x)> http://forums.cirad.fr/logiciel-R/viewtopic.php?t=9737
humdataAL$origine="hum"
d1 <- subset(humdataAL, select = c("uvindex", "origine")) ## création du subset venant de humdataAL http://larmarange.github.io/analyse-R/fusion-de-tables.html
dim(d1)
head(d1, 3)

d2 <- subset(DSKdataAL, select = c("uvindex", "origine")) ## création du subset venant de DSK
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

## Kruskal-Wallis chi-squared = 2533.3, df = 1, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents !

summary(humdataAL$uvindex)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.000   4.000   6.000   5.495   7.000  10.000     204 
summary(DSKdataAL$uvindex)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.000   1.000   3.000   3.058   5.000   9.000     907  


### 12.3. Calcul d'un IC via le test de Wilcoxon sur la médiane (cf. section 3.2 de Poinsot et C:\3VG\MSH\Lyme\Smartick17\data\TCD7\R\IC_foret_AL_CLC5.R)

### Comme distribution non normale

wilcox.test(humdataAL$uvindex,
            conf.int=TRUE
)$conf.int ## cette ligne pour demander l'IC 

### [1] 5.500017 5.999976
### attr(,"conf.level")
### [1] 0.95

### NO Warning messages:

### Soit un IC à 95% = [5.5 ; 6.0] autour de la /!\ médiane /!\


wilcox.test(DSKdataAL$uvindex,
            conf.int=TRUE
)$conf.int ## cette ligne pour demander l'IC 

### [1] 3.000037 3.000026
### attr(,"conf.level")
### [1] 0.95

### Soit un IC à 95% = [3.000037 ; 3.000026] autour de la /!\ médiane /!\

