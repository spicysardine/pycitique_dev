######
###### Comparaison des signalements et des données Météo DSK (non plus moyennées mais DSK brutes) Île-de-France vs Alsace-Lorraine

###### Pour les 10 variables météorologiques disponibles

###### Réalisation Vincent GODARD <vgodard@univ-paris8.fr>

###### Remerciements à Jude Peterson CIRIUS <j.cirius@yahoo.com> pour sa contribution initiale dans les boucles



# 1. Introduction
### Alloue la librairie "set()" puis vérifie que c'est bien importé "getwd()", il pointe dessus!

## 1.1 XPS13
setwd("C:/3VG/MSH/lyme/Smartick17/pycitique_miror/R")
getwd()

## 1.2 OP7570
# setwd("D:/3VG/MSH/lyme/Smartick17/pycitique_miror/R")
# getwd()

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


### Création des subset pour l'AL

humdataAL <- subset(humdata, humdata$departement_code == "54" |  humdata$departement_code == "55" |  humdata$departement_code == "57" |  humdata$departement_code == "88" |  humdata$departement_code == "67" | humdata$departement_code == "68")

DSKdataAL <- subset(DSKdata, DSKdata$departement_code == "54" |  DSKdata$departement_code == "55" |  DSKdata$departement_code == "57" |  DSKdata$departement_code == "88" |  DSKdata$departement_code == "67" | DSKdata$departement_code == "68")




## 2.1 Analyse du tableau de données

# ls(humdataIDF) ## liste les variables
# ls(DSKdataIDF)

# ls(humdataAL) ## liste les variables
# ls(DSKdataAL)



# 3. Construction des tableaux par Boostrap

## 3.1. Variable : temperature

### 3.1.1 Construction des data frames

table351 <- humdataIDF$temperature
table352 <- DSKdataIDF$temperature
table353 <- humdataAL$temperature
table354 <- DSKdataAL$temperature


c1 <- c(0:999)*0 ## création d'une table remplie de 0
## ** pour initialiser les dimensions et la valeurs des cellules du dataframe qui suit


temperatureDF <- data.frame("humdataIDF"=c1,"DSKdataIDF"=c1,"humdataAL"=c1,"DSKdataAL"=c1) ## ** création d'un DF 
## initialisé avec les valeurs de c1 pour les cellules et le nombre de lignes.  
  

## boucle qui calcule 1000 fois sur un échantillon de 50 tirages des moyennes à partir du vecteur tab35*

## /!\ il faut exclure les NA du calcul de la moyenne /!\


for (i in 1:1000){
    temperatureDF$humdataIDF[i]          <- mean(sample(table351,50,replace=T), na.rm = T);
    temperatureDF$DSKdataIDF[i]          <- mean(sample(table352,50,replace=T), na.rm = T);
    temperatureDF$humdataAL[i]          <- mean(sample(table353,50,replace=T), na.rm = T);
    temperatureDF$DSKdataAL[i]          <- mean(sample(table354,50,replace=T), na.rm = T);
}

### 3.1.2 Construction des tests

## Calcul des tests de comparaison entre les régions
## https://perso.univ-rennes1.fr/denis.poinsot/Statistiques_%20pour_statophobes/R%20pour%20les%20statophobes.pdf

aa <- wilcox.test(table351 , table353) ## comparaison des médianes avec p-value < 2.2e-16 donc différences significatives

ab <- wilcox.test(table352 , table354) ## comparaison des médianes avec p-value < 2.2e-16 donc différences significatives

aa$statistic
aa$p.value
ab$statistic
ab$p.value


### 3.1.3 Stockage des résultats

## stockage des résultats des tests sous forme de textes dans "idf_vs_AL.txt"

sink("idf_vs_AL.txt", append=TRUE, split =FALSE) # écrit le résultat à la suite d ceux déjà dans "idf_vs_AL.txt"
print("temperature") # imprime le nom de la variable avant le résultat du test
print(aa)
print(ab) # imprime le résultat du test dans le fichier "idf_vs_AL.txt"
sink() 


## stockage des résultats des tests sous forme de tableaux dans "idf_vs_AL.csv"

output_Fr=data.frame(aa$statistic, aa$p.value, ab$statistic, ab$p.value) ## création des 4 champs +1 (avec W !)
output_Fr ## contrôle par affichage du résultat

write.table(output_Fr,"idf_vs_AL.csv", append = TRUE, sep=";", row.names = TRUE)


## 3.2. Variable : temperaturehigh

### 3.2.1 Construction des data frames

table351 <- humdataIDF$temperaturehigh
table352 <- DSKdataIDF$temperaturehigh
table353 <- humdataAL$temperaturehigh
table354 <- DSKdataAL$temperaturehigh


c1 <- c(0:999)*0 ## création d'une table remplie de 0
## ** pour initialiser les dimensions et la valeurs des cellules du dataframe qui suit


temperaturehighDF <- data.frame("humdataIDF"=c1,"DSKdataIDF"=c1,"humdataAL"=c1,"DSKdataAL"=c1) ## ** création d'un DF 
## initialisé avec les valeurs de c1 pour les cellules et le nombre de lignes.  


## boucle qui calcule 1000 fois sur un échantillon de 50 tirages des moyennes à partir du vecteur tab35*

## /!\ il faut exclure les NA du calcul de la moyenne /!\


for (i in 1:1000){
    temperaturehighDF$humdataIDF[i]          <- mean(sample(table351,50,replace=T), na.rm = T);
    temperaturehighDF$DSKdataIDF[i]          <- mean(sample(table352,50,replace=T), na.rm = T);
    temperaturehighDF$humdataAL[i]          <- mean(sample(table353,50,replace=T), na.rm = T);
    temperaturehighDF$DSKdataAL[i]          <- mean(sample(table354,50,replace=T), na.rm = T);
}

### 3.2.2 Construction des tests

## Calcul des tests de comparaison entre les régions
## https://perso.univ-rennes1.fr/denis.poinsot/Statistiques_%20pour_statophobes/R%20pour%20les%20statophobes.pdf

aa <- wilcox.test(table351 , table353) ## comparaison des médianes avec p-value < 2.2e-16 donc différences significatives

ab <- wilcox.test(table352 , table354) ## comparaison des médianes avec p-value < 2.2e-16 donc différences significatives

aa$statistic
aa$p.value
ab$statistic
ab$p.value


### 3.2.3 Stockage des résultats

## stockage des résultats des tests sous forme de textes dans "idf_vs_AL.txt"

sink("idf_vs_AL.txt", append=TRUE, split =FALSE) # écrit le résultat à la suite d ceux déjà dans "idf_vs_AL.txt"
print("temperaturehigh") # imprime le nom de la variable avant le résultat du test
print(aa)
print(ab) # imprime le résultat du test dans le fichier "idf_vs_AL.txt"
sink() 


## stockage des résultats des tests sous forme de tableaux dans "idf_vs_AL.csv"

output_Fr=data.frame(aa$statistic, aa$p.value, ab$statistic, ab$p.value) ## création des 4 champs +1 (avec W !)
output_Fr ## contrôle par affichage du résultat

write.table(output_Fr,"idf_vs_AL.csv", append = TRUE, sep=";", row.names = TRUE)


## 3.3. Variable : humidity

### 3.3.1 Construction des data frames

table351 <- humdataIDF$humidity
table352 <- DSKdataIDF$humidity*100 ## DSK données entre 0 et 1 et pas en %
table353 <- humdataAL$humidity
table354 <- DSKdataAL$humidity*100 ## DSK données entre 0 et 1 et pas en %


c1 <- c(0:999)*0 ## création d'une table remplie de 0
## ** pour initialiser les dimensions et la valeurs des cellules du dataframe qui suit


humidityDF <- data.frame("humdataIDF"=c1,"DSKdataIDF"=c1,"humdataAL"=c1,"DSKdataAL"=c1) ## ** création d'un DF 
## initialisé avec les valeurs de c1 pour les cellules et le nombre de lignes.  


## boucle qui calcule 1000 fois sur un échantillon de 50 tirages des moyennes à partir du vecteur tab35*

## /!\ il faut exclure les NA du calcul de la moyenne /!\


for (i in 1:1000){
  humidityDF$humdataIDF[i]          <- mean(sample(table351,50,replace=T), na.rm = T);
  humidityDF$DSKdataIDF[i]          <- mean(sample(table352,50,replace=T), na.rm = T);
  humidityDF$humdataAL[i]          <- mean(sample(table353,50,replace=T), na.rm = T);
  humidityDF$DSKdataAL[i]          <- mean(sample(table354,50,replace=T), na.rm = T);
}

### 3.3.2 Construction des tests

## Calcul des tests de comparaison entre les régions
## https://perso.univ-rennes1.fr/denis.poinsot/Statistiques_%20pour_statophobes/R%20pour%20les%20statophobes.pdf

aa <- wilcox.test(table351 , table353) ## comparaison des médianes avec p-value < 2.2e-16 donc différences significatives

ab <- wilcox.test(table352 , table354) ## comparaison des médianes avec p-value < 2.2e-16 donc différences significatives

aa$statistic
aa$p.value
ab$statistic
ab$p.value


### 3.3.3 Stockage des résultats

## stockage des résultats des tests sous forme de textes dans "idf_vs_AL.txt"

sink("idf_vs_AL.txt", append=TRUE, split =FALSE) # écrit le résultat à la suite d ceux déjà dans "idf_vs_AL.txt"
print("humidity") # imprime le nom de la variable avant le résultat du test
print(aa)
print(ab) # imprime le résultat du test dans le fichier "idf_vs_AL.txt"
sink() 


## stockage des résultats des tests sous forme de tableaux dans "idf_vs_AL.csv"

output_Fr=data.frame(aa$statistic, aa$p.value, ab$statistic, ab$p.value) ## création des 4 champs +1 (avec W !)
output_Fr ## contrôle par affichage du résultat

write.table(output_Fr,"idf_vs_AL.csv", append = TRUE, sep=";", row.names = TRUE)


## 3.4. Variable : dewpoint

### 3.4.1 Construction des data frames

table351 <- humdataIDF$dewpoint
table352 <- DSKdataIDF$dewpoint
table353 <- humdataAL$dewpoint
table354 <- DSKdataAL$dewpoint


c1 <- c(0:999)*0 ## création d'une table remplie de 0
## ** pour initialiser les dimensions et la valeurs des cellules du dataframe qui suit


dewpointDF <- data.frame("humdataIDF"=c1,"DSKdataIDF"=c1,"humdataAL"=c1,"DSKdataAL"=c1) ## ** création d'un DF 
## initialisé avec les valeurs de c1 pour les cellules et le nombre de lignes.  


## boucle qui calcule 1000 fois sur un échantillon de 50 tirages des moyennes à partir du vecteur tab35*

## /!\ il faut exclure les NA du calcul de la moyenne /!\


for (i in 1:1000){
  dewpointDF$humdataIDF[i]          <- mean(sample(table351,50,replace=T), na.rm = T);
  dewpointDF$DSKdataIDF[i]          <- mean(sample(table352,50,replace=T), na.rm = T);
  dewpointDF$humdataAL[i]          <- mean(sample(table353,50,replace=T), na.rm = T);
  dewpointDF$DSKdataAL[i]          <- mean(sample(table354,50,replace=T), na.rm = T);
}

### 3.4.2 Construction des tests

## Calcul des tests de comparaison entre les régions
## https://perso.univ-rennes1.fr/denis.poinsot/Statistiques_%20pour_statophobes/R%20pour%20les%20statophobes.pdf

aa <- wilcox.test(table351 , table353) ## comparaison des médianes avec p-value < 2.2e-16 donc différences significatives

ab <- wilcox.test(table352 , table354) ## comparaison des médianes avec p-value < 2.2e-16 donc différences significatives

aa$statistic
aa$p.value
ab$statistic
ab$p.value


### 3.4.3 Stockage des résultats

## stockage des résultats des tests sous forme de textes dans "idf_vs_AL.txt"

sink("idf_vs_AL.txt", append=TRUE, split =FALSE) # écrit le résultat à la suite d ceux déjà dans "idf_vs_AL.txt"
print("dewpoint") # imprime le nom de la variable avant le résultat du test
print(aa)
print(ab) # imprime le résultat du test dans le fichier "idf_vs_AL.txt"
sink() 


## stockage des résultats des tests sous forme de tableaux dans "idf_vs_AL.csv"

output_Fr=data.frame(aa$statistic, aa$p.value, ab$statistic, ab$p.value) ## création des 4 champs +1 (avec W !)
output_Fr ## contrôle par affichage du résultat

write.table(output_Fr,"idf_vs_AL.csv", append = TRUE, sep=";", row.names = TRUE)


## 3.5. Variable : pressure

### 3.5.1 Construction des data frames

table351 <- humdataIDF$pressure
table352 <- DSKdataIDF$pressure
table353 <- humdataAL$pressure
table354 <- DSKdataAL$pressure


c1 <- c(0:999)*0 ## création d'une table remplie de 0
## ** pour initialiser les dimensions et la valeurs des cellules du dataframe qui suit


pressureDF <- data.frame("humdataIDF"=c1,"DSKdataIDF"=c1,"humdataAL"=c1,"DSKdataAL"=c1) ## ** création d'un DF 
## initialisé avec les valeurs de c1 pour les cellules et le nombre de lignes.  


## boucle qui calcule 1000 fois sur un échantillon de 50 tirages des moyennes à partir du vecteur tab35*

## /!\ il faut exclure les NA du calcul de la moyenne /!\


for (i in 1:1000){
  pressureDF$humdataIDF[i]          <- mean(sample(table351,50,replace=T), na.rm = T);
  pressureDF$DSKdataIDF[i]          <- mean(sample(table352,50,replace=T), na.rm = T);
  pressureDF$humdataAL[i]          <- mean(sample(table353,50,replace=T), na.rm = T);
  pressureDF$DSKdataAL[i]          <- mean(sample(table354,50,replace=T), na.rm = T);
}

### 3.5.2 Construction des tests

## Calcul des tests de comparaison entre les régions
## https://perso.univ-rennes1.fr/denis.poinsot/Statistiques_%20pour_statophobes/R%20pour%20les%20statophobes.pdf

aa <- wilcox.test(table351 , table353) ## comparaison des médianes avec p-value < 2.2e-16 donc différences significatives

ab <- wilcox.test(table352 , table354) ## comparaison des médianes avec p-value < 2.2e-16 donc différences significatives

aa$statistic
aa$p.value
ab$statistic
ab$p.value


### 3.5.3 Stockage des résultats

## stockage des résultats des tests sous forme de textes dans "idf_vs_AL.txt"

sink("idf_vs_AL.txt", append=TRUE, split =FALSE) # écrit le résultat à la suite d ceux déjà dans "idf_vs_AL.txt"
print("pressure") # imprime le nom de la variable avant le résultat du test
print(aa)
print(ab) # imprime le résultat du test dans le fichier "idf_vs_AL.txt"
sink() 


## stockage des résultats des tests sous forme de tableaux dans "idf_vs_AL.csv"

output_Fr=data.frame(aa$statistic, aa$p.value, ab$statistic, ab$p.value) ## création des 4 champs +1 (avec W !)
output_Fr ## contrôle par affichage du résultat

write.table(output_Fr,"idf_vs_AL.csv", append = TRUE, sep=";", row.names = TRUE)


## 3.6. Variable : windspeed

### 3.6.1 Construction des data frames

table351 <- humdataIDF$windspeed
table352 <- DSKdataIDF$windspeed
table353 <- humdataAL$windspeed
table354 <- DSKdataAL$windspeed


c1 <- c(0:999)*0 ## création d'une table remplie de 0
## ** pour initialiser les dimensions et la valeurs des cellules du dataframe qui suit


windspeedDF <- data.frame("humdataIDF"=c1,"DSKdataIDF"=c1,"humdataAL"=c1,"DSKdataAL"=c1) ## ** création d'un DF 
## initialisé avec les valeurs de c1 pour les cellules et le nombre de lignes.  


## boucle qui calcule 1000 fois sur un échantillon de 50 tirages des moyennes à partir du vecteur tab35*

## /!\ il faut exclure les NA du calcul de la moyenne /!\


for (i in 1:1000){
  windspeedDF$humdataIDF[i]          <- mean(sample(table351,50,replace=T), na.rm = T);
  windspeedDF$DSKdataIDF[i]          <- mean(sample(table352,50,replace=T), na.rm = T);
  windspeedDF$humdataAL[i]          <- mean(sample(table353,50,replace=T), na.rm = T);
  windspeedDF$DSKdataAL[i]          <- mean(sample(table354,50,replace=T), na.rm = T);
}

### 3.6.2 Construction des tests

## Calcul des tests de comparaison entre les régions
## https://perso.univ-rennes1.fr/denis.poinsot/Statistiques_%20pour_statophobes/R%20pour%20les%20statophobes.pdf

aa <- wilcox.test(table351 , table353) ## comparaison des médianes avec p-value < 2.2e-16 donc différences significatives

ab <- wilcox.test(table352 , table354) ## comparaison des médianes avec p-value < 2.2e-16 donc différences significatives

aa$statistic
aa$p.value
ab$statistic
ab$p.value


### 3.6.3 Stockage des résultats

## stockage des résultats des tests sous forme de textes dans "idf_vs_AL.txt"

sink("idf_vs_AL.txt", append=TRUE, split =FALSE) # écrit le résultat à la suite d ceux déjà dans "idf_vs_AL.txt"
print("windspeed") # imprime le nom de la variable avant le résultat du test
print(aa)
print(ab) # imprime le résultat du test dans le fichier "idf_vs_AL.txt"
sink() 


## stockage des résultats des tests sous forme de tableaux dans "idf_vs_AL.csv"

output_Fr=data.frame(aa$statistic, aa$p.value, ab$statistic, ab$p.value) ## création des 4 champs +1 (avec W !)
output_Fr ## contrôle par affichage du résultat

write.table(output_Fr,"idf_vs_AL.csv", append = TRUE, sep=";", row.names = TRUE)


## 3.7. Variable : visibility

### 3.7.1 Construction des data frames

table351 <- humdataIDF$visibility
table352 <- DSKdataIDF$visibility
table353 <- humdataAL$visibility
table354 <- DSKdataAL$visibility


c1 <- c(0:999)*0 ## création d'une table remplie de 0
## ** pour initialiser les dimensions et la valeurs des cellules du dataframe qui suit


visibilityDF <- data.frame("humdataIDF"=c1,"DSKdataIDF"=c1,"humdataAL"=c1,"DSKdataAL"=c1) ## ** création d'un DF 
## initialisé avec les valeurs de c1 pour les cellules et le nombre de lignes.  


## boucle qui calcule 1000 fois sur un échantillon de 50 tirages des moyennes à partir du vecteur tab35*

## /!\ il faut exclure les NA du calcul de la moyenne /!\


for (i in 1:1000){
  visibilityDF$humdataIDF[i]          <- mean(sample(table351,50,replace=T), na.rm = T);
  visibilityDF$DSKdataIDF[i]          <- mean(sample(table352,50,replace=T), na.rm = T);
  visibilityDF$humdataAL[i]          <- mean(sample(table353,50,replace=T), na.rm = T);
  visibilityDF$DSKdataAL[i]          <- mean(sample(table354,50,replace=T), na.rm = T);
}

### 3.7.2 Construction des tests

## Calcul des tests de comparaison entre les régions
## https://perso.univ-rennes1.fr/denis.poinsot/Statistiques_%20pour_statophobes/R%20pour%20les%20statophobes.pdf

aa <- wilcox.test(table351 , table353) ## comparaison des médianes avec p-value < 2.2e-16 donc différences significatives

ab <- wilcox.test(table352 , table354) ## comparaison des médianes avec p-value < 2.2e-16 donc différences significatives

aa$statistic
aa$p.value
ab$statistic
ab$p.value


### 3.7.3 Stockage des résultats

## stockage des résultats des tests sous forme de textes dans "idf_vs_AL.txt"

sink("idf_vs_AL.txt", append=TRUE, split =FALSE) # écrit le résultat à la suite d ceux déjà dans "idf_vs_AL.txt"
print("visibility") # imprime le nom de la variable avant le résultat du test
print(aa)
print(ab) # imprime le résultat du test dans le fichier "idf_vs_AL.txt"
sink() 


## stockage des résultats des tests sous forme de tableaux dans "idf_vs_AL.csv"

output_Fr=data.frame(aa$statistic, aa$p.value, ab$statistic, ab$p.value) ## création des 4 champs +1 (avec W !)
output_Fr ## contrôle par affichage du résultat

write.table(output_Fr,"idf_vs_AL.csv", append = TRUE, sep=";", row.names = TRUE)


## 3.8. Variable : cloudcover

### 3.8.1 Construction des data frames

table351 <- humdataIDF$cloudcover
table352 <- DSKdataIDF$cloudcover*100 ## DSK données entre 0 et 1 et pas en %
table353 <- humdataAL$cloudcover
table354 <- DSKdataAL$cloudcover*100 ## DSK données entre 0 et 1 et pas en %


c1 <- c(0:999)*0 ## création d'une table remplie de 0
## ** pour initialiser les dimensions et la valeurs des cellules du dataframe qui suit


cloudcoverDF <- data.frame("humdataIDF"=c1,"DSKdataIDF"=c1,"humdataAL"=c1,"DSKdataAL"=c1) ## ** création d'un DF 
## initialisé avec les valeurs de c1 pour les cellules et le nombre de lignes.  


## boucle qui calcule 1000 fois sur un échantillon de 50 tirages des moyennes à partir du vecteur tab35*

## /!\ il faut exclure les NA du calcul de la moyenne /!\


for (i in 1:1000){
  cloudcoverDF$humdataIDF[i]          <- mean(sample(table351,50,replace=T), na.rm = T);
  cloudcoverDF$DSKdataIDF[i]          <- mean(sample(table352,50,replace=T), na.rm = T);
  cloudcoverDF$humdataAL[i]          <- mean(sample(table353,50,replace=T), na.rm = T);
  cloudcoverDF$DSKdataAL[i]          <- mean(sample(table354,50,replace=T), na.rm = T);
}

### 3.8.2 Construction des tests

## Calcul des tests de comparaison entre les régions
## https://perso.univ-rennes1.fr/denis.poinsot/Statistiques_%20pour_statophobes/R%20pour%20les%20statophobes.pdf

aa <- wilcox.test(table351 , table353) ## comparaison des médianes avec p-value < 2.2e-16 donc différences significatives

ab <- wilcox.test(table352 , table354) ## comparaison des médianes avec p-value < 2.2e-16 donc différences significatives

aa$statistic
aa$p.value
ab$statistic
ab$p.value


### 3.8.3 Stockage des résultats

## stockage des résultats des tests sous forme de textes dans "idf_vs_AL.txt"

sink("idf_vs_AL.txt", append=TRUE, split =FALSE) # écrit le résultat à la suite d ceux déjà dans "idf_vs_AL.txt"
print("cloudcover") # imprime le nom de la variable avant le résultat du test
print(aa)
print(ab) # imprime le résultat du test dans le fichier "idf_vs_AL.txt"
sink() 


## stockage des résultats des tests sous forme de tableaux dans "idf_vs_AL.csv"

output_Fr=data.frame(aa$statistic, aa$p.value, ab$statistic, ab$p.value) ## création des 4 champs +1 (avec W !)
output_Fr ## contrôle par affichage du résultat

write.table(output_Fr,"idf_vs_AL.csv", append = TRUE, sep=";", row.names = TRUE)


## 3.9. Variable : windgust

### 3.9.1 Construction des data frames

table351 <- humdataIDF$windgust
table352 <- DSKdataIDF$windgust
table353 <- humdataAL$windgust
table354 <- DSKdataAL$windgust


c1 <- c(0:999)*0 ## création d'une table remplie de 0
## ** pour initialiser les dimensions et la valeurs des cellules du dataframe qui suit


windgustDF <- data.frame("humdataIDF"=c1,"DSKdataIDF"=c1,"humdataAL"=c1,"DSKdataAL"=c1) ## ** création d'un DF 
## initialisé avec les valeurs de c1 pour les cellules et le nombre de lignes.  


## boucle qui calcule 1000 fois sur un échantillon de 50 tirages des moyennes à partir du vecteur tab35*

## /!\ il faut exclure les NA du calcul de la moyenne /!\


for (i in 1:1000){
  windgustDF$humdataIDF[i]          <- mean(sample(table351,50,replace=T), na.rm = T);
  windgustDF$DSKdataIDF[i]          <- mean(sample(table352,50,replace=T), na.rm = T);
  windgustDF$humdataAL[i]          <- mean(sample(table353,50,replace=T), na.rm = T);
  windgustDF$DSKdataAL[i]          <- mean(sample(table354,50,replace=T), na.rm = T);
}

### 3.9.2 Construction des tests

## Calcul des tests de comparaison entre les régions
## https://perso.univ-rennes1.fr/denis.poinsot/Statistiques_%20pour_statophobes/R%20pour%20les%20statophobes.pdf

aa <- wilcox.test(table351 , table353) ## comparaison des médianes avec p-value < 2.2e-16 donc différences significatives

ab <- wilcox.test(table352 , table354) ## comparaison des médianes avec p-value < 2.2e-16 donc différences significatives

aa$statistic
aa$p.value
ab$statistic
ab$p.value


### 3.9.3 Stockage des résultats

## stockage des résultats des tests sous forme de textes dans "idf_vs_AL.txt"

sink("idf_vs_AL.txt", append=TRUE, split =FALSE) # écrit le résultat à la suite d ceux déjà dans "idf_vs_AL.txt"
print("windgust") # imprime le nom de la variable avant le résultat du test
print(aa)
print(ab) # imprime le résultat du test dans le fichier "idf_vs_AL.txt"
sink() 


## stockage des résultats des tests sous forme de tableaux dans "idf_vs_AL.csv"

output_Fr=data.frame(aa$statistic, aa$p.value, ab$statistic, ab$p.value) ## création des 4 champs +1 (avec W !)
output_Fr ## contrôle par affichage du résultat

write.table(output_Fr,"idf_vs_AL.csv", append = TRUE, sep=";", row.names = TRUE)


## 3.10. Variable : uvindex

### 3.10.1 Construction des data frames

table351 <- humdataIDF$uvindex
table352 <- DSKdataIDF$uvindex
table353 <- humdataAL$uvindex
table354 <- DSKdataAL$uvindex


c1 <- c(0:999)*0 ## création d'une table remplie de 0
## ** pour initialiser les dimensions et la valeurs des cellules du dataframe qui suit


uvindexDF <- data.frame("humdataIDF"=c1,"DSKdataIDF"=c1,"humdataAL"=c1,"DSKdataAL"=c1) ## ** création d'un DF 
## initialisé avec les valeurs de c1 pour les cellules et le nombre de lignes.  


## boucle qui calcule 1000 fois sur un échantillon de 50 tirages des moyennes à partir du vecteur tab35*

## /!\ il faut exclure les NA du calcul de la moyenne /!\


for (i in 1:1000){
  uvindexDF$humdataIDF[i]          <- mean(sample(table351,50,replace=T), na.rm = T);
  uvindexDF$DSKdataIDF[i]          <- mean(sample(table352,50,replace=T), na.rm = T);
  uvindexDF$humdataAL[i]          <- mean(sample(table353,50,replace=T), na.rm = T);
  uvindexDF$DSKdataAL[i]          <- mean(sample(table354,50,replace=T), na.rm = T);
}

### 3.10.2 Construction des tests

## Calcul des tests de comparaison entre les régions
## https://perso.univ-rennes1.fr/denis.poinsot/Statistiques_%20pour_statophobes/R%20pour%20les%20statophobes.pdf

aa <- wilcox.test(table351 , table353) ## comparaison des médianes avec p-value < 2.2e-16 donc différences significatives

ab <- wilcox.test(table352 , table354) ## comparaison des médianes avec p-value < 2.2e-16 donc différences significatives

aa$statistic
aa$p.value
ab$statistic
ab$p.value


### 3.10.3 Stockage des résultats

## stockage des résultats des tests sous forme de textes dans "idf_vs_AL.txt"

sink("idf_vs_AL.txt", append=TRUE, split =FALSE) # écrit le résultat à la suite d ceux déjà dans "idf_vs_AL.txt"
print("uvindex") # imprime le nom de la variable avant le résultat du test
print(aa)
print(ab) # imprime le résultat du test dans le fichier "idf_vs_AL.txt"
sink() 


## stockage des résultats des tests sous forme de tableaux dans "idf_vs_AL.csv"

output_Fr=data.frame(aa$statistic, aa$p.value, ab$statistic, ab$p.value) ## création des 4 champs +1 (avec W !)
output_Fr ## contrôle par affichage du résultat

write.table(output_Fr,"idf_vs_AL.csv", append = TRUE, sep=";", row.names = TRUE)




