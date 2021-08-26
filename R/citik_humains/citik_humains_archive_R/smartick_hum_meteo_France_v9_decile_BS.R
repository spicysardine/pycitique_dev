###### Code figure n°7 – Profils météorologiques associés aux 14 657 lieux et dates de signalements comparés à ceux des mêmes dates
###### mais pour un semis de lieux aléatoires (France, July 15th 2017 – April 5th 2020, soit 995 jours).
###### Code Tableau n°5 – En Île-de-France, selon le 1er décile, la moyenne et le 9ème décile :
###### Paramètres météorologiques associés aux 14 657 lieux et dates de signalements comparés à ceux des mêmes dates
###### mais pour un semis de lieux aléatoires (France, July 2017 – April 2020, soit 995 jours).
###### Code Tableau n°6 – En Alsace-Lorraine, selon le 1er décile, la moyenne et le 9ème décile :
###### Paramètres météorologiques associés aux 14 657 lieux et dates de signalements comparés à ceux des mêmes dates
###### mais pour un semis de lieux aléatoires (France, July 2017 – April 2020, soit 995 jours).
###### Code Tableau n°7 – En Rhône-Alpes, selon le 1er décile, la moyenne et le 9ème décile :
###### Paramètres météorologiques associés aux 14 657 lieux et dates de signalements comparés à ceux des mêmes dates
###### mais pour un semis de lieux aléatoires (France, January 2017 – April 2020, soit 995 jours).





###### Analyse des signalements et des données Météo DSK (non plus moyennées mais DSK brutes) pour la France entière

###### D1, Moy, D9 et leurs IC

###### Réalisation Vincent GODARD <vgodard@univ-paris8.fr>

###### Remerciements à Jude Peterson CIRIUS <j.cirius@yahoo.com> pour sa contribution décisive dans les boucles


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

# 2. Importation de données citik_humains_clean_weather_strict.csv et darksky_donnee_brute_700_def3.csv

### Or, if .csv file, use this si données séparées par ","
### quote = "'" pour le caractère d’échappement
### stringAsFactpors pour voir les chaînes de caractères sans interprétation.

## DataHum <- read.csv('D:\\MediaCours\\AM2Cours\\SIG_analyse\\analysePT\\PrTutoreVG\\humdata.csv',sep='\t') version Jude


## 2.0 Création des subsets

humdata <- read.csv("../../pycitique/data/donnee_signalements_avec_meteo_dsk/citik_humains_clean_weather_strict.csv", sep = ",", dec = ".", quote = "'", stringsAsFactors = FALSE)

DSKdata <- read.csv("../../pycitique/data/donnee_meteo_nationale_comparative/darksky/darksky_donnee_brute_700_def3.csv", sep = ",", dec = ".", quote = "'", stringsAsFactors = FALSE)


## /!\ Libérer la région à traiter  /!\


### France entière

# humdata <- humdata

# DSKdata <- DSKdata

### Création des subset pour l'IDF


# humdata <- subset(humdata, humdata$departement_code == "75" |  humdata$departement_code == "77" |  humdata$departement_code == "78" |  humdata$departement_code == "91" |  humdata$departement_code == "92" | humdata$departement_code == "93" |  humdata$departement_code == "94" | humdata$departement_code == "95")

# DSKdata <- subset(DSKdata, DSKdata$departement_code == "75" |  DSKdata$departement_code == "77" |  DSKdata$departement_code == "78" |  DSKdata$departement_code == "91" |  DSKdata$departement_code == "92" | DSKdata$departement_code == "93" |  DSKdata$departement_code == "94" | DSKdata$departement_code == "95")


### Création des subset pour l'AL

# humdata <- subset(humdata, humdata$departement_code == "54" |  humdata$departement_code == "55" |  humdata$departement_code == "57" |  humdata$departement_code == "88" |  humdata$departement_code == "67" | humdata$departement_code == "68")

# DSKdata <- subset(DSKdata, DSKdata$departement_code == "54" |  DSKdata$departement_code == "55" |  DSKdata$departement_code == "57" |  DSKdata$departement_code == "88" |  DSKdata$departement_code == "67" | DSKdata$departement_code == "68")


### Création des subset pour RA

# humdata <- subset(humdata, humdata$departement_code == "01" |  humdata$departement_code == "07" |  humdata$departement_code == "26" |  humdata$departement_code == "38" |  humdata$departement_code == "42" | humdata$departement_code == "69" | humdata$departement_code == "73" | humdata$departement_code == "74")

# DSKdata <- subset(DSKdata, DSKdata$departement_code == "01" |  DSKdata$departement_code == "07" |  DSKdata$departement_code == "26" |  DSKdata$departement_code == "38" |  DSKdata$departement_code == "42" | DSKdata$departement_code == "69" | DSKdata$departement_code == "73" | DSKdata$departement_code == "74")




## 2.1 Analyse du tableau de données

## ls(humdata) ## liste les variables
## ls(DSKdata)




# 3. Boostrap pour stabiliser indicateurs et intervalles de confiance du t.test d'une moyenne (médiane/quartile... voir "IC_foret_AL_CLC5.R" par exemple et Poinsot, 2005, R pour les statophobes, pp.13-1510)

### Calcul d'un IC via en utilisant le Bootstrap pour des petits échantillons ou une absence de normalité avérée (cf. section 3.4 de Poinsot, p.14)

### création d'un vecteur (tableau à une seule rangée) nommé table devant accueillir 1000 valeurs numériques => les 1000 valeurs de moyennes obtenues.


## 3.1 temperature

### 3.1.1 humdata$

var <-humdata$temperature ## création d'un vecteur générique (var) pour un remplacement plus aisé

c1 <- c(0:999)*0 ## création d'une table remplie de 0
## ** pour initialiser les dimensions et la valeurs des cellules du dataframe qui suit


humDF=data.frame("decile_1"=c1,"Moy"=c1,"decile_9"=c1) ## **création d'un DF initialisé avec les valeurs de c1 pour les cellules et le nombre de lignes.
## humDF

## boucle qui calcule 1000 fois sur un échantillon de 50 tirages des moyennes ou les déciles un et neuf à partir du vecteur var

## /!\ il faut exclure les NA du calcul de la moyenne également /!\
## /!\ j'ai renommé le decile_3 en decile_9 /!\

for (i in 1:1000){
    humDF$decile_1[i]     <- quantile(sample(var,50,replace=T),0.1, na.rm = T);
    humDF$Moy[i]          <- mean(sample(var,50,replace=T), na.rm = T);
    humDF$decile_9[i]     <- quantile(sample(var,50,replace=T),0.9, na.rm = T);
}

aa <- mean(humDF$decile_1) ##♥ moyenne des 1000 1er déciles
ba <- mean(humDF$Moy) ##♥ moyenne des 1000 moyennes
ca <- mean(humDF$decile_9) ##♥ moyenne des 1000 9ème déciles


humDF_D1.sort=sort(humDF$decile_1)
humDF_Moy.sort=sort(humDF$Moy)
humDF_D9.sort=sort(humDF$decile_9)


length_ss_NA_humDF_D1.sort <- length(na.omit(humDF_D1.sort))
length_ss_NA_humDF_Moy.sort <- length(na.omit(humDF_Moy.sort))
length_ss_NA_humDF_D9.sort <- length(na.omit(humDF_D9.sort))

length_ss_NA_humDF_D1.sort
length_ss_NA_humDF_Moy.sort
length_ss_NA_humDF_D9.sort


## c(humDF_D1.sort[25], humDF_D1.sort[975])
ab <- humDF_D1.sort[25]
ac <- humDF_D1.sort[975]

## c(humDF_Moy.sort[25], humDF_Moy.sort[975])
bb <- humDF_Moy.sort[25]
bc <- humDF_Moy.sort[975]

## c(humDF_D1.sort[25], humDF_D1.sort[975])
cb <- humDF_D9.sort[25]
cc <- humDF_D9.sort[975]


output_Fr=data.frame(aa, ab, ac, ba, bb, bc, ca, cb, cc)
output_Fr ## affichage du résultat

write.table(output_Fr,"results_decil_Fr.txt", append = TRUE, sep=";", col.names = TRUE, row.names = TRUE)



### 3.1.2 DSKdata$

var <-DSKdata$temperature ## création d'un vecteur générique (var) pour un remplacement plus aisé

c1 <- c(0:999)*0 ## création d'une table remplie de 0
## ** pour initialiser les dimensions et la valeurs des cellules du dataframe qui suit


DSKDF=data.frame("decile_1"=c1,"Moy"=c1,"decile_9"=c1) ## **création d'un DF initialisé avec les valeurs de c1 pour les cellules et le nombre de lignes.
## DSKDF

## boucle qui calcule 1000 fois sur un échantillon de 50 tirages des moyennes ou les déciles un et neuf à partir du vecteur var

## /!\ il faut exclure les NA du calcul de la moyenne également /!\
## /!\ j'ai renommé le decile_3 en decile_9 /!\

for (i in 1:1000){
    DSKDF$decile_1[i]     <- quantile(sample(var,50,replace=T),0.1, na.rm = T);
    DSKDF$Moy[i]          <- mean(sample(var,50,replace=T), na.rm = T);
    DSKDF$decile_9[i]     <- quantile(sample(var,50,replace=T),0.9, na.rm = T);
}

aa <- mean(DSKDF$decile_1) ##♥ moyenne des 1000 1er déciles
ba <- mean(DSKDF$Moy) ##♥ moyenne des 1000 moyennes
ca <- mean(DSKDF$decile_9) ##♥ moyenne des 1000 9ème déciles


DSKDF_D1.sort=sort(DSKDF$decile_1)
DSKDF_Moy.sort=sort(DSKDF$Moy)
DSKDF_D9.sort=sort(DSKDF$decile_9)

length_ss_NA_DSKDF_D1.sort <- length(na.omit(DSKDF_D1.sort))
length_ss_NA_DSKDF_Moy.sort <- length(na.omit(DSKDF_Moy.sort))
length_ss_NA_DSKDF_D9.sort <- length(na.omit(DSKDF_D9.sort))

length_ss_NA_DSKDF_D1.sort
length_ss_NA_DSKDF_Moy.sort
length_ss_NA_DSKDF_D9.sort


## c(DSKDF_D1.sort[25], DSKDF_D1.sort[975])
ab <- DSKDF_D1.sort[25]
ac <- DSKDF_D1.sort[975]

## c(DSKDF_Moy.sort[25], DSKDF_Moy.sort[975])
bb <- DSKDF_Moy.sort[25]
bc <- DSKDF_Moy.sort[975]

## c(DSKDF_D1.sort[25], DSKDF_D1.sort[975])
cb <- DSKDF_D9.sort[25]
cc <- DSKDF_D9.sort[975]


output_Fr=data.frame(aa, ab, ac, ba, bb, bc, ca, cb, cc)
output_Fr ## affichage du résultat

write.table(output_Fr,"results_decil_Fr.txt", append = TRUE, sep=";", row.names = TRUE)


## 3.2 temperaturehigh

### 3.2.1 humdata$

var <-humdata$temperaturehigh ## création d'un vecteur générique (var) pour un remplacement plus aisé

c1 <- c(0:999)*0 ## création d'une table remplie de 0
## ** pour initialiser les dimensions et la valeurs des cellules du dataframe qui suit


humDF=data.frame("decile_1"=c1,"Moy"=c1,"decile_9"=c1) ## **création d'un DF initialisé avec les valeurs de c1 pour les cellules et le nombre de lignes.
## humDF

## boucle qui calcule 1000 fois sur un échantillon de 50 tirages des moyennes ou les déciles un et neuf à partir du vecteur var

## /!\ il faut exclure les NA du calcul de la moyenne également /!\
## /!\ j'ai renommé le decile_3 en decile_9 /!\

for (i in 1:1000){
    humDF$decile_1[i]     <- quantile(sample(var,50,replace=T),0.1, na.rm = T);
    humDF$Moy[i]          <- mean(sample(var,50,replace=T), na.rm = T);
    humDF$decile_9[i]     <- quantile(sample(var,50,replace=T),0.9, na.rm = T);
}

aa <- mean(humDF$decile_1) ##♥ moyenne des 1000 1er déciles
ba <- mean(humDF$Moy) ##♥ moyenne des 1000 moyennes
ca <- mean(humDF$decile_9) ##♥ moyenne des 1000 9ème déciles


humDF_D1.sort=sort(humDF$decile_1)
humDF_Moy.sort=sort(humDF$Moy)
humDF_D9.sort=sort(humDF$decile_9)


length_ss_NA_humDF_D1.sort <- length(na.omit(humDF_D1.sort))
length_ss_NA_humDF_Moy.sort <- length(na.omit(humDF_Moy.sort))
length_ss_NA_humDF_D9.sort <- length(na.omit(humDF_D9.sort))

length_ss_NA_humDF_D1.sort
length_ss_NA_humDF_Moy.sort
length_ss_NA_humDF_D9.sort


## c(humDF_D1.sort[25], humDF_D1.sort[975])
ab <- humDF_D1.sort[25]
ac <- humDF_D1.sort[975]

## c(humDF_Moy.sort[25], humDF_Moy.sort[975])
bb <- humDF_Moy.sort[25]
bc <- humDF_Moy.sort[975]

## c(humDF_D1.sort[25], humDF_D1.sort[975])
cb <- humDF_D9.sort[25]
cc <- humDF_D9.sort[975]


output_Fr=data.frame(aa, ab, ac, ba, bb, bc, ca, cb, cc)
output_Fr ## affichage du résultat

write.table(output_Fr,"results_decil_Fr.txt", append = TRUE, sep=";", col.names = TRUE, row.names = TRUE)



### 3.2.2 DSKdata$

var <-DSKdata$temperaturehigh ## création d'un vecteur générique (var) pour un remplacement plus aisé

c1 <- c(0:999)*0 ## création d'une table remplie de 0
## ** pour initialiser les dimensions et la valeurs des cellules du dataframe qui suit


DSKDF=data.frame("decile_1"=c1,"Moy"=c1,"decile_9"=c1) ## **création d'un DF initialisé avec les valeurs de c1 pour les cellules et le nombre de lignes.
## DSKDF

## boucle qui calcule 1000 fois sur un échantillon de 50 tirages des moyennes ou les déciles un et neuf à partir du vecteur var

## /!\ il faut exclure les NA du calcul de la moyenne également /!\
## /!\ j'ai renommé le decile_3 en decile_9 /!\

for (i in 1:1000){
    DSKDF$decile_1[i]     <- quantile(sample(var,50,replace=T),0.1, na.rm = T);
    DSKDF$Moy[i]          <- mean(sample(var,50,replace=T), na.rm = T);
    DSKDF$decile_9[i]     <- quantile(sample(var,50,replace=T),0.9, na.rm = T);
}

aa <- mean(DSKDF$decile_1) ##♥ moyenne des 1000 1er déciles
ba <- mean(DSKDF$Moy) ##♥ moyenne des 1000 moyennes
ca <- mean(DSKDF$decile_9) ##♥ moyenne des 1000 9ème déciles


DSKDF_D1.sort=sort(DSKDF$decile_1)
DSKDF_Moy.sort=sort(DSKDF$Moy)
DSKDF_D9.sort=sort(DSKDF$decile_9)

length_ss_NA_DSKDF_D1.sort <- length(na.omit(DSKDF_D1.sort))
length_ss_NA_DSKDF_Moy.sort <- length(na.omit(DSKDF_Moy.sort))
length_ss_NA_DSKDF_D9.sort <- length(na.omit(DSKDF_D9.sort))

length_ss_NA_DSKDF_D1.sort
length_ss_NA_DSKDF_Moy.sort
length_ss_NA_DSKDF_D9.sort


## c(DSKDF_D1.sort[25], DSKDF_D1.sort[975])
ab <- DSKDF_D1.sort[25]
ac <- DSKDF_D1.sort[975]

## c(DSKDF_Moy.sort[25], DSKDF_Moy.sort[975])
bb <- DSKDF_Moy.sort[25]
bc <- DSKDF_Moy.sort[975]

## c(DSKDF_D1.sort[25], DSKDF_D1.sort[975])
cb <- DSKDF_D9.sort[25]
cc <- DSKDF_D9.sort[975]


output_Fr=data.frame(aa, ab, ac, ba, bb, bc, ca, cb, cc)
output_Fr ## affichage du résultat

write.table(output_Fr,"results_decil_Fr.txt", append = TRUE, sep=";", row.names = TRUE)


## 3.3 humidity

### 3.3.1 humdata$

var <-humdata$humidity ## création d'un vecteur générique (var) pour un remplacement plus aisé

c1 <- c(0:999)*0 ## création d'une table remplie de 0
## ** pour initialiser les dimensions et la valeurs des cellules du dataframe qui suit


humDF=data.frame("decile_1"=c1,"Moy"=c1,"decile_9"=c1) ## **création d'un DF initialisé avec les valeurs de c1 pour les cellules et le nombre de lignes.
## humDF

## boucle qui calcule 1000 fois sur un échantillon de 50 tirages des moyennes ou les déciles un et neuf à partir du vecteur var

## /!\ il faut exclure les NA du calcul de la moyenne également /!\
## /!\ j'ai renommé le decile_3 en decile_9 /!\

for (i in 1:1000){
    humDF$decile_1[i]     <- quantile(sample(var,50,replace=T),0.1, na.rm = T);
    humDF$Moy[i]          <- mean(sample(var,50,replace=T), na.rm = T);
    humDF$decile_9[i]     <- quantile(sample(var,50,replace=T),0.9, na.rm = T);
}

aa <- mean(humDF$decile_1) ##♥ moyenne des 1000 1er déciles
ba <- mean(humDF$Moy) ##♥ moyenne des 1000 moyennes
ca <- mean(humDF$decile_9) ##♥ moyenne des 1000 9ème déciles


humDF_D1.sort=sort(humDF$decile_1)
humDF_Moy.sort=sort(humDF$Moy)
humDF_D9.sort=sort(humDF$decile_9)


length_ss_NA_humDF_D1.sort <- length(na.omit(humDF_D1.sort))
length_ss_NA_humDF_Moy.sort <- length(na.omit(humDF_Moy.sort))
length_ss_NA_humDF_D9.sort <- length(na.omit(humDF_D9.sort))

length_ss_NA_humDF_D1.sort
length_ss_NA_humDF_Moy.sort
length_ss_NA_humDF_D9.sort


## c(humDF_D1.sort[25], humDF_D1.sort[975])
ab <- humDF_D1.sort[25]
ac <- humDF_D1.sort[975]

## c(humDF_Moy.sort[25], humDF_Moy.sort[975])
bb <- humDF_Moy.sort[25]
bc <- humDF_Moy.sort[975]

## c(humDF_D1.sort[25], humDF_D1.sort[975])
cb <- humDF_D9.sort[25]
cc <- humDF_D9.sort[975]


output_Fr=data.frame(aa, ab, ac, ba, bb, bc, ca, cb, cc)
output_Fr ## affichage du résultat

write.table(output_Fr,"results_decil_Fr.txt", append = TRUE, sep=";", col.names = TRUE, row.names = TRUE)



### 3.3.2 DSKdata$

var <-DSKdata$humidity ## création d'un vecteur générique (var) pour un remplacement plus aisé

c1 <- c(0:999)*0 ## création d'une table remplie de 0
## ** pour initialiser les dimensions et la valeurs des cellules du dataframe qui suit


DSKDF=data.frame("decile_1"=c1,"Moy"=c1,"decile_9"=c1) ## **création d'un DF initialisé avec les valeurs de c1 pour les cellules et le nombre de lignes.
## DSKDF

## boucle qui calcule 1000 fois sur un échantillon de 50 tirages des moyennes ou les déciles un et neuf à partir du vecteur var

## /!\ il faut exclure les NA du calcul de la moyenne également /!\
## /!\ j'ai renommé le decile_3 en decile_9 /!\

for (i in 1:1000){
    DSKDF$decile_1[i]     <- quantile(sample(var*100,50,replace=T),0.1, na.rm = T);
    DSKDF$Moy[i]          <- mean(sample(var*100,50,replace=T), na.rm = T);
    DSKDF$decile_9[i]     <- quantile(sample(var*100,50,replace=T),0.9, na.rm = T);
}

aa <- mean(DSKDF$decile_1) ##♥ moyenne des 1000 1er déciles
ba <- mean(DSKDF$Moy) ##♥ moyenne des 1000 moyennes
ca <- mean(DSKDF$decile_9) ##♥ moyenne des 1000 9ème déciles


DSKDF_D1.sort=sort(DSKDF$decile_1)
DSKDF_Moy.sort=sort(DSKDF$Moy)
DSKDF_D9.sort=sort(DSKDF$decile_9)

length_ss_NA_DSKDF_D1.sort <- length(na.omit(DSKDF_D1.sort))
length_ss_NA_DSKDF_Moy.sort <- length(na.omit(DSKDF_Moy.sort))
length_ss_NA_DSKDF_D9.sort <- length(na.omit(DSKDF_D9.sort))

length_ss_NA_DSKDF_D1.sort
length_ss_NA_DSKDF_Moy.sort
length_ss_NA_DSKDF_D9.sort


## c(DSKDF_D1.sort[25], DSKDF_D1.sort[975])
ab <- DSKDF_D1.sort[25]
ac <- DSKDF_D1.sort[975]

## c(DSKDF_Moy.sort[25], DSKDF_Moy.sort[975])
bb <- DSKDF_Moy.sort[25]
bc <- DSKDF_Moy.sort[975]

## c(DSKDF_D1.sort[25], DSKDF_D1.sort[975])
cb <- DSKDF_D9.sort[25]
cc <- DSKDF_D9.sort[975]


output_Fr=data.frame(aa, ab, ac, ba, bb, bc, ca, cb, cc)
output_Fr ## affichage du résultat

write.table(output_Fr,"results_decil_Fr.txt", append = TRUE, sep=";", row.names = TRUE)

## 3.4 dewpoint

### 3.4.1 humdata$

var <-humdata$dewpoint ## création d'un vecteur générique (var) pour un remplacement plus aisé

c1 <- c(0:999)*0 ## création d'une table remplie de 0
## ** pour initialiser les dimensions et la valeurs des cellules du dataframe qui suit


humDF=data.frame("decile_1"=c1,"Moy"=c1,"decile_9"=c1) ## **création d'un DF initialisé avec les valeurs de c1 pour les cellules et le nombre de lignes.
## humDF

## boucle qui calcule 1000 fois sur un échantillon de 50 tirages des moyennes ou les déciles un et neuf à partir du vecteur var

## /!\ il faut exclure les NA du calcul de la moyenne également /!\
## /!\ j'ai renommé le decile_3 en decile_9 /!\

for (i in 1:1000){
    humDF$decile_1[i]     <- quantile(sample(var,50,replace=T),0.1, na.rm = T);
    humDF$Moy[i]          <- mean(sample(var,50,replace=T), na.rm = T);
    humDF$decile_9[i]     <- quantile(sample(var,50,replace=T),0.9, na.rm = T);
}

aa <- mean(humDF$decile_1) ##♥ moyenne des 1000 1er déciles
ba <- mean(humDF$Moy) ##♥ moyenne des 1000 moyennes
ca <- mean(humDF$decile_9) ##♥ moyenne des 1000 9ème déciles


humDF_D1.sort=sort(humDF$decile_1)
humDF_Moy.sort=sort(humDF$Moy)
humDF_D9.sort=sort(humDF$decile_9)


length_ss_NA_humDF_D1.sort <- length(na.omit(humDF_D1.sort))
length_ss_NA_humDF_Moy.sort <- length(na.omit(humDF_Moy.sort))
length_ss_NA_humDF_D9.sort <- length(na.omit(humDF_D9.sort))

length_ss_NA_humDF_D1.sort
length_ss_NA_humDF_Moy.sort
length_ss_NA_humDF_D9.sort


## c(humDF_D1.sort[25], humDF_D1.sort[975])
ab <- humDF_D1.sort[25]
ac <- humDF_D1.sort[975]

## c(humDF_Moy.sort[25], humDF_Moy.sort[975])
bb <- humDF_Moy.sort[25]
bc <- humDF_Moy.sort[975]

## c(humDF_D1.sort[25], humDF_D1.sort[975])
cb <- humDF_D9.sort[25]
cc <- humDF_D9.sort[975]


output_Fr=data.frame(aa, ab, ac, ba, bb, bc, ca, cb, cc)
output_Fr ## affichage du résultat

write.table(output_Fr,"results_decil_Fr.txt", append = TRUE, sep=";", col.names = TRUE, row.names = TRUE)


length_ss_NA <- 1000 - length(length_ss_NA_humDF_D1.sort)
length_ss_NA
length_ss_NA <- 1000 - length(length_ss_NA_humDF_Moy.sort)
length_ss_NA
length_ss_NA <- 1000 - length(length_ss_NA_humDF_D9.sort)
length_ss_NA


### 3.4.2 DSKdata$

var <-DSKdata$dewpoint ## création d'un vecteur générique (var) pour un remplacement plus aisé

c1 <- c(0:999)*0 ## création d'une table remplie de 0
## ** pour initialiser les dimensions et la valeurs des cellules du dataframe qui suit


DSKDF=data.frame("decile_1"=c1,"Moy"=c1,"decile_9"=c1) ## **création d'un DF initialisé avec les valeurs de c1 pour les cellules et le nombre de lignes.
## DSKDF

## boucle qui calcule 1000 fois sur un échantillon de 50 tirages des moyennes ou les déciles un et neuf à partir du vecteur var

## /!\ il faut exclure les NA du calcul de la moyenne également /!\
## /!\ j'ai renommé le decile_3 en decile_9 /!\

for (i in 1:1000){
    DSKDF$decile_1[i]     <- quantile(sample(var,50,replace=T),0.1, na.rm = T);
    DSKDF$Moy[i]          <- mean(sample(var,50,replace=T), na.rm = T);
    DSKDF$decile_9[i]     <- quantile(sample(var,50,replace=T),0.9, na.rm = T);
}

aa <- mean(DSKDF$decile_1) ##♥ moyenne des 1000 1er déciles
ba <- mean(DSKDF$Moy) ##♥ moyenne des 1000 moyennes
ca <- mean(DSKDF$decile_9) ##♥ moyenne des 1000 9ème déciles


DSKDF_D1.sort=sort(DSKDF$decile_1)
DSKDF_Moy.sort=sort(DSKDF$Moy)
DSKDF_D9.sort=sort(DSKDF$decile_9)

length_ss_NA_DSKDF_D1.sort <- length(na.omit(DSKDF_D1.sort))
length_ss_NA_DSKDF_Moy.sort <- length(na.omit(DSKDF_Moy.sort))
length_ss_NA_DSKDF_D9.sort <- length(na.omit(DSKDF_D9.sort))

length_ss_NA_DSKDF_D1.sort
length_ss_NA_DSKDF_Moy.sort
length_ss_NA_DSKDF_D9.sort


## c(DSKDF_D1.sort[25], DSKDF_D1.sort[975])
ab <- DSKDF_D1.sort[25]
ac <- DSKDF_D1.sort[975]

## c(DSKDF_Moy.sort[25], DSKDF_Moy.sort[975])
bb <- DSKDF_Moy.sort[25]
bc <- DSKDF_Moy.sort[975]

## c(DSKDF_D1.sort[25], DSKDF_D1.sort[975])
cb <- DSKDF_D9.sort[25]
cc <- DSKDF_D9.sort[975]


output_Fr=data.frame(aa, ab, ac, ba, bb, bc, ca, cb, cc)
output_Fr ## affichage du résultat

write.table(output_Fr,"results_decil_Fr.txt", append = TRUE, sep=";", row.names = TRUE)


length_ss_NA <- 1000 - length(length_ss_NA_DSKDF_D1.sort)
length_ss_NA
length_ss_NA <- 1000 - length(length_ss_NA_DSKDF_Moy.sort)
length_ss_NA
length_ss_NA <- 1000 - length(length_ss_NA_DSKDF_D9.sort)
length_ss_NA

# [1] 0

## 3.5 pressure

### 3.5.1 humdata$

var <-humdata$pressure ## création d'un vecteur générique (var) pour un remplacement plus aisé

c1 <- c(0:999)*0 ## création d'une table remplie de 0
## ** pour initialiser les dimensions et la valeurs des cellules du dataframe qui suit


humDF=data.frame("decile_1"=c1,"Moy"=c1,"decile_9"=c1) ## **création d'un DF initialisé avec les valeurs de c1 pour les cellules et le nombre de lignes.
## humDF

## boucle qui calcule 1000 fois sur un échantillon de 50 tirages des moyennes ou les déciles un et neuf à partir du vecteur var

## /!\ il faut exclure les NA du calcul de la moyenne également /!\
## /!\ j'ai renommé le decile_3 en decile_9 /!\

for (i in 1:1000){
    humDF$decile_1[i]     <- quantile(sample(var,50,replace=T),0.1, na.rm = T);
    humDF$Moy[i]          <- mean(sample(var,50,replace=T), na.rm = T);
    humDF$decile_9[i]     <- quantile(sample(var,50,replace=T),0.9, na.rm = T);
}

aa <- mean(humDF$decile_1) ##♥ moyenne des 1000 1er déciles
ba <- mean(humDF$Moy) ##♥ moyenne des 1000 moyennes
ca <- mean(humDF$decile_9) ##♥ moyenne des 1000 9ème déciles


humDF_D1.sort=sort(humDF$decile_1)
humDF_Moy.sort=sort(humDF$Moy)
humDF_D9.sort=sort(humDF$decile_9)


length_ss_NA_humDF_D1.sort <- length(na.omit(humDF_D1.sort))
length_ss_NA_humDF_Moy.sort <- length(na.omit(humDF_Moy.sort))
length_ss_NA_humDF_D9.sort <- length(na.omit(humDF_D9.sort))

length_ss_NA_humDF_D1.sort
length_ss_NA_humDF_Moy.sort
length_ss_NA_humDF_D9.sort


## c(humDF_D1.sort[25], humDF_D1.sort[975])
ab <- humDF_D1.sort[25]
ac <- humDF_D1.sort[975]

## c(humDF_Moy.sort[25], humDF_Moy.sort[975])
bb <- humDF_Moy.sort[25]
bc <- humDF_Moy.sort[975]

## c(humDF_D1.sort[25], humDF_D1.sort[975])
cb <- humDF_D9.sort[25]
cc <- humDF_D9.sort[975]


output_Fr=data.frame(aa, ab, ac, ba, bb, bc, ca, cb, cc)
output_Fr ## affichage du résultat

write.table(output_Fr,"results_decil_Fr.txt", append = TRUE, sep=";", col.names = TRUE, row.names = TRUE)



### 3.5.2 DSKdata$

var <-DSKdata$pressure ## création d'un vecteur générique (var) pour un remplacement plus aisé

c1 <- c(0:999)*0 ## création d'une table remplie de 0
## ** pour initialiser les dimensions et la valeurs des cellules du dataframe qui suit


DSKDF=data.frame("decile_1"=c1,"Moy"=c1,"decile_9"=c1) ## **création d'un DF initialisé avec les valeurs de c1 pour les cellules et le nombre de lignes.
## DSKDF

## boucle qui calcule 1000 fois sur un échantillon de 50 tirages des moyennes ou les déciles un et neuf à partir du vecteur var

## /!\ il faut exclure les NA du calcul de la moyenne également /!\
## /!\ j'ai renommé le decile_3 en decile_9 /!\

for (i in 1:1000){
    DSKDF$decile_1[i]     <- quantile(sample(var,50,replace=T),0.1, na.rm = T);
    DSKDF$Moy[i]          <- mean(sample(var,50,replace=T), na.rm = T);
    DSKDF$decile_9[i]     <- quantile(sample(var,50,replace=T),0.9, na.rm = T);
}

aa <- mean(DSKDF$decile_1) ##♥ moyenne des 1000 1er déciles
ba <- mean(DSKDF$Moy) ##♥ moyenne des 1000 moyennes
ca <- mean(DSKDF$decile_9) ##♥ moyenne des 1000 9ème déciles


DSKDF_D1.sort=sort(DSKDF$decile_1)
DSKDF_Moy.sort=sort(DSKDF$Moy)
DSKDF_D9.sort=sort(DSKDF$decile_9)

length_ss_NA_DSKDF_D1.sort <- length(na.omit(DSKDF_D1.sort))
length_ss_NA_DSKDF_Moy.sort <- length(na.omit(DSKDF_Moy.sort))
length_ss_NA_DSKDF_D9.sort <- length(na.omit(DSKDF_D9.sort))

length_ss_NA_DSKDF_D1.sort
length_ss_NA_DSKDF_Moy.sort
length_ss_NA_DSKDF_D9.sort


## c(DSKDF_D1.sort[25], DSKDF_D1.sort[975])
ab <- DSKDF_D1.sort[25]
ac <- DSKDF_D1.sort[975]

## c(DSKDF_Moy.sort[25], DSKDF_Moy.sort[975])
bb <- DSKDF_Moy.sort[25]
bc <- DSKDF_Moy.sort[975]

## c(DSKDF_D1.sort[25], DSKDF_D1.sort[975])
cb <- DSKDF_D9.sort[25]
cc <- DSKDF_D9.sort[975]


output_Fr=data.frame(aa, ab, ac, ba, bb, bc, ca, cb, cc)
output_Fr ## affichage du résultat

write.table(output_Fr,"results_decil_Fr.txt", append = TRUE, sep=";", row.names = TRUE)


## 3.6 windspeed

### 3.6.1 humdata$

var <-humdata$windspeed ## création d'un vecteur générique (var) pour un remplacement plus aisé

c1 <- c(0:999)*0 ## création d'une table remplie de 0
## ** pour initialiser les dimensions et la valeurs des cellules du dataframe qui suit


humDF=data.frame("decile_1"=c1,"Moy"=c1,"decile_9"=c1) ## **création d'un DF initialisé avec les valeurs de c1 pour les cellules et le nombre de lignes.
## humDF

## boucle qui calcule 1000 fois sur un échantillon de 50 tirages des moyennes ou les déciles un et neuf à partir du vecteur var

## /!\ il faut exclure les NA du calcul de la moyenne également /!\
## /!\ j'ai renommé le decile_3 en decile_9 /!\

for (i in 1:1000){
    humDF$decile_1[i]     <- quantile(sample(var,50,replace=T),0.1, na.rm = T);
    humDF$Moy[i]          <- mean(sample(var,50,replace=T), na.rm = T);
    humDF$decile_9[i]     <- quantile(sample(var,50,replace=T),0.9, na.rm = T);
}

aa <- mean(humDF$decile_1) ##♥ moyenne des 1000 1er déciles
ba <- mean(humDF$Moy) ##♥ moyenne des 1000 moyennes
ca <- mean(humDF$decile_9) ##♥ moyenne des 1000 9ème déciles


humDF_D1.sort=sort(humDF$decile_1)
humDF_Moy.sort=sort(humDF$Moy)
humDF_D9.sort=sort(humDF$decile_9)


length_ss_NA_humDF_D1.sort <- length(na.omit(humDF_D1.sort))
length_ss_NA_humDF_Moy.sort <- length(na.omit(humDF_Moy.sort))
length_ss_NA_humDF_D9.sort <- length(na.omit(humDF_D9.sort))

length_ss_NA_humDF_D1.sort
length_ss_NA_humDF_Moy.sort
length_ss_NA_humDF_D9.sort


## c(humDF_D1.sort[25], humDF_D1.sort[975])
ab <- humDF_D1.sort[25]
ac <- humDF_D1.sort[975]

## c(humDF_Moy.sort[25], humDF_Moy.sort[975])
bb <- humDF_Moy.sort[25]
bc <- humDF_Moy.sort[975]

## c(humDF_D1.sort[25], humDF_D1.sort[975])
cb <- humDF_D9.sort[25]
cc <- humDF_D9.sort[975]


output_Fr=data.frame(aa, ab, ac, ba, bb, bc, ca, cb, cc)
output_Fr ## affichage du résultat

write.table(output_Fr,"results_decil_Fr.txt", append = TRUE, sep=";", col.names = TRUE, row.names = TRUE)



### 3.6.2 DSKdata$

var <-DSKdata$windspeed ## création d'un vecteur générique (var) pour un remplacement plus aisé

c1 <- c(0:999)*0 ## création d'une table remplie de 0
## ** pour initialiser les dimensions et la valeurs des cellules du dataframe qui suit


DSKDF=data.frame("decile_1"=c1,"Moy"=c1,"decile_9"=c1) ## **création d'un DF initialisé avec les valeurs de c1 pour les cellules et le nombre de lignes.
## DSKDF

## boucle qui calcule 1000 fois sur un échantillon de 50 tirages des moyennes ou les déciles un et neuf à partir du vecteur var

## /!\ il faut exclure les NA du calcul de la moyenne également /!\
## /!\ j'ai renommé le decile_3 en decile_9 /!\

for (i in 1:1000){
    DSKDF$decile_1[i]     <- quantile(sample(var,50,replace=T),0.1, na.rm = T);
    DSKDF$Moy[i]          <- mean(sample(var,50,replace=T), na.rm = T);
    DSKDF$decile_9[i]     <- quantile(sample(var,50,replace=T),0.9, na.rm = T);
}

aa <- mean(DSKDF$decile_1) ##♥ moyenne des 1000 1er déciles
ba <- mean(DSKDF$Moy) ##♥ moyenne des 1000 moyennes
ca <- mean(DSKDF$decile_9) ##♥ moyenne des 1000 9ème déciles


DSKDF_D1.sort=sort(DSKDF$decile_1)
DSKDF_Moy.sort=sort(DSKDF$Moy)
DSKDF_D9.sort=sort(DSKDF$decile_9)

length_ss_NA_DSKDF_D1.sort <- length(na.omit(DSKDF_D1.sort))
length_ss_NA_DSKDF_Moy.sort <- length(na.omit(DSKDF_Moy.sort))
length_ss_NA_DSKDF_D9.sort <- length(na.omit(DSKDF_D9.sort))

length_ss_NA_DSKDF_D1.sort
length_ss_NA_DSKDF_Moy.sort
length_ss_NA_DSKDF_D9.sort


## c(DSKDF_D1.sort[25], DSKDF_D1.sort[975])
ab <- DSKDF_D1.sort[25]
ac <- DSKDF_D1.sort[975]

## c(DSKDF_Moy.sort[25], DSKDF_Moy.sort[975])
bb <- DSKDF_Moy.sort[25]
bc <- DSKDF_Moy.sort[975]

## c(DSKDF_D1.sort[25], DSKDF_D1.sort[975])
cb <- DSKDF_D9.sort[25]
cc <- DSKDF_D9.sort[975]


output_Fr=data.frame(aa, ab, ac, ba, bb, bc, ca, cb, cc)
output_Fr ## affichage du résultat

write.table(output_Fr,"results_decil_Fr.txt", append = TRUE, sep=";", row.names = TRUE)


## 3.7 visibility

### 3.7.1 humdata$

var <-humdata$visibility ## création d'un vecteur générique (var) pour un remplacement plus aisé

c1 <- c(0:999)*0 ## création d'une table remplie de 0
## ** pour initialiser les dimensions et la valeurs des cellules du dataframe qui suit


humDF=data.frame("decile_1"=c1,"Moy"=c1,"decile_9"=c1) ## **création d'un DF initialisé avec les valeurs de c1 pour les cellules et le nombre de lignes.
## humDF

## boucle qui calcule 1000 fois sur un échantillon de 50 tirages des moyennes ou les déciles un et neuf à partir du vecteur var

## /!\ il faut exclure les NA du calcul de la moyenne également /!\
## /!\ j'ai renommé le decile_3 en decile_9 /!\

for (i in 1:1000){
    humDF$decile_1[i]     <- quantile(sample(var,50,replace=T),0.1, na.rm = T);
    humDF$Moy[i]          <- mean(sample(var,50,replace=T), na.rm = T);
    humDF$decile_9[i]     <- quantile(sample(var,50,replace=T),0.9, na.rm = T);
}

aa <- mean(humDF$decile_1) ##♥ moyenne des 1000 1er déciles
ba <- mean(humDF$Moy) ##♥ moyenne des 1000 moyennes
ca <- mean(humDF$decile_9) ##♥ moyenne des 1000 9ème déciles


humDF_D1.sort=sort(humDF$decile_1)
humDF_Moy.sort=sort(humDF$Moy)
humDF_D9.sort=sort(humDF$decile_9)


length_ss_NA_humDF_D1.sort <- length(na.omit(humDF_D1.sort))
length_ss_NA_humDF_Moy.sort <- length(na.omit(humDF_Moy.sort))
length_ss_NA_humDF_D9.sort <- length(na.omit(humDF_D9.sort))

length_ss_NA_humDF_D1.sort
length_ss_NA_humDF_Moy.sort
length_ss_NA_humDF_D9.sort


## c(humDF_D1.sort[25], humDF_D1.sort[975])
ab <- humDF_D1.sort[25]
ac <- humDF_D1.sort[975]

## c(humDF_Moy.sort[25], humDF_Moy.sort[975])
bb <- humDF_Moy.sort[25]
bc <- humDF_Moy.sort[975]

## c(humDF_D1.sort[25], humDF_D1.sort[975])
cb <- humDF_D9.sort[25]
cc <- humDF_D9.sort[975]


output_Fr=data.frame(aa, ab, ac, ba, bb, bc, ca, cb, cc)
output_Fr ## affichage du résultat

write.table(output_Fr,"results_decil_Fr.txt", append = TRUE, sep=";", col.names = TRUE, row.names = TRUE)



### 3.7.2 DSKdata$

var <-DSKdata$visibility ## création d'un vecteur générique (var) pour un remplacement plus aisé

c1 <- c(0:999)*0 ## création d'une table remplie de 0
## ** pour initialiser les dimensions et la valeurs des cellules du dataframe qui suit


DSKDF=data.frame("decile_1"=c1,"Moy"=c1,"decile_9"=c1) ## **création d'un DF initialisé avec les valeurs de c1 pour les cellules et le nombre de lignes.
## DSKDF

## boucle qui calcule 1000 fois sur un échantillon de 50 tirages des moyennes ou les déciles un et neuf à partir du vecteur var

## /!\ il faut exclure les NA du calcul de la moyenne également /!\
## /!\ j'ai renommé le decile_3 en decile_9 /!\

for (i in 1:1000){
    DSKDF$decile_1[i]     <- quantile(sample(var,50,replace=T),0.1, na.rm = T);
    DSKDF$Moy[i]          <- mean(sample(var,50,replace=T), na.rm = T);
    DSKDF$decile_9[i]     <- quantile(sample(var,50,replace=T),0.9, na.rm = T);
}

aa <- mean(DSKDF$decile_1) ##♥ moyenne des 1000 1er déciles
ba <- mean(DSKDF$Moy) ##♥ moyenne des 1000 moyennes
ca <- mean(DSKDF$decile_9) ##♥ moyenne des 1000 9ème déciles


DSKDF_D1.sort=sort(DSKDF$decile_1)
DSKDF_Moy.sort=sort(DSKDF$Moy)
DSKDF_D9.sort=sort(DSKDF$decile_9)

length_ss_NA_DSKDF_D1.sort <- length(na.omit(DSKDF_D1.sort))
length_ss_NA_DSKDF_Moy.sort <- length(na.omit(DSKDF_Moy.sort))
length_ss_NA_DSKDF_D9.sort <- length(na.omit(DSKDF_D9.sort))

length_ss_NA_DSKDF_D1.sort
length_ss_NA_DSKDF_Moy.sort
length_ss_NA_DSKDF_D9.sort


## c(DSKDF_D1.sort[25], DSKDF_D1.sort[975])
ab <- DSKDF_D1.sort[25]
ac <- DSKDF_D1.sort[975]

## c(DSKDF_Moy.sort[25], DSKDF_Moy.sort[975])
bb <- DSKDF_Moy.sort[25]
bc <- DSKDF_Moy.sort[975]

## c(DSKDF_D1.sort[25], DSKDF_D1.sort[975])
cb <- DSKDF_D9.sort[25]
cc <- DSKDF_D9.sort[975]


output_Fr=data.frame(aa, ab, ac, ba, bb, bc, ca, cb, cc)
output_Fr ## affichage du résultat

write.table(output_Fr,"results_decil_Fr.txt", append = TRUE, sep=";", row.names = TRUE)


## 3.8 cloudcover

### 3.8.1 humdata$

var <-humdata$cloudcover ## création d'un vecteur générique (var) pour un remplacement plus aisé

c1 <- c(0:999)*0 ## création d'une table remplie de 0
## ** pour initialiser les dimensions et la valeurs des cellules du dataframe qui suit


humDF=data.frame("decile_1"=c1,"Moy"=c1,"decile_9"=c1) ## **création d'un DF initialisé avec les valeurs de c1 pour les cellules et le nombre de lignes.
## humDF

## boucle qui calcule 1000 fois sur un échantillon de 50 tirages des moyennes ou les déciles un et neuf à partir du vecteur var

## /!\ il faut exclure les NA du calcul de la moyenne également /!\
## /!\ j'ai renommé le decile_3 en decile_9 /!\

for (i in 1:1000){
    humDF$decile_1[i]     <- quantile(sample(var,50,replace=T),0.1, na.rm = T);
    humDF$Moy[i]          <- mean(sample(var,50,replace=T), na.rm = T);
    humDF$decile_9[i]     <- quantile(sample(var,50,replace=T),0.9, na.rm = T);
}

aa <- mean(humDF$decile_1) ##♥ moyenne des 1000 1er déciles
ba <- mean(humDF$Moy) ##♥ moyenne des 1000 moyennes
ca <- mean(humDF$decile_9) ##♥ moyenne des 1000 9ème déciles


humDF_D1.sort=sort(humDF$decile_1)
humDF_Moy.sort=sort(humDF$Moy)
humDF_D9.sort=sort(humDF$decile_9)


length_ss_NA_humDF_D1.sort <- length(na.omit(humDF_D1.sort))
length_ss_NA_humDF_Moy.sort <- length(na.omit(humDF_Moy.sort))
length_ss_NA_humDF_D9.sort <- length(na.omit(humDF_D9.sort))

length_ss_NA_humDF_D1.sort
length_ss_NA_humDF_Moy.sort
length_ss_NA_humDF_D9.sort


## c(humDF_D1.sort[25], humDF_D1.sort[975])
ab <- humDF_D1.sort[25]
ac <- humDF_D1.sort[975]

## c(humDF_Moy.sort[25], humDF_Moy.sort[975])
bb <- humDF_Moy.sort[25]
bc <- humDF_Moy.sort[975]

## c(humDF_D1.sort[25], humDF_D1.sort[975])
cb <- humDF_D9.sort[25]
cc <- humDF_D9.sort[975]


output_Fr=data.frame(aa, ab, ac, ba, bb, bc, ca, cb, cc)
output_Fr ## affichage du résultat

write.table(output_Fr,"results_decil_Fr.txt", append = TRUE, sep=";", col.names = TRUE, row.names = TRUE)



### 3.8.2 DSKdata$

var <-DSKdata$cloudcover ## création d'un vecteur générique (var) pour un remplacement plus aisé

c1 <- c(0:999)*0 ## création d'une table remplie de 0
## ** pour initialiser les dimensions et la valeurs des cellules du dataframe qui suit


DSKDF=data.frame("decile_1"=c1,"Moy"=c1,"decile_9"=c1) ## **création d'un DF initialisé avec les valeurs de c1 pour les cellules et le nombre de lignes.
## DSKDF

## boucle qui calcule 1000 fois sur un échantillon de 50 tirages des moyennes ou les déciles un et neuf à partir du vecteur var

## /!\ il faut exclure les NA du calcul de la moyenne également /!\
## /!\ j'ai renommé le decile_3 en decile_9 /!\

for (i in 1:1000){
    DSKDF$decile_1[i]     <- quantile(sample(var*100,50,replace=T),0.1, na.rm = T);
    DSKDF$Moy[i]          <- mean(sample(var*100,50,replace=T), na.rm = T);
    DSKDF$decile_9[i]     <- quantile(sample(var*100,50,replace=T),0.9, na.rm = T);
}

aa <- mean(DSKDF$decile_1) ##♥ moyenne des 1000 1er déciles
ba <- mean(DSKDF$Moy) ##♥ moyenne des 1000 moyennes
ca <- mean(DSKDF$decile_9) ##♥ moyenne des 1000 9ème déciles


DSKDF_D1.sort=sort(DSKDF$decile_1)
DSKDF_Moy.sort=sort(DSKDF$Moy)
DSKDF_D9.sort=sort(DSKDF$decile_9)

length_ss_NA_DSKDF_D1.sort <- length(na.omit(DSKDF_D1.sort))
length_ss_NA_DSKDF_Moy.sort <- length(na.omit(DSKDF_Moy.sort))
length_ss_NA_DSKDF_D9.sort <- length(na.omit(DSKDF_D9.sort))

length_ss_NA_DSKDF_D1.sort
length_ss_NA_DSKDF_Moy.sort
length_ss_NA_DSKDF_D9.sort


## c(DSKDF_D1.sort[25], DSKDF_D1.sort[975])
ab <- DSKDF_D1.sort[25]
ac <- DSKDF_D1.sort[975]

## c(DSKDF_Moy.sort[25], DSKDF_Moy.sort[975])
bb <- DSKDF_Moy.sort[25]
bc <- DSKDF_Moy.sort[975]

## c(DSKDF_D1.sort[25], DSKDF_D1.sort[975])
cb <- DSKDF_D9.sort[25]
cc <- DSKDF_D9.sort[975]


output_Fr=data.frame(aa, ab, ac, ba, bb, bc, ca, cb, cc)
output_Fr ## affichage du résultat

write.table(output_Fr,"results_decil_Fr.txt", append = TRUE, sep=";", row.names = TRUE)


## 3.9 windgust

### 3.9.1 humdata$

var <-humdata$windgust ## création d'un vecteur générique (var) pour un remplacement plus aisé

c1 <- c(0:999)*0 ## création d'une table remplie de 0
## ** pour initialiser les dimensions et la valeurs des cellules du dataframe qui suit


humDF=data.frame("decile_1"=c1,"Moy"=c1,"decile_9"=c1) ## **création d'un DF initialisé avec les valeurs de c1 pour les cellules et le nombre de lignes.
## humDF

## boucle qui calcule 1000 fois sur un échantillon de 50 tirages des moyennes ou les déciles un et neuf à partir du vecteur var

## /!\ il faut exclure les NA du calcul de la moyenne également /!\
## /!\ j'ai renommé le decile_3 en decile_9 /!\

for (i in 1:1000){
    humDF$decile_1[i]     <- quantile(sample(var,50,replace=T),0.1, na.rm = T);
    humDF$Moy[i]          <- mean(sample(var,50,replace=T), na.rm = T);
    humDF$decile_9[i]     <- quantile(sample(var,50,replace=T),0.9, na.rm = T);
}

aa <- mean(humDF$decile_1) ##♥ moyenne des 1000 1er déciles
ba <- mean(humDF$Moy) ##♥ moyenne des 1000 moyennes
ca <- mean(humDF$decile_9) ##♥ moyenne des 1000 9ème déciles


humDF_D1.sort=sort(humDF$decile_1)
humDF_Moy.sort=sort(humDF$Moy)
humDF_D9.sort=sort(humDF$decile_9)


length_ss_NA_humDF_D1.sort <- length(na.omit(humDF_D1.sort))
length_ss_NA_humDF_Moy.sort <- length(na.omit(humDF_Moy.sort))
length_ss_NA_humDF_D9.sort <- length(na.omit(humDF_D9.sort))

length_ss_NA_humDF_D1.sort
length_ss_NA_humDF_Moy.sort
length_ss_NA_humDF_D9.sort


## c(humDF_D1.sort[25], humDF_D1.sort[975])
ab <- humDF_D1.sort[25]
ac <- humDF_D1.sort[975]

## c(humDF_Moy.sort[25], humDF_Moy.sort[975])
bb <- humDF_Moy.sort[25]
bc <- humDF_Moy.sort[975]

## c(humDF_D1.sort[25], humDF_D1.sort[975])
cb <- humDF_D9.sort[25]
cc <- humDF_D9.sort[975]


output_Fr=data.frame(aa, ab, ac, ba, bb, bc, ca, cb, cc)
output_Fr ## affichage du résultat

write.table(output_Fr,"results_decil_Fr.txt", append = TRUE, sep=";", col.names = TRUE, row.names = TRUE)



### 3.9.2 DSKdata$

var <-DSKdata$windgust ## création d'un vecteur générique (var) pour un remplacement plus aisé

c1 <- c(0:999)*0 ## création d'une table remplie de 0
## ** pour initialiser les dimensions et la valeurs des cellules du dataframe qui suit


DSKDF=data.frame("decile_1"=c1,"Moy"=c1,"decile_9"=c1) ## **création d'un DF initialisé avec les valeurs de c1 pour les cellules et le nombre de lignes.
## DSKDF

## boucle qui calcule 1000 fois sur un échantillon de 50 tirages des moyennes ou les déciles un et neuf à partir du vecteur var

## /!\ il faut exclure les NA du calcul de la moyenne également /!\
## /!\ j'ai renommé le decile_3 en decile_9 /!\

for (i in 1:1000){
    DSKDF$decile_1[i]     <- quantile(sample(var,50,replace=T),0.1, na.rm = T);
    DSKDF$Moy[i]          <- mean(sample(var,50,replace=T), na.rm = T);
    DSKDF$decile_9[i]     <- quantile(sample(var,50,replace=T),0.9, na.rm = T);
}

aa <- mean(DSKDF$decile_1) ##♥ moyenne des 1000 1er déciles
ba <- mean(DSKDF$Moy) ##♥ moyenne des 1000 moyennes
ca <- mean(DSKDF$decile_9) ##♥ moyenne des 1000 9ème déciles


DSKDF_D1.sort=sort(DSKDF$decile_1)
DSKDF_Moy.sort=sort(DSKDF$Moy)
DSKDF_D9.sort=sort(DSKDF$decile_9)

length_ss_NA_DSKDF_D1.sort <- length(na.omit(DSKDF_D1.sort))
length_ss_NA_DSKDF_Moy.sort <- length(na.omit(DSKDF_Moy.sort))
length_ss_NA_DSKDF_D9.sort <- length(na.omit(DSKDF_D9.sort))

length_ss_NA_DSKDF_D1.sort
length_ss_NA_DSKDF_Moy.sort
length_ss_NA_DSKDF_D9.sort


## c(DSKDF_D1.sort[25], DSKDF_D1.sort[975])
ab <- DSKDF_D1.sort[25]
ac <- DSKDF_D1.sort[975]

## c(DSKDF_Moy.sort[25], DSKDF_Moy.sort[975])
bb <- DSKDF_Moy.sort[25]
bc <- DSKDF_Moy.sort[975]

## c(DSKDF_D1.sort[25], DSKDF_D1.sort[975])
cb <- DSKDF_D9.sort[25]
cc <- DSKDF_D9.sort[975]


output_Fr=data.frame(aa, ab, ac, ba, bb, bc, ca, cb, cc)
output_Fr ## affichage du résultat

write.table(output_Fr,"results_decil_Fr.txt", append = TRUE, sep=";", row.names = TRUE)


## 3.10 uvindex

### 3.10.1 humdata$

var <-humdata$uvindex ## création d'un vecteur générique (var) pour un remplacement plus aisé

c1 <- c(0:999)*0 ## création d'une table remplie de 0
## ** pour initialiser les dimensions et la valeurs des cellules du dataframe qui suit


humDF=data.frame("decile_1"=c1,"Moy"=c1,"decile_9"=c1) ## **création d'un DF initialisé avec les valeurs de c1 pour les cellules et le nombre de lignes.
## humDF

## boucle qui calcule 1000 fois sur un échantillon de 50 tirages des moyennes ou les déciles un et neuf à partir du vecteur var

## /!\ il faut exclure les NA du calcul de la moyenne également /!\
## /!\ j'ai renommé le decile_3 en decile_9 /!\

for (i in 1:1000){
    humDF$decile_1[i]     <- quantile(sample(var,50,replace=T),0.1, na.rm = T);
    humDF$Moy[i]          <- mean(sample(var,50,replace=T), na.rm = T);
    humDF$decile_9[i]     <- quantile(sample(var,50,replace=T),0.9, na.rm = T);
}

aa <- mean(humDF$decile_1) ##♥ moyenne des 1000 1er déciles
ba <- mean(humDF$Moy) ##♥ moyenne des 1000 moyennes
ca <- mean(humDF$decile_9) ##♥ moyenne des 1000 9ème déciles


humDF_D1.sort=sort(humDF$decile_1)
humDF_Moy.sort=sort(humDF$Moy)
humDF_D9.sort=sort(humDF$decile_9)


length_ss_NA_humDF_D1.sort <- length(na.omit(humDF_D1.sort))
length_ss_NA_humDF_Moy.sort <- length(na.omit(humDF_Moy.sort))
length_ss_NA_humDF_D9.sort <- length(na.omit(humDF_D9.sort))

length_ss_NA_humDF_D1.sort
length_ss_NA_humDF_Moy.sort
length_ss_NA_humDF_D9.sort


## c(humDF_D1.sort[25], humDF_D1.sort[975])
ab <- humDF_D1.sort[25]
ac <- humDF_D1.sort[975]

## c(humDF_Moy.sort[25], humDF_Moy.sort[975])
bb <- humDF_Moy.sort[25]
bc <- humDF_Moy.sort[975]

## c(humDF_D1.sort[25], humDF_D1.sort[975])
cb <- humDF_D9.sort[25]
cc <- humDF_D9.sort[975]


output_Fr=data.frame(aa, ab, ac, ba, bb, bc, ca, cb, cc)
output_Fr ## affichage du résultat

write.table(output_Fr,"results_decil_Fr.txt", append = TRUE, sep=";", col.names = TRUE, row.names = TRUE)



### 3.10.2 DSKdata$

var <-DSKdata$uvindex ## création d'un vecteur générique (var) pour un remplacement plus aisé

c1 <- c(0:999)*0 ## création d'une table remplie de 0
## ** pour initialiser les dimensions et la valeurs des cellules du dataframe qui suit


DSKDF=data.frame("decile_1"=c1,"Moy"=c1,"decile_9"=c1) ## **création d'un DF initialisé avec les valeurs de c1 pour les cellules et le nombre de lignes.
## DSKDF

## boucle qui calcule 1000 fois sur un échantillon de 50 tirages des moyennes ou les déciles un et neuf à partir du vecteur var

## /!\ il faut exclure les NA du calcul de la moyenne également /!\
## /!\ j'ai renommé le decile_3 en decile_9 /!\

for (i in 1:1000){
    DSKDF$decile_1[i]     <- quantile(sample(var,50,replace=T),0.1, na.rm = T);
    DSKDF$Moy[i]          <- mean(sample(var,50,replace=T), na.rm = T);
    DSKDF$decile_9[i]     <- quantile(sample(var,50,replace=T),0.9, na.rm = T);
}

aa <- mean(DSKDF$decile_1) ##♥ moyenne des 1000 1er déciles
ba <- mean(DSKDF$Moy) ##♥ moyenne des 1000 moyennes
ca <- mean(DSKDF$decile_9) ##♥ moyenne des 1000 9ème déciles


DSKDF_D1.sort=sort(DSKDF$decile_1)
DSKDF_Moy.sort=sort(DSKDF$Moy)
DSKDF_D9.sort=sort(DSKDF$decile_9)

length_ss_NA_DSKDF_D1.sort <- length(na.omit(DSKDF_D1.sort))
length_ss_NA_DSKDF_Moy.sort <- length(na.omit(DSKDF_Moy.sort))
length_ss_NA_DSKDF_D9.sort <- length(na.omit(DSKDF_D9.sort))

length_ss_NA_DSKDF_D1.sort
length_ss_NA_DSKDF_Moy.sort
length_ss_NA_DSKDF_D9.sort


## c(DSKDF_D1.sort[25], DSKDF_D1.sort[975])
ab <- DSKDF_D1.sort[25]
ac <- DSKDF_D1.sort[975]

## c(DSKDF_Moy.sort[25], DSKDF_Moy.sort[975])
bb <- DSKDF_Moy.sort[25]
bc <- DSKDF_Moy.sort[975]

## c(DSKDF_D1.sort[25], DSKDF_D1.sort[975])
cb <- DSKDF_D9.sort[25]
cc <- DSKDF_D9.sort[975]


output_Fr=data.frame(aa, ab, ac, ba, bb, bc, ca, cb, cc)
output_Fr ## affichage du résultat


write.table(output_Fr,"results_decil_Fr.txt", append = TRUE, sep=";", row.names = TRUE)


## /!\ Renommer le output en fonction de la région traitée  /!\


### France entière




