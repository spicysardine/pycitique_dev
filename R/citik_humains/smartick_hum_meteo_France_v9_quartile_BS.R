###### Code Tableau n°4 – Pour la France entière, selon le 1er quartile, le 2ème quartile (la médiane) et le 3ème quartile :
###### Paramètres météorologiques associés aux 14 657 lieux et dates de signalements comparés à ceux des mêmes dates
###### mais pour un semis de lieux aléatoires (France, July 2017 – April 2020, soit 995 jours).

###### Analyse des signalements et des données Météo DSK (non plus moyennées mais DSK brutes) pour la France entière
###### D1, Moy, D9 et leurs IC


# 1. Introduction
### Alloue la librairie "set()" puis vérifie que c'est bien importé "getwd()", il pointe dessus!

## 1.1 XPS13
#setwd("C:/3VG/MSH/lyme/Smartick17/pycitique_miror/R")
#getwd()

## 1.2 OP7570
setwd("D:/3VG/MSH/lyme/Smartick17/pycitique_miror/R")
setwd('./')
getwd()


##### 2. Importation de données citik_humains_clean_weather_strict.csv et darksky_donnee_brute_700_def3.csv
### Or, if .csv file, use this si données séparées par ","
### quote = "'" pour le caractère d’échappement
### stringAsFactpors pour voir les chaînes de caractères sans interprétation.

## 2.0 Création des subsets
### France entière

humdata_ <- read.csv("../../../pycitique/data/donnee_signalements_avec_meteo_dsk/citik_humains_clean_weather_strict.csv",
                     sep = ",",
                     dec = ".",
                     quote = "'",
                     stringsAsFactors = FALSE)

names(humdata_)

# DSKdata <- read.csv("../pycitique/data/donnee_meteo_nationale_comparative/darksky/darksky_donnee_brute_700_def3.csv", sep = ",", dec = ".", quote = "'", stringsAsFactors = FALSE)

require(RPostgreSQL)
drv <- PostgreSQL()
con <- dbConnect(drv, db='localbase10', user='beetroot', passwor='root')
query <- dbSendQuery(con, 'SELECT * FROM meteo.darksky_maille_700_extraction')

DSKdata <- fetch(query, n=-1)
names(DSKdata)

## /!\ Libérer la région à traiter  /!\

### Création des subset pour l'IDF

humdata_idf <- humdata[humdata$departement_code == c("75","77","78",91:95) , ]

### Création des subset pour l'AL

# humdata <- subset(humdata, humdata$departement_code == "54"  "55" "57"  "88" "67"  "68")

### Création des subset pour RA

# DSKdata <- subset(DSKdata, DSKdata$departement_code == "01" "07" "26" "38" "42" "69" "73" "74")

## 2.1 Analyse du tableau de données

## ls(humdata) ## liste les variables
## ls(DSKdata)

# 3. Boostrap pour stabiliser indicateurs et intervalles de confiance du t.test d'une moyenne
#(médiane/quartile... voir "IC_foret_AL_CLC5.R" par exemple et Poinsot, 2005, R pour les statophobes, pp.13-1510)

### Calcul d'un IC via en utilisant le Bootstrap pour des petits échantillons ou une absence
#de normalité avérée (cf. section 3.4 de Poinsot, p.14)

### création d'un vecteur (tableau à une seule rangée) nommé table devant accueillir 1000 valeurs numériques
# => les 1000 valeurs de moyennes obtenues.

#### 3.1 temperature

### 3.1.1 humdata$

decile <- function(param){

        c1 <- c(0:999)*NA ## création d'une table remplie de 0
        ## ** pour initialiser les dimensions et la valeurs des cellules du dataframe qui suit
        
        humDF=data.frame("decile_25"=c1,"decile_50"=c1,"decile_75"=c1) ## **création d'un DF initialisé avec les valeurs de c1 pour les cellules et le nombre de lignes.
        
        ## boucle qui calcule 1000 fois sur un échantillon de 50 tirages des moyennes ou les déciles un et neuf à partir du vecteur var
        
        ## /!\ il faut exclure les NA du calcul de la moyenne également /!\
        ## /!\ j'ai renommé le decile_3 en decile_75 /!\
        
        for (i in 1:1000){
            humDF$decile_25[i] <- quantile(sample(param,50,replace=T), 0.25, na.rm = T);
            humDF$decile_50[i] <- quantile(sample(param,50,replace=T), 0.5, na.rm = T);
            humDF$decile_75[i] <- quantile(sample(param,50,replace=T), 0.75, na.rm = T);
        }
        
        aa <- mean(humDF$decile_25) ## moyenne des 1000 1er quartiles
        ba <- mean(humDF$decile_50) ## moyenne des 1000 2eme quartiles
        ca <- mean(humDF$decile_75) ## moyenne des 1000 3eme quartiles
        
        
        humDF_D25.sort=sort(humDF$decile_25)
        humDF_D50.sort=sort(humDF$decile_50)
        humDF_D75.sort=sort(humDF$decile_75)
        
        
        length_ss_NA_humDF_D25.sort <- length(na.omit(humDF_D25.sort))
        length_ss_NA_humDF_D50.sort <- length(na.omit(humDF_D50.sort))
        length_ss_NA_humDF_D75.sort <- length(na.omit(humDF_D75.sort))
        
        length_ss_NA_humDF_D25.sort
        length_ss_NA_humDF_D50.sort
        length_ss_NA_humDF_D75.sort
        
        
        ## c(humDF_D25.sort[25], humDF_D25.sort[975])
        ab <- humDF_D25.sort[25]
        ac <- humDF_D25.sort[975]
        
        ## c(humDF_D50.sort[25], humDF_D50.sort[975])
        bb <- humDF_D50.sort[25]
        bc <- humDF_D50.sort[975]
        
        ## c(humDF_D25.sort[25], humDF_D25.sort[975])
        cb <- humDF_D75.sort[25]
        cc <- humDF_D75.sort[975]
        
        
        output_Fr=data.frame(aa, ab, ac, ba, bb, bc, ca, cb, cc)
        output_Fr ## affichage du résultat
        
        # write.table(output_Fr,"results_quartil_Fr.txt", append = TRUE, sep=";", col.names = TRUE, row.names = TRUE)

}




names <- names(humdata)
mode(names)


rm(names)









