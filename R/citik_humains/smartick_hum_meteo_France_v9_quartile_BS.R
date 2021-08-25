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

quartile <- function(param){

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
        
        moy_quart1 <- mean(humDF$decile_25) ## moyenne des 1000 1er quartiles
        moy_quart2 <- mean(humDF$decile_50) ## moyenne des 1000 2eme quartiles
        moy_quart3 <- mean(humDF$decile_75) ## moyenne des 1000 3eme quartiles
        
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
        quart1_IC_bas <- humDF_D25.sort[25]
        quart1_IC_haut <- humDF_D25.sort[975]
        
        ## c(humDF_D50.sort[25], humDF_D50.sort[975])
        quart2_IC_bas <- humDF_D50.sort[25]
        quart2_IC_haut <- humDF_D50.sort[975]
        
        ## c(humDF_D25.sort[25], humDF_D25.sort[975])
        quart3_IC_bas <- humDF_D75.sort[25]
        quart3_IC_haut <- humDF_D75.sort[975]
        
        # La fonction renvoie un vecteur numeric
        output_Fr = c(moy_quart1, quart1_IC_bas, quart1_IC_haut,
                                moy_quart2, quart2_IC_bas, quart2_IC_haut,
                                                moy_quart3, quart3_IC_bas, quart3_IC_haut)
        
        # print(output_Fr) ## affichage du résultat
        return(output_Fr)
        # write.table(output_Fr,"results_quartil_Fr.txt", append = TRUE, sep=";", col.names = TRUE, row.names = TRUE)

}


# is dot is.numeric 

# vectornames <- c("temperaturehigh", "humidity", "dewpoint", "pressure", "windspeed")

# sapply applique la fonction a droite au data frame cible en retournant un vecteur booléen
index <- sapply(humdata, is.numeric)
#vecteur booléen
index

#liste vide a remplir
ic_table <- list()
ic_table

# boucle de calcule implementant la fonction quartile
# avec filtrage par le vecteur index sur le dataframe humdata
for (name in names(humdata[,index])  ){
        
        # isolation de la colonne cible pour chaque iteration de la boucle
        param <- humdata[,name]
        # affectation dans une variable intermediaire
        result <- quartile(param)
        # insertion du vecteru numeric resultant dans la liste
        ic_table[[name]] <- result
         
}

# transformation (cast) de la list en data.frame
ic_table <- as.data.frame(ic_table)
# transposition du tableau (data.frame)
ic_table <- t(ic_table)

# Creation d’un vecteur de nom de colonne pour le tableau transpose
nom_de_colonne = c('moy_quart1', 'quart1_IC_bas', 'quart1_IC_haut',
                      'moy_quart2', 'quart2_IC_bas', 'quart2_IC_haut',
                        'moy_quart3', 'quart3_IC_bas', 'quart3_IC_haut')

# renommage des colonnes
colnames(ic_table) <- nom_de_colonne
# visualisation  de la table
View(ic_table)

# export au format csv
write.csv(ic_table, 'ic_quartiles.csv')



# empty_list <- list('un', 2, 'quatre', 45, T)
# empty_list[[6]] <- vectornames
# 
# empty_list$six[3]
# 
# list_names <- c('un', 'deux', 'trois', 'quatre', 'cinq', 'six')
# names(empty_list) <- list_names 

# empty <- humdata[, "temperaturehigh"]
# 
# quartile(empty)

# 
# quartile(humdata$temperature)
# 
# dim(humdata)
# names(humdata)
# 
# # Dans l’indexation R on commence toujours par 1
# for(i in 37:length(humdata)){
#         
#        name <- names(humdata[i])
#        print(name)
#        
# }
# 
# vectornames
# 
# 
# for( name in vectornames ){
#         
#         print(name)
#         param <- humdata[,name]
#         result <- quartile(param)
#         print(result)
#         
# }




