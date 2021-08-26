
###############################################################################################################################
###### Code pour Tableau n°4 – Pour la France entière, selon le 1er quartile, le 2ème quartile (la médiane) et le 3ème quartile :
###### Paramètres météorologiques associés aux 14 657 lieux et dates de signalements comparés à ceux des mêmes dates
###### mais pour un semis de lieux aléatoires (France, July 2017 – April 2020, soit 995 jours).
###### Analyse des signalements et des données Météo DSK (non plus moyennées mais DSK brutes) pour la France entière
###### Q1, Q2 (Médiane), Q3 et leurs IC
###### Date : 25/08/2021
###### Authors : Khaldoune Hilami, Vincent Godard
##############################################################################################################################
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
##############################################################################################################################

#### 1. Mise en place de l’environnement de travail
setwd('./')
getwd()

##### 2. Importation de données citik_humains_clean_weather_strict.csv et darksky_donnee_brute_700_def3.csv
### Le separateur du csv est la virgule "," et "'" comme caractere d’echapement
### quote = "'" pour le caractère d’échappement
### stringAsFactpors pour importer les chaînes de caractères sans les interpreter en tant que facteurs.

## 2.1 Création des dataframes France entière des données CSV de signalements lies a la meteo

# donnees de signalement pour les humains
humdata <- read.csv("../../data/donnee_signalements_avec_meteo_dsk/citik_humains_clean_weather_strict.csv",
                        sep = ",",
                        dec = ".",
                        quote = "'",
                        stringsAsFactors = FALSE)

## pour connaître verifier les variables du tableau human data (facultatif)
names(humdata) 

## 2.2 Création des dataframe France entière des données meteo du semis a 700 points 
# DSKdata <- read.csv("../data/donnee_meteo_nationale_comparative/darksky/darksky_donnee_brute_700_def3.csv",
#                         sep = ",",
#                         dec = ".",
#                         quote = "'",
#                         stringsAsFactors = FALSE)

## pour connaître verifier les variables du tableau darksky data
# names(DSKdata)

## 2.3 Methode d’importation depuis la BDD geographique PostgreSQL/PostGIS
## /!\ Ne pas commenter ni supprimer /!\
require(RPostgreSQL)
drv <- PostgreSQL()
con <- dbConnect(drv, db='localbase10', user='beetroot')
humdata_curs_query <- dbSendQuery(con, 'SELECT * FROM citik.citik_humains_clean_weather_strict')
# recuperation de la donnee par le curseur humdata_curs_query
humdata <- fetch(humdata_curs_query, n=-1)
# recuperation de la donnee par le curseur DSKdata_curs_query
DSKdata_curs_query <- dbSendQuery(con, 'SELECT * FROM meteo.darksky_maille_700_extraction_dpt')
DSKdata <- fetch(DSKdata_curs_query, n=-1)         

## 2.4 Création des dataframe par région comparées dans l'article

### 2.4.1 Création des subset pour l'IDF:
# signalements
humdata_idf <- humdata[humdata$departement_code %in% c("75","77","78",91:95) , ]
# semi meteo
DSKdata_idf <- DSKdata[DSKdata$departement_code %in% c("75","77","78",91:95) , ]

### 2.4.2 Création des subset pour l'AL:
# signalements
humdata_al <- humdata[ humdata$departement_code %in% c("54", "55", "57",  "88", "67", "68") , ]
# semi meteo
DSKdata_al <- DSKdata[ DSKdata$departement_code %in% c("54", "55", "57",  "88", "67", "68") , ]

### 2.4.3 Création des subset pour RA:
# signalements
humdata_ra <- humdata[ humdata$departement_code %in% c("01", "07", "26", "38", "42", "69", "73", "74") , ]
# semi meteo
DSKdata_ra <- DSKdata[ DSKdata$departement_code %in% c("01", "07", "26", "38", "42", "69", "73", "74") , ]

### 5. Boostrap pour stabiliser indicateurs et intervalles de confiance du t.test d'une moyenne
#Cf. médiane/quartile Poinsot, 2005, R pour les statophobes, pp.13-1510

### Fonction de Calcul d'un IC en utilisant le Bootstrap pour des petits échantillons ou avec une absence
### de normalité avérée (cf. section 3.4 de Poinsot, p.14)
### création d'un vecteur (tableau à une seule rangée) nommé table devant accueillir
# 1000 valeurs numériques => les 1000 valeurs de moyennes obtenues.

ic_calculator <- function(param, calcul){
        
        ## création d'une table 1000x3 index vides
        ## pour initialiser les dimensions et la valeurs des cellules du dataframe qui suit
        ## création d'un DF initialisé avec les valeurs de c1 pour les cellules et le nombre de lignes.
        humDF <- data.frame("lower_quantile"=c(0:999),"middl_quantile"=c(0:999),"upper_quantile"=c(0:999))*NA 

        ## boucle qui calcule 1000 fois sur un échantillon de 50 tirages les centiles 25, 50 et 75 à partir du vecteur index (cf. infra)
        ## /!\ il faut exclure les NA du calcul de la moyenne également /!\
        ## /!\ j'ai renommé le centile_3 en centile_75 /!\
        
        if(calcul=='quartile'){
                
                for (i in 1:1000){
                        humDF$lower_quantile[i] <- quantile(sample(param,50,replace=T), 0.25, na.rm = T);
                        humDF$middl_quantile[i] <- quantile(sample(param,50,replace=T), 0.50, na.rm = T);
                        humDF$upper_quantile[i] <- quantile(sample(param,50,replace=T), 0.75, na.rm = T);
                }
                
        }else if (calcul=='decile'){
                
                for (i in 1:1000){
                        humDF$lower_quantile[i]    <- quantile(sample(param,50,replace=T),0.1, na.rm = T);
                        humDF$middl_quantile[i]    <- mean(sample(param,50,replace=T), na.rm = T);
                        humDF$upper_quantile[i]    <- quantile(sample(param,50,replace=T),0.9, na.rm = T);
                }
                
        } else{
                print('Aucune opération valide demandée')
        }

        moy_quart1 <- mean(humDF$lower_quantile) ## moyenne des 1000 1er quantiles
        moy_quart2 <- mean(humDF$middl_quantile) ## moyenne des 1000 2eme quantiles
        moy_quart3 <- mean(humDF$upper_quantile) ## moyenne des 1000 3eme quantiles

        humDF_C25.sort=sort(humDF$lower_quantile)
        humDF_C50.sort=sort(humDF$middl_quantile)
        humDF_C75.sort=sort(humDF$upper_quantile)

        length_C25.sort <- length(na.omit(humDF_C25.sort))
        length_C50.sort <- length(na.omit(humDF_C50.sort))
        length_C75.sort <- length(na.omit(humDF_C75.sort))

        ## c(humDF_C25.sort[25], humDF_C25.sort[975])
        quart1_IC_bas <- humDF_C25.sort[25]
        quart1_IC_haut <- humDF_C25.sort[975]

        ## c(humDF_C50.sort[25], humDF_C50.sort[975])
        quart2_IC_bas <- humDF_C50.sort[25]
        quart2_IC_haut <- humDF_C50.sort[975]

        ## c(humDF_C75.sort[25], humDF_C75.sort[975])
        quart3_IC_bas <- humDF_C75.sort[25]
        quart3_IC_haut <- humDF_C75.sort[975]

        # La fonction retourne ce vecteur numeric contenant les 
        # trois moyennes et leur intervale de confiance
        output_Fr = c(moy_quart1, quart1_IC_bas, quart1_IC_haut,
                        moy_quart2, quart2_IC_bas, quart2_IC_haut,
                          moy_quart3, quart3_IC_bas, quart3_IC_haut)
        
        return(output_Fr)

}

ic_table_maker <- function(reportingdf, randomdf, paramvector, calcul){
                
        
        #liste vide a remplir
        ic_table <- list()
        
        # boucle de calcule implementant la fonction quartile
        # avec filtrage par le vecteur vectornames sur le dataframe humdata
        for (name in paramvector ){
        
                # isolation de la colonne cible pour chaque iteration de la boucle
                param_hum <- reportingdf[,name]
                # affectation dans une variable intermediaire
                result_hum <- ic_calculator(param_hum, calcul)
                # isolation de la colonne cible pour chaque iteration de la boucle
                param_dsk <- randomdf[,name]
                # affectation dans une variable intermediaire
                result_dsk <- ic_calculator(param_dsk, calcul)
                # insertion du vecteru numeric resultant dans la liste
                ic_table[[paste(name,'_reporting', sep='')]] <- result_hum
                # insertion du vecteru numeric resultant dans la liste
                ic_table[[paste(name,'_random', sep='')]] <- result_dsk
                rm(list='param_hum','result_hum','param_dsk','result_dsk')
        
        }
        
        # transformation (cast) de la list en data.frame
        ic_table <- as.data.frame(ic_table)
        # transposition du tableau (data.frame)
        ic_table <- as.data.frame(t(ic_table))
        
        # Creation d’un vecteur de nom de colonne pour le tableau transpose
        nom_de_colonne = c('moy_quant1', 'quant1_IC_bas', 'quant1_IC_haut',
                              'moy_quant2', 'quant2_IC_bas', 'quant2_IC_haut',
                                'moy_quant3', 'quant3_IC_bas', 'quant3_IC_haut')
        
        # renommage des colonnes
        colnames(ic_table) <- nom_de_colonne
        # visualisation  de la table
        View(ic_table)
        
        # export au format csv
        write.csv(ic_table, 'ic_quartiles.csv')

}

### Vecteur de caracteres contenant les parametres meteo a traiter
vectornames <- c("temperature",  "temperaturelow", "temperaturehigh", "humidity",
                      "dewpoint",   "pressure",      "windspeed",
                         "visibility", "cloudcover",   "windgust", "uvindex")


ic_table_maker(humdata, DSKdata, vectornames, calcul='decile' )
ic_table_maker(humdata_al, DSKdata_al, vectornames, calcul='decile' )
ic_table_maker(humdata_idf, DSKdata_idf, vectornames,  calcul='decile')
ic_table_maker(humdata_ra, DSKdata_ra, vectornames,  calcul='decile')


