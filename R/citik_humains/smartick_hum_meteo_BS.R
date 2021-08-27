
###############################################################################################################################
###### Code pour Tableau n°4 – Pour la France entière, selon le 1er quantile, le 2ème quantile (la médiane) et le 3ème quantile :
###### Paramètres météorologiques associés aux 14 657 lieux et dates de signalements comparés à ceux des mêmes dates
###### mais pour un semis de lieux aléatoires (France, July 2017 – April 2020, soit 995 jours).
###### Analyse des signalements et des données Météo DSK (non plus moyennées mais DSK brutes) pour la France entière
###### Q1, Q2 (Médiane), Q3 et leurs IC
###### Date : 25/08/2021
###### Authors : Khaldoune Hilami, Vincent Godard
##############################################################################################################################
###### Code figure n°7 – Profils météorologiques associés aux 14 657 lieux et dates de signalements comparés à ceux des mêmes 
###### dates, mais pour un semis de lieux aléatoires (France, January 17th 2017 – April 5th 2020, soit 995 jours).
###### Code Tableau n°5 – En Île-de-France, selon le 1er décile, la moyenne et le 9ème décile :
###### Paramètres météorologiques associés aux 14 657 lieux et dates de signalements comparés à ceux des mêmes dates
###### mais pour un semis de lieux aléatoires (France, January 2017 – April 2020, soit 995 jours).
###### Code Tableau n°6 – En Alsace-Lorraine, selon le 1er décile, la moyenne et le 9ème décile :
###### Paramètres météorologiques associés aux 14 657 lieux et dates de signalements comparés à ceux des mêmes dates
###### mais pour un semis de lieux aléatoires (France, January 2017 – April 2020, soit 995 jours).
###### Code Tableau n°7 – En Rhône-Alpes, selon le 1er décile, la moyenne et le 9ème décile :
###### Paramètres météorologiques associés aux 14 657 lieux et dates de signalements comparés à ceux des mêmes dates
###### mais pour un semis de lieux aléatoires (France, January 2017 – April 2020, soit 995 jours).
##############################################################################################################################

#_____________________________ Preparation de la donnee et definition des fonctions  _________________________________________#

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
# names(humdata) 

## 2.2 Création des dataframe France entière des données meteo du semis a 700 points 
# DSKdata <- read.csv("../data/donnee_meteo_nationale_comparative/darksky/darksky_donnee_brute_700_dpt.csv",
#                         sep = ",",
#                         dec = ".",
#                         quote = "'",
#                         stringsAsFactors = FALSE)

## pour connaître verifier les variables du tableau darksky data
# names(DSKdata)

### 2.3 Methode d’importation depuis la BDD geographique PostgreSQL/PostGIS
## /!\ Ne pas commenter ni supprimer /!\
require(RPostgreSQL)
require(tidyverse)
require(DT)

## Parametres de connectin a la base PostgreSQL
drv <- PostgreSQL()
con <- dbConnect(drv, db='localbase10', user='beetroot')

# recuperation de la donnee par le curseur humdata_curs_query
humdata_curs_query <- dbSendQuery(con, 'SELECT * FROM citik.citik_humains_clean_weather_strict')
humdata <- fetch(humdata_curs_query, n=-1)
# recuperation de la donnee par le curseur DSKdata_curs_query
DSKdata_curs_query <- dbSendQuery(con, 'SELECT * FROM meteo.darksky_maille_700_extraction_dpt')
DSKdata <- fetch(DSKdata_curs_query, n=-1)         
# recuperation de la donnee par le curseur curs_mf
MFdata_curs_query <- dbSendQuery(con, 'SELECT * FROM meteo.mf_synop42_avg order by date_iso asc')
MFdata <- fetch(MFdata_curs_query, n=-1)

## Uniformisation des parametres en % de MF
# humidite
MFdata$humidite_floor <- floor(MFdata$humidite)
MFdata$humidite_ceiling <- ceiling(MFdata$humidite)
# nebulosite
MFdata$nebulosite_floor <- floor(MFdata$nebulosite)
MFdata$nebulosite_ceiling <- ceiling(MFdata$nebulosite)

### 2.4 Création des dataframe par région comparées dans l'article

## 2.4.1 Création des subset pour l'IDF:
# signalements
humdata_idf <- humdata[humdata$departement_code %in% c("75","77","78",91:95) , ]
# semi meteo
DSKdata_idf <- DSKdata[DSKdata$departement_code %in% c("75","77","78",91:95) , ]

## 2.4.2 Création des subset pour l'AL:
# signalements
humdata_al <- humdata[ humdata$departement_code %in% c("54", "55", "57",  "88", "67", "68") , ]
# semi meteo
DSKdata_al <- DSKdata[ DSKdata$departement_code %in% c("54", "55", "57",  "88", "67", "68") , ]

### 2.4.3 Création des subset pour RA:
# signalements
humdata_ra <- humdata[ humdata$departement_code %in% c("01", "07", "26", "38", "42", "69", "73", "74") , ]
# semi meteo
DSKdata_ra <- DSKdata[ DSKdata$departement_code %in% c("01", "07", "26", "38", "42", "69", "73", "74") , ]

### 2.4.3. Sélection de la période hivernale "longue" (6 mois)
# signalements
humdata_winter17_long <- humdata[humdata$date_piqure_saisie >= "2017-10-01" & humdata$date_piqure_saisie  <= "2018-03-31",] 
humdata_winter18_long <- humdata[humdata$date_piqure_saisie >= "2018-10-01" & humdata$date_piqure_saisie  <= "2019-03-31",] 
humdata_winter19_long <- humdata[humdata$date_piqure_saisie >= "2019-10-01" & humdata$date_piqure_saisie  <= "2020-03-31",] 
humdata_winter_long <-rbind(humdata_winter17_long, humdata_winter18_long, humdata_winter19_long)
# semi meteo
DSKdata_winter17_long <- DSKdata[DSKdata$date_releve >= "2017-10-01" & DSKdata$date_releve <= "2018-03-31",]
DSKdata_winter18_long <- DSKdata[DSKdata$date_releve >= "2018-10-01" & DSKdata$date_releve <= "2019-03-31",]
DSKdata_winter19_long <- DSKdata[DSKdata$date_releve >= "2019-10-01" & DSKdata$date_releve <= "2020-03-31",]
DSKdata_winter_long <-rbind(DSKdata_winter17_long, DSKdata_winter18_long, DSKdata_winter19_long)

### 2.4.3. Sélection de la période hivernale "courte" (4 mois)
# signalements
humdata_winter17_short <- humdata[humdata$date_piqure_saisie >= "2017-11-01" & humdata$date_piqure_saisie  <= "2018-02-28",] 
humdata_winter18_short <- humdata[humdata$date_piqure_saisie >= "2018-11-01" & humdata$date_piqure_saisie  <= "2019-02-28",] 
humdata_winter19_short <- humdata[humdata$date_piqure_saisie >= "2019-11-01" & humdata$date_piqure_saisie  <= "2020-02-28",] 
humdata_winter_short <-rbind(humdata_winter17_short, humdata_winter18_short, humdata_winter19_short)
# semi meteo
DSKdata_winter17_short <- DSKdata[DSKdata$date_releve >= "2017-11-01" & DSKdata$date_releve <= "2018-02-28",]
DSKdata_winter18_short <- DSKdata[DSKdata$date_releve >= "2018-11-01" & DSKdata$date_releve <= "2019-02-28",]
DSKdata_winter19_short <- DSKdata[DSKdata$date_releve >= "2019-11-01" & DSKdata$date_releve <= "2020-02-28",]
DSKdata_winter_short <-rbind(DSKdata_winter17_short, DSKdata_winter18_short, DSKdata_winter19_short)

### 5. Boostrap pour stabiliser indicateurs et intervalles de confiance du t.test d'une moyenne
# Cf. médiane/quantile Poinsot, 2005, R pour les statophobes, pp.13-1510

### Fonction de Calcul d'un IC en utilisant le Bootstrap pour des petits échantillons ou avec une absence
# de normalité avérée (cf. section 3.4 de Poinsot, p.14)
# création d'un vecteur (tableau à une seule rangée) nommé table devant accueillir
# 1000 valeurs numériques => les 1000 valeurs de moyennes obtenues.

ic_calculator <- function(param, calcul){
        
        ## création d'une table 1000x3 index vides
        ## pour initialiser les dimensions et la valeurs des cellules du dataframe qui suit
        ## création d'un DF initialisé avec les valeurs de c1 pour les cellules et le nombre de lignes.
        humDF <- data.frame("lower_quantile"=c(0:999),"middl_quantile"=c(0:999),"upper_quantile"=c(0:999))*NA 

        ## boucle qui calcule 1000 fois sur un échantillon de 50 tirages les centiles 25, 50 et 75 à partir du vecteur index (cf. infra)
        ## /!\ il faut exclure les NA du calcul de la moyenne également /!\
        ## /!\ j'ai renommé le centile_3 en centile_75 /!\
        
        ## Calcule conditionnel selon l’option de la variable calcul
        # Calcul de quartiles
        if(calcul=='quartile'){
                
                for (i in 1:1000){
                        humDF$lower_quantile[i] <- quantile(sample(param, 50, replace=T), 0.25, na.rm = T);
                        humDF$middl_quantile[i] <- quantile(sample(param, 50, replace=T), 0.50, na.rm = T);
                        humDF$upper_quantile[i] <- quantile(sample(param, 50, replace=T), 0.75, na.rm = T);
                }
        # Calcul de deciles        
        }else if (calcul=='decile'){
                
                for (i in 1:1000){
                        humDF$lower_quantile[i]    <- quantile(sample(param, 50, replace=T), 0.1, na.rm = T);
                        humDF$middl_quantile[i]    <- mean(sample(param, 50, replace=T), na.rm = T);
                        humDF$upper_quantile[i]    <- quantile(sample(param, 50, replace=T), 0.9, na.rm = T);
                }
                
        } else{
                print('Aucune opération valide demandée')
        }

        moy_quant1 <- mean(humDF$lower_quantile) ## moyenne des 1000 1er quantiles
        moy_quant2 <- mean(humDF$middl_quantile) ## moyenne des 1000 2eme quantiles
        moy_quant3 <- mean(humDF$upper_quantile) ## moyenne des 1000 3eme quantiles

        humDF_C25.sort=sort(humDF$lower_quantile)
        humDF_C50.sort=sort(humDF$middl_quantile)
        humDF_C75.sort=sort(humDF$upper_quantile)

        ## recuperation des bornes de l’IC
        quant1_IC_bas <- humDF_C25.sort[25]
        quant1_IC_haut <- humDF_C25.sort[975]

        ## recuperation des bornes de l’IC
        quant2_IC_bas <- humDF_C50.sort[25]
        quant2_IC_haut <- humDF_C50.sort[975]

        ## recuperation des bornes de l’IC
        quant3_IC_bas <- humDF_C75.sort[25]
        quant3_IC_haut <- humDF_C75.sort[975]

        # La fonction retourne ce vecteur numeric contenant les 
        # trois moyennes et leur intervalle de confiance
        ic_vector = c(moy_quant1, quant1_IC_bas, quant1_IC_haut,
                        moy_quant2, quant2_IC_bas, quant2_IC_haut,
                          moy_quant3, quant3_IC_bas, quant3_IC_haut)
        # arrondire a deux decimales
        ic_vector <- round(ic_vector, digits=2)
        
        return(ic_vector)

}

ic_table_maker <- function(reportingdf, randomdf, paramvector, calcul){
                
        #liste vide a remplir
        ic_table <- list()
        
        # boucle de calcule implementant la fonction quantile
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
        
        ## export au format csv (de-commenter en cas de besoin)
        # date <- format(Sys.time(), "%A_%b_%d_%Hh%Mm%Ss_%Y")
        # filename <- paste('ic', calcul, date, '.csv', sep = '_')
        # write.csv(ic_table, filename )
        
        return(ic_table)

}

### 6. Fonctions de abrication des graphiaues comparatifs avec ggplot2
make_hist <- function(paramdsk, parammf){
  
  p <- ggplot(DSKdata, aes(DSKdata[,paramdsk]))+
    geom_histogram( color='green', fill='black', aes(y=..density..), alpha=.55)+
    geom_density(data = MFdata, color='blue', aes(MFdata[,parammf]), fill='light blue', alpha=.2)
  
  return(p)
}

batch_histogram <- function (dsk_paramnames, mf_paramnames){
  
    # Liste vide pour accueillir les nom de parametres
    paramlist <- list()
    
    # boucle de remplissage de la liste de correspondance
    for (i in 1:11){
      cat(dsk_paramnames[i], '|------>', mf_paramnames[i],'\n')
      paramlist[[ dsk_paramnames[i] ]] <- c(dsk_paramnames[i], mf_paramnames[i])
    }
    
    # boucle de fabrication des graphiaues
    for (param in paramlist ){
      
      paramdsk <- param[1]
      parammf <- param[2]
      # print(paramdsk, parmmf)
      p <- make_hist(paramdsk, parammf)
      p <- p+ xlab(label= paramdsk)
      print(p)
      
    }
}

#___________________________________________ Programme principal ________________________________________________________#

### Vecteur de caracteres contenant les parametres meteo a traiter
vectornames <- c("temperature",  "temperaturelow", "temperaturehigh", "humidity",
                      "dewpoint",   "pressure",      "windspeed",
                         "visibility", "cloudcover",   "windgust", "uvindex")

### Calcule et generation rapide et automatique des tables statistiques

## Periode annuelle sur la France entier ou les trois regions d’etude
# France entiere
france_quartile <- ic_table_maker(humdata, DSKdata, vectornames, calcul='quartile' )
datatable(france_quartile)
#idf
idf_decile <- ic_table_maker(humdata_idf, DSKdata_idf, vectornames, calcul='decile')
datatable(idf_decile)
#alsace
alsace_decile <- ic_table_maker(humdata_al, DSKdata_al, vectornames, calcul='decile' )
datatable(alsace_decile)
#rhone-alpes
rhone_alpes_decile <- ic_table_maker(humdata_ra, DSKdata_ra, vectornames, calcul='decile')
datatable(rhone_alpes_decile)

## Periode hivernale longue deciles
ic_hiver_long_decile <- ic_table_maker(humdata_winter_long, DSKdata_winter_long, vectornames, calcul='decile')
datatable(ic_hiver_long_decile)
# Periode hivernale courte deciles
ic_hiver_short_decile <- ic_table_maker(humdata_winter_short, DSKdata_winter_short, vectornames, calcul='decile')
datatable(ic_hiver_short_decile)
# Periode hivernale longue quartiles
ic_hiver_long_decile <- ic_table_maker(humdata_winter_long, DSKdata_winter_long, vectornames, calcul='quartile')
datatable(ic_hiver_long_decile)
# Periode hivernale courte quartiles
ic_hiver_short_decile <- ic_table_maker(humdata_winter_short, DSKdata_winter_short, vectornames, calcul='quartile')
datatable(ic_hiver_short_decile)

### Fabrication rapide et automatiaue des graphiaues
## Vecteurs de caracteres contenant les parametres meteo a comparer
dsk_paramnames <- c("temperature", "temperaturelow", "temperaturehigh", 
                    "humidity", "dewpoint", "pressure", "windspeed",
                    "visibility", "cloudcover", "windgust", 'precipintensity')

mf_paramnames <- c('temperature', 'temperature_nocturne', 'temperature_diurne',
                   'humidite_floor', 'point_rose', 'press_mer', 'vvent',
                   'visibilite', 'nebulosite_floor','rafale_10min', 'precip_24h')

batch_histogram(dsk_paramnames, mf_paramnames)




