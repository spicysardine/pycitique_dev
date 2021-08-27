##### Code figure n°5 – Profils comparés de 9 paramètres météorologiques pours 42 stations synoptiques de Météo France
##### et leur équivalent Dark Sky (France, July 2017 – April 2020, soit 995 jours).
setwd('/home/beetroot/Developer/python/CNRS/projetCitique/pycitique/R/citik_humains')
getwd()
### Or, if .csv file, use this si données séparées par ";"
# DSKdata <- read.csv("../../data/donnee_meteo_nationale_comparative/comparaison/dsk_moyennes_journalieres_maille_42.csv",
#                         header = TRUE,
#                         sep = ";",
#                         dec = ".",
#                         stringsAsFactors = F)
# 
# MFdata <- read.csv("../../data/donnee_meteo_nationale_comparative/comparaison/mf_moyennes_journalieres_maille_42.csv",
#                        header = TRUE,
#                        sep = ";",
#                        dec = ".",
#                       stringsAsFactors = F)

require(RPostgreSQL)
require(DT)
require(tidyverse)

drv <- PostgreSQL()
con <- dbConnect(drv, db='localbase10', user='beetroot')

curs_dsk <- dbSendQuery(con, 'SELECT * FROM meteo.darksky_synop42_avg order by date_releve asc')
DSKdata <- fetch(curs, n=-1)

curs_mf <- dbSendQuery(con, 'SELECT * FROM meteo.mf_synop42_avg order by date_iso asc')
MFdata <- fetch(curs_mf, n=-1)

## Uniformisation des parametres en % de MF
MFdata$humidite_floor <- floor(MFdata$humidite)
MFdata$humidite_ceiling <- ceiling(MFdata$humidite)

MFdata$nebulosite_floor <- floor(MFdata$nebulosite)
MFdata$nebulosite_ceiling <- ceiling(MFdata$nebulosite)

###### Fonction de abrication des graphiaues comparatifs avec ggplot2
make_hist <- function(paramdsk, parammf){
    
      p <- ggplot(DSKdata, aes(DSKdata[,paramdsk]))+
        geom_histogram( color='green', fill='black', aes(y=..density..), alpha=.55)+
        geom_density(data = MFdata, color='blue', aes(MFdata[,parammf]), fill='light blue', alpha=.2)
       
        return(p)
}


### Vecteur de caracteres contenant les parametres meteo a traiter
dsk_paramnames <- c("temperature", "temperaturelow", "temperaturehigh", 
                          "humidity", "dewpoint", "pressure", "windspeed",
                            "visibility", "cloudcover", "windgust", 'precipintensity')

mf_paramnames <- c('temperature', 'temperature_nocturne', 'temperature_diurne',
                        'humidite_floor', 'point_rose', 'press_mer', 'vvent',
                          'visibilite', 'nebulosite_floor','rafale_10min', 'precip_24h')

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

make_hist('cloudcover','nebulosite')
make_hist('cloudcover','nebulosite_floor')
make_hist('cloudcover','nebulosite_ceiling')







