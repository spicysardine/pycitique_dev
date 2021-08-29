##### Code figure n°6 – Profils temporels des variables météorologiques associés aux 14 657 lieux et dates de signalements comparés à ceux des mêmes dates

##### mais pour un semis de lieux aléatoires (France, April 1st 2017 – April 5th 2020, soit 1100 jours).
#
#
# Script origine : "script_commente_vincent_vg_xps13.R" pour les températures, développé pour les autres paramètres

## 1.1 XPS13
setwd("C:/3VG/MSH/lyme/Smartick17/pycitique_miror/R")
getwd()

## 1.2 OP7570
setwd("D:/3VG/MSH/lyme/Smartick17/pycitique_miror/R")
getwd()

#Remove all objects
rm(list = ls() )


humadatatot <- read.csv("../../pycitique/data/donnee_signalements_avec_meteo_dsk/citik_humains_clean_weather_strict.csv", sep = ",", dec = ".", quote = "'", stringsAsFactors = FALSE)

humadata <- subset(humadatatot, humadatatot$nbr_tique < 25) ## seulement pour les signalements < 25 tiques

dataset_dsk700 <- read.csv("../../pycitique/data/donnee_meteo_nationale_comparative/darksky/darksky_moyennes_journalieres_maille_700.csv", sep = ",", dec = ".", quote = "'", stringsAsFactors = FALSE)

dataset_dsk700$temperature <- ((dataset_dsk700$temperaturehigh+dataset_dsk700$temperaturelow)/2)

## création d'un champ date "datepiq_YMD" où "humdatatot$date_piqure_saisie" de char devient date

require(ggplot2)

library("lubridate")
dataset_dsk700$date_piq <- ymd(dataset_dsk700$date_releve)
humadata$date_piq <- ymd(humadata$date_piqure_saisie)

###♦ Ecart entre le 2017-07-15 (début officiel de signalement tiques)  et le "2020-04-05" (le max dans la base humadata)

time_length(interval(start = ymd("2017-04-01"), end = ymd("2020-04-01")), unit = "days")
# [1] 1096 jours


## méthode d'ajustement "geom_smooth(method = "loess")" https://www.datanovia.com/en/fr/blog/comment-tracer-une-ligne-lisse-avec-ggplot2/

## Smoothed conditional means voir les arguments dont method https://ggplot2.tidyverse.org/reference/geom_smooth.html

## méthode "loess" https://rdrr.io/r/stats/loess.html

## méthode "gam" https://rdrr.io/cran/mgcv/man/gam.html


# 2.  Variable : temperature

## 2.1 méthode loess (pour moins de 1000 observations)

ggplot(humadata, aes(x=date_piq))+
  geom_point(aes(y=temperature), color='blue', size=.2, alpha=.7)+
  geom_smooth(aes(y=temperature), color='blue')+
  geom_point(data = dataset_dsk700, aes(date_piq, temperature), color='red', size=.2, alpha=.7)+
  geom_smooth(data=dataset_dsk700, color='red', mapping = aes(date_piq, temperature))+
  ggtitle('Temporal distribution of temperatures associated with reports and witnesses \n for the whole of France from 2017-04-01 to 2020-04-01')+
  xlab(label = 'Date')+
  ylab(label=' Temperature (°C)')+
  xlim( as.Date('2017-04-01'), as.Date('2020-04-01') )+
  theme(plot.title = element_text(hjust = .5, face = 'bold', size = 14))+
  theme(axis.title.x = element_text(face = 'bold', size = 12))+
  theme(axis.title.y = element_text(face = 'bold', size = 12))

ggsave("ggplot_temperature_loess.png")


## 2.2 méthode gam

ggplot(humadata, aes(x=date_piq))+
  geom_point(aes(y=temperature), color='blue', size=.2, alpha=.7)+
  geom_smooth(aes(y=temperature), color='blue', method="gam")+
  geom_point(data = dataset_dsk700, aes(date_piq, temperature), color='red', size=.2, alpha=.7)+
  geom_smooth(data=dataset_dsk700, color='red', method="gam", mapping = aes(date_piq, temperature))+
  ggtitle('Temporal distribution of temperatures associated with reports and witnesses \n for the whole of France from 2017-04-01 to 2020-04-01')+
  xlab(label = 'Date')+
  ylab(label=' Temperature (°C)')+
  xlim( as.Date('2017-04-01'), as.Date('2020-04-01') )+
  theme(plot.title = element_text(hjust = .5, face = 'bold', size = 14))+
  theme(axis.title.x = element_text(face = 'bold', size = 12))+
  theme(axis.title.y = element_text(face = 'bold', size = 12))

ggsave("ggplot_temperature_gam.png")

#♥ 3. Variable : temperaturehigh

## 3.2 méthode gam

ggplot(humadata, aes(x=date_piq))+
  geom_point(aes(y=temperaturehigh), color='blue', size=.2, alpha=.7)+
  geom_smooth(aes(y=temperaturehigh), color='blue', method="gam")+
  geom_point(data = dataset_dsk700, aes(date_piq, temperaturehigh), color='red', size=.2, alpha=.7)+
  geom_smooth(data=dataset_dsk700, color='red', method="gam", mapping = aes(date_piq, temperaturehigh))+
  ggtitle('Temporal distribution of high temperature associated with reports and \n witnesses for the whole of France from 2017-04-01 to 2020-04-01')+
  xlab(label = 'Date')+
  ylab(label=' High temperature (°C)')+
  xlim( as.Date('2017-04-01'), as.Date('2020-04-01') )+
  theme(plot.title = element_text(hjust = .5, face = 'bold', size = 14))+
  theme(axis.title.x = element_text(face = 'bold', size = 12))+
  theme(axis.title.y = element_text(face = 'bold', size = 12))

ggsave("ggplot_temperaturehigh_gam.png")

#♥ 4. Variable : humidity

## 4.2 méthode gam

ggplot(humadata, aes(x=date_piq))+
  geom_point(aes(y=humidity), color='blue', size=.2, alpha=.7)+
  geom_smooth(aes(y=humidity), color='blue', method="gam")+
  geom_point(data = dataset_dsk700, aes(date_piq, humidity*100), color='red', size=.2, alpha=.7)+
  geom_smooth(data=dataset_dsk700, color='red', method="gam", mapping = aes(date_piq, humidity*100))+
  ggtitle('Temporal distribution of humidity associated with reports and \n witnesses for the whole of France from 2017-04-01 to 2020-04-01')+
  xlab(label = 'Date')+
  ylab(label=' humidity (%)')+
  xlim( as.Date('2017-04-01'), as.Date('2020-04-01') )+
  theme(plot.title = element_text(hjust = .5, face = 'bold', size = 14))+
  theme(axis.title.x = element_text(face = 'bold', size = 12))+
  theme(axis.title.y = element_text(face = 'bold', size = 12))

ggsave("ggplot_humidity_gam.png")


#♥ 5. Variable : dewpoint

## 5.2 méthode gam

ggplot(humadata, aes(x=date_piq))+
  geom_point(aes(y=dewpoint), color='blue', size=.2, alpha=.7)+
  geom_smooth(aes(y=dewpoint), color='blue', method="gam")+
  geom_point(data = dataset_dsk700, aes(date_piq, dewpoint), color='red', size=.2, alpha=.7)+
  geom_smooth(data=dataset_dsk700, color='red', method="gam", mapping = aes(date_piq, dewpoint))+
  ggtitle('Temporal distribution of dewpoint associated with reports and \n witnesses for the whole of France from 2017-04-01 to 2020-04-01')+
  xlab(label = 'Date')+
  ylab(label=' Dewpoint (°C)')+
  xlim( as.Date('2017-04-01'), as.Date('2020-04-01') )+
  theme(plot.title = element_text(hjust = .5, face = 'bold', size = 14))+
  theme(axis.title.x = element_text(face = 'bold', size = 12))+
  theme(axis.title.y = element_text(face = 'bold', size = 12))

ggsave("ggplot_dewpoint_gam.png")


#♥ 6. Variable : pressure

## 6.2 méthode gam

ggplot(humadata, aes(x=date_piq))+
  geom_point(aes(y=pressure), color='blue', size=.2, alpha=.7)+
  geom_smooth(aes(y=pressure), color='blue', method="gam")+
  geom_point(data = dataset_dsk700, aes(date_piq, pressure), color='red', size=.2, alpha=.7)+
  geom_smooth(data=dataset_dsk700, color='red', method="gam", mapping = aes(date_piq, pressure))+
  ggtitle('Temporal distribution of pressure associated with reports and \n witnesses for the whole of France from 2017-04-01 to 2020-04-01')+
  xlab(label = 'Date')+
  ylab(label=' Atmospheric Pressure (hPa)')+
  xlim( as.Date('2017-04-01'), as.Date('2020-04-01') )+
  theme(plot.title = element_text(hjust = .5, face = 'bold', size = 14))+
  theme(axis.title.x = element_text(face = 'bold', size = 12))+
  theme(axis.title.y = element_text(face = 'bold', size = 12))

ggsave("ggplot_pressure_gam.png")

#♥ 7. Variable : windspeed

## 7.2 méthode gam

ggplot(humadata, aes(x=date_piq))+
  geom_point(aes(y=windspeed), color='blue', size=.2, alpha=.7)+
  geom_smooth(aes(y=windspeed), color='blue', method="gam")+
  geom_point(data = dataset_dsk700, aes(date_piq, windspeed), color='red', size=.2, alpha=.7)+
  geom_smooth(data=dataset_dsk700, color='red', method="gam", mapping = aes(date_piq, windspeed))+
  ggtitle('Temporal distribution of windspeed associated with reports and \n witnesses for the whole of France from 2017-04-01 to 2020-04-01')+
  xlab(label = 'Date')+
  ylab(label=' Windspeed (m/s)')+
  xlim( as.Date('2017-04-01'), as.Date('2020-04-01') )+
  theme(plot.title = element_text(hjust = .5, face = 'bold', size = 14))+
  theme(axis.title.x = element_text(face = 'bold', size = 12))+
  theme(axis.title.y = element_text(face = 'bold', size = 12))

ggsave("ggplot_windspeed_gam.png")


#♥ 8. Variable : visibility

## 8.2 méthode gam

ggplot(humadata, aes(x=date_piq))+
  geom_point(aes(y=visibility), color='blue', size=.2, alpha=.7)+
  geom_smooth(aes(y=visibility), color='blue', method="gam")+
  geom_point(data = dataset_dsk700, aes(date_piq, visibility), color='red', size=.2, alpha=.7)+
  geom_smooth(data=dataset_dsk700, color='red', method="gam", mapping = aes(date_piq, visibility))+
  ggtitle('Temporal distribution of visibility associated with reports and \n witnesses for the whole of France from 2017-04-01 to 2020-04-01')+
  xlab(label = 'Date')+
  ylab(label=' Visibility (km)')+
  xlim( as.Date('2017-04-01'), as.Date('2020-04-01') )+
  theme(plot.title = element_text(hjust = .5, face = 'bold', size = 14))+
  theme(axis.title.x = element_text(face = 'bold', size = 12))+
  theme(axis.title.y = element_text(face = 'bold', size = 12))

ggsave("ggplot_visibility_gam.png")


#♥ 9. Variable : cloudcover

## 9.2 méthode gam

ggplot(humadata, aes(x=date_piq))+
  geom_point(aes(y=cloudcover), color='blue', size=.2, alpha=.7)+
  geom_smooth(aes(y=cloudcover), color='blue', method="gam")+
  geom_point(data = dataset_dsk700, aes(date_piq, cloudcover*100), color='red', size=.2, alpha=.7)+
  geom_smooth(data=dataset_dsk700, color='red', method="gam", mapping = aes(date_piq, cloudcover*100))+
  ggtitle('Temporal distribution of cloudcover associated with reports and \n witnesses for the whole of France from 2017-04-01 to 2020-04-01')+
  xlab(label = 'Date')+
  ylab(label=' Cloud cover (%)')+
  xlim( as.Date('2017-04-01'), as.Date('2020-04-01') )+
  theme(plot.title = element_text(hjust = .5, face = 'bold', size = 14))+
  theme(axis.title.x = element_text(face = 'bold', size = 12))+
  theme(axis.title.y = element_text(face = 'bold', size = 12))

ggsave("ggplot_cloudcover_gam.png")

#♥ 10. Variable : windgust

## 10.2 méthode gam

ggplot(humadata, aes(x=date_piq))+
  geom_point(aes(y=windgust), color='blue', size=.2, alpha=.7)+
  geom_smooth(aes(y=windgust), color='blue', method="gam")+
  geom_point(data = dataset_dsk700, aes(date_piq, windgust), color='red', size=.2, alpha=.7)+
  geom_smooth(data=dataset_dsk700, color='red', method="gam", mapping = aes(date_piq, windgust))+
  ggtitle('Temporal distribution of windgust associated with reports and \n witnesses for the whole of France from 2017-04-01 to 2020-04-01')+
  xlab(label = 'Date')+
  ylab(label=' Wind gust (m/s)')+
  xlim( as.Date('2017-04-01'), as.Date('2020-04-01') )+
  theme(plot.title = element_text(hjust = .5, face = 'bold', size = 14))+
  theme(axis.title.x = element_text(face = 'bold', size = 12))+
  theme(axis.title.y = element_text(face = 'bold', size = 12))

ggsave("ggplot_windgust_gam.png")

#♥ 11. Variable : uvindex

## 11.2 méthode gam

ggplot(humadata, aes(x=date_piq))+
  geom_point(aes(y=uvindex), color='blue', size=.2, alpha=.7)+
  geom_smooth(aes(y=uvindex), color='blue', method="gam")+
  geom_point(data = dataset_dsk700, aes(date_piq, uvindex), color='red', size=.2, alpha=.7)+
  geom_smooth(data=dataset_dsk700, color='red', method="gam", mapping = aes(date_piq, uvindex))+
  ggtitle('Temporal distribution of uvindex associated with reports and \n witnesses for the whole of France from 2017-04-01 to 2020-04-01')+
  xlab(label = 'Date')+
  ylab(label=' UV index ')+
  xlim( as.Date('2017-04-01'), as.Date('2020-04-01') )+
  theme(plot.title = element_text(hjust = .5, face = 'bold', size = 14))+
  theme(axis.title.x = element_text(face = 'bold', size = 12))+
  theme(axis.title.y = element_text(face = 'bold', size = 12))

ggsave("ggplot_uvindex_gam.png")



#Explication détaillée du graphique
#la date de la piqûre est en abscisse
#la donnée principale est tirée du dataframe humadata qui représente la table de donnée de signalements
# citik_humains_clean_weather_strict.csv
#dans mes script je travaille directememnt sur la base de donnée géographique postgis
#la donnée météo témoin dataset_dsk700 provient de la table darksky_maille_700_avg
#le grahique est établi à partir de la colonne temperature représentat
#la température moyenne obtenue en moyennant temphigh et templow
ggplot(humadata, aes(x=date_piqure_saisie))+
  #c'est la ligne qui affiche les poctuels des signalements
  geom_point(aes(y=temperature), color='blue', size=.2, alpha=.7)+
  #Cette ligne établit la courbe lisse noire des poinctuels
  #par défaut elle utilise la méthode GAM ou general additive method si le nombre de points est
  #supérieur à 1000, en utilisant en arrière plan la méthode method="gam", formula = y ~ s(x)
  #comme paramètre, donc la fonction s(x) du packet R mgcv
  # pour plus de détail consultez les références que je vous avais envoyés dans les mails précédents
  #autrement l'explication de la méthode additive est en dehors du sujet de l'article
  geom_smooth(aes(y=temperature), color='black')+
  #cette lingne de code établit la ligne verte de la température témoin
  geom_line( data = dataset_dsk700, aes(date_releve, temperature), color='green')+
  #Meme chose que ci-dessus mais courebe lisse rouge de la donnée météo, donc température témoin
  geom_smooth(data=dataset_dsk700, color='red', mapping = aes(date_releve, temperature))+
  #titre du graph
  ggtitle('Comparaison between vartiations in time of signaling vs witness temperatures \n from 2018-03-01 to 2018-10-01')+
  #Le reste sont les thèmes et labels des axes
  xlab(label = 'Date')+
  ylab(label=' Temperature degrees Celsius')+
  #Cette ligne est facultative, elle sert uniquement au cas où on a besoin
  #de zoom sur une période de l'année
  xlim( as.Date('2018-03-01'), as.Date('2018-10-01') )+
  #le thème du graphique
  theme(plot.title = element_text(hjust = .5, face = 'bold', size = 14))+
  theme(axis.title.x = element_text(face = 'bold', size = 12))+
  theme(axis.title.y = element_text(face = 'bold', size = 12))
