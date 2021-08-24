#Scirpte avec commentaires explicant la construction des graphiques de séries temporelles

# Le repertoire de travail est le repertoire courant
setwd('./')
getwd()

# appel des bibliotheques
require(ggplot2)
require(mgcv)

# Import de la donnee

data_signalements <- read.csv("../../../data/donnee_signalements_avec_meteo_dsk/citik_humains_clean_weather_strict.csv",
                       header=TRUE,
                       sep=",",
                       dec=".",
                       quote = "'",
                       stringsAsFactors = FALSE
                       )

# On ne garde que les signalements dont le nombre de tiques est inferieur a vingt-cinq et dont le sex
# declarant est connu
datasubset <-  data_signalements[data_signalements$nbr_tique < 25 & data_signalements$sex_pique != '',]
  
dataset_dsk700 <- read.csv("../../../data/donnee_meteo_nationale_comparative/darksky/darksky_moyennes_journalieres_maille_700.csv",
                           header=TRUE,
                           sep=",",
                           dec=".",
                           quote = '"',
                           stringsAsFactors = FALSE
                           )


## Definition du theme des graphiaues (pour evitier les repetitions de code)
plotstyle <-  theme(plot.title = element_text(hjust = .5, face = 'bold', size = 14))+
              theme(axis.title = element_text(face = 'bold', size = 12))

# Code du Graphique complet de la comparaison temporelle des températures de signalement et témoins
# Le principe est le meme pour tout les paramètres qu'il suffirait de mettre à la place de la 
# variable temperature
ggplot(datasubset, aes(x=as.Date(date_piqure_saisie) ))+
  geom_point(aes(y=temperature), color='blue', size=.2, alpha=.7)+
  geom_smooth(aes(y=temperature), color='black')+
  geom_line( data = dataset_dsk700, aes(as.Date(as.Date(date_releve)), temperature), color='green')+
  geom_smooth(data=dataset_dsk700, color='red', mapping = aes(as.Date(as.Date(date_releve)), temperature))+
  # ggtitle('Comparaison between vartiations in time of signaling vs witness temperatures \n from 2018-03-01 to 2018-10-01')+
  xlab(label = 'Date')+
  ylab(label=' Temperature degrees Celsius')+
  # xlim( as.Date('2018-03-01'), as.Date('2018-10-01') )+ 
  theme(plot.title = element_text(hjust = .5, face = 'bold', size = 14))+
  theme(axis.title.x = element_text(face = 'bold', size = 12))+
  theme(axis.title.y = element_text(face = 'bold', size = 12))

#Explication détaillée du graphique
#la date de la piqûre est en abscisse
#la donnée principale est tirée du dataframe datasubset qui représente la table de donnée de signalements 
# citik_humains_clean_weather_strict.csv
#dans mes script je travaille directememnt sur la base de donnée géographique postgis
#la donnée météo témoin dataset_dsk700 provient de la table darksky_maille_700_avg
#le grahique est établi à partir de la colonne temperature représentat
#la température moyenne obtenue en moyennant temphigh et templow
ggplot( datasubset, aes(x=as.Date(date_piqure_saisie) ) )+
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
  geom_line( data = dataset_dsk700, aes(as.Date(date_releve), temperature), color='green')+
  #Meme chose que ci-dessus mais courbe lisse rouge de la donnée météo, donc température témoin
  geom_smooth(data=dataset_dsk700, color='red', mapping = aes(as.Date(date_releve), temperature))+
  #titre du graph
  ggtitle('Comparaison between vartiations in time of signaling vs witness temperatures \n from 2018-03-01 to 2018-10-01')+
  #Le reste sont les thèmes et labels des axes
  xlab(label = 'Date')+
  ylab(label=' Temperature degrees Celsius')+
  #Cette ligne est facultative, elle sert uniquement au cas où on a besoin 
  #de zoom sur une période de l'année
  # xlim( as.Date('2018-03-01'), as.Date('2018-10-01') )+
  #le thème du graphique
  theme(plot.title = element_text(hjust = .5, face = 'bold', size = 14))+
  theme(axis.title.x = element_text(face = 'bold', size = 12))+
  theme(axis.title.y = element_text(face = 'bold', size = 12))



##-------------------- Le reste des parametres -----------------------------------##


## temperature from 2018-03-01 to 2018-10-01
ggplot(datasubset, aes(as.Date(date_piqure_saisie)))+
  geom_point(aes(y=temperature), color='blue', size=.2, alpha=.7)+
  geom_smooth(aes(y=temperature), color='black')+
  geom_line( data = dataset_dsk700, aes(as.Date(date_releve), temperature), color='#00CC00')+
  geom_smooth(data=dataset_dsk700, color='red', mapping = aes(as.Date(date_releve), temperature))+
  ggtitle('Comparaison between vartiations in time of signaling vs witness temperatures \n from 2018-03-01 to 2018-10-01')+
  xlab(label = 'Date')+
  ylab(label=' Temperature degrees Celsius')+
  xlim( as.Date('2018-03-01'), as.Date('2018-10-01') )+
  plotstyle

# humidity
ggplot(datasubset, aes(as.Date(date_piqure_saisie)))+
  geom_point(aes(y=humidity), color='blue', size=.2, alpha=.7)+
  geom_smooth(aes(y=humidity), color='black')+
  geom_line( data = dataset_dsk700, aes(as.Date(date_releve), humidity), color='green')+
  geom_smooth(data=dataset_dsk700, color='red', mapping = aes(as.Date(date_releve), humidity))+
  ggtitle('Comparaison between vartiations in time of signaling vs witness humidity')+
  xlab(label = 'Date')+
  ylab(label=' Humidity %')+
  plotstyle

# dew point
ggplot(datasubset, aes(as.Date(date_piqure_saisie)))+
  geom_point(aes(y=dewpoint), color='blue', size=.2, alpha=.7)+
  geom_smooth(aes(y=dewpoint), color='black')+
  geom_line( data = dataset_dsk700, aes(as.Date(date_releve), dewpoint), color='green')+
  geom_smooth(data=dataset_dsk700, color='red', mapping = aes(as.Date(date_releve), dewpoint))+
  ggtitle('Comparaison between vartiations in time of signaling vs witness dew point')+
  xlab(label = 'Date')+
  ylab(label='Dew point degrees Celsius')+
  plotstyle

#Ultra Violet Index
ggplot(datasubset, aes(as.Date(date_piqure_saisie)))+
  geom_point(aes(y=uvindex), color='blue', size=.2, alpha=.7)+
  geom_smooth(aes(y=uvindex), color='black')+
  geom_line( data = dataset_dsk700, aes(as.Date(date_releve), uvindex), color='green')+
  geom_smooth(data=dataset_dsk700, color='red', mapping = aes(as.Date(date_releve), uvindex))+
  ggtitle('Comparaison between vartiations in time of signaling vs witness Ultra violet index')+
  xlab(label = 'Date')+
  ylab(label='Ultra violet index scale 0 to 10')+
  plotstyle


# Day temperature
ggplot(datasubset, aes(as.Date(date_piqure_saisie)))+
  geom_point(aes(y=temperaturehigh), color='blue', size=.2, alpha=.7)+
  geom_smooth(aes(y=temperaturehigh), color='black')+
  geom_line( data = dataset_dsk700, aes(as.Date(date_releve), temperaturehigh), color='green')+
  geom_smooth(data=dataset_dsk700, color='red', mapping = aes(as.Date(date_releve), temperaturehigh))+
  ggtitle('Comparaison between vartiations in time of signaling vs witness day temperatures')+
  xlab(label = 'Date')+
  ylab(label=' Day Temperature degrees Celsius')+
  plotstyle


# night Temperature
ggplot(datasubset, aes(as.Date(date_piqure_saisie)))+
  geom_point(aes(y=temperaturelow), color='blue', size=.2, alpha=.7)+
  geom_smooth(aes(y=temperaturelow), color='black')+
  geom_line( data = dataset_dsk700, aes(as.Date(date_releve), temperaturelow), color='green')+
  geom_smooth(data=dataset_dsk700, color='red', mapping = aes(as.Date(date_releve), temperaturelow))+
  ggtitle('Comparaison between vartiations in time of signaling vs witness night temperatures')+
  xlab(label = 'Date')+
  ylab(label=' Night Temperature degrees Celsius')+
  plotstyle


# Pressure
ggplot(datasubset, aes(as.Date(date_piqure_saisie)))+
  geom_point(aes(y=pressure), color='blue', size=.2, alpha=.7)+
  geom_smooth(aes(y=pressure), color='black')+
  geom_line( data = dataset_dsk700, aes(as.Date(date_releve),y=as.numeric(pressure)), color='green')+
  geom_smooth(data=dataset_dsk700, color='red', mapping = aes(as.Date(date_releve),y=as.numeric(pressure)))+
  ggtitle('Comparaison between vartiations in time of signaling vs witness pressure')+
  xlab(label = 'Date')+
  ylab(label=' pressure hPa')+
  plotstyle



