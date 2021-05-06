################## Library calls and preliminary database connection ####################################
getwd()
setwd("./")

require(ggplot2)
require(RPostgreSQL)
require(MASS)
library(mgcv)

####### Getting data from postgres database for citik experimentation

drv <- PostgreSQL()
con <- dbConnect(drv, dbname= 'localbase10', user='beetroot', password='root', port=5432, host='localhost')
query <- 'SELECT * FROM citik.citik_humains_clean_weather_strict'
res <- dbSendQuery(con, query)
dataset <- fetch(res, n=-1)

datasubset <- dataset[dataset$nbr_tique < 25 & dataset$sex_pique != '',]


#-------------------------------- Breakdown of number of ticks collected ---------------------------------#

############# Summary of the Number of colleced ticks aggregate #################

query_aggnbr_tique <-"select sex_pique, sum(nbr_tique) from citik.citik_humains_clean_weather_strict where sex_pique != '' group by sex_pique"
res <- dbSendQuery(con, query_aggnbr_tique)
aggnbr_tique <- fetch(res, n=-1)

ggplot(aggnbr_tique, aes(sex_pique, sum))+
  geom_bar(stat = "identity")

ggplot(datasubset, aes(sex_pique))+
  geom_bar()

ggplot(datasubset, aes(nbr_tique, color=sex_pique))+
  geom_bar()+
  facet_wrap(~sex_pique)

ggplot(datasubset, aes(nbr_tique))+
  geom_density(color='blue')

ggplot(datasubset, aes(nbr_tique, colour=sex_pique))+
  geom_density()+
  facet_wrap(~sex_pique, ncol = 1)

ggplot(datasubset, aes(sex_pique, nbr_tique, colour=sex_pique))+
  geom_violin()

ggplot(datasubset, aes(sex_pique, nbr_tique, colour=sex_pique))+
  geom_boxplot()

################## Time series analysis using point distribution #################

ggplot(datasubset, aes(date_piqure_saisie, nbr_tique))+
  geom_line(color='blue')

ggplot(datasubset, aes(date_piqure_saisie, nbr_tique, colour=sex_pique))+
  geom_point(size=.2)

ggplot(datasubset, aes(date_piqure_saisie))+
  geom_dotplot()

ggplot(datasubset, aes(date_piqure_saisie, nbr_tique, colour=sex_pique))+
  geom_jitter(aes(size=nbr_tique), alpha=.4)+
  geom_smooth(aes(weight = nbr_tique), method = lm, size = 1)

ggplot(datasubset, aes(date_piqure_saisie, nbr_tique, colour=sex_pique))+
  geom_jitter(aes(size=nbr_tique), alpha=.4)+
  facet_wrap(~region)

ggplot(datasubset, aes(date_piqure_saisie, nbr_tique, colour=sex_pique))+
  geom_point(size=.2)+
  facet_wrap(~departement)

ggplot(datasubset, aes(date_piqure_saisie, nbr_tique, colour=sex_pique))+
  geom_point(size=.2)+
  facet_wrap(~environnement)

ggplot(datasubset, aes(date_piqure_saisie, nbr_tique, colour=sex_pique))+
  geom_point(size=.2)+
  facet_wrap(~raison_presence)

ggplot(datasubset, aes(date_piqure_saisie, nbr_tique, colour=sex_pique))+
  geom_point(size=.2)+
  facet_wrap(~icon)

################## Time series analysis using line geometry distribution #################
ggplot(datasubset, aes(date_piqure_saisie, nbr_tique))+
  geom_line()

################## Time series analysis using density geometry distribution #################
ggplot(datasubset, aes(date_piqure_saisie, fill=sex_pique, color=sex_pique))+
  geom_density(alpha=.2)+
  facet_wrap(~sex_pique, ncol = 1)

ggplot(datasubset, aes(date_piqure_saisie, colour=sex_pique))+
  geom_freqpoly(binwidth = 5)+
  facet_wrap(~sex_pique, ncol = 1)

ggplot(datasubset, aes(date_piqure_saisie, colour=sex_pique))+
  geom_area()+
  facet_wrap(~sex_pique, ncol = 1)

ggplot(datasubset, aes(date_piqure_saisie, colour=sex_pique))+
  geom_histogram(binwidth = 1, alpha=.7)+
  facet_wrap(~sex_pique, ncol = 1)

ggplot(datasubset, aes(date_piqure_saisie, colour=sex_pique))+
  geom_histogram(binwidth = 5, alpha=.7)+
  facet_wrap(~region)

ggplot(datasubset, aes(date_piqure_saisie, colour=sex_pique))+
  geom_histogram(binwidth = 5, alpha=.7)+
  facet_wrap(~departement)

ggplot(datasubset, aes(date_piqure_saisie, colour=sex_pique))+
  geom_histogram(binwidth = 5, alpha=.7)+
  facet_wrap(~raison_presence)

ggplot(datasubset, aes(date_piqure_saisie, colour=sex_pique))+
  geom_histogram(binwidth = 5, alpha=.7)+
  facet_wrap(~age)


ggplot(datasubset, aes(date_piqure_saisie, colour=sex_pique))+
  geom_histogram(binwidth = 5, alpha=.7)+
  facet_wrap(~precision_geo)


ggplot(datasubset, aes(date_piqure_saisie, colour=sex_pique))+
  geom_histogram(binwidth = 5, alpha=.7)+
  facet_wrap(~environnement)

ggplot(datasubset, aes(environnement, date_piqure_saisie, colour=sex_pique))+
  geom_violin()

ggplot(datasubset, aes(environnement, date_piqure_saisie, colour=sex_pique))+
  geom_boxplot()


################# Study of regional effect on weather parameters #####################

ggplot(datasubset, aes(date_piqure_saisie, humidity, colour=sex_pique))+
  geom_point(size=.2)+
  facet_wrap(~region)+
  geom_smooth()

ggplot(datasubset, aes(date_piqure_saisie, temperaturelow, colour=sex_pique))+
  geom_point(size=.2)+
  facet_wrap(~region)+
  geom_smooth(span=1)

ggplot(datasubset, aes(date_piqure_saisie, temperaturehigh, colour=sex_pique))+
  geom_point(size=.2)+
  facet_wrap(~region)+
  geom_smooth(span=1)

ggplot(datasubset, aes(date_piqure_saisie, pressure, colour=sex_pique))+
  geom_point(size=.2)+
  facet_wrap(~region)+
  geom_smooth(span=1)

ggplot(datasubset, aes(date_piqure_saisie, windgust, colour=sex_pique))+
  geom_point(size=.2)+
  facet_wrap(~region)+
  geom_smooth(span=1)

ggplot(datasubset, aes(date_piqure_saisie, dewpoint, colour=sex_pique))+
  geom_point(size=.2)+
facet_wrap(~region)+
  geom_smooth(span=1)


#############################  Average weather data analysis Mainland France & Corsica ##############################


#############################  dsk synop 42  ##############################
query_dsk42 <- 'SELECT * FROM meteo.darksky_synop42_avg'
res <- dbSendQuery(con, query_dsk42)
dataset_dsk42 <- fetch(res, n=-1)

ggplot(dataset_dsk42, aes(date_releve, humidity))+
  geom_line(color='blue')+
  geom_smooth()

ggplot(dataset_dsk42, aes(date_releve, temperature))+
  geom_line(color='blue')+
  geom_smooth()

ggplot(dataset_dsk42, aes(date_releve, pressure))+
  geom_line(color='blue')+
  geom_smooth()



#############################  dsk synop 700 ##############################
query_dsk700 <- 'SELECT * FROM meteo.darksky_maille_700_avg'
res <- dbSendQuery(con, query_dsk700)
dataset_dsk700 <- fetch(res, n=-1)

ggplot(dataset_dsk700, aes(date_releve, humidity))+
  geom_line(color='blue')+
  geom_smooth()

ggplot(dataset_dsk700, aes(date_releve, temperature))+
  geom_line(color='blue')+
  geom_smooth()

ggplot(dataset_dsk700, aes(date_releve, pressure))+
  geom_line(color='blue')+
  geom_smooth()


#############################  meteo france synop 42  ##############################
query_mf42 <- 'SELECT *, floor(humidite) as humround FROM meteo.mf_synop42_avg'
res <- dbSendQuery(con, query_mf42)
dataset_mf42 <- fetch(res, n=-1)


ggplot(dataset_mf42, aes(date_iso, humround))+
  geom_line(color='blue')+
  geom_smooth()

ggplot(dataset_mf42, aes(date_iso, temperature))+
  geom_line(color='blue')+
  geom_smooth()

ggplot(dataset_mf42, aes(date_iso, press_mer))+
  geom_line(color='blue')+
  geom_smooth()


exagerated_subset <- dataset[dataset$nbr_tique > 15 & dataset$sex_pique != '',]


ggplot(exagerated_subset, aes(sex_pique))+geom_bar()

ggplot(exagerated_subset, aes(nbr_tique, colour=sex_pique))+
  geom_freqpoly(binwidth = 1)

ggplot(exagerated_subset, aes(nbr_tique, colour=sex_pique))+
  geom_density()

ggplot(exagerated_subset, aes(sex_pique, nbr_tique, colour=sex_pique))+
  geom_violin()

ggplot(exagerated_subset, aes(sex_pique, nbr_tique, colour=sex_pique))+
  geom_boxplot()








