############################################## Traitement de la donnée pour les animaux ##############################################

setwd("./")
getwd()
########installation et activation des librairies nécessaires à l'importation et l'analyse de la BDD#######
########Installer puis activer le package de gestion de base de donée postgresql 
library (survey)

######## compilation du code de connexion ####
library("RPostgreSQL")
#drv <-dbDriver("PostgreSQL")
con <- dbConnect(PostgreSQL()
                 , host="127.0.0.1"
                 , port="5432"
                 , user= "beetroot"
                 , password="root"
                 , dbname="localbase10")
##############################################

# lisTab <- dbListTables(con)
# lisTab

# Donnée darksky
wadata <- as.data.frame(dbGetQuery(con, 
"
SELECT
 id
,lat
,lon
,date_piqure_saisie
,st_astext(geom)  as geom
,annee_extract
,qui_pique
,annee_extract
,sex_animal
,nbr_tique
,precision_geo
,environnement
,environnement_precision
,village
,ville
,chef_lieu
,departement
,departement_code
,region
,code_postal
,pays
,code_pays
,adresse_complette
,autre_information
,timezone
,time
,summary
,icon
,sunrisetime
,sunsettime
,moonphase
,precipintensity
,precipintensitymax
,precipintensitymaxtime
,precipaccumulation
,precipprobability
,temperaturehigh
,temperaturehightime
,temperaturelow
,temperaturelowtime
,dewpoint
,humidity
,pressure
,visibility
,windgust
,windgusttime
,windspeed
,windbearing
,cloudcover
,cloudcovererror
,uvindex
,uvindextime
,temperaturemin
,temperaturemintime
,temperaturemax
,temperaturemaxtime
,apparenttemperaturemin
,apparenttemperaturemintime
,apparenttemperaturemax
,apparenttemperaturemaxtime
,apparenttemperaturehigh
,apparenttemperaturehightime
,apparenttemperaturelow
,apparenttemperaturelowtime

FROM citik.citik_animaux_clean_weather_strict
;"
))

# str(wadata)

## données MF

# humidité
hdata <-  dbGetQuery(con, "SELECT * FROM  meteo.humidityDB order by date ; " )

# point de rosée
pdrData <-  dbGetQuery(con, "SELECT * FROM  meteo.ptroseDB  order by date ; " )

# pression
pdata <-  dbGetQuery(con, "SELECT * FROM  meteo.pressionDB  order by date ; " )

# vitesse du vent
vdata <-  dbGetQuery(con, "SELECT * FROM  meteo.vventDB  order by date ; " )

# visibilite horizontale
visData <-  dbGetQuery(con, "SELECT * FROM  meteo.visibiliteDB  order by date ; " )

# nebulosité relative
nebData <-  dbGetQuery(con, "SELECT * FROM  meteo.nebulositeDB  order by date ; " )

# rafale de vent
rafData <-  dbGetQuery(con, "SELECT * FROM  meteo.rafal_10minDB  order by date ; " )

# intensité de précipitation
precData <-  dbGetQuery(con, "SELECT * FROM  meteo.precip_01hDB  order by date ; " )

# précipitation max sur 24 h
prec24Data <-  dbGetQuery(con, "SELECT * FROM  meteo.precip_24hDB  order by date ; " )

#température journalière
# tempData <- dbGetQuery(con, "SELECT * FROM  meteo.tmpDB  order by date ; " )
# tempData2 <-  dbGetQuery(con, "SELECT * FROM  meteo.tmpOffset2DB  order by date ; " )

#température diurnes ( de 6 à 21 h)
tempDayData <- dbGetQuery(con, "SELECT * FROM  meteo.tempdayDB  order by date ; " )
tempDayData2 <-  dbGetQuery(con, "SELECT * FROM  meteo.tempdayOffset2DB  order by date ; " )

#température nocturnes ( de 18 à 21 h)
tempNightData <- dbGetQuery(con, "SELECT * FROM  meteo.tempnightDB  order by date ; " )
tempNightData2 <-  dbGetQuery(con, "SELECT * FROM  meteo.tempnightOffset2DB  order by date ; " )


##### code de déconnexion ####
dbDisconnect(con)
#inspection des jeux de données darksky
# ls(wadata)
# str(wadata)
# summary(wadata)
nra <- nrow(wadata)

#@@@@@@@@@@   Graphiques sectorisés  @@@@@@@@@@@@@@@#

# SEX
pie(table(wadata$sex_animal[wadata$sex_animal != '']), cex= 1.5, main = 'Sexe', cex.main=2, col = c("#FF8000", "#0080FF") )

# Paysage
pie(table(wadata$environnement[wadata$environnement != '']),
    cex= 1.5,
    main = 'Paysages',
    cex.main=2,
    col = c("#FF8000", "#0080FF", "#CCCC00", "#003801", "#073991","#AFFF07", "#EEFA55", "#13FFBB")
)

# Dominante météo
pie(table(wadata$icon[wadata$icon != '']),
    cex= 1.5,
    main = 'Dominante Météo',
    cex.main=2,
    col = c("#FF8000", "#0080FF", "#CCCC00", "#003801", "#AFFF07", "#EEFA55")
)

# précision géographique:
pie(table(wadata$precision_geo[wadata$precision_geo != '']),
    cex= 1.5,
    main = 'précision géographique de la déclaration',
    cex.main=2,
    col = c("#FF8000", "#0080FF", "#CCCC00")
)

# Animal piqué:
pie(table(wadata$qui_pique[wadata$qui_pique != '' ] ),
    cex= 1.5,
    main = 'Animal piqué',
    cex.main=2,
    col = c("#FF8000", "#0080FF", "#CCCC00", "#00ff00")
)

#@@@@@@@@@@   Histogramme UV @@@@@@@@@@@@@@@#

 
# Indice UV
barplot(table(wadata$uvindex),
        col="grey",
        main = paste("Fréquence des morsures par rayonnement UV (échelle de l'OMS 1-10) \n ",nra," signalements  animaux (France, 2017-20)"),
        ylab = "Fréquence",
        xlab = "rayonnement UV (échelle de 1-10) ")

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@   Comparatifs de courbes  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#



########################################### comparatif Humidité  #########################################
length(wadata$humidity)
length(hdata$humiditydb)

range(wadata$humidity, na.rm = 1)
range( (hdata$humiditydb), na.rm = 1 )

BR2 <- seq(from= 0, to= 100, by=1)
BR2
length(BR2)

y<-hist(wadata$humidity*100, breaks = BR2, freq=F,
        col="grey",
        cex=10,
        main = paste("Fréquence de signalements de piqûres de tiques en fonction de l'humidité\n (2017-2020),",nra,"  signalements  animaux (France, 2017-20)"),
        ylab = "Densité (Somme=1)",
        xlab = "Humidité (%)",
        ylim = c(0, .07),
        xlim = c(0, 100),
        cex.main = 1.3,
        cex.lab = 1.5,
        cex.axis = 1.5
)

### courbe non lissée
HH2 <- hist((hdata$humiditydb), breaks = BR2,  plot = F)
lines(HH2$mids, HH2$density, lwd = 2, col = "green")

### courbe lissée
lines(density((hdata$humiditydb), na.rm = 1), lwd = 2, col = "red") 

text(90, 0.06, paste("N =",nra," signalements" ), cex = 1,  col = "black")
text(55, 0.05, paste("M = Mesure Nationale" ), cex = 1 ,  col = "red")

sum(y$density)
sum(HH2$density)

########################################### comparatif point de rosée #########################################

## inspection du jeu de donnée
length(wadata$dewpoint)
length(pdrData$ptrosedb)

range(wadata$dewpoint, na.rm = TRUE)
range( (pdrData$ptrosedb), na.rm = 1 )

Br3 <- seq(from=-16 , to=23 , by=1)
Br3
length(Br3)

z<-hist(wadata$dewpoint, breaks = Br3, freq=F,
        col="grey",
        main = paste("Fréquence des morsures par point de rosée (°C) \n ",nra," signalements  animaux (France, 2017-20)"),
        ylab = "Denisté  (Somme=1)",
        xlab = "point de rosée (°C)",
        ylim = c(0,.15),
        cex.main = 1.3,
        cex.lab = 1.5,
        cex.axis = 1.5
)

### courbe non lissée
HH3 <- hist((pdrData$ptrosedb), breaks = Br3,  plot = F)
lines(HH3$mids, HH3$density, lwd = 2, col = "green")

### courbe lissée
lines(density((pdrData$ptrosedb), na.rm = 1), lwd = 2, col = "red")

text(25, 0.10, paste("N = ",nra," signalements" ), cex = 1,  col = "black")
text(04, 0.10, paste("M = Mesure Nationale" ), cex = 1 ,  col = "red")

sum(z$density)
sum(HH3$density)

########################################### comparatif pression atmosphérique #########################################

length(wadata$pressure)
length(pdata$pressiondb)

range(wadata$pressure, na.rm = TRUE)
range( (pdata$pressiondb)/100, na.rm = 1 ) # division par 100 pour obtenir la valeur en hPa.

Br4 <- seq(from=950 , to=1050 , by=1)
Br4
length(Br4)

a<-hist(wadata$pressure, breaks = Br4, freq=F,
        col="grey",
        main = paste("Fréquence des morsures par pression athmosphérique (hPa) \n ",nra," signalements  animaux (France, 2017-20)"),
        ylab = "Denisté  (Somme=1)",
        xlab = "pression athmosphérique (hPa)",
        ylim = c(0, .1),
        xlim = c(970, 1050),
        cex.main = 1.3,
        cex.lab = 1.5,
        cex.axis = 1.5
)

### courbe non lissée
HH4 <- hist((pdata$pressiondb)/100, breaks = Br4,  plot = F)
lines(HH4$mids, HH4$density, lwd = 2, col = "green")

### courbe lissée
lines(density((pdata$pressiondb)/100, na.rm = 1), lwd = 2, col = "red")

text(1030, 0.3, paste("N = ",nra," signalements" ), cex = 1,  col = "black")
text(1010, 0.10, paste("M = Mesure Nationale" ), cex = 1 ,  col = "red")

sum(a$density)
sum(HH4$density)

########################################### comparatif visibilité horizontale en km #########################################

length(wadata$visibility) # en km
length(visData$visibilitedb) # en m

range((wadata$visibility), na.rm = 1)
range( (visData$visibilite/1000), na.rm = 1 )

Br6 <- seq(from=0 , to=37 , by=1)
Br6
length(Br6)

c<-hist((wadata$visibility), breaks = Br6, freq=F,
        col="grey",
        main = paste("Fréquence des morsures par visibilité athmosphérique horizontale (km) \n ",nra," signalements  animaux (France, 2017-20)"),
        ylab = "Densité (Somme=1)",
        xlab = "visibilité athmosphérique horizontale (km)",
        ylim = c(0,.30),
        xlim = c(0,40),
        cex.main = 1.3,
        cex.lab = 1.5,
        cex.axis = 1.5
)

### courbe non lissée
HH6 <- hist((visData$visibilitedb)/1000, breaks = Br6,  plot = F)
lines(HH6$mids, HH6$density, lwd = 2, col = "green")

### courbe lissée
lines(density((visData$visibilitedb)/1000, na.rm = 1), lwd = 2, col = "red")

text(22, 0.4, paste("N = ",nra," signalements" ), cex = 1,  col = "black")
text(20, 0.1, paste("M = Mesure Nationale" ), cex = 1 ,  col = "red")

sum(c$density)
sum(HH6$density)

########################################### comparatif couvert nuageux (nebulosité relative) #########################################

length(wadata$cloudcover)
length(nebData$nebulositedb)

range(wadata$cloudcover, na.rm = 1)*100
range( (nebData$nebulositedb), na.rm = 1 )

Br7 <- seq(from=0 , to=100 , by=1)
Br7
length(Br7)

d<-hist((wadata$cloudcover)*100, breaks = Br7, freq=F,
        col="grey",
        main = paste("Fréquence des morsures par couvert nuageux (%) \n ",nra," signalements  animaux (France, 2017-20)"),
        ylab = "Denisté  (Somme=1)",
        xlab = "couvert nuageux (%)",
        ylim = c(0,.07),
        xlim = c(0,100),
        cex.main = 1.3,
        cex.lab = 1.5,
        cex.axis = 1.5
)

### courbe non lissée
HH7 <- hist((nebData$nebulositedb), breaks = Br7,  plot = F)
lines(HH7$mids, HH7$density, lwd = 2, col = "green")

### courbe lissée
lines(density((nebData$nebulositedb), na.rm = 1), lwd = 2, col = "red")

text(50, 0.04, paste("N = ",nra," signalements" ), cex = 1,  col = "black")
text(75, 0.05, paste("M = Mesure Nationale" ), cex = 1 ,  col = "red")

sum(d$density)
sum(HH7$density)

########################################### comparatif vitesse du vent m/s #########################################

length(wadata$windspeed)
length(vdata$vventdb)

range(wadata$windspeed, na.rm = 1)
range( (vdata$vventdb), na.rm = 1 )

Br5 <- seq(from=0 , to=14 , by=.1)
Br5
length(Br5)

b<-hist(wadata$windspeed, breaks = Br5, freq=F,
        col="grey",
        main = paste("Fréquence des morsures par vitesse du vent (m/s) \n ",nra," signalements  animaux (France, 2017-20)"),
        ylab = "Denisté  (Somme=10)",
        xlab = "vitesse du vent (m/s)",
        ylim = c(0,.6),
        xlim = c(0,13),
        cex.main = 1.3,
        cex.lab = 1.5,
        cex.axis = 1.5
)

### courbe non lissée
HH5 <- hist((vdata$vventdb), breaks = Br5,  plot = F)
lines(HH5$mids, HH5$density, lwd = 2, col = "green")

### courbe lissée
lines(density((vdata$vventdb), na.rm = 1), lwd = 2, col = "red")

text(6, 0.6, paste("N = ",nra," signalements" ), cex = 1,  col = "black")
text(6, 0.3, paste("M = Mesure Nationale" ), cex = 1 ,  col = "red")

sum(b$density)
sum(HH5$density)

########################################### comparatif rafale de vent #########################################

length(wadata$windgust) # km/h
length(rafData$rafal_10mindb)

range(wadata$windgust, na.rm = 1)
range( (rafData$rafal_10mindb), na.rm = 1 )

Br8 <- seq(from= 0, to= 50 , by=1)
Br8
length(Br8)

e<-hist(wadata$windgust, 
        breaks = Br8, 
        freq=F,
        col="grey",
        main = paste("Fréquence des morsures par rafale de vent (m/s) \n ",nra," signalements  animaux (France, 2017-20)"),
        ylab = "Denisté  (Somme=1)",
        xlab = "rafale de vent (m/s)",
        ylim = c(0,.3),
        cex.main = 1.3,
        cex.lab = 1.5,
        cex.axis = 1.5
)

## courbe non lissée
HH8 <- hist((rafData$rafal_10mindb), breaks = Br8,  plot = F)
lines(HH8$mids, HH8$density, lwd = 2, col = "green")

### courbe lissée
lines(density((rafData$rafal_10mindb), na.rm = 1), lwd = 2, col = "red")

text(30, 0.20, paste("N = ",nra," signalements" ), cex = 1,  col = "black")
text(10, 0.10, paste("M = Mesure Nationale" ), cex = 1 ,  col = "red")

sum(e$density)
sum(HH8$density)

########################################### comparatif de l'intensité de précipitation sur 1 h  #########################################

length(wadata$precipintensity) # mm/h
length(precData$precip_01hdb) # mm/h

range(wadata$precipintensity, na.rm = 1 )
range( (precData$precip_01hdb), na.rm = 1 )

Br9 <- seq(from=-1 , to=16 , by=.01)
Br9
length(Br9)

f<-hist(wadata$precipintensity[wadata$precipintensity > 0.01], 
        breaks = Br9, 
        freq=F,
        col="grey",
        main = paste("Fréquence des morsures par intensité de précipitation (mm/h) \n ",nra," signalements  animaux (France, 2017-20) \n (les valeurs 0 sont exlues car très nombreuses)"),
        ylab = "Denisté  (Somme=100)",
        xlab = "intensité de précipitation (mm/h)",
        ylim = c(0,20),
        xlim = c(0,1),
        cex.main = 1.5,
        cex.lab = 1.5,
        cex.axis = 1.5
)

### courbe lissée
lines(density((precData$precip_01hdb), na.rm = 1), lwd = 2, col = "red")

### courbe non lissée
HH9 <- hist((precData$precip_01hdb), breaks = Br9,  plot = F)
lines(HH9$mids, HH9$density, lwd = 2, col = "green")

text(0.8, 15, paste("N = ",nra," signalements" ), cex = 1,  col = "black")
text(.4, 5, paste("M = Mesure Nationale" ), cex = 1 ,  col = "red")

sum(f$density)
sum(HH9$density)

########################################### comparatif de l'intensité de précipitation sur 24 h  #########################################

length(wadata$precipintensitymax) # mm/h
length(prec24Data$precip_24hdb) # mm/h

range(wadata$precipintensitymax, na.rm = 1 )
range( (prec24Data$precip_24hdb), na.rm = 1 )

Br9 <- seq(from=-1 , to=72 , by=.01)
Br9
length(Br9)

f<-hist(wadata$precipintensitymax[wadata$precipintensitymax > 0.01], 
        breaks = Br9, 
        freq=F,
        col="grey",
        main = paste("Fréquence des morsures par intensité de précipitation sur 24 h (mm/h) \n ",nra," signalements  animaux (France, 2017-20) \n (les valeurs 0 sont exlues car très nombreuses)"),
        ylab = "Denisté  (Somme=100)",
        xlab = "intensité de précipitation (mm/h)",
        ylim = c(0,10),
        xlim = c(0,3),
        cex.main = 1.5,
        cex.lab = 1.5,
        cex.axis = 1.5
)

### courbe lissée
lines(density((prec24Data$precip_24hdb), na.rm = 1), lwd = 2, col = "red")

### courbe non lissée
HH9 <- hist((prec24Data$precip_24hdb), breaks = Br9,  plot = F)
lines(HH9$mids, HH9$density, lwd = 2, col = "green")

text(0.8, 15, paste("N = ",nra," signalements" ), cex = 1,  col = "black")
text(.4, 5, paste("M = Mesure Nationale" ), cex = 1 ,  col = "red")

sum(f$density)
sum(HH9$density)

#################################################### comparatif Température diurne ( de 6 à 21h ) #################################################### 
length(wadata$temperaturehigh)
length(tempDayData$tempdaydb)

range(wadata$temperaturehigh, na.rm = 1)
range(tempDayData$tempdaydb, na.rm = 1 )

BR10 <- seq(from= -4, to=42, by=1)
BR10
length(BR10)

### histogramme de température diurnes comparé:
g<-hist(wadata$temperaturehigh, breaks = BR10, freq=F,
        col="grey",
        main = paste("Fréquence de signalements de piqûres de tiques en fonction des températures \n diurnes ( de 06 à 21h ) , ",nra," signalements  animaux (France, 2017-20)"),
        ylab = "Denisté  (Somme=1)",
        xlab = "Températures diurnes (T°C)",
        ylim = c(0,.1),
        xlim = c(-10,40),
        cex.main = 1.3,
        cex.lab = 1.5,
        cex.axis = 1.5
)

### courbe non lissée
HH10 <- hist(tempDayData$tempdaydb, breaks = BR10,  plot=F)
lines(HH10$mids, HH10$density, lwd = 2, col = "green")

### courbe lissée, kernel
lines(density(tempDayData$tempdaydb, na.rm = 1), lwd = 2, col = "red")

text(30, 0.08, paste("N = ",nra," signalements" ), cex = 1,  col = "black")
text(05, 0.08, paste("M = Mesure Nationale" ), cex = 1 ,  col = "red")

sum(g$density)
sum(HH10$density)

#################################################### comparatif Température nocturnes (de 21 à 06h) ####################################################
length(wadata$temperaturelow)
length(tempNightData$tempnightdb )

range(wadata$temperaturelow, na.rm = 1)
range(tempNightData$tempnightdb , na.rm = 1 )

BR11 <- seq(from= -9, to= 30, by=1)
BR11
length(BR11)


### histogramme de température nocturnes comparé:

h<-hist(wadata$temperaturelow, breaks = BR11, freq=F,
        col="grey",
        main = paste("Fréquence de signalements de piqûres de tiques en fonction des températures \n nocturnes ( de  21 à 06h ) , ",nra," signalements  animaux (France, 2017-20)"),
        ylab = "Denisté  (Somme=1)",
        xlab = "Températures nocturnes (T°C)",
        ylim = c(0,.11),
        xlim = c(-10,30),
        cex.main = 1.3,
        cex.lab = 1.5,
        cex.axis = 1.5
)

### courbe non lissée
HH11 <- hist(tempNightData$tempnightdb, breaks = BR11,  plot=F)

lines(HH11$mids, HH11$density, lwd = 2, col = "green")

### courbe lissée, kernel
lines(density(tempNightData$tempnightdb, na.rm = 1), lwd = 2, col = "red")

text(23, 0.08, paste("N = ",nra," signalements" ), cex = 1,  col = "black")
text(05, 0.08, paste("M = Mesure Nationale" ), cex = 1 ,  col = "red")

sum(h$density)
sum(HH11$density)


############################################################# EOS ####################################################################

####### rgdal ######

# library(rgdal)
# library(sp)
# 
# dbname = "localbase10"
# host = "localhost"
# user = "beetroot"
# pass = "root"
# name = "citik.citik_animaux_clean" # Postgis table
# 
# dsn = paste0("PG:dbname='",dbname,"' host='",host,"' user='",user,"' password='",pass,"'")
# 
# res = readOGR(dsn, name)
# type(res)
# library(ggplot2)
# ggplot2(res)
# 
# require(rgeos)
# require(sf)
#   poly=readWKT(testwadata)
# 
# proj4string(poly) = CRS("+init=epsg:3857")
# plot(res)
