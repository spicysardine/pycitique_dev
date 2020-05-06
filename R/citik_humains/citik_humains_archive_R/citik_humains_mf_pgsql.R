setwd("./")
getwd()
########installation et activation des librairies nécessaires à l'importation et l'analyse de la BDD#######
########Installer puis activer le package de gestion de base de donée postgresql 
library(questionr)
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

#la requete a mis 3 min et 28 s pour renvoyer 1 million d'observations. 
wdata <- as.data.frame(dbGetQuery(con, 
                                  "
SELECT
 id
,lat
,lon
,date_piqure_saisie
,st_astext(geom)  as geom
,annee_extract
,sex_pique
,age
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
,autre_info
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

FROM citik.citik_humains_clean_weather_strict
"
) )


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

#température diurnes ( de 18 à 21 h)
tempNightData <- dbGetQuery(con, "SELECT * FROM  meteo.tempnightDB  order by date ; " )
tempNightData2 <-  dbGetQuery(con, "SELECT * FROM  meteo.tempnightOffset2DB  order by date ; " )

#inspection des jeux de données darksky
# ls(wdata)
# str(wdata)
# summary(wdata)
nr <- nrow(wdata)


#@@@@@@@@@@   Camemberts  @@@@@@@@@@@@@@@#


# SEX

pie(table(wdata$sex_pique[wdata$sex_pique != '']),  main = 'Sexe',  col = c("#FF8000", "#0080FF") )

# AGE

pie(table(wdata$age[wdata$age != '']),
    cex= 1.5,
    main = 'Âge',
    cex.main=2,
    clockwise = 1,
    col = c("#FF8000", "#0080FF", "#CCCC00", "#003801", "#AFFF07", "#EEFA55", "#13FFBB", "#5765AA", "#51ABCD")
)

# Paysage

pie(table(wdata$environnement[wdata$environnement != '']),
    cex= 1.5,
    main = 'Paysages',
    cex.main=2,
    col = c("#FF8000", "#0080FF", "#CCCC00", "#003801", "#AFFF07", "#EEFA55", "#13FFBB")
)

# Dominante météo

pie(table(wdata$icon[wdata$icon != '']),
    cex= 1.5,
    main = 'Dominante Météo',
    cex.main=2,
    col = c("#FF8000", "#0080FF", "#CCCC00", "#003801", "#AFFF07", "#EEFA55")
)

# précision géographique:

pie(table(wdata$precision_geo[wdata$precision_geo != '']),
    cex= 1.5,
    main = 'précision géographique de la déclaration',
    cex.main=2,
    col = c("#FF8000", "#0080FF", "#CCCC00")
)


#@@@@@@@@@@   Histogramme  @@@@@@@@@@@@@@@#


## Indice UV
barplot(table(wdata$uvindex),
        col="grey",
        main = paste("Fréquence des morsures par rayonnement UV (échelle de l'OMS 1-10) \n ",nr," signalements  humains (France HDTOM, 2017-20)"),
        ylab = "Fréquence",
        xlab = "rayonnement UV (échelle de 1-10) ")


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@   Comparatifs de courbes  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#



########################################### comparatif Humidité  #########################################

length(wdata$humidity)
length(hdata$humiditydb)

range(wdata$humidity, na.rm = 1)
range( (hdata$humiditydb), na.rm = 1 )

BR2 <- seq(from= 0, to= 100, by=1)
BR2
length(BR2)

y<-hist(wdata$humidity*100, breaks = BR2, freq=F,
        col="grey",
        main = paste("Fréquence de signalements de piqûres de tiques en fonction de l'humidité\n (2017-2020),",nr,"  signalements  humains (France HDTOM, 2017-20)"),
        ylab = "Densité (Somme=1)",
        xlab = "Humidité (%)",
        ylim = c(0, .07),
        xlim = c(0, 100),
        
)

### courbe non lissée
HH2 <- hist((hdata$humiditydb), breaks = BR2,  plot = F)
lines(HH2$mids, HH2$density, lwd = 2, col = "green")

### courbe lissée
lines(density((hdata$humiditydb), na.rm = 1), lwd = 2, col = "red") 

text(90, 0.06, paste("N =",nr," signalements" ), col = "black")
text(55, 0.05, paste("M = Mesure Nationale" ),  col = "red")


sum(y$density)
sum(HH2$density)

########################################### comparatif point de rosée #########################################

## inspection du jeu de donnée
length(wdata$dewpoint)
length(pdrData$ptrosedb)

range(wdata$dewpoint, na.rm = TRUE)
range( (pdrData$ptrosedb), na.rm = 1 )

Br3 <- seq(from=-12 , to=25 , by=1)
Br3
length(Br3)

z<-hist(wdata$dewpoint, breaks = Br3, freq=F,
        col="grey",
        main = paste("Fréquence des morsures par point de rosée (°C) \n ",nr," signalements  humains (France HDTOM, 2017-20)"),
        ylab = "Denisté  (Somme=1)",
        xlab = "point de rosée (°C)",
        ylim = c(0,.15),
        
)


### courbe non lissée
HH3 <- hist((pdrData$ptrosedb), breaks = Br3,  plot = F)
lines(HH3$mids, HH3$density, lwd = 2, col = "green")

### courbe lissée
lines(density((pdrData$ptrosedb), na.rm = 1), lwd = 2, col = "red")

text(25, 0.10, paste("N = ",nr," signalements" ), col = "black")
text(04, 0.10, paste("M = Mesure Nationale" ),  col = "red")

sum(z$density)
sum(HH3$density)

########################################### comparatif pression atmosphérique #########################################

length(wdata$pressure)
length(pdata$pressiondb)

range(wdata$pressure, na.rm = TRUE)
range( (pdata$pressiondb)/100, na.rm = 1 ) # division par 100 pour obtenir la valeur en hPa.

Br4 <- seq(from=950 , to=1050 , by=1)
Br4
length(Br4)

a<-hist(wdata$pressure, breaks = Br4, freq=F,
        col="grey",
        main = paste("Fréquence des morsures par pression athmosphérique (hPa) \n ",nr," signalements  humains (France HDTOM, 2017-20)"),
        ylab = "Denisté  (Somme=1)",
        xlab = "pression athmosphérique (hPa)",
        ylim = c(0, .1),
        xlim = c(970, 1050),
        
)

### courbe non lissée
HH4 <- hist((pdata$pressiondb)/100, breaks = Br4,  plot = F)
lines(HH4$mids, HH4$density, lwd = 2, col = "green")

### courbe lissée
lines(density((pdata$pressiondb)/100, na.rm = 1), lwd = 2, col = "red")

text(1030, 0.3, paste("N = ",nr," signalements" ),col = "black")
text(1010, 0.10, paste("M = Mesure Nationale" ), col = "red")

sum(a$density)
sum(HH4$density)

########################################### comparatif visibilité horizontale en km #########################################

length(wdata$visibility) # en km
length(visData$visibilitedb) # en m

range((wdata$visibility), na.rm = 1)
range( (visData$visibilitedb/1000), na.rm = 1 )

Br6 <- seq(from=0 , to=37 , by=1)
Br6
length(Br6)

c<-hist((wdata$visibility), breaks = Br6, freq=F,
        col="grey",
        main = paste("Fréquence des morsures par visibilité athmosphérique horizontale (km) \n ",nr," signalements  humains (France HDTOM, 2017-20)"),
        ylab = "Densité (Somme=1)",
        xlab = "visibilité athmosphérique horizontale (km)",
        ylim = c(0,.30),
        xlim = c(0,40),
        
)

### courbe non lissée
HH6 <- hist((visData$visibilitedb)/1000, breaks = Br6,  plot = F)
lines(HH6$mids, HH6$density, lwd = 2, col = "green")

### courbe lissée
lines(density((visData$visibilitedb)/1000, na.rm = 1), lwd = 2, col = "red")

text(22, 0.4, paste("N = ",nr," signalements" ),col = "black")
text(20, 0.1, paste("M = Mesure Nationale" ), col = "red")

sum(c$density)
sum(HH6$density)

########################################### comparatif couvert nuageux (nebulosité relative) #########################################

length(wdata$cloudcover)
length(nebData$nebulositedb)

range(wdata$cloudcover, na.rm = 1)*100
range( (nebData$nebulositedb), na.rm = 1 )

Br7 <- seq(from=0 , to=100 , by=1)
Br7
length(Br7)

d<-hist((wdata$cloudcover)*100, breaks = Br7, freq=F,
        col="grey",
        main = paste("Fréquence des morsures par couvert nuageux (%) \n ",nr," signalements  humains (France HDTOM, 2017-20)"),
        ylab = "Denisté  (Somme=1)",
        xlab = "couvert nuageux (%)",
        ylim = c(0,.07),
        xlim = c(0,100),
        
)

### courbe non lissée
HH7 <- hist((nebData$nebulositedb), breaks = Br7,  plot = F)
lines(HH7$mids, HH7$density, lwd = 2, col = "green")

### courbe lissée
lines(density((nebData$nebulositedb), na.rm = 1), lwd = 2, col = "red")

text(50, 0.04, paste("N = ",nr," signalements" ),col = "black")
text(75, 0.05, paste("M = Mesure Nationale" ), col = "red")

sum(d$density)
sum(HH7$density)

########################################### comparatif vitesse du vent m/s #########################################

length(wdata$windspeed)
length(vdata$vventdb)

range(wdata$windspeed, na.rm = 1)
range( (vdata$vventdb), na.rm = 1 )

Br5 <- seq(from=0 , to=13 , by=.1)
Br5
length(Br5)



b<-hist(wdata$windspeed, breaks = Br5, freq=F,
        col="grey",
        main = paste("Fréquence des morsures par vitesse du vent (m/s) \n ",nr," signalements  humains (France HDTOM, 2017-20)"),
        ylab = "Denisté  (Somme=10)",
        xlab = "vitesse du vent (m/s)",
        ylim = c(0,.6),
        xlim = c(0,13),
        
)

### courbe non lissée
HH5 <- hist((vdata$vventdb), breaks = Br5,  plot = F)
lines(HH5$mids, HH5$density, lwd = 2, col = "green")

### courbe lissée
lines(density((vdata$vventdb), na.rm = 1), lwd = 2, col = "red")

text(6, 0.6, paste("N = ",nr," signalements" ),col = "black")
text(6, 0.3, paste("M = Mesure Nationale" ), col = "red")


sum(b$density)
sum(HH5$density)

########################################### comparatif rafale de vent #########################################

length(wdata$windgust) # km/h
length(rafData$rafal_10mindb)

range(wdata$windgust, na.rm = 1)
range( (rafData$rafal_10mindb), na.rm = 1 )

Br8 <- seq(from= 0, to= 50 , by=1)
Br8
length(Br8)



e<-hist(wdata$windgust, 
        breaks = Br8, 
        freq=F,
        col="grey",
        main = paste("Fréquence des morsures par rafale de vent (m/s) \n ",nr," signalements  humains (France HDTOM, 2017-20)"),
        ylab = "Denisté  (Somme=1)",
        xlab = "rafale de vent (m/s)",
        ylim = c(0,.3),
        
)

## courbe non lissée
HH8 <- hist((rafData$rafal_10mindb), breaks = Br8,  plot = F)
lines(HH8$mids, HH8$density, lwd = 2, col = "green")

### courbe lissée
lines(density((rafData$rafal_10mindb), na.rm = 1), lwd = 2, col = "red")

text(30, 0.20, paste("N = ",nr," signalements" ),col = "black")
text(10, 0.10, paste("M = Mesure Nationale" ), col = "red")

sum(e$density)
sum(HH8$density)

########################################### comparatif de l'intensité de précipitation sur 1 h  #########################################

length(wdata$precipintensity) # mm/h
length(precData$precip_01hdb) # mm/h

range(wdata$precipintensity, na.rm = 1 )
range( (precData$precip_01hdb), na.rm = 1 )

Br9 <- seq(from=-1 , to=15 , by=.01)
Br9
length(Br9)



f<-hist(wdata$precipintensity[wdata$precipintensity > 0.01], 
        breaks = Br9, 
        freq=F,
        col="grey",
        main = paste("Fréquence des morsures par intensité de précipitation (mm/h) \n ",nr," signalements  humains (France HDTOM, 2017-20) \n (les valeurs 0 sont exlues car très nombreuses)"),
        ylab = "Denisté  (Somme=100)",
        xlab = "intensité de précipitation (mm/h)",
        ylim = c(0,20),
        xlim = c(0,1),
        
)

### courbe lissée
lines(density((precData$precip_01hdb), na.rm = 1), lwd = 2, col = "red")

### courbe non lissée
HH9 <- hist((precData$precip_01hdb), breaks = Br9,  plot = F)
lines(HH9$mids, HH9$density, lwd = 2, col = "green")

text(0.8, 15, paste("N = ",nr," signalements" ),col = "black")
text(.4, 5, paste("M = Mesure Nationale" ), col = "red")



sum(f$density)
sum(HH9$density)

########################################### comparatif de l'intensité de précipitation sur 24 h  #########################################

length(wdata$precipintensitymax) # mm/h
length(prec24Data$precip_24hdb) # mm/h

range(wdata$precipintensitymax, na.rm = 1 )
range( (prec24Data$precip_24h), na.rm = 1 )

Br9 <- seq(from=-1 , to=72 , by=.01)
Br9
length(Br9)



f<-hist(wdata$precipintensitymax[wdata$precipintensitymax > 0.01], 
        breaks = Br9, 
        freq=F,
        col="grey",
        main = paste("Fréquence des morsures par intensité de précipitation sur 24 h (mm/h) \n ",nr," signalements  humains (France HDTOM, 2017-20) \n (les valeurs 0 sont exlues car très nombreuses)"),
        ylab = "Denisté  (Somme=100)",
        xlab = "intensité de précipitation (mm/h)",
        ylim = c(0,10),
        xlim = c(0,3),
        
)

### courbe lissée
lines(density((prec24Data$precip_24hdb), na.rm = 1), lwd = 2, col = "red")

### courbe non lissée
HH9 <- hist((prec24Data$precip_24hdb), breaks = Br9,  plot = F)
lines(HH9$mids, HH9$density, lwd = 2, col = "green")

text(0.8, 15, paste("N = ",nr," signalements" ),col = "black")
text(.4, 5, paste("M = Mesure Nationale" ), col = "red")



sum(f$density)
sum(HH9$density)

#################################################### comparatif Température diurne ( de 6 à 21h ) #################################################### 
length(wdata$temperaturehigh)
length(tempDayData$tempdaydb)

range(wdata$temperaturehigh, na.rm = 1)
range(tempDayData$tempdaydb, na.rm = 1 )

BR10 <- seq(from= -3, to= 44, by=1)
BR10
length(BR10)



### histogramme de température diurnes comparé:
g<-hist(wdata$temperaturehigh, breaks = BR10, freq=F,
        col="grey",
        main = paste("Fréquence de signalements de piqûres de tiques en fonction des températures \n diurnes ( de 06 à 21h ) , ",nr," signalements  humains (France HDTOM, 2017-20)"),
        ylab = "Denisté  (Somme=1)",
        xlab = "Températures diurnes (T°C)",
        ylim = c(0,.1),
        xlim = c(-10,40),
        
)

### courbe non lissée
HH10 <- hist(tempDayData$tempdaydb, breaks = BR10,  plot=F)
lines(HH10$mids, HH10$density, lwd = 2, col = "green")

### courbe lissée, kernel
lines(density(tempDayData$tempdaydb, na.rm = 1), lwd = 2, col = "red")

text(30, 0.08, paste("N = ",nr," signalements" ),col = "black")
text(05, 0.08, paste("M = Mesure Nationale" ), col = "red")

sum(g$density)
sum(HH10$density)

# #################################################### comparatif Température nocturnes (de 21 à 06h) #################################################### 
length(wdata$temperaturelow)
length(tempNightData$tempnightdb )

range(wdata$temperaturelow, na.rm = 1)
range(tempNightData$tempnightdb , na.rm = 1 )

BR11 <- seq(from= -9, to= 30, by=1)
BR11
length(BR11)


### histogramme de température nocturnes comparé:



h<-hist(wdata$temperaturelow, breaks = BR11, freq=F,
        col="grey",
        main = paste("Fréquence de signalements de piqûres de tiques en fonction des températures \n nocturnes ( de  21 à 06h ) , ",nr," signalements  humains (France HDTOM, 2017-20)"),
        ylab = "Denisté  (Somme=1)",
        xlab = "Températures nocturnes (T°C)",
        ylim = c(0,.11),
        xlim = c(-10,30),
        
)

### courbe non lissée
HH11 <- hist(tempNightData$tempnightdb, breaks = BR11,  plot=F)

lines(HH11$mids, HH11$density, lwd = 2, col = "green")

### courbe lissée, kernel
lines(density(tempNightData$tempnightdb, na.rm = 1), lwd = 2, col = "red")

text(23, 0.08, paste("N = ",nr," signalements" ),col = "black")
text(05, 0.08, paste("M = Mesure Nationale" ), col = "red")

sum(h$density)
sum(HH11$density)

############################################################# EOS ####################################################################

##### code de déconnexion ####
dbDisconnect(con)


