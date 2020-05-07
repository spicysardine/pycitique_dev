##################### Traitement de la donnée darksky vs météo-france pour les animaux ################################

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
-------------- données générales
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
,timezone
,time
,summary
,icon
---------------- données météo
,humidity
,dewpoint
,pressure
,windspeed
,visibility
,cloudcover
,windgust
,uvindex
,precipintensity
,precipintensitymax
,temperaturehigh
,temperaturelow

FROM citik.citik_animaux_clean_weather_strict
where qui_pique = 'Chat'
;"
))

# str(wadata)

## données MF
mfdata <-  dbGetQuery(con, "SELECT * FROM meteo.mf_synop42_avg ; " )


##### code de déconnexion ####
dbDisconnect(con)

#inspection des jeux de données darksky
# ls(wadata)
# str(wadata)
# summary(wadata)
nra <- nrow(wadata)

# déput de construction du pdf
pdf( file = "../../PDF/citik_chat_DSK_vs_MF_charts.pdf",
     onefile = TRUE,
     paper="a4r",
     width = 11,
     height = 9,
     family = "Helvetica"
)

#@@@@@@@@@@   Graphiques sectorisés  @@@@@@@@@@@@@@@#

# SEX
pie(table(wadata$sex_animal[wadata$sex_animal != '']), cex= 1.5, main = 'Sexe', cex.main=2, col = c("#FF8000", "#0080FF") )

# Paysage
pie(table(wadata$environnement[wadata$environnement != '']),
    cex= 1,
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

# Indice UV
barplot(table(wadata$uvindex),
        col="grey",
        main = paste("Fréquence des morsures par rayonnement UV (échelle de l'OMS 1-10) \n ",nra," signalements  animaux (France HDTOM, 2017-20)"),
        ylab = "Fréquence",
        xlab = "rayonnement UV (échelle de 1-10) ",
        ylim = c(0,1000)
        )


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@   Comparatifs de courbes  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#



########################################### comparatif Humidité  #########################################
length(wadata$humidity)
length(mfdata$humidite)

range(wadata$humidity, na.rm = 1)
range( (mfdata$humidite), na.rm = 1 )

BR2 <- seq(from= 0, to= 100, by=1)
BR2
length(BR2)

y<-hist(wadata$humidity*100, breaks = BR2, freq=F,
        col="grey",
        cex=10,
        main = paste("Fréquence de signalements de piqûres de tiques en fonction de l'humidité\n (2017-2020),",nra,"  signalements  animaux (France HDTOM, 2017-20)"),
        ylab = "Densité (Somme=1)",
        xlab = "Humidité (%)",
        ylim = c(0, .07),
        xlim = c(0, 100),
        cex.main = 1.3,
        cex.lab = 1.5,
        cex.axis = 1.5
)

### courbe non lissée
HH2 <- hist((mfdata$humidite), breaks = BR2,  plot = F)
lines(HH2$mids, HH2$density, lwd = 2, col = "green")

### courbe lissée
lines(density((mfdata$humidite), na.rm = 1), lwd = 2, col = "red")

text(90, 0.06, paste("N =",nra," signalements" ), cex = 1,  col = "black")
text(55, 0.05, paste("M = Mesure Nationale MF" ), cex = 1 ,  col = "red")

sum(y$density)
sum(HH2$density)

########################################### comparatif point de rosée #########################################

## inspection du jeu de donnée
length(wadata$dewpoint)
length(mfdata$point_rose)

range(wadata$dewpoint, na.rm = TRUE)
range( (mfdata$point_rose), na.rm = 1 )

Br3 <- seq(from=-16 , to=23 , by=1)
Br3
length(Br3)

z<-hist(wadata$dewpoint, breaks = Br3, freq=F,
        col="grey",
        main = paste("Fréquence des morsures par point de rosée (°C) \n ",nra," signalements  animaux (France HDTOM, 2017-20)"),
        ylab = "Denisté  (Somme=1)",
        xlab = "point de rosée (°C)",
        ylim = c(0,.10),
        cex.main = 1.3,
        cex.lab = 1.5,
        cex.axis = 1.5
)

### courbe non lissée
HH3 <- hist((mfdata$point_rose), breaks = Br3,  plot = F)
lines(HH3$mids, HH3$density, lwd = 2, col = "green")

### courbe lissée
lines(density((mfdata$point_rose), na.rm = 1), lwd = 2, col = "red")

text(25, 0.10, paste("N = ",nra," signalements" ), cex = 1,  col = "black")
text(04, 0.10, paste("M = Mesure Nationale MF" ), cex = 1 ,  col = "red")

sum(z$density)
sum(HH3$density)

########################################### comparatif pression atmosphérique #########################################

length(wadata$pressure)
length(mfdata$press_mer)

range(wadata$pressure, na.rm = TRUE)
range( (mfdata$press_mer)/100, na.rm = 1 ) # division par 100 pour obtenir la valeur en hPa.

Br4 <- seq(from=950 , to=1050 , by=1)
Br4
length(Br4)

a<-hist(wadata$pressure, breaks = Br4, freq=F,
        col="grey",
        main = paste("Fréquence des morsures par pression athmosphérique (hPa) \n ",nra," signalements  animaux (France HDTOM, 2017-20)"),
        ylab = "Denisté  (Somme=1)",
        xlab = "pression athmosphérique (hPa)",
        ylim = c(0, .08),
        xlim = c(950, 1050),
        cex.main = 1.3,
        cex.lab = 1.5,
        cex.axis = 1.5
)

### courbe non lissée
HH4 <- hist((mfdata$press_mer)/100, breaks = Br4,  plot = F)
lines(HH4$mids, HH4$density, lwd = 2, col = "green")

### courbe lissée
lines(density((mfdata$press_mer)/100, na.rm = 1), lwd = 2, col = "red")

text(1030, 0.3, paste("N = ",nra," signalements" ), cex = 1,  col = "black")
text(1010, 0.10, paste("M = Mesure Nationale MF" ), cex = 1 ,  col = "red")

sum(a$density)
sum(HH4$density)

########################################### comparatif visibilité horizontale en km #########################################

length(wadata$visibility) # en km
length(mfdata$visibilite) # en m

range((wadata$visibility), na.rm = 1)
range( (mfdata$visibilite/1000), na.rm = 1 )

Br6 <- seq(from=0 , to=37 , by=1)
Br6
length(Br6)

c<-hist((wadata$visibility), breaks = Br6, freq=F,
        col="grey",
        main = paste("Fréquence des morsures par visibilité athmosphérique horizontale (km) \n ",nra," signalements  animaux (France HDTOM, 2017-20)"),
        ylab = "Densité (Somme=1)",
        xlab = "visibilité athmosphérique horizontale (km)",
        ylim = c(0,.30),
        xlim = c(0,40),
        cex.main = 1.3,
        cex.lab = 1.5,
        cex.axis = 1.5
)

### courbe non lissée
HH6 <- hist((mfdata$visibilite)/1000, breaks = Br6,  plot = F)
lines(HH6$mids, HH6$density, lwd = 2, col = "green")

### courbe lissée
lines(density((mfdata$visibilite)/1000, na.rm = 1), lwd = 2, col = "red")

text(22, 0.4, paste("N = ",nra," signalements" ), cex = 1,  col = "black")
text(25, 0.1, paste("M = Mesure Nationale MF" ), cex = 1 ,  col = "red")

sum(c$density)
sum(HH6$density)

########################################### comparatif couvert nuageux (nebulosité relative) #########################################

length(wadata$cloudcover)
length(mfdata$nebulosite)

range(wadata$cloudcover, na.rm = 1)*100
range( (mfdata$nebulosite), na.rm = 1 )

Br7 <- seq(from=0 , to=100 , by=1)
Br7
length(Br7)

d<-hist((wadata$cloudcover)*100, breaks = Br7, freq=F,
        col="grey",
        main = paste("Fréquence des morsures par couvert nuageux (%) \n ",nra," signalements  animaux (France HDTOM, 2017-20)"),
        ylab = "Denisté  (Somme=1)",
        xlab = "couvert nuageux (%)",
        ylim = c(0,.07),
        xlim = c(0,100),
        cex.main = 1.3,
        cex.lab = 1.5,
        cex.axis = 1.5
)

### courbe non lissée
HH7 <- hist((mfdata$nebulosite), breaks = Br7,  plot = F)
lines(HH7$mids, HH7$density, lwd = 2, col = "green")

### courbe lissée
lines(density((mfdata$nebulosite), na.rm = 1), lwd = 2, col = "red")

text(50, 0.04, paste("N = ",nra," signalements" ), cex = 1,  col = "black")
text(75, 0.05, paste("M = Mesure Nationale MF" ), cex = 1 ,  col = "red")

sum(d$density)
sum(HH7$density)

########################################### comparatif vitesse du vent m/s #########################################

length(wadata$windspeed)
length(mfdata$vvent)

range(wadata$windspeed, na.rm = 1)
range( (mfdata$vvent), na.rm = 1 )

Br5 <- seq(from=0 , to=14 , by=.1)
Br5
length(Br5)

b<-hist(wadata$windspeed, breaks = Br5, freq=F,
        col="grey",
        main = paste("Fréquence des morsures par vitesse du vent (m/s) \n ",nra," signalements  animaux (France HDTOM, 2017-20)"),
        ylab = "Denisté  (Somme=10)",
        xlab = "vitesse du vent (m/s)",
        ylim = c(0,.5),
        xlim = c(0,13),
        cex.main = 1.3,
        cex.lab = 1.5,
        cex.axis = 1.5
)

### courbe non lissée
HH5 <- hist((mfdata$vvent), breaks = Br5,  plot = F)
lines(HH5$mids, HH5$density, lwd = 2, col = "green")

### courbe lissée
lines(density((mfdata$vvent), na.rm = 1), lwd = 2, col = "red")

text(6, 0.6, paste("N = ",nra," signalements" ), cex = 1,  col = "black")
text(6, 0.3, paste("M = Mesure Nationale MF" ), cex = 1 ,  col = "red")

sum(b$density)
sum(HH5$density)

########################################### comparatif rafale de vent #########################################

length(wadata$windgust) # km/h
length(mfdata$rafale_10min)

range(wadata$windgust, na.rm = 1)
range( (mfdata$rafale_10min), na.rm = 1 )

Br8 <- seq(from= 0, to= 50 , by=1)
Br8
length(Br8)

e<-hist(wadata$windgust,
        breaks = Br8,
        freq=F,
        col="grey",
        main = paste("Fréquence des morsures par rafale de vent (m/s) \n ",nra," signalements  animaux (France HDTOM, 2017-20)"),
        ylab = "Denisté  (Somme=1)",
        xlab = "rafale de vent (m/s)",
        ylim = c(0,.3),
        xlim = c(0,30),
        cex.main = 1.3,
        cex.lab = 1.5,
        cex.axis = 1.5
)

## courbe non lissée
HH8 <- hist((mfdata$rafale_10min), breaks = Br8,  plot = F)
lines(HH8$mids, HH8$density, lwd = 2, col = "green")

### courbe lissée
lines(density((mfdata$rafale_10min), na.rm = 1), lwd = 2, col = "red")

text(11, 0.20, paste("N = ",nra," signalements" ), cex = 1,  col = "black")
text(15, 0.10, paste("M = Mesure Nationale MF" ), cex = 1 ,  col = "red")

sum(e$density)
sum(HH8$density)

########################################### comparatif de l'intensité de précipitation sur 1 h  #########################################

length(wadata$precipintensity) # mm/h
length(mfdata$precip_01h) # mm/h

range(wadata$precipintensity, na.rm = 1 )
range( (mfdata$precip_01h), na.rm = 1 )

Br9 <- seq(from=-1 , to=16 , by=.01)
Br9
length(Br9)

f<-hist(wadata$precipintensity,
        breaks = Br9,
        freq=F,
        col="grey",
        main = paste("Fréquence des morsures par intensité de précipitation (mm/h) \n ",nra," signalements  animaux (France HDTOM, 2017-20)"),
        ylab = "Denisté  (Somme=100)",
        xlab = "intensité de précipitation (mm/h)",
        ylim = c(0,35),
        xlim = c(0,1),
        cex.main = 1.5,
        cex.lab = 1.5,
        cex.axis = 1.5
)

### courbe lissée
lines(density((mfdata$precip_01h), na.rm = 1), lwd = 2, col = "red")

### courbe non lissée
HH9 <- hist((mfdata$precip_01h), breaks = Br9,  plot = F)
lines(HH9$mids, HH9$density, lwd = 2, col = "green")

text(0.8, 15, paste("N = ",nra," signalements" ), cex = 1,  col = "black")
text(.4, 5, paste("M = Mesure Nationale MF" ), cex = 1 ,  col = "red")

sum(f$density)
sum(HH9$density)

########################################### comparatif de l'intensité de précipitation sur 24 h  #########################################

length(wadata$precipintensitymax) # mm/h
length(mfdata$precip_24h) # mm/h

range(wadata$precipintensitymax, na.rm = 1 )
range( (mfdata$precip_24h), na.rm = 1 )

Br9 <- seq(from=-1 , to=72 , by=.01)
Br9
length(Br9)

f<-hist(wadata$precipintensitymax,
        breaks = Br9,
        freq=F,
        col="grey",
        main = paste("Fréquence des morsures par intensité de précipitation sur 24 h (mm/h) \n ",nra," signalements  animaux (France HDTOM, 2017-20)"),
        ylab = "Denisté  (Somme=100)",
        xlab = "intensité de précipitation sur 24 h (mm/h)",
        ylim = c(0,35),
        xlim = c(0,3),
        cex.main = 1.5,
        cex.lab = 1.5,
        cex.axis = 1.5
)

### courbe lissée
lines(density((mfdata$precip_24h), na.rm = 1), lwd = 2, col = "red")

### courbe non lissée
HH9 <- hist((mfdata$precip_24h), breaks = Br9,  plot = F)
lines(HH9$mids, HH9$density, lwd = 2, col = "green")

text(0.8, 15, paste("N = ",nra," signalements" ), cex = 1,  col = "black")
text(.4, 5, paste("M = Mesure Nationale MF" ), cex = 1 ,  col = "red")

sum(f$density)
sum(HH9$density)

#################################################### comparatif Température diurne ( de 6 à 21h ) ####################################################
length(wadata$temperaturehigh)
length(mfdata$temperature_diurne)

range(wadata$temperaturehigh, na.rm = 1)
range(mfdata$temperature_diurne, na.rm = 1 )

BR10 <- seq(from= -4, to=42, by=1)
BR10
length(BR10)



### histogramme de température diurnes comparé:
g<-hist(wadata$temperaturehigh, breaks = BR10, freq=F,
        col="grey",
        main = paste("Fréquence de signalements de piqûres de tiques en fonction des températures \n diurnes ( de 06 à 21h ) , ",nra," signalements  animaux (France HDTOM, 2017-20)"),
        ylab = "Denisté  (Somme=1)",
        xlab = "Températures diurnes (T°C)",
        ylim = c(0,.1),
        xlim = c(-10,40),
        cex.main = 1.3,
        cex.lab = 1.5,
        cex.axis = 1.5
)

### courbe non lissée
HH10 <- hist(mfdata$temperature_diurne, breaks = BR10,  plot=F)
lines(HH10$mids, HH10$density, lwd = 2, col = "green")

### courbe lissée, kernel
lines(density(mfdata$temperature_diurne, na.rm = 1), lwd = 2, col = "red")

### courbe lissée, hyp +2 C°
HH10hyp <- hist(mfdata$temperature_diurne_offset2, breaks = BR10,  plot=F)
lines(density(mfdata$temperature_diurne_offset2, na.rm = 1), lwd = 2, col = "blue")

text(30, 0.08, paste("N = ",nra," signalements" ), cex = 1,  col = "black")
text(05, 0.08, paste("M = Mesure Nationale MF" ), cex = 1 ,  col = "red")
text(0, 0.04, paste("M+ = Hypothèse +2 C°" ), cex =1 ,  col = "blue")
text(15.3, 0.082, paste("M = Courbe brute" ), cex =1 ,  col = "green")

sum(g$density)
sum(HH10$density)

#################################################### comparatif Température nocturnes (de 21 à 06h) ####################################################
length(wadata$temperaturelow)
length(mfdata$temperature_nocturne )

range(wadata$temperaturelow, na.rm = 1)
range(mfdata$temperature_nocturne , na.rm = 1 )

BR11 <- seq(from= -9, to= 30, by=1)
BR11
length(BR11)


### histogramme de température nocturnes comparé:

h<-hist(wadata$temperaturelow, breaks = BR11, freq=F,
        col="grey",
        main = paste("Fréquence de signalements de piqûres de tiques en fonction des températures \n nocturnes ( de  21 à 06h ) , ",nra," signalements  animaux (France HDTOM, 2017-20)"),
        ylab = "Denisté  (Somme=1)",
        xlab = "Températures nocturnes (T°C)",
        ylim = c(0,.11),
        xlim = c(-10,30),
        cex.main = 1.3,
        cex.lab = 1.5,
        cex.axis = 1.5
)

### courbe non lissée
HH11 <- hist(mfdata$temperature_nocturne, breaks = BR11,  plot=F)

lines(HH11$mids, HH11$density, lwd = 2, col = "green")

### courbe lissée, kernel
lines(density(mfdata$temperature_nocturne, na.rm = 1), lwd = 2, col = "red")

### courbe lissée,  hyp +2 C°
HH11hyp <- hist(mfdata$temperature_nocturne_offset2, breaks = BR11,  plot=F)
lines(density(mfdata$temperature_nocturne_offset2, na.rm = 1), lwd = 2, col = "blue")

text(23, 0.08, paste("N = ",nra," signalements" ), cex = 1,  col = "black")
text(03, 0.08, paste("M = Mesure Nationale MF" ), cex = 1 ,  col = "red")
text(06, 0.10, paste("M = Courbe brute" ), cex =1 ,  col = "green")
text(-1, 0.04, paste("M+ = Hypothèse +2 C°" ), cex =1 ,  col = "blue")
sum(h$density)
sum(HH11$density)


############################################################# EOS ####################################################################

# fin de construction du pdf
dev.off()

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
