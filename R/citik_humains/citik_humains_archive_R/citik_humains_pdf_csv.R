# définition du chemin
setwd("./")
getwd()
# importation du fichier de donnée météo pour les  humains (France, 2017-20)

## donées darksky
# wdata <- read.csv("citik_humains_clean_weather.csv", sep = ',', quote = "'",stringsAsFactors = FALSE)
wdata <- read.csv("citik_humains_clean_weather_strict.csv", sep = ',', quote = "'", stringsAsFactors = FALSE)

## données MF

# humidité
hdata <-  read.csv("../param/humidityDB.csv", sep = ';', stringsAsFactors = FALSE , header = TRUE)
hdata2 <- read.csv("../param/humidityOffse2tDB.csv", sep = ';', stringsAsFactors = FALSE )

# point de rosée
pdrData <-  read.csv("../param/ptroseDB.csv", sep = ';', stringsAsFactors = FALSE )
pdrData2 <- read.csv("../param/ptroseOffset2DB.csv", sep = ';', stringsAsFactors = FALSE )

# pression
pdata <-  read.csv("../param/pressionDB.csv", sep = ';', stringsAsFactors = FALSE )

# vitesse du vent
vdata <-  read.csv("../param/vventDB.csv", sep = ';', stringsAsFactors = FALSE )

# visibilite horizontale
visData <-  read.csv("../param/visibiliteDB.csv", sep = ';', stringsAsFactors = FALSE )

# nebulosité relative
nebData <-  read.csv("../param/nebulositeDB.csv", sep = ';', stringsAsFactors = FALSE )

# rafale de vent
rafData <-  read.csv("../param/rafal_10minDB.csv", sep = ';', stringsAsFactors = FALSE )

# intensité de précipitation
precData <-  read.csv("../param/precip_01hDB.csv", sep = ';', stringsAsFactors = FALSE )

# précipitation max sur 24 h
prec24Data <-  read.csv("../param/precip_24hDB.csv", sep = ';', stringsAsFactors = FALSE )

#température journalière
tempData <- read.csv("../param/tmpDB.csv", sep = ';', stringsAsFactors = FALSE )
tempData2 <-  read.csv("../param/tmpOffset2DB.csv", sep = ';', stringsAsFactors = FALSE )

#température diurnes ( de 6 à 21 h)
tempDayData <- read.csv("../param/tempdayDB.csv", sep = ';', stringsAsFactors = FALSE )
tempDayData2 <-  read.csv("../param/tempdayOffset2DB.csv", sep = ';', stringsAsFactors = FALSE )

#température diurnes ( de 18 à 21 h)
tempNightData <- read.csv("../param/tempnightDB.csv", sep = ';', stringsAsFactors = FALSE )
tempNightData2 <-  read.csv("../param/tempnightOffset2DB.csv", sep = ';', stringsAsFactors = FALSE )

#température max
tempMaxData <- read.csv("../param/tempmax24DB.csv", sep = ';', stringsAsFactors = FALSE )
tempMaxData2 <-  read.csv("../param/tempmax24Offset2DB.csv", sep = ';', stringsAsFactors = FALSE )

#température min
tempMinData <- read.csv("../param/tempmin24DB.csv", sep = ';', stringsAsFactors = FALSE )
tempMinData2 <-  read.csv("../param/tempmin24Offset2DB.csv", sep = ';', stringsAsFactors = FALSE )

#inspection des jeux de données darksky
# ls(wdata)
# str(wdata)
# summary(wdata)
nr <- nrow(wdata)
#@@@@@@@@@@   Camemberts  @@@@@@@@@@@@@@@#

pdf( file = "./plots_humains/citik_humains_charts.pdf",
     onefile = TRUE,
     paper="a4r",
     width = 11,
     height = 9,
     family = "Helvetica"
     )

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


# # Humidité
# hist(wdata$humidity, 
#      breaks = 80,
#      col="grey",
#      main = paste("Fréquence des morsures par humidité (%) \n ", nr,"signalements  humains (France, 2017-20)"),
#      ylab = "Fréquence",
#      xlab = "humidité (%)",
#      # ylim = c(0, 0.05),
#      # xlim = c(0, 10)
# )
# 
# # Pression
# hist(wdata$pressure, 
#      breaks = 40,
#      col="grey",
#      main = paste("Fréquence des morsures par pression athmosphérique (hPa) \n ",nr," signalements  humains (France, 2017-20)"),
#      ylab = "Fréquence",
#      xlab = "pression athmosphérique (hPa)",
#      ylim = c(0, 2000),
#      xlim = c(970, 1060)
# )
# 
# # Intensité de précipitation
# hist(wdata$precipintensity, 
#      breaks = 100,
#      col="grey",
#      main = paste("Fréquence des morsures par intensité de précipitation (mm/h) \n ",nr," signalements  humains (France, 2017-20)"),
#      ylab = "Fréquence",
#      xlab = "intensité de précipitation (mm/h)")
# 
# # Couvert nuageux
# hist(wdata$cloudcover, 
#      breaks = 110,
#      col="grey",
#      main = paste("Fréquence des morsures par couvert nuageux (%) \n ",nr," signalements  humains (France, 2017-20)"),
#      ylab = "Fréquence",
#      xlab = "couvert nuageux (%)")
# 
# # Point de rosée
# hist(wdata$dewpoint, 
#      breaks = 100,
#      col="grey",
#      main = paste("Fréquence des morsures par point de rosée (°C) \n ",nr," signalements  humains (France, 2017-20)"),
#      ylab = "Fréquence",
#      xlab = "point de rosée (°C)")
# 
# # Température Diurnes
# hist(wdata$temperaturehigh, 
#      breaks = 100,
#      col="grey",
#      main = paste("Fréquence des morsures par températures diurnes (°C) \n ",nr," signalements  humains (France, 2017-20)"),
#      ylab = "Fréquence",
#      xlab = "température de jour (°C)")
# 
# # Température Nocturnes
# hist(wdata$temperaturelow, 
#      breaks = 100,
#      col="grey",
#      main = paste("Fréquence des morsures par températures nocturnes (°C) \n ",nr," signalements  humains (France, 2017-20)"),
#      ylab = "Fréquence",
#      xlab = "température de nuit (°C)")
# 
# # Indice UV
# barplot(table(wdata$uvindex),
#         col="grey",
#         main = paste("Fréquence des morsures par rayonnement UV (échelle de l'OMS 1-10) \n ",nr," signalements  humains (France, 2017-20)"),
#         ylab = "Fréquence",
#         xlab = "rayonnement UV (échelle de 1-10) ")
# 
# # Visibilité
# hist(wdata$visibility,
#      breaks=24,
#      col="grey",
#      main = paste("Fréquence des morsures par visibilité athmosphérique horizontale (km) \n ",nr," signalements  humains (France, 2017-20)"),
#      ylab = "Fréquence",
#      xlab = "visibilité athmosphérique (km)")
# 
# # Vitesse du vent
# hist(wdata$windspeed,
#      breaks = 50,
#      col="grey",
#      main = paste("Fréquence des morsures par vitesse du vent (m/s) \n ",nr," signalements  humains (France, 2017-20)"),
#      ylab = "Fréquence",
#      xlab = "vitesse du vent (m/s)")
# 
# # Rafale du vent
# hist(wdata$windgust,
#      breaks = 100,
#      col="grey",
#      main = paste("Fréquence des morsures par rafale de vent (m/s) \n ",nr," signalements  humains (France, 2017-20)"),
#      ylab = "Fréquence",
#      xlab = "rafale de vent (m/s)")


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@   Comparatifs de courbes  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#



########################################### comparatif Humidité  #########################################

length(wdata$humidity)
length(hdata$humidite)

range(wdata$humidity, na.rm = 1)
range( (hdata$humidite), na.rm = 1 )

BR2 <- seq(from= 0, to= 100, by=1)
BR2
length(BR2)



y<-hist(wdata$humidity*100, breaks = BR2, freq=F,
        col="grey",
        main = paste("Fréquence de signalements de piqûres de tiques en fonction de l'humidité\n (2017-2020),",nr,"  signalements  humains (France, 2017-20)"),
        ylab = "Densité (Somme=1)",
        xlab = "Humidité (%)",
        ylim = c(0, .07),
        xlim = c(0, 100),
       
)

### courbe non lissée
HH2 <- hist((hdata$humidite), breaks = BR2,  plot = F)
lines(HH2$mids, HH2$density, lwd = 2, col = "green")

### courbe lissée
lines(density((hdata$humidite), na.rm = 1), lwd = 2, col = "red") 

text(90, 0.06, paste("N =",nr," signalements" ), col = "black")
text(55, 0.05, paste("M = Mesure Nationale" ),  col = "red")



sum(y$density)
sum(HH2$density)

########################################### comparatif point de rosée #########################################

## inspection du jeu de donnée
length(wdata$dewpoint)
length(pdrData$point_rose)

range(wdata$dewpoint, na.rm = TRUE)
range( (pdrData$point_rose), na.rm = 1 )

Br3 <- seq(from=-12 , to=25 , by=1)
Br3
length(Br3)

z<-hist(wdata$dewpoint, breaks = Br3, freq=F,
        col="grey",
        main = paste("Fréquence des morsures par point de rosée (°C) \n ",nr," signalements  humains (France, 2017-20)"),
        ylab = "Denisté  (Somme=1)",
        xlab = "point de rosée (°C)",
        ylim = c(0,.15),
      
)


### courbe non lissée
HH3 <- hist((pdrData$point_rose), breaks = Br3,  plot = F)
lines(HH3$mids, HH3$density, lwd = 2, col = "green")

### courbe lissée
lines(density((pdrData$point_rose), na.rm = 1), lwd = 2, col = "red")

text(25, 0.10, paste("N = ",nr," signalements" ), col = "black")
text(04, 0.10, paste("M = Mesure Nationale" ),  col = "red")



sum(z$density)
sum(HH3$density)

########################################### comparatif pression atmosphérique #########################################

length(wdata$pressure)
length(pdata$press_sta)

range(wdata$pressure, na.rm = TRUE)
range( (pdata$press_sta)/100, na.rm = 1 ) # division par 100 pour obtenir la valeur en hPa.

Br4 <- seq(from=950 , to=1050 , by=1)
Br4
length(Br4)



a<-hist(wdata$pressure, breaks = Br4, freq=F,
        col="grey",
        main = paste("Fréquence des morsures par pression athmosphérique (hPa) \n ",nr," signalements  humains (France, 2017-20)"),
        ylab = "Denisté  (Somme=1)",
        xlab = "pression athmosphérique (hPa)",
        ylim = c(0, .1),
        xlim = c(970, 1050),
       
)

### courbe non lissée
HH4 <- hist((pdata$press_sta)/100, breaks = Br4,  plot = F)
lines(HH4$mids, HH4$density, lwd = 2, col = "green")

### courbe lissée
lines(density((pdata$press_sta)/100, na.rm = 1), lwd = 2, col = "red")

text(1030, 0.3, paste("N = ",nr," signalements" ),col = "black")
text(1010, 0.10, paste("M = Mesure Nationale" ), col = "red")



sum(a$density)
sum(HH4$density)


########################################### comparatif visibilité horizontale en km #########################################

length(wdata$visibility) # en km
length(visData$visibilite) # en m

range((wdata$visibility), na.rm = 1)
range( (visData$visibilite/1000), na.rm = 1 )

Br6 <- seq(from=0 , to=37 , by=1)
Br6
length(Br6)




c<-hist((wdata$visibility), breaks = Br6, freq=F,
        col="grey",
        main = paste("Fréquence des morsures par visibilité athmosphérique horizontale (km) \n ",nr," signalements  humains (France, 2017-20)"),
        ylab = "Densité (Somme=1)",
        xlab = "visibilité athmosphérique horizontale (km)",
        ylim = c(0,.30),
        xlim = c(0,40),
       
)

### courbe non lissée
HH6 <- hist((visData$visibilite)/1000, breaks = Br6,  plot = F)
lines(HH6$mids, HH6$density, lwd = 2, col = "green")

### courbe lissée
lines(density((visData$visibilite)/1000, na.rm = 1), lwd = 2, col = "red")

text(22, 0.4, paste("N = ",nr," signalements" ),col = "black")
text(20, 0.1, paste("M = Mesure Nationale" ), col = "red")



sum(c$density)
sum(HH6$density)

########################################### comparatif couvert nuageux (nebulosité relative) #########################################

length(wdata$cloudcover)
length(nebData$nebulosite)

range(wdata$cloudcover, na.rm = 1)*100
range( (nebData$nebulosite), na.rm = 1 )

Br7 <- seq(from=0 , to=100 , by=1)
Br7
length(Br7)



d<-hist((wdata$cloudcover)*100, breaks = Br7, freq=F,
        col="grey",
        main = paste("Fréquence des morsures par couvert nuageux (%) \n ",nr," signalements  humains (France, 2017-20)"),
        ylab = "Denisté  (Somme=1)",
        xlab = "couvert nuageux (%)",
        ylim = c(0,.07),
        xlim = c(0,100),
       
)

### courbe non lissée
HH7 <- hist((nebData$nebulosite), breaks = Br7,  plot = F)
lines(HH7$mids, HH7$density, lwd = 2, col = "green")

### courbe lissée
lines(density((nebData$nebulosite), na.rm = 1), lwd = 2, col = "red")

text(50, 0.04, paste("N = ",nr," signalements" ),col = "black")
text(75, 0.05, paste("M = Mesure Nationale" ), col = "red")



sum(d$density)
sum(HH7$density)


########################################### comparatif vitesse du vent m/s #########################################

length(wdata$windspeed)
length(vdata$vvent)

range(wdata$windspeed, na.rm = 1)
range( (vdata$vvent), na.rm = 1 )

Br5 <- seq(from=0 , to=13 , by=.1)
Br5
length(Br5)



b<-hist(wdata$windspeed, breaks = Br5, freq=F,
        col="grey",
        main = paste("Fréquence des morsures par vitesse du vent (m/s) \n ",nr," signalements  humains (France, 2017-20)"),
        ylab = "Denisté  (Somme=10)",
        xlab = "vitesse du vent (m/s)",
        ylim = c(0,.6),
        xlim = c(0,13),
       
)

### courbe non lissée
HH5 <- hist((vdata$vvent), breaks = Br5,  plot = F)
lines(HH5$mids, HH5$density, lwd = 2, col = "green")

### courbe lissée
lines(density((vdata$vvent), na.rm = 1), lwd = 2, col = "red")

text(6, 0.6, paste("N = ",nr," signalements" ),col = "black")
text(6, 0.3, paste("M = Mesure Nationale" ), col = "red")



sum(b$density)
sum(HH5$density)

########################################### comparatif rafale de vent #########################################

length(wdata$windgust) # km/h
length(rafData$rafale_10min)

range(wdata$windgust, na.rm = 1)
range( (rafData$rafale_10min), na.rm = 1 )

Br8 <- seq(from= 0, to= 50 , by=1)
Br8
length(Br8)



e<-hist(wdata$windgust, 
        breaks = Br8, 
        freq=F,
        col="grey",
        main = paste("Fréquence des morsures par rafale de vent (m/s) \n ",nr," signalements  humains (France, 2017-20)"),
        ylab = "Denisté  (Somme=1)",
        xlab = "rafale de vent (m/s)",
        ylim = c(0,.3),
    
)

## courbe non lissée
HH8 <- hist((rafData$rafale_10min), breaks = Br8,  plot = F)
lines(HH8$mids, HH8$density, lwd = 2, col = "green")

### courbe lissée
lines(density((rafData$rafale_10min), na.rm = 1), lwd = 2, col = "red")

text(30, 0.20, paste("N = ",nr," signalements" ),col = "black")
text(10, 0.10, paste("M = Mesure Nationale" ), col = "red")



sum(e$density)
sum(HH8$density)


########################################### comparatif de l'intensité de précipitation sur 1 h  #########################################

length(wdata$precipintensity) # mm/h
length(precData$precip_01h) # mm/h

range(wdata$precipintensity, na.rm = 1 )
range( (precData$precip_01h), na.rm = 1 )

Br9 <- seq(from=0 , to=15 , by=.01)
Br9
length(Br9)



f<-hist(wdata$precipintensity[wdata$precipintensity > 0.01], 
        breaks = Br9, 
        freq=F,
        col="grey",
        main = paste("Fréquence des morsures par intensité de précipitation (mm/h) \n ",nr," signalements  humains (France, 2017-20) \n (les valeurs 0 sont exlues car très nombreuses)"),
        ylab = "Denisté  (Somme=100)",
        xlab = "intensité de précipitation (mm/h)",
        ylim = c(0,20),
        xlim = c(0,1),

)

### courbe lissée
lines(density((precData$precip_01h), na.rm = 1), lwd = 2, col = "red")

### courbe non lissée
HH9 <- hist((precData$precip_01h), breaks = Br9,  plot = F)
lines(HH9$mids, HH9$density, lwd = 2, col = "green")

text(0.8, 15, paste("N = ",nr," signalements" ),col = "black")
text(.4, 5, paste("M = Mesure Nationale" ), col = "red")



sum(f$density)
sum(HH9$density)

########################################### comparatif de l'intensité de précipitation sur 24 h  #########################################

length(wdata$precipintensitymax) # mm/h
length(prec24Data$precip_24h) # mm/h

range(wdata$precipintensitymax, na.rm = 1 )
range( (prec24Data$precip_24h), na.rm = 1 )

Br9 <- seq(from=0 , to=72 , by=.01)
Br9
length(Br9)



f<-hist(wdata$precipintensitymax[wdata$precipintensitymax > 0.01], 
        breaks = Br9, 
        freq=F,
        col="grey",
        main = paste("Fréquence des morsures par intensité de précipitation sur 24 h (mm/h) \n ",nr," signalements  humains (France, 2017-20) \n (les valeurs 0 sont exlues car très nombreuses)"),
        ylab = "Denisté  (Somme=100)",
        xlab = "intensité de précipitation (mm/h)",
        ylim = c(0,10),
        xlim = c(0,3),
       
)

### courbe lissée
lines(density((prec24Data$precip_24h), na.rm = 1), lwd = 2, col = "red")

### courbe non lissée
HH9 <- hist((prec24Data$precip_24h), breaks = Br9,  plot = F)
lines(HH9$mids, HH9$density, lwd = 2, col = "green")

text(0.8, 15, paste("N = ",nr," signalements" ),col = "black")
text(.4, 5, paste("M = Mesure Nationale" ), col = "red")



sum(f$density)
sum(HH9$density)

#################################################### comparatif Température diurne ( de 6 à 21h ) #################################################### 
length(wdata$temperaturehigh)
length(tempDayData$temperature)

range(wdata$temperaturehigh, na.rm = 1)
range(tempDayData$temperature, na.rm = 1 )

BR10 <- seq(from= -1, to= 44, by=1)
BR10
length(BR10)



### histogramme de température diurnes comparé:
g<-hist(wdata$temperaturehigh, breaks = BR10, freq=F,
        col="grey",
        main = paste("Fréquence de signalements de piqûres de tiques en fonction des températures \n diurnes ( de 06 à 21h ) , ",nr," signalements  humains (France, 2017-20)"),
        ylab = "Denisté  (Somme=1)",
        xlab = "Températures diurnes (T°C)",
        ylim = c(0,.1),
        xlim = c(-10,40),
      
)

### courbe non lissée
HH10 <- hist(tempDayData$temperature, breaks = BR10,  plot=F)
lines(HH10$mids, HH10$density, lwd = 2, col = "green")

### courbe lissée, kernel
lines(density(tempDayData$temperature, na.rm = 1), lwd = 2, col = "red") 

text(30, 0.08, paste("N = ",nr," signalements" ),col = "black")
text(05, 0.08, paste("M = Mesure Nationale" ), col = "red")



sum(g$density)
sum(HH10$density)

#################################################### comparatif Température nocturnes (de 21 à 06h) #################################################### 
length(wdata$temperaturelow)
length(tempNightData$temperature )

range(wdata$temperaturelow, na.rm = 1)
range(tempNightData$temperature , na.rm = 1 )

BR11 <- seq(from= -9, to= 30, by=1)
BR11
length(BR11)


### histogramme de température nocturnes comparé:



h<-hist(wdata$temperaturelow, breaks = BR11, freq=F,
        col="grey",
        main = paste("Fréquence de signalements de piqûres de tiques en fonction des températures \n nocturnes ( de  21 à 06h ) , ",nr," signalements  humains (France, 2017-20)"),
        ylab = "Denisté  (Somme=1)",
        xlab = "Températures nocturnes (T°C)",
        ylim = c(0,.11),
        xlim = c(-10,30),
      
)

### courbe non lissée
HH11 <- hist(tempNightData$temperature, breaks = BR11,  plot=F)

lines(HH11$mids, HH11$density, lwd = 2, col = "green")

### courbe lissée, kernel
lines(density(tempNightData$temperature, na.rm = 1), lwd = 2, col = "red") 

text(23, 0.08, paste("N = ",nr," signalements" ),col = "black")
text(05, 0.08, paste("M = Mesure Nationale" ), col = "red")



sum(h$density)
sum(HH11$density)



#################################################### comparatif Température maximales 24h #################################################### 
length(wdata$temperaturemax)
length(tempMaxData$temp_max_24 )

range(wdata$temperaturemax, na.rm = 1)
range(tempMaxData$temp_max_24 , na.rm = 1 )

BR12 <- seq(from= -1, to= 44, by=1)
BR12
length(BR12)


### histogramme de température nocturnes comparé:



i<-hist(wdata$temperaturemax, breaks = BR12, freq=F,
        col="grey",
        main = paste("Fréquence de signalements de piqûres de tiques en fonction des températures \n maximales , ",nr," signalements  humains (France, 2017-20)"),
        ylab = "Denisté  (Somme=1)",
        xlab = "Températures maximales (T°C)",
        ylim = c(0,.3),
        xlim = c(-10,44),
      
)

### courbe non lissée
HH12 <- hist(tempMaxData$temp_max_24, breaks = BR12,  plot=F)

lines(HH12$mids, HH12$density, lwd = 2, col = "green")

### courbe lissée, kernel
lines(density(tempMaxData$temp_max_24, na.rm = 1), lwd = 2, col = "red") 

text(23, 0.08, paste("N = ",nr," signalements" ),col = "black")
text(05, 0.08, paste("M = Mesure Nationale" ), col = "red")



sum(i$density)
sum(HH12$density)

#################################################### comparatif Température minimales 24h #################################################### 
length(wdata$temperaturemin)
length(tempMinData$temp_min_24 )

range(wdata$temperaturemin, na.rm = 1)
range(tempMinData$temp_min_24 , na.rm = 1 )

BR13 <- seq(from= -18, to= 26, by=1)
BR13
length(BR13)


### histogramme de température minimales comparé:



j<-hist(wdata$temperaturemin, breaks = BR13, freq=F,
        col="grey",
        main = paste("Fréquence de signalements de piqûres de tiques en fonction des températures \n minimales , ",nr," signalements  humains (France, 2017-20)"),
        ylab = "Denisté  (Somme=1)",
        xlab = "Températures minimales (T°C)",
        ylim = c(0,.5),
        xlim = c(-18,30),
  
)

### courbe non lissée
HH13 <- hist(tempMinData$temp_min_24, breaks = BR13,  plot=F)

lines(HH13$mids, HH13$density, lwd = 2, col = "green")

### courbe lissée, kernel
lines(density(tempMinData$temp_min_24, na.rm = 1), lwd = 2, col = "red") 

text(23, 0.08, paste("N = ",nr," signalements" ),col = "black")
text(05, 0.08, paste("M = Mesure Nationale" ), col = "red")



sum(j$density)
    sum(HH13$density)

dev.off()

############################################################# EOS ####################################################################