###### Comparaison des données Météo MF et DSK smartick_meteo_v5

# Définition du répertoire de travail. NB: Il n'y null besoin de préciser le chemin absolu.
# "./" signifie répertoire courant, soit pycitique/R/citik_humains
getwd()
setwd("./")

## Pour la fonction qqPlot()
require(car)

# Import de la donnée comparative météo-France (MF) et darksky (DSK)
# La donnée MF est issue de 42 stations synoptiques réparties sur le territoire nationale
dskdatavg<- read.csv("../../data/donnee_meteo_nationale_comparative/darksky/darksky_moyennes_journalieres_maille_42.csv", 
                     header = TRUE, sep = ",", dec = ".")
mfdatavg <- read.csv("../../data/donnee_meteo_nationale_comparative/meteoFrance/mf_moyennes_journalieres_maille_42.csv",
                     header = TRUE, sep = ",", dec = ".")

## vérification des jeux de donnée
ls(dskdatavg)
ls(mfdatavg)

##sommaire de la donnée et analyse primaire
summary(dskdatavg)
summary(mfdatavg)

### Création du template pour la fonction de génération de l'histogramme
weatherHistogram <- function(paramDSK, paramMF, Breaks, paramName, SIunit){
        
        ### Préparation des intervals
        rangeDsk <- range(paramDSK, na.rm = TRUE)
        rangeMf <- range(paramMF, na.rm = TRUE)
        rangeAll <- c(rangeDsk,rangeMf)
        minBreak <- min(rangeAll)
        maxBreak <- max(rangeAll)
        
        #### définition des intervalles
        Brx <- seq(from= minBreak-10, to= maxBreak+10, by=Breaks) ## tient compte des deux distributions
        
        HHdens <- hist(paramDSK, breaks = Brx, plot = F)
        HHylimax <- max(HHdens$density)
        HHylimin <- min(HHdens$density)
        
        ### freq=F => des fréquences relatives et pas des effectifs
        hist(paramDSK, breaks=Brx,
             freq=F, # fréquences
             col="grey",
             main = paste("Moyenne de ",paramName,SIunit," \n entre 1/1/2017 et 5/4/2020 soit 1191 jours"),
             ylab = "densités",
             xlab = paste("Moyenne de ",paramName,SIunit),
             ylim = c(HHylimin+(HHylimin*25),HHylimax+(HHylimax*.25)),
             cex.main = 1.3,
             cex.lab = 1.5,
             cex.axis = 1.5
        )
        
        ### calcul des paramètres pour la fonction lines() à superposer à l'histo
        HHx <- hist(paramMF, breaks = Brx,  plot=F)
        lines(HHx$mids, HHx$density, lwd = 2, col = "green") ### courbe non lissée 
        lines(density(paramMF, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel
        
}

### Création du template pour la fonction des tests statistiques
weatherStats <- function(paramDSK, paramMF, paramName, SIunit){
        
        ######### Test des distributions statistiques
        cat("\n####### Test de normalité ",paramName," ",SIunit,"#######\n")
        qqPlot(paramDSK, ylab = paramName)
        qqPlot(paramMF, ylab = paramName)
        cat("\n Fait: voir les grahiques dans la case des Plots de Rstudio \n")
        
        cat("\n####### Shapiro test #######\n")
        print( shapiro.test(paramDSK) ) 
        print( shapiro.test(paramMF) ) 
        
        cat("\n#######  t.test entre les différentes DSK et MF ",paramName," ",SIunit,"#######\n")
        print(t.test(paramDSK, paramMF))
        
        cat("\n####### fabrication des données pour le test KW ",paramName," ",SIunit,"#######\n")
        dskdf <- data.frame(param=paramDSK, type='dsk')
        mfdf <- data.frame(param=paramMF, type='mf')
        kwdata <- rbind(dskdf,mfdf)
        cat("Objet de donnée pour KW Fabriqué:\n")
        cat("Sommaire de la donnée:\n")
        print( summary(kwdata) )
        
        cat("\n####### Kruskal-Wallis test ",paramName," ",SIunit,"#######\n")
        ## ~ signifie : "en fonction de"
        print( kruskal.test(param ~ type, data = kwdata) ) 
        
        cat("\n####### Test de wilcoxon ",paramName," ",SIunit,"#######\n")
        print( wilcox.test(param ~ type, data = kwdata) )
}

####### boucle de calcul itératif

### Définition des listes des paramètres

templist <- list(dskParam=dskdatavg$temperature, mfParam=mfdatavg$temperature, Breaks=1, paramName='Température', SIunit='C°')
humlist <- list(dskParam=dskdatavg$humidity, mfParam=trunc(mfdatavg$humidite), Breaks=1, paramName='Humidité', SIunit='%')
ptrlist <- list(dskParam=dskdatavg$dewpoint, mfParam=mfdatavg$point_rose, Breaks=1, paramName='Point de rosée', SIunit='C°')
presslist <- list(dskParam=dskdatavg$pressure, mfParam=mfdatavg$press_mer, Breaks=1, paramName='Pression', SIunit='hPa')
vvlist <- list(dskParam=dskdatavg$windspeed, mfParam=mfdatavg$vvent, Breaks=0.5, paramName='Vitesse du vent', SIunit='m/s')
visiblist <- list(dskParam=dskdatavg$visibility, mfParam=mfdatavg$visibilite, Breaks=1, paramName='Visibilité', SIunit='%')
neblist <-list(dskParam=dskdatavg$cloudcover, mfParam=trunc(mfdatavg$nebulosite), Breaks=2, paramName='Nébulosité', SIunit='%')
raflist <- list(dskParam=dskdatavg$windgust, mfParam=mfdatavg$rafale_10min, Breaks=0.5, paramName='rafale de vent sur 10 min', SIunit='m/s')
preciplist01 <- list(dskParam=dskdatavg$precipintensity, mfParam=mfdatavg$precip_01h, Breaks=0.1, paramName='Intensité de précipitation 1h', SIunit='%')
preciplist24 <- list(dskParam=dskdatavg$precipintensitymax, mfParam=mfdatavg$precip_24h, Breaks=0.1, paramName='Intensité de précipitation 24h', SIunit='%')

paramlist <- list(templist, humlist, ptrlist, presslist, vvlist, visiblist, neblist, raflist, preciplist01, preciplist24)
length(paramlist)

for( param in paramlist) {
        
        print(param$paramName)
        ### Histogramme du paramètres moyennes
        weatherHistogram(param$dskParam, param$mfParam, param$Breaks, param$paramName, param$SIunit)
        
        ### Test des distributions statistiques
        weatherStats(param$dskParam, param$mfParam, param$paramName, param$SIunit)
}

# ############################## Analyse des Températures moyennes ###############################################
# 
# ### Affectation des variables:
# ## initialisation de la donnée.
# dsktemperature <- dskdatavg$temperature
# mftemperature <- mfdatavg$temperature
# 
# ### Histogramme des Températures moyennes
# weatherHistogram(dsktemperature, mftemperature, Breaks=1, 'Température', 'C°')
# 
# ######### Test des distributions statistiques
# weatherStats(dsktemperature, mftemperature, 'Température C°')
# 
# ############################## Analyse de l'humidité moyenne DSK vs MF #########################################
# 
# ### Affectation des variables:
# ## Attention la donnée de l'humidité MF contient des fractions. e.g: 83.66
# ## tandis que la donnée DSK n'en contient pas. e.g: 83
# dskhumidity <- dskdatavg$humidity
# mfhumidity <- trunc(mfdatavg$humidite)
# 
# ### Histogramme de l'humidité moyenne "humidite" pour MF et "humidity" pour DSK
# weatherHistogram(dskhumidity, mfhumidity, Breaks = 1,'Humidité', '%')
# 
# ######### Test de distributions statistiques:
# weatherStats(dskhumidity, mfhumidity, 'Humidité %')
# 
# ############################## Analyse du point de rosé moyen ##########################################
# 
# ### Affectation des variables:
# dskpr <- dskdatavg$dewpoint
# mfpr <- mfdatavg$point_rose
# 
# ############# Histogramme du point de rosé moyen
# weatherHistogram(dskpr, mfpr, Breaks = 1, 'Point de rosée', 'C°')
# 
# ########## Test de distributions statistiques:
# weatherStats(dskpr, mfpr, 'Point de rosée C°')
# 
# 
# ##############################  Analyse de la pression moyenne au niveau de la mer ##############################
# 
# ### Affectation des variables:
# dskpress <- dskdatavg$pressure
# mfpress <- mfdatavg$press_mer
# 
# ############ Histogramme de la pression moyenne au niveau de la mer "press_mer" pour MF et "pressure" pour DSK
# weatherHistogram(dskpress, mfpress, Breaks=1 ,'Pression', 'hPa')
# 
# ########## Test de distributions statistiques:
# weatherStats(dskpress, mfpress, 'Pression hPa')
# 
# ##############################  Analyse de la vitesse moyenne du vent ###########################################
# 
# ### Affectation des variables:
# dskwindspeed <- dskdatavg$windspeed
# mfwindspeed <- mfdatavg$vvent
# 
# ### Histogramme de la vitesse moyenne du vent "vvent" pour MF et "windspeed" pour DSK
# weatherHistogram(dskwindspeed, mfwindspeed, Breaks=0.5, 'Vitesse du vent', 'm/s')
# 
# ########## Test de distributions statistiques:
# weatherStats(dskwindspeed, mfwindspeed, 'Vitesse moyenne')
# 
# ##############################  Analyse de la visibilite moyenne ################################################
# 
# ### Affectation des variables:
# dskvisibility <- dskdatavg$visibility
# mfvisibility <- mfdatavg$visibilite
# 
# ################ Histogramme de la visibilite moyenne "visibilite" pour MF et "visibility"
# weatherHistogram(dskvisibility, mfvisibility, Breaks = 1 ,'Visibilité', '%')
# 
# ########## Test de distributions statistiques:
# weatherStats(dskvisibility, mfvisibility, 'Visibilité %')
# 
# ##############################  Analyse de la nebulosité moyenne ################################################
# 
# ### Affectation des variables:
# ## Attention la donnée de la nébulosité moyenne MF contient des fractions. e.g: 83.66
# ## tandis que la donnée DSK n'en contient pas. e.g: 83
# dskcloud <- dskdatavg$cloudcover
# mfcloud <- trunc(mfdatavg$nebulosite)
# 
# ### Histogramme de la nebulosité moyenne "nebulosite" pour MF et "cloudcover" pour DSK
# weatherHistogram(dskcloud, mfcloud, Breaks = 2, 'Nébulosité', '%')
# 
# ########## Test de distributions statistiques:
# weatherStats(dskcloud, mfcloud, 'Nébulosité %')
# 
# ##############################  Analyse de la rafale_10min moyenne #############################################
# 
# ### Affectation des variables:
# dskwg <- dskdatavg$windgust
# mfwg <- mfdatavg$rafale_10min
# 
# ### Histogramme de la rafale_10min moyenne "rafale_10min" pour MF et "windgust" pour DSK
# weatherHistogram(dskwg, mfwg, Breaks = .5,'rafale de vent sur 10 min', 'm/s')
# 
# ########## Test de distributions statistiques:
# weatherStats(dskwg, mfwg, 'Rafale de vent sur 10 min m/s')
# 
# ##############################  Analyse de la quantité moyenne de précipitation par heure ######################
# 
# ### Affectation des variables:
# dskprecip <- dskdatavg$precipintensity
# mfprecip <- mfdatavg$precip_01h
# 
# ### Histogramme de la precip_24h moyenne "precip_24h" pour MF et "precipintensity" pour DSK
# weatherHistogram(dskprecip, mfprecip, Breaks=0.1, 'intensité de Précipitation', 'mm/h')
# 
# ########## Test de distributions statistiques:
# weatherStats(dskprecip, mfprecip, 'Précipitation mm/h')
# 
# 
# ##############################  Analyse de la quantité moyenne de précipitation par 24 heure ######################
# 
# ### Affectation des variables:
# dskprecip24 <- dskdatavg$precipintensitymax
# mfprecip24 <- mfdatavg$precip_24h
# 
# ### Histogramme de la precip_24h moyenne "precip_24h" pour MF et "precipintensity" pour DSK
# weatherHistogram(dskprecip24, mfprecip24, Breaks=0.1, 'intensité de Précipitation', 'mm/h')
# 
# ########## Test de distributions statistiques:
# weatherStats(dskprecip24, mfprecip24, 'Précipitation mm/h')
# 
# 
# 
# 



