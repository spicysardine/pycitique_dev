###### Comparaison des données Météo MF et DSK smartick_meteo_v5
###### Réalisation <V.godard@univ-paris8.fr> & khaldoune Hilami <khaldoune.hilami@yandex.com>


# Définition du répertoire de travail. NB: Il n'y null besoin de préciser le chemin absolu.
# "./" signifie répertoire courant, soit pycitique/R/citik_humains
getwd()
setwd("./")

## Pour la fonction qqPlot()
require(car)

# Import de la donnée comparative météo-France (MF) et darksky (DSK)
# La donnée MF est issue de 42 stations synoptiques réparties sur le territoire nationale
dskdatavg<- read.csv(
        "../../data/donnee_meteo_nationale_comparative/darksky/darksky_moyennes_journalieres_maille_42.csv", 
        header = TRUE, sep = ",", dec = ".")
mfdatavg <- read.csv(
        "../../data/donnee_meteo_nationale_comparative/meteoFrance/mf_moyennes_journalieres_maille_42.csv",
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
             main = paste("Average",paramName,"(", SIunit, ")"," \n between 1/1/2017 and 5/4/2020, i.e. 1191 days"),
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
        
        legend(
                "topright",
                legend=c("Darksky","MF lissée", "MF brute"),
                col = c("grey","blue", "green"),
                lty=c(1,1,1),
                cex=c(.75,.75,.75)
               )
        
        # text(01, 0.1, paste("Darksky"), cex = 1.2,  col = "black") ## légende difficile à caler pour tous les graphiques !
        # text(01, 0.2, paste("Météo France"), cex = 1.2,  col = "blue") ## légende difficile à caler pour tous les graphiques !
        
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
templist     <- list(dskParam=dskdatavg$temperature,        mfParam=mfdatavg$temperature,       Breaks=1,   paramName='Température',                    SIunit='C°' )
humlist      <- list(dskParam=dskdatavg$humidity,           mfParam=trunc(mfdatavg$humidite),   Breaks=1,   paramName='Humidité',                       SIunit='%'  )
ptrlist      <- list(dskParam=dskdatavg$dewpoint,           mfParam=mfdatavg$point_rose,        Breaks=1,   paramName='Point de rosée',                 SIunit='C°' )
presslist    <- list(dskParam=dskdatavg$pressure,           mfParam=mfdatavg$press_mer,         Breaks=1,   paramName='Pression',                       SIunit='hPa')
vvlist       <- list(dskParam=dskdatavg$windspeed,          mfParam=mfdatavg$vvent,             Breaks=0.5, paramName='Vitesse du vent',                SIunit='m/s')
visiblist    <- list(dskParam=dskdatavg$visibility,         mfParam=mfdatavg$visibilite,        Breaks=1,   paramName='Visibilité',                     SIunit='%'  )
neblist      <- list(dskParam=dskdatavg$cloudcover,         mfParam=trunc(mfdatavg$nebulosite), Breaks=2,   paramName='Nébulosité',                     SIunit='%'  )
raflist      <- list(dskParam=dskdatavg$windgust,           mfParam=mfdatavg$rafale_10min,      Breaks=0.5, paramName='rafale de vent sur 10 min',      SIunit='m/s')
preciplist01 <- list(dskParam=dskdatavg$precipintensity,    mfParam=mfdatavg$precip_01h,        Breaks=0.1, paramName='Intensité de précipitation 1h',  SIunit='%'  )
preciplist24 <- list(dskParam=dskdatavg$precipintensitymax, mfParam=mfdatavg$precip_24h,        Breaks=0.1, paramName='Intensité de précipitation 24h', SIunit='%'  )

## Liste nichée principale, contenant les listes de paramètres
paramlist <- list(templist, humlist, ptrlist, presslist, vvlist, visiblist, neblist, raflist, preciplist01, preciplist24)

## Boucle principale de fabrication des graphiques et calcules statistics
for( param in paramlist) {
        
        print(param$paramName)
  
        ## Histogramme des paramètres moyennes
        weatherHistogram(param$dskParam, param$mfParam, param$Breaks, param$paramName, param$SIunit)
        
        ## Test des distributions statistiques
        weatherStats(param$dskParam, param$mfParam, param$paramName, param$SIunit)
}
