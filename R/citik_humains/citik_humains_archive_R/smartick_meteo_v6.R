##### Code figure n°5 – Profils comparés de 9 paramètres météorologiques pours 42 stations synoptiques de Météo France
##### et leur équivalent Dark Sky (France, July 2017 – April 2020, soit 995 jours).

###### Code Tableau n°2 – Comparaison de la moyenne quotidienne de 9 paramètres météorologiques pour 42 stations synoptiques de Météo France

##### et leur équivalent Dark Sky (France, July 2017 – April 2020, soit 995 jours).

######
###### Comparaison des données Météo MF et DSK

# 1. Introduction
### Alloue la librairie "set()" puis vérifie que c'est bien importé "getwd()", il pointe dessus!

# 2. Lab 1 : Importation de données dsk_moyennes_journalieres_maille_42.csv et mf_moyennes_journalieres_maille_42.csv (issues de 42 stations météo de MF)

### Or, if .csv file, use this si données séparées par ";"
DSKdata_tot <- read.csv("../../../pycitique/data/donnee_meteo_nationale_comparative/comparaison/dsk_moyennes_journalieres_maille_42.csv", header = TRUE, sep = ";", dec = ".")

MFdata_tot <- read.csv("../../../pycitique/data/donnee_meteo_nationale_comparative/comparaison/mf_moyennes_journalieres_maille_42.csv", header = TRUE, sep = ";", dec = ".")

## 2.1 Création sous tableau de données > 14/07/2017

### 2.1.1 Constitution

## création d'un champ date "date_releve_DMY" où "DSKdata_tot$date_releve" de char(ractère) devient date
#
## création d'un champ date "date_iso_YMD" où "MFdata_tot$date_iso" de char(ractère) devient date

# install.packages(c("lubridate", "magrittr")) ##♥ https://fr.wikibooks.org/wiki/Programmer_en_R/Manipuler_les_dates_et_les_heures

library("lubridate")
#library("magrittr")

## pour lire les dates DSK qui sont DD/MM/YYYY
DSKdata_tot$date_releve_DMY <- dmy(DSKdata_tot$date_releve)

## pour lire les dates MF qui sont YYYY-MM-DD
MFdata_tot$date_iso_YMD <- ymd(MFdata_tot$date_iso)


###☺ 2.1.2 sous tableaux temporels
DSKdata  <- subset(DSKdata_tot, DSKdata_tot$date_releve_DMY > "2017-07-14") ## seulement pour les signalements >= 15/07/2020

head(DSKdata[, 1:7], 3)

MFdata  <- subset(MFdata_tot, MFdata_tot$date_iso_YMD > "2017/07/14") ## seulement pour les signalements >= 15/07/2020

head(MFdata[, 1:7], 3)


## L'étendue se calcule sur le champ date
range(DSKdata$date_releve_DMY, na.rm = TRUE)
## [1] "2017-07-15" "2020-04-05"

range(MFdata$date_iso_YMD, na.rm = TRUE)
##♠ [1] "2017-07-15" "2020-04-05"


### 2.1.3 Analyse

ls(DSKdata) ## liste les variables
ls(MFdata)


str(DSKdata) ##  a diagnostic function and an alternative to summary
str(MFdata)


summary(DSKdata)
summary(MFdata)



## 2.2 Analyse des Températures moyennes DSK vs MF (méthode Alice Favre)

### 2.2.1. Histogramme des Températures moyennes "temperature" pour MF et "temperature" pour DSK = (tempHigh + tempLow)/2 pour 42 stations Météo France

####Après avoir vérifié que le nombre d'obs n'est pas égal avec
length(DSKdata$temperature)
length(na.omit(DSKdata$temperature)) ## pour tester si NA
length(MFdata$temperature)
length(na.omit(MFdata$temperature)) ## pour tester si NA


##### et que l'étendue n'est pas la même
range(DSKdata$temperature, na.rm = TRUE)
range(MFdata$temperature, na.rm = TRUE)

#### on définit les breaks pour l'abscisse commune
BRt <- seq(from= -5, to= 30, by=2) ## tient compte des deux distributions

#### puis on fait l'histo en utilisant

### freq=F => des fréquences relatives et pas des effectifs

hist(DSKdata$temperature, breaks = BRt,
     freq=F, # fréquences
     col="grey",
     main = "Average Temperatures for 42 Météo France Synoptic Stations \n and Dark Sky (France, july 2017 - february 2020), 995 days",
     ylab = "Densities",
     xlab = "Average Temperatures (T°C)"
     )

### calcul des paramètres pour la fonction lines() à superposer à l'histo

HH2 <- hist(MFdata$temperature, breaks = BRt,  plot=F)

    #lines(HH2$mids, HH2$density, lwd = 2, col = "orange") ### courbe non lissée  ## SO
    lines(density(MFdata$temperature, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel

text(03, 0.07, paste("Dark Sky"), cex = 1.2,  col = "black")
text(02, 0.06, paste("Météo France"), cex = 1.2,  col = "blue")

## Si on voulait le nb d'enregistrements tudiés à la place du texte "Dark Sky" ou MF, il faudrait :
    ### text(03, 0.07, paste("N =", sum(complete.cases(DSKdata$temperature))), cex = 1.2,  col = "black")
    ### text(02, 0.06, paste("N =", sum(complete.cases(MFdata$temperature))), cex = 1.2,  col = "blue")

## Si on voulait les courbes à +1.5°C ou +2°C, il faudrait fabriquer et afficher :
    ### lines(density(MFdata$temperature_p15D, na.rm = TRUE), lwd = 2, col = "green") ### courbe liss?e, kernel
    ### lines(density(MFdata$temperature_p2D, na.rm = TRUE), lwd = 2, col = "blue") ### courbe liss?e, kernel


### 2.2.2. Test des distributions appariées
## References :
## Patrick Royston (1982). An extension of Shapiro and Wilk's W test for normality to large samples. Applied Statistics, 31, 115–124. doi: 10.2307/2347973.

shapiro.test(DSKdata$temperature) # W = 0.97673, p-value = 6.398e-13 => p-value significative, l'échantillon ne suit pas une loi normale.

shapiro.test(MFdata$temperature) # W = 0.97696, p-value = 7.649e-13 => p-value significative, l'échantillon ne suit pas une loi normale.


### t.test entre les différentes températures
    t.test(MFdata$temperature,DSKdata$temperature, paired = TRUE) 
    # t = -16.503, df = 995, p-value < 2.2e-16 => p-value significative, les 2 échantillons seraient significativement différents !

    # t.test(MFdata$temperature_p1D,DSKdata$temperature) ## SO
    # t.test(MFdata$temperature_p15D,DSKdata$temperature) ## SO
    # t.test(MFdata$temperature_p2D,DSKdata$temperature) ## SO

## References :
## Myles Hollander and Douglas A. Wolfe (1973), Nonparametric Statistical Methods. New York: John Wiley & Sons. Pages 115–120.

## http://www.sthda.com/english/wiki/paired-samples-wilcoxon-test-in-r

##♥ Pour répondre à la question : Is there any significant changes in the temperature between Météo France et Dark Sky?


    ### /!\ abandon de cette partie => allez à la création de sous ensembles (15 lignes plus bas)

    ### Import des données pour le test wilcox.test() sur données MFdata$temperature, DSKdata$temperature in : KW_temp_maille_42.csv
    WCtempdata <- read.csv("../../data/donnee_meteo_nationale_comparative/comparaison/KW_temp_maille_42.csv", header = TRUE, sep = ";", dec = ".")
    summary(WCtempdata)

    wilcox.test(temp ~ type, data = WCtempdata) ## ~ signifie : "en fonction de"

    ## Wilcoxon rank sum test with continuity correction
    ## data:  temp by type
    ## W = 736073, p-value = 0.1099 => p-value non significative, les 2 échantillons ne sont pas significativement différents !

    ## création de deux sous-ensembles "subset"s" avec juste les variables à comparer et l'origine de l'éditeur

    d1 <- subset(DSKdata, select = c("temperature", "origine")) ## création du subset venant de DSK http://larmarange.github.io/analyse-R/fusion-de-tables.html
    dim(d1)
    d2 <- subset(MFdata, select = c("temperature", "origine")) ## création du subset venant de MF
    dim(d2)

    ## pas utile ici, ils ont le mêm nom "temperature" chez DSK et MF
    ## pour utiliser la fusion "rbind" il faut que les champs aient le même nom (c'est le 1er qui pose problème)
    names(d2)[1]
    names(d2)[1]<-"temperature" ## renommage du 1er champ du tableau 2 https://forum.framasoft.org/viewtopic.php?p=75052
    names(d2)[1]

    WCtemperaturedata <- rbind(d1, d2) ## fusion des deux sous tableaux
    head(WCtemperaturedata)

    summary(WCtemperaturedata)

    wilcox.test(temperature ~ origine, data = WCtemperaturedata) ## ~ signifie : "en fonction de"
    res <- wilcox.test(temperature ~ origine, data = WCtemperaturedata)
    aa <- res$statistic
    ab <- res$p.value
    ac <- res$data.name
    aa
    ab
    ac

    ## Wilcoxon rank sum test with continuity correction
    ## data:  temperature by origine
    ## W = 515491, p-value = 0.1291 => p-value non significative, les 2 échantillons ne sont pas significativement différents !


## 2.3. Analyse de l'humidité moyenne DSK vs MF (méthode Alice Favre)


### 2.3.1. Histogramme de l'humidité moyenne "humidite" pour MF et "humidity" pour DSK pour 42 stations Météo France

####Après avoir vérifié que le nombre d'obs n'est pas égal avec
    length(DSKdata$humidity)
    length(na.omit(DSKdata$humidity)) ## pour tester si NA
    length(MFdata$humidite)
    length(na.omit(MFdata$humidite)) ## pour tester si NA


##### et que l'étendue n'est pas la même
    range(DSKdata$humidity, na.rm = TRUE)
    range(MFdata$humidite, na.rm = TRUE)

#### on définit les breaks pour l'abscisse commune
    BRt <- seq(from= 45, to= 95, by=2) ## tient compte des deux distributions

#### puis on fait l'histo en utilisant

### freq=F => des fréquences relatives et pas des effectifs

    hist(DSKdata$humidity, breaks = BRt,
         freq=F, # fréquences
         col="grey",
         main = "Average Humidity for 42 Météo France Synoptic Stations \n and Dark Sky (France, july 2017 - february 2020), 995 days",
         ylab = "Densities",
         xlab = "Average Humidity (%)"
    )

### calcul des paramètres pour la fonction lines() à superposer à l'histo

    HH2 <- hist(MFdata$humidite, breaks = BRt,  plot=F)

#lines(HH2$mids, HH2$density, lwd = 2, col = "orange") ### courbe non lissée  ## SO
    lines(density(MFdata$humidite, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel

    text(60, 0.04, paste("Dark Sky"), cex = 1.2,  col = "black")
    text(55, 0.03, paste("Météo France"), cex = 1.2,  col = "blue")

## Si on voulait le nb d'enregistrements tudiés à la place du texte "Dark Sky" ou MF, il faudrait :
    ### text(03, 0.07, paste("N =", sum(complete.cases(DSKdata$humidity))), cex = 1.2,  col = "black")
    ### text(02, 0.06, paste("N =", sum(complete.cases(MFdata$humidite))), cex = 1.2,  col = "blue")

## Si on voulait les courbes à +1.5°C ou +2°C, il faudrait fabriquer et afficher :
    ### lines(density(MFdata$humidite_p15D, na.rm = TRUE), lwd = 2, col = "green") ### courbe liss?e, kernel
    ### lines(density(MFdata$humidite_p2D, na.rm = TRUE), lwd = 2, col = "blue") ### courbe liss?e, kernel


### 2.3.2. Test des distributions appariées
## References :
## Patrick Royston (1982). An extension of Shapiro and Wilk's W test for normality to large samples. Applied Statistics, 31, 115–124. doi: 10.2307/2347973.

    shapiro.test(DSKdata$humidity) # W = 0.97228, p-value = 7.44e-13 => p-value significative, l'échantillon ne suit pas une loi normale.

    shapiro.test(round(MFdata$humidite)) # W = 0.97469, p-value = 3.687e-12 => p-value significative, l'échantillon ne suit pas une loi normale.

qqplot(round(MFdata$humidite),DSKdata$humidity)

### t.test entre les différentes températures
    t.test(round(MFdata$humidite),DSKdata$humidity) # t = 2.9493, df = 1979.5, p-value = 0.003222 => p-value significative, les 2 échantillons sont significativement différents !

    # t.test(MFdata$humidite_p1D,DSKdata$humidity) ## SO
    # t.test(MFdata$humidite_p15D,DSKdata$humidity) ## SO
    # t.test(MFdata$humidite_p2D,DSKdata$humidity) ## SO

## References :
## Myles Hollander and Douglas A. Wolfe (1973), Nonparametric Statistical Methods. New York: John Wiley & Sons. Pages 115–120.

## http://www.sthda.com/english/wiki/paired-samples-wilcoxon-test-in-r


    ### /!\ abandon de cette partie => allez à la création de sous ensembles (15 lignes plus bas)
    ### Import des données pour le test wilcox.test() sur données MFdata$humidite, DSKdata$humidity in : KW_temp_maille_42.csv
    WChumdata <- read.csv("../../data/donnee_meteo_nationale_comparative/comparaison/KW_hum_maille_42.csv", header = TRUE, sep = ";", dec = ".")
    summary(WChumdata)

    wilcox.test(round(hum) ~ type, data = WChumdata) ## ~ signifie : "en fonction de"

    ## Wilcoxon rank sum test with continuity correction
    ## data:  round(hum) by type
    ## W = 658562, p-value = 0.002515 => p-value significative, les 2 échantillons sont significativement différents !


    ## création de deux sous-ensembles "subset"s" avec juste les variables à comparer et l'origine de l'éditeur

    d1 <- subset(DSKdata, select = c("humidity", "origine")) ## création du subset venant de DSK http://larmarange.github.io/analyse-R/fusion-de-tables.html
    dim(d1)
    d2 <- subset(MFdata, select = c("humidite", "origine")) ## création du subset venant de MF
    dim(d2)

    ## pour utiliser la fusion "rbind" il faut que les champs aient le même nom (c'est le 1er qui pose problème)
    names(d2)[1]
    names(d2)[1]<-"humidity" ## renommage du 1er champ du tableau 2 https://forum.framasoft.org/viewtopic.php?p=75052
    names(d2)[1]

    WChumiditydata <- rbind(d1, d2) ## fusion des deux sous tableaux
    head(WChumiditydata)

    summary(WChumiditydata)

    wilcox.test(humidity ~ origine, data = WChumiditydata) ## ~ signifie : "en fonction de"
    res <- wilcox.test(humidity ~ origine, data = WChumiditydata)

    ba <- res$statistic
    bb <- res$p.value
    bc <- res$data.name
    ba
    bb
    bc

    ## Wilcoxon rank sum test with continuity correction
    ## data:  humidity by origine
    ## W = 456287, p-value = 0.001969 => p-value significative, les 2 échantillons sont significativement différents !





## 2.4. Analyse du point de rosé moyen DSK vs MF (méthode Alice Favre)


### 2.4.1. Histogramme du point de rosé moyen "point_rose" pour MF et "dewpoint" pour DSK pour 42 stations Météo France

####Après avoir vérifié que le nombre d'obs n'est pas égal avec
    length(DSKdata$dewpoint)
    length(na.omit(DSKdata$dewpoint)) ## pour tester si NA
    length(MFdata$point_rose)
    length(na.omit(MFdata$point_rose)) ## pour tester si NA


##### et que l'étendue n'est pas la même
    range(DSKdata$dewpoint, na.rm = TRUE)
    range(MFdata$point_rose, na.rm = TRUE)

#### on définit les breaks pour l'abscisse commune
    BRt <- seq(from= -15, to= 20, by=2) ## tient compte des deux distributions

#### puis on fait l'histo en utilisant

### freq=F => des fréquences relatives et pas des effectifs

    hist(DSKdata$dewpoint, breaks = BRt,
         freq=F, # fréquences
         col="grey",
         main = "Average Dew Point for 42 Météo France Synoptic Stations \n and Dark Sky (France, july 2017 - february 2020), 995 days",
         ylab = "Densities",
         xlab = "Average Dew Point (T°C)"
    )

### calcul des paramètres pour la fonction lines() à superposer à l'histo

    HH2 <- hist(MFdata$point_rose, breaks = BRt,  plot=F)

#lines(HH2$mids, HH2$density, lwd = 2, col = "orange") ### courbe non lissée  ## SO
    lines(density(MFdata$point_rose, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel

    text(-5, 0.05, paste("Dark Sky"), cex = 1.2,  col = "black")
    text(-10, 0.03, paste("Météo France"), cex = 1.2,  col = "blue")

    ## Si on voulait le nb d'enregistrements étudiés à la place du texte "Dark Sky" ou MF, il faudrait :
    ### text(03, 0.07, paste("N =", sum(complete.cases(DSKdata$dewpoint))), cex = 1.2,  col = "black")
    ### text(02, 0.06, paste("N =", sum(complete.cases(MFdata$point_rose))), cex = 1.2,  col = "blue")

    ## Si on voulait les courbes à +1.5°C ou +2°C, il faudrait fabriquer et afficher :
    ### lines(density(MFdata$point_rose_p15D, na.rm = TRUE), lwd = 2, col = "green") ### courbe liss?e, kernel
    ### lines(density(MFdata$point_rose_p2D, na.rm = TRUE), lwd = 2, col = "blue") ### courbe liss?e, kernel


### 2.4.2. Test des distributions appariées
## References :
## Patrick Royston (1982). An extension of Shapiro and Wilk's W test for normality to large samples. Applied Statistics, 31, 115–124. doi: 10.2307/2347973.

    shapiro.test(DSKdata$dewpoint) # W = 0.97812, p-value = 1.924e-12 => p-value significative, l'échantillon ne suit pas une loi normale.

    shapiro.test(MFdata$point_rose) # W = 0.97813, p-value = 1.932e-12 => p-value significative, l'échantillon ne suit pas une loi normale.


### t.test entre les différentes températures
    t.test(MFdata$point_rose,DSKdata$dewpoint) # t = 0.063932, df = 2378.9, p-value = 0.949 => p-value non significative, les 2 échantillons ne sont pas significativement différents !

    # t.test(MFdata$point_rose_p1D,DSKdata$dewpoint) ## SO
    # t.test(MFdata$point_rose_p15D,DSKdata$dewpoint) ## SO
    # t.test(MFdata$point_rose_p2D,DSKdata$dewpoint) ## SO

## References :
## Myles Hollander and Douglas A. Wolfe (1973), Nonparametric Statistical Methods. New York: John Wiley & Sons. Pages 115–120.

    ## http://www.sthda.com/english/wiki/paired-samples-wilcoxon-test-in-r

### Import des données pour le test wilcox.test() sur données MFdata$point_rose, DSKdata$dewpoint

    ## création de deux sous-ensembles "subset"s" avec juste les variables à comparer et l'origine de l'éditeur

    d1 <- subset(DSKdata, select = c("dewpoint", "origine")) ## création du subset venant de DSK http://larmarange.github.io/analyse-R/fusion-de-tables.html
    dim(d1)
    head(d1, 3)
    d2 <- subset(MFdata, select = c("point_rose", "origine")) ## création du subset venant de MF
    dim(d2)
    head(d2, 3)

    ## pour utiliser la fusion "rbind" il faut que les champs aient le même nom (c'est le 1er qui pose problème)
    names(d2)[1]
    names(d2)[1]<-"dewpoint" ## renommage du 1er champ du tableau 2 https://forum.framasoft.org/viewtopic.php?p=75052
    names(d2)[1]

    WCdewpointdata <- rbind(d1, d2) ## fusion des deux sous tableaux
    head(WCdewpointdata)

    summary(WCdewpointdata)

    wilcox.test(dewpoint ~ origine, data = WCdewpointdata) ## ~ signifie : "en fonction de"
    res <- wilcox.test(dewpoint ~ origine, data = WCdewpointdata)

    ca <- res$statistic
    cb <- res$p.value
    cc <- res$data.name
    ca
    cb
    cc


    ## Wilcoxon rank sum test with continuity correction
    ## data:  dewpoint by origine
    ## W = 494687, p-value = 0.9181 => p-value non significative, les 2 échantillons ne sont pas significativement différents !



## 2.5. Analyse de la pression moyenne au niveau de la mer DSK vs MF (méthode Alice Favre)


### 2.5.1. Histogramme de la pression moyenne au niveau de la mer "press_mer" pour MF et "pressure" pour DSK pour 42 stations Météo France

####Après avoir vérifié que le nombre d'obs n'est pas égal avec
    length(DSKdata$pressure)
    length(na.omit(DSKdata$pressure))
    length(MFdata$press_mer)
    length(na.omit(MFdata$press_mer))

##### et que l'étendue n'est pas la même
    range(DSKdata$pressure, na.rm = TRUE)
    range(MFdata$press_mer, na.rm = TRUE)

#### on définit les breaks pour l'abscisse commune
    BRt <- seq(from= 980, to= 1045, by=2) ## tient compte des deux distributions

    #### puis on fait l'histo en utilisant

    ### freq=F => des fréquences relatives et pas des effectifs

    hist(DSKdata$pressure, breaks = BRt,
         freq=F, # fréquences
         col="grey",
         main = "Average Pressure for 42 Météo France Synoptic Stations \n and Dark Sky (France, july 2017 - february 2020), 995 days",
         ylab = "Densities",
         xlab = "Average pressure (hPa)"
    )

    ### calcul des paramètres pour la fonction lines() à superposer à l'histo

    HH2 <- hist(MFdata$press_mer, breaks = BRt,  plot=F)

    #lines(HH2$mids, HH2$density, lwd = 2, col = "orange") ### courbe non lissée  ## SO
    lines(density(MFdata$press_mer, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel

    text(1000, 0.05, paste("Dark Sky"), cex = 1.2,  col = "black")
    text(990, 0.03, paste("Météo France"), cex = 1.2,  col = "blue")

    ## Si on voulait le nb d'enregistrements étudiés à la place du texte "Dark Sky" ou MF, il faudrait :
    ### text(03, 0.07, paste("N =", sum(complete.cases(DSKdata$pressure))), cex = 1.2,  col = "black")
    ### text(02, 0.06, paste("N =", sum(complete.cases(MFdata$press_mer))), cex = 1.2,  col = "blue")

    ## Si on voulait les courbes à +1.5°C ou +2°C, il faudrait fabriquer et afficher :
    ### lines(density(MFdata$press_mer_p15D, na.rm = TRUE), lwd = 2, col = "green") ### courbe liss?e, kernel
    ### lines(density(MFdata$press_mer_p2D, na.rm = TRUE), lwd = 2, col = "blue") ### courbe liss?e, kernel


    ### 2.5.2. Test des distributions appariées
    ## References :
    ## Patrick Royston (1982). An extension of Shapiro and Wilk's W test for normality to large samples. Applied Statistics, 31, 115–124. doi: 10.2307/2347973.

    shapiro.test(DSKdata$pressure) # W = 0.98046, p-value = 6.908e-11 => p-value significative, l'échantillon ne suit pas une loi normale.

    shapiro.test(MFdata$press_mer) # W = 0.9784, p-value = 2.414e-12 => p-value significative, l'échantillon ne suit pas une loi normale.


    ### t.test entre les différentes températures
    t.test(MFdata$press_mer,DSKdata$pressure) # t = -0.34466, df = 2233.7, p-value = 0.7304 => p-value non significative, les 2 échantillons ne sont pas significativement différents !

    # t.test(MFdata$press_mer_p1D,DSKdata$pressure) ## SO
    # t.test(MFdata$press_mer_p15D,DSKdata$pressure) ## SO
    # t.test(MFdata$press_mer_p2D,DSKdata$pressure) ## SO

    ## References :
    ## Myles Hollander and Douglas A. Wolfe (1973), Nonparametric Statistical Methods. New York: John Wiley & Sons. Pages 115–120.

    ## http://www.sthda.com/english/wiki/paired-samples-wilcoxon-test-in-r

    ### Import des données pour le test wilcox.test() sur données MFdata$press_mer, DSKdata$pressure

    ## création de deux sous-ensembles "subset"s" avec juste les variables à comparer et l'origine de l'éditeur

    d1 <- subset(DSKdata, select = c("pressure", "origine")) ## création du subset venant de DSK http://larmarange.github.io/analyse-R/fusion-de-tables.html
    dim(d1)
    d2 <- subset(MFdata, select = c("press_mer", "origine")) ## création du subset venant de MF
    dim(d2)

    ## pour utiliser la fusion "rbind" il faut que les champs aient le même nom (c'est le 1er qui pose problème)
    names(d2)[1]
    names(d2)[1]<-"pressure" ## renommage du 1er champ du tableau 2 https://forum.framasoft.org/viewtopic.php?p=75052
    names(d2)[1]

    WCpressuredata <- rbind(d1, d2) ## fusion des deux sous tableaux
    head(WCpressuredata)

    summary(WCpressuredata)

    wilcox.test(pressure ~ origine, data = WCpressuredata) ## ~ signifie : "en fonction de"
    res <- wilcox.test(pressure ~ origine, data = WCpressuredata)

    da <- res$statistic
    db <- res$p.value
    dc <- res$data.name
    da
    db
    dc
    ## Wilcoxon rank sum test with continuity correction
    ## data:  pressure by origine
    ## W = 449544, p-value = 0.5346 => p-value non significative, les 2 échantillons ne sont pas significativement différents !


## 2.6. Analyse de la vitesse moyenne du vent DSK vs MF (méthode Alice Favre)


### 2.6.1. Histogramme de la vitesse moyenne du vent "vvent" pour MF et "windspeed" pour DSK pour 42 stations Météo France

####Après avoir vérifié que le nombre d'obs n'est pas égal avec
    length(DSKdata$windspeed)
    length(na.omit(DSKdata$windspeed)) ## pour tester si NA
    length(MFdata$vvent)
    length(na.omit(MFdata$vvent)) ## pour tester si NA

##### et que l'étendue n'est pas la même
    range(DSKdata$windspeed, na.rm = TRUE)
    range(MFdata$vvent, na.rm = TRUE)

#### on définit les breaks pour l'abscisse commune
    BRt <- seq(from= 0, to= 10, by=1) ## tient compte des deux distributions

#### puis on fait l'histo en utilisant

    ### freq=F => des fréquences relatives et pas des effectifs

    hist(DSKdata$windspeed, breaks = BRt,
         freq=F, # fréquences
         col="grey",
         main = "Average Wind Speed for 42 Météo France Synoptic Stations \n and Dark Sky (France, july 2017 - february 2020), 995 days",
         ylab = "Densities",
         xlab = "Average Wind Speed (m/s)"
    )

### calcul des paramètres pour la fonction lines() à superposer à l'histo

    HH2 <- hist(MFdata$vvent, breaks = BRt,  plot=F)

    #lines(HH2$mids, HH2$density, lwd = 2, col = "orange") ### courbe non lissée  ## SO
    lines(density(MFdata$vvent, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel

    text(6, 0.20, paste("Dark Sky"), cex = 1.2,  col = "black")
    text(6, 0.30, paste("Météo France"), cex = 1.2,  col = "blue")

    ## Si on voulait le nb d'enregistrements étudiés à la place du texte "Dark Sky" ou MF, il faudrait :
    ### text(03, 0.07, paste("N =", sum(complete.cases(DSKdata$windspeed))), cex = 1.2,  col = "black")
    ### text(02, 0.06, paste("N =", sum(complete.cases(MFdata$vvent))), cex = 1.2,  col = "blue")

    ## Si on voulait les courbes à +1.5°C ou +2°C, il faudrait fabriquer et afficher :
    ### lines(density(MFdata$vvent_p15D, na.rm = TRUE), lwd = 2, col = "green") ### courbe liss?e, kernel
    ### lines(density(MFdata$vvent_p2D, na.rm = TRUE), lwd = 2, col = "blue") ### courbe liss?e, kernel


    ### 2.6.2. Test des distributions appariées
    ## References :
    ## Patrick Royston (1982). An extension of Shapiro and Wilk's W test for normality to large samples. Applied Statistics, 31, 115–124. doi: 10.2307/2347973.

    shapiro.test(DSKdata$windspeed) # W = 0.93208, p-value < 2.2e-16 => p-value significative, l'échantillon ne suit pas une loi normale.

    shapiro.test(MFdata$vvent) # W = 0.93244, p-value < 2.2e-16 => p-value significative, l'échantillon ne suit pas une loi normale.


    ### t.test entre les différentes températures
    t.test(MFdata$vvent,DSKdata$windspeed) # t = -0.73363, df = 2378.3, p-value = 0.4632 => p-value non significative, les 2 échantillons ne sont pas significativement différents !

    # t.test(MFdata$vvent_p1D,DSKdata$windspeed) ## SO
    # t.test(MFdata$vvent_p15D,DSKdata$windspeed) ## SO
    # t.test(MFdata$vvent_p2D,DSKdata$windspeed) ## SO

    ## References :
    ## Myles Hollander and Douglas A. Wolfe (1973), Nonparametric Statistical Methods. New York: John Wiley & Sons. Pages 115–120.

    ## http://www.sthda.com/english/wiki/paired-samples-wilcoxon-test-in-r

    ### Import des données pour le test wilcox.test() sur données MFdata$vvent, DSKdata$windspeed

    ## création de deux sous-ensembles "subset"s" avec juste les variables à comparer et l'origine de l'éditeur

    d1 <- subset(DSKdata, select = c("windspeed", "origine")) ## création du subset venant de DSK http://larmarange.github.io/analyse-R/fusion-de-tables.html
    dim(d1)
    d2 <- subset(MFdata, select = c("vvent", "origine")) ## création du subset venant de MF
    dim(d2)

    ## pour utiliser la fusion "rbind" il faut que les champs aient le même nom (c'est le 1er qui pose problème)
    names(d2)[1]
    names(d2)[1]<-"windspeed" ## renommage du 1er champ du tableau 2 https://forum.framasoft.org/viewtopic.php?p=75052
    names(d2)[1]

    WCwindspeeddata <- rbind(d1, d2) ## fusion des deux sous tableaux
    head(WCwindspeeddata)

    summary(WCwindspeeddata)

    wilcox.test(windspeed ~ origine, data = WCwindspeeddata) ## ~ signifie : "en fonction de"
    res <- wilcox.test(windspeed ~ origine, data = WCwindspeeddata)

    ea <- res$statistic
    eb <- res$p.value
    ec <- res$data.name
    ea
    eb
    ec
    ## Wilcoxon rank sum test with continuity correction
    ## data:  windspeed by origine
    ## W = 507585, p-value = 0.3671 => p-value non significative, les 2 échantillons ne sont pas significativement différents !


## 2.7. Analyse de la visibilite moyenne DSK vs MF (méthode Alice Favre)


### 2.7.1. Histogramme de la visibilite moyenne "visibilite" pour MF et "visibility" pour DSK pour 42 stations Météo France

####Après avoir vérifié que le nombre d'obs n'est pas égal avec
    length(DSKdata$visibility)
    length(na.omit(DSKdata$visibility)) ## pour tester si NA
    length(MFdata$visibilite)
    length(na.omit(MFdata$visibilite)) ## pour tester si NA

##### et que l'étendue n'est pas la même
    range(DSKdata$visibility, na.rm = TRUE)
    range(MFdata$visibilite, na.rm = TRUE)

#### on définit les breaks pour l'abscisse commune
    BRt <- seq(from= 5, to= 40, by=2) ## tient compte des deux distributions

    #### puis on fait l'histo en utilisant

    ### freq=F => des fréquences relatives et pas des effectifs

    hist(DSKdata$visibility, breaks = BRt,
         freq=F, # fréquences
         col="grey",
         main = "Average Visibility for 42 Météo France Synoptic Stations \n and Dark Sky (France, july 2017 - february 2020), 995 days",
         ylab = "Densities",
         xlab = "Average Visibility (km)"
    )

    ### calcul des paramètres pour la fonction lines() à superposer à l'histo

    HH2 <- hist(MFdata$visibilite, breaks = BRt,  plot=F)

    #lines(HH2$mids, HH2$density, lwd = 2, col = "orange") ### courbe non lissée  ## SO
    lines(density(MFdata$visibilite, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel

    text(17, 0.20, paste("Dark Sky"), cex = 1.2,  col = "black")
    text(32, 0.102, paste("Météo France"), cex = 1.2,  col = "blue")

    ## Si on voulait le nb d'enregistrements étudiés à la place du texte "Dark Sky" ou MF, il faudrait :
    ### text(03, 0.07, paste("N =", sum(complete.cases(DSKdata$visibility))), cex = 1.2,  col = "black")
    ### text(02, 0.06, paste("N =", sum(complete.cases(MFdata$visibilite))), cex = 1.2,  col = "blue")

    ## Si on voulait les courbes à +1.5°C ou +2°C, il faudrait fabriquer et afficher :
    ### lines(density(MFdata$visibilite_p15D, na.rm = TRUE), lwd = 2, col = "green") ### courbe liss?e, kernel
    ### lines(density(MFdata$visibilite_p2D, na.rm = TRUE), lwd = 2, col = "blue") ### courbe liss?e, kernel


    ### 2.7.2. Test des distributions appariées
    ## References :
    ## Patrick Royston (1982). An extension of Shapiro and Wilk's W test for normality to large samples. Applied Statistics, 31, 115–124. doi: 10.2307/2347973.

    shapiro.test(DSKdata$visibility) # W = 0.93231, p-value < 2.2e-16 => p-value significative, l'échantillon ne suit pas une loi normale.

    shapiro.test(MFdata$visibilite) # W = W = 0.97402, p-value = 8.433e-14 => p-value significative, l'échantillon ne suit pas une loi normale.


    ### t.test entre les différentes températures
    t.test(MFdata$visibilite,DSKdata$visibility) # t = 92.446, df = 1369.9, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents !

    # t.test(MFdata$visibilite_p1D,DSKdata$visibility) ## SO
    # t.test(MFdata$visibilite_p15D,DSKdata$visibility) ## SO
    # t.test(MFdata$visibilite_p2D,DSKdata$visibility) ## SO

    ## References :
    ## Myles Hollander and Douglas A. Wolfe (1973), Nonparametric Statistical Methods. New York: John Wiley & Sons. Pages 115–120.

    ## http://www.sthda.com/english/wiki/paired-samples-wilcoxon-test-in-r

    ### Import des données pour le test wilcox.test() sur données MFdata$visibilite, DSKdata$visibility

    ## création de deux sous-ensembles "subset"s" avec juste les variables à comparer et l'origine de l'éditeur

    d1 <- subset(DSKdata, select = c("visibility", "origine")) ## création du subset venant de DSK http://larmarange.github.io/analyse-R/fusion-de-tables.html
    dim(d1)
    d2 <- subset(MFdata, select = c("visibilite", "origine")) ## création du subset venant de MF
    dim(d2)

    ## pour utiliser la fusion "rbind" il faut que les champs aient le même nom (c'est le 1er qui pose problème)
    names(d2)[1]
    names(d2)[1]<-"visibility" ## renommage du 1er champ du tableau 2 https://forum.framasoft.org/viewtopic.php?p=75052
    names(d2)[1]

    WCvisibilitydata <- rbind(d1, d2) ## fusion des deux sous tableaux
    head(WCvisibilitydata)

    summary(WCvisibilitydata)

    wilcox.test(visibility ~ origine, data = WCvisibilitydata) ## ~ signifie : "en fonction de"
    res <- wilcox.test(visibility ~ origine, data = WCvisibilitydata)

    fa <- res$statistic
    fb <- res$p.value
    fc <- res$data.name
    fa
    fb
    fc
    ## Wilcoxon rank sum test with continuity correction
    ## data:  visibility by origine
    ## W = 4550.5, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents !



## 2.8. Analyse de la nebulosité moyenne DSK vs MF (méthode Alice Favre)


### 2.8.1. Histogramme de la nebulosité moyenne "nebulosite" pour MF et "cloudcover" pour DSK pour 42 stations Météo France

####Après avoir vérifié que le nombre d'obs n'est pas égal avec
    length(DSKdata$cloudcover)
    length(na.omit(DSKdata$cloudcover)) ## pour tester si NA
    length(MFdata$nebulosite)
    length(na.omit(MFdata$nebulosite)) ## pour tester si NA

##### et que l'étendue n'est pas la même
    range(DSKdata$cloudcover, na.rm = TRUE)
    range(MFdata$nebulosite, na.rm = TRUE)

    #### on définit les breaks pour l'abscisse commune
    BRt <- seq(from= 5, to= 100, by=2) ## tient compte des deux distributions

    #### puis on fait l'histo en utilisant

    ### freq=F => des fréquences relatives et pas des effectifs

    hist(DSKdata$cloudcover, breaks = BRt,
         freq=F, # fréquences
         col="grey",
         main = "Average Cloud Cover for 42 Météo France Synoptic Stations \n and Dark Sky (France, july 2017 - february 2020), 995 days",
         ylab = "Densities",
         xlab = "Average Cloud Cover (%)"
    )

    ### calcul des paramètres pour la fonction lines() à superposer à l'histo

    HH2 <- hist(MFdata$nebulosite, breaks = BRt,  plot=F)

    #lines(HH2$mids, HH2$density, lwd = 2, col = "orange") ### courbe non lissée  ## SO
    lines(density(MFdata$nebulosite, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel

    text(40, 0.015, paste("Dark Sky"), cex = 1.2,  col = "black")
    text(55, 0.030, paste("Météo France"), cex = 1.2,  col = "blue")

    ## Si on voulait le nb d'enregistrements étudiés à la place du texte "Dark Sky" ou MF, il faudrait :
    ### text(03, 0.07, paste("N =", sum(complete.cases(DSKdata$cloudcover))), cex = 1.2,  col = "black")
    ### text(02, 0.06, paste("N =", sum(complete.cases(MFdata$nebulosite))), cex = 1.2,  col = "blue")

    ## Si on voulait les courbes à +1.5°C ou +2°C, il faudrait fabriquer et afficher :
    ### lines(density(MFdata$nebulosite_p15D, na.rm = TRUE), lwd = 2, col = "green") ### courbe liss?e, kernel
    ### lines(density(MFdata$nebulosite_p2D, na.rm = TRUE), lwd = 2, col = "blue") ### courbe liss?e, kernel


    ### 2.8.2. Test des distributions appariées
    ## References :
    ## Patrick Royston (1982). An extension of Shapiro and Wilk's W test for normality to large samples. Applied Statistics, 31, 115–124. doi: 10.2307/2347973.

    shapiro.test(DSKdata$cloudcover) # W = 0.90472, p-value < 2.2e-16 => p-value significative, l'échantillon ne suit pas une loi normale.

    shapiro.test(MFdata$nebulosite) # W = 0.82479, p-value < 2.2e-16 => p-value significative, l'échantillon ne suit pas une loi normale.


    ### t.test entre les différentes températures
    t.test(MFdata$nebulosite,DSKdata$cloudcover) # t = 19.15, df = 2349.5, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents !

    # t.test(MFdata$nebulosite_p1D,DSKdata$cloudcover) ## SO
    # t.test(MFdata$nebulosite_p15D,DSKdata$cloudcover) ## SO
    # t.test(MFdata$nebulosite_p2D,DSKdata$cloudcover) ## SO

    ## References :
    ## Myles Hollander and Douglas A. Wolfe (1973), Nonparametric Statistical Methods. New York: John Wiley & Sons. Pages 115–120.

    ## http://www.sthda.com/english/wiki/paired-samples-wilcoxon-test-in-r

    ### Import des données pour le test wilcox.test() sur données MFdata$nebulosite, DSKdata$cloudcover

    ## création de deux sous-ensembles "subset"s" avec juste les variables à comparer et l'origine de l'éditeur

    d1 <- subset(DSKdata, select = c("cloudcover", "origine")) ## création du subset venant de DSK http://larmarange.github.io/analyse-R/fusion-de-tables.html
    dim(d1)
    d2 <- subset(MFdata, select = c("nebulosite", "origine")) ## création du subset venant de MF
    dim(d2)

    ## pour utiliser la fusion "rbind" il faut que les champs aient le même nom (c'est le 1er qui pose problème)
    names(d2)[1]
    names(d2)[1]<-"cloudcover" ## renommage du 1er champ du tableau 2 https://forum.framasoft.org/viewtopic.php?p=75052
    names(d2)[1]

    WCcloudcoverdata <- rbind(d1, d2) ## fusion des deux sous tableaux
    head(WCcloudcoverdata)

    summary(WCcloudcoverdata)

    wilcox.test(cloudcover ~ origine, data = WCcloudcoverdata) ## ~ signifie : "en fonction de"
    res <- wilcox.test(cloudcover ~ origine, data = WCcloudcoverdata)

    ga <- res$statistic
    gb <- res$p.value
    gc <- res$data.name
    ga
    gb
    gc
    ## Wilcoxon rank sum test with continuity correction
    ## data:  cloudcover by origine
    ## W = 219704, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents !



## 2.9. Analyse de la rafale_10min moyenne DSK vs MF (méthode Alice Favre)


### 2.9.1. Histogramme de la rafale_10min moyenne "rafale_10min" pour MF et "windgust" pour DSK pour 42 stations Météo France

####Après avoir vérifié que le nombre d'obs n'est pas égal avec
    length(DSKdata$windgust)
    length(na.omit(DSKdata$windgust)) ## pour tester si NA
    length(MFdata$rafale_10min)
    length(na.omit(MFdata$rafale_10min)) ## pour tester si NA

    ##### et que l'étendue n'est pas la même
    range(DSKdata$windgust, na.rm = TRUE)
    range(MFdata$rafale_10min, na.rm = TRUE)

    #### on définit les breaks pour l'abscisse commune
    BRt <- seq(from= 2, to= 25, by=1) ## tient compte des deux distributions

    #### puis on fait l'histo en utilisant

    ### freq=F => des fréquences relatives et pas des effectifs

    hist(DSKdata$windgust, breaks = BRt,
         freq=F, # fréquences
         col="grey",
         main = "Average Wind Gust for 42 Météo France Synoptic Stations \n and Dark Sky (France, july 2017 - february 2020), 995 days",
         ylab = "Densities",
         xlab = "Average Wind Gust (m/s)"
    )

    ### calcul des paramètres pour la fonction lines() à superposer à l'histo

    HH2 <- hist(MFdata$rafale_10min, breaks = BRt,  plot=F)

    #lines(HH2$mids, HH2$density, lwd = 2, col = "orange") ### courbe non lissée  ## SO
    lines(density(MFdata$rafale_10min, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel

    text(16, 0.05, paste("Dark Sky"), cex = 1.2,  col = "black")
    text(13, 0.12, paste("Météo France"), cex = 1.2,  col = "blue")

    ## Si on voulait le nb d'enregistrements étudiés à la place du texte "Dark Sky" ou MF, il faudrait :
    ### text(03, 0.07, paste("N =", sum(complete.cases(DSKdata$windgust))), cex = 1.2,  col = "black")
    ### text(02, 0.06, paste("N =", sum(complete.cases(MFdata$rafale_10min))), cex = 1.2,  col = "blue")

    ## Si on voulait les courbes à +1.5°C ou +2°C, il faudrait fabriquer et afficher :
    ### lines(density(MFdata$rafale_10min_p15D, na.rm = TRUE), lwd = 2, col = "green") ### courbe liss?e, kernel
    ### lines(density(MFdata$rafale_10min_p2D, na.rm = TRUE), lwd = 2, col = "blue") ### courbe liss?e, kernel


    ### 2.9.2. Test des distributions appariées
    ## References :
    ## Patrick Royston (1982). An extension of Shapiro and Wilk's W test for normality to large samples. Applied Statistics, 31, 115–124. doi: 10.2307/2347973.

    shapiro.test(DSKdata$windgust) # W = 0.92466, p-value < 2.2e-16 => p-value significative, l'échantillon ne suit pas une loi normale.

    shapiro.test(MFdata$rafale_10min) # W = 0.92202, p-value < 2.2e-16 => p-value significative, l'échantillon ne suit pas une loi normale.


    ### t.test entre les différentes températures
    t.test(MFdata$rafale_10min,DSKdata$windgust) # t = -29.089, df = 1644.3, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents !

    # t.test(MFdata$rafale_10min_p1D,DSKdata$windgust) ## SO
    # t.test(MFdata$rafale_10min_p15D,DSKdata$windgust) ## SO
    # t.test(MFdata$rafale_10min_p2D,DSKdata$windgust) ## SO

    ## References :
    ## Myles Hollander and Douglas A. Wolfe (1973), Nonparametric Statistical Methods. New York: John Wiley & Sons. Pages 115–120.

    ## http://www.sthda.com/english/wiki/paired-samples-wilcoxon-test-in-r

    ### Import des données pour le test wilcox.test() sur données MFdata$rafale_10min, DSKdata$windgust

    ## création de deux sous-ensembles "subset"s" avec juste les variables à comparer et l'origine de l'éditeur

    d1 <- subset(DSKdata, select = c("windgust", "origine")) ## création du subset venant de DSK http://larmarange.github.io/analyse-R/fusion-de-tables.html
    dim(d1)
    d2 <- subset(MFdata, select = c("rafale_10min", "origine")) ## création du subset venant de MF
    dim(d2)

    ## pour utiliser la fusion "rbind" il faut que les champs aient le même nom (c'est le 1er qui pose problème)
    names(d2)[1]
    names(d2)[1]<-"windgust" ## renommage du 1er champ du tableau 2 https://forum.framasoft.org/viewtopic.php?p=75052
    names(d2)[1]

    WCwindgustdata <- rbind(d1, d2) ## fusion des deux sous tableaux
    head(WCwindgustdata)

    summary(WCwindgustdata)

    wilcox.test(windgust ~ origine, data = WCwindgustdata) ## ~ signifie : "en fonction de"
    res <- wilcox.test(windgust ~ origine, data = WCwindgustdata)

    ha <- res$statistic
    hb <- res$p.value
    hc <- res$data.name
    ha
    hb
    hc
    ## Wilcoxon rank sum test with continuity correction
    ## data:  windgust by origine
    ## W = 754994, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents !



## 2.10. Analyse de la quantité moyenne de précipitation par heure DSK vs MF (méthode Alice Favre)


### 2.10.1. Histogramme de la precip_24h moyenne "precip_24h" pour MF et "precipintensity" pour DSK pour 42 stations Météo France

####Après avoir vérifié que le nombre d'obs n'est pas égal avec
    length(DSKdata$precipintensity)
    length(na.omit(DSKdata$precipintensity)) ## pour tester si NA
    length(MFdata$precip_24h)
    length(na.omit(MFdata$precip_24h)) ## pour tester si NA

##### et que l'étendue n'est pas la même
    range(DSKdata$precipintensity, na.rm = TRUE)
    range(MFdata$precip_24h, na.rm = TRUE)

    #### on définit les breaks pour l'abscisse commune
    BRt <- seq(from= -1, to= 16, by=0.1) ## tient compte des deux distributions

    #### puis on fait l'histo en utilisant

    ### freq=F => des fréquences relatives et pas des effectifs

    hist(DSKdata$precipintensity, breaks = BRt,
         freq=F, # fréquences
         col="grey",
         main = "Average Precipitation Intensity for 42 Météo France Synoptic Stations \n and Dark Sky (France, july 2017 - february 2020), 995 days",
         ylab = "Densities",
         xlab = "Average Precipitation Intensity (mm/h)"
    )

    ### calcul des paramètres pour la fonction lines() à superposer à l'histo

    HH2 <- hist(MFdata$precip_24h, breaks = BRt,  plot=F)

    #lines(HH2$mids, HH2$density, lwd = 2, col = "orange") ### courbe non lissée  ## SO
    lines(density(MFdata$precip_24h, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel

    text(3, 3, paste("Dark Sky"), cex = 1.2,  col = "black")
    text(3, 1, paste("Météo France"), cex = 1.2,  col = "blue")

    ## Si on voulait le nb d'enregistrements étudiés à la place du texte "Dark Sky" ou MF, il faudrait :
    ### text(03, 0.07, paste("N =", sum(complete.cases(DSKdata$precipintensity))), cex = 1.2,  col = "black")
    ### text(02, 0.06, paste("N =", sum(complete.cases(MFdata$precip_24h))), cex = 1.2,  col = "blue")

    ## Si on voulait les courbes à +1.5°C ou +2°C, il faudrait fabriquer et afficher :
    ### lines(density(MFdata$precip_24h_p15D, na.rm = TRUE), lwd = 2, col = "green") ### courbe liss?e, kernel
    ### lines(density(MFdata$precip_24h_p2D, na.rm = TRUE), lwd = 2, col = "blue") ### courbe liss?e, kernel


    ### 2.10.2. Test des distributions appariées
    ## References :
    ## Patrick Royston (1982). An extension of Shapiro and Wilk's W test for normality to large samples. Applied Statistics, 31, 115–124. doi: 10.2307/2347973.

    shapiro.test(DSKdata$precipintensity) # W = 0.41001, p-value < 2.2e-16 => p-value significative, l'échantillon ne suit pas une loi normale.

    shapiro.test(MFdata$precip_24h) # W = 0.80774, p-value < 2.2e-16 => p-value significative, l'échantillon ne suit pas une loi normale.


    ### t.test entre les différentes températures
    t.test(MFdata$precip_24h,DSKdata$precipintensity) # t = 28.767, df = 1199.6, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents !

    # t.test(MFdata$precip_24h_p1D,DSKdata$precipintensity) ## SO
    # t.test(MFdata$precip_24h_p15D,DSKdata$precipintensity) ## SO
    # t.test(MFdata$precip_24h_p2D,DSKdata$precipintensity) ## SO

    ## References :
    ## Myles Hollander and Douglas A. Wolfe (1973), Nonparametric Statistical Methods. New York: John Wiley & Sons. Pages 115–120.

    ## http://www.sthda.com/english/wiki/paired-samples-wilcoxon-test-in-r

    ### Import des données pour le test wilcox.test() sur données MFdata$precip_24h, DSKdata$precipintensity

    ## création de deux sous-ensembles "subset"s" avec juste les variables à comparer et l'origine de l'éditeur

    d1 <- subset(DSKdata, select = c("precipintensity", "origine")) ## création du subset venant de DSK http://larmarange.github.io/analyse-R/fusion-de-tables.html
    dim(d1)
    d2 <- subset(MFdata, select = c("precip_24h", "origine")) ## création du subset venant de MF
    dim(d2)

    ## pour utiliser la fusion "rbind" il faut que les champs aient le même nom (c'est le 1er qui pose problème)
    names(d2)[1]
    names(d2)[1]<-"precipintensity" ## renommage du 1er champ du tableau 2 https://forum.framasoft.org/viewtopic.php?p=75052
    names(d2)[1]

    WCprecintensdata <- rbind(d1, d2) ## fusion des deux sous tableaux
    head(WCprecintensdata)

    summary(WCprecintensdata)

    wilcox.test(precipintensity ~ origine, data = WCprecintensdata) ## ~ signifie : "en fonction de"
    res <- wilcox.test(precipintensity ~ origine, data = WCprecintensdata)

    ia <- res$statistic
    ib <- res$p.value
    ic <- res$data.name
    ia
    ib
    ic
    ## Wilcoxon rank sum test with continuity correction
    ## data:  precipintensity by origine
    ## W = 83243, p-value < 2.2e-16 => p-value significative, les 2 échantillons sont significativement différents !



    # 3. Export des résultats des tests dans un tableau

    output_Fr=data.frame(aa, ab, ac, ba, bb, bc, ca, cb, cc, da, db, dc, ea, eb, ec, fa, fb, fc, ga, gb, gc, ha, hb, hc, ia, ib, ic)
    output_Fr ## affichage du résultat

    write.table(output_Fr,"results_test-meteo_Fr.txt", append = TRUE, sep=";", col.names = TRUE, row.names = TRUE)
