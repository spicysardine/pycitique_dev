######
###### Analyse des signalements depuis le 2017-07-15 (début officiel de signalement tiques) et le "2020-04-05" (le max dans la base humadata)

## Courbe exprimée en fonction du temps : https://sites.google.com/site/rgraphiques/realiser-des-graphiques-avec-le-logiciel-r/les-graphiques/autres-types-de-graphiques/donn%C3%A9es-calendaires-temporelles-avec-r/graphique-en-fonction-du-temps-avec-le-logiciel-r-exemple#h.p_ID_47

# 1. Introduction
### Alloue la librairie "set()" puis vérifie que c'est bien importé "getwd()", il pointe dessus!

## 1.1 XPS13
setwd("C:/3VG/MSH/lyme/Smartick17/pycitique_miror/R")
getwd()

#Remove all objects
rm(list = ls() )
rm()

## 1.2 OP7570
setwd("D:/3VG/MSH/lyme/Smartick17/pycitique_miror/R")
getwd()

#Remove all objects
rm(list = ls() )
rm()

# 2. Lab 1 : Importation de données citik_humains_clean_weather_strict.csv / darksky_donnee_brute_700_def3.csv / citik_animaux_clean_weather_strict.csv

### Or, if .csv file, use this si données séparées par ","
### quote = "'" pour le caractère d’échappement
### stringAsFactpors pour voir les chaînes de caractères sans interprétation.


## 2.1 Toutes durées confondues

humdatatot <- read.csv("../../pycitique/data/donnee_signalements_avec_meteo_dsk/citik_humains_clean_weather_strict.csv", sep = ",", dec = ".", quote = "'", stringsAsFactors = FALSE)

# DSKdata <- read.csv("../../pycitique/data/donnee_meteo_nationale_comparative/darksky/darksky_donnee_brute_700_def3.csv", sep = ",", dec = ".", quote = "'", stringsAsFactors = FALSE)

anidatatot <- read.csv("../../pycitique/data/donnee_signalements_avec_meteo_dsk/citik_animaux_clean_weather_strict.csv", sep = ",", dec = ".", quote = "'", stringsAsFactors = FALSE)


table(anidatatot$qui_pique) ## pour connaître l'effectif

##      NA? Autre   Chat Cheval  Chien
##      7    124   2208    298   2289



## 2.2 Du 2017-07-15 (début officiel de signalement tiques) et le "2020-04-05" (le max dans la base humadata)

# install.packages(c("lubridate", "magrittr")) ##♥ https://fr.wikibooks.org/wiki/Programmer_en_R/Manipuler_les_dates_et_les_heures
library("lubridate")
library("magrittr")

###♦ Ecart entre le 2017-07-15 et aujourd'hui

time_length(interval(start = ymd("2017-07-15"), end = today()), unit = "days")
# [1] 1215 le 11/11/2020
# [1] 1245 le 11/12/2020


###♦ Ecart entre le 2017-07-15 (début officiel de signalement tiques)  et le "2020-04-05" (le max dans la base humadata)

time_length(interval(start = ymd("2017-07-15"), end = ymd("2020-04-05")), unit = "days")
# [1] 995


###• 2.2.1 humdata

## création d'un champ datenum = date en nombre de jours avec la fct date_length

humdatatot$datenum <- time_length(interval(start = ymd(humdatatot$date_piqure_saisie), end =  today()), unit = "days")  ## à l'envers

# humdatatot$datenum=time_length(interval(start = today(), end = ymd(humdatatot$date_piqure_saisie)), unit = "days") ## à l'endroit
range(humdatatot$datenum, na.rm = TRUE)
# humdatatot$datenum ## tout affiché
## [1]  220 1394  ## à l'envers
## [1] -1402  -228

## création d'un champ date "datepiq_YMD" où "humdatatot$date_piqure_saisie" de char devient date
humdatatot$datepiq_YMD <- ymd(humdatatot$date_piqure_saisie)

## création d'un champ nbr = effectif par jour (numérique remlie de 1)

humdatatot$nbr <- 1  ##

humdata  <- subset(humdatatot, humdatatot$datepiq_YMD > "2017-07-14") ## seulement pour les signalements >= 15/07/2020
# humdata  <- subset(humdatatot, humdatatot$datenum > -1223) ## seulement pour les signalements > 15/07/2020

range(humdata$datenum, na.rm = TRUE)
##♠ [1]  220 1215  ## à l'envers
##♠ [1] -1222  -228

###• 2.2.2 anidata

anidatatot$datenum <- time_length(interval(start = ymd(anidatatot$date_piqure_saisie), end = today()), unit = "days")
range(anidatatot$datenum, na.rm = TRUE)

## [1]  257 1398

## création d'un champ date "datepiq_YMD" où "anidatatot$date_piqure_saisie" de char devient date
anidatatot$datepiq_YMD <- ymd(anidatatot$date_piqure_saisie)

## création d'un champ nbr = effectif par jour (numérique remlie de 1)

anidatatot$nbr <- 1  ##

anidata  <- subset(anidatatot, anidatatot$datepiq_YMD > "2017-07-14") ## seulement pour les signalements >= 15/07/2020

# anidata  <- subset(anidatatot, anidatatot$datenum < 1216) ## seulement pour les signalements > 15/07/2020

range(anidata$datenum, na.rm = TRUE)
##♠ [1]  257 1215

anidatachat <- subset(anidata, anidata$qui_pique == "Chat")
anidatachien <- subset(anidata, anidata$qui_pique == "Chien")



## 2.3 Création des subsets

### Création des subset pour l'AL

humdataAL <- subset(humdata, humdata$departement_code == "08" | humdata$departement_code == "10" | humdata$departement_code == "51" | humdata$departement_code == "52" | humdata$departement_code == "54" |  humdata$departement_code == "55" |  humdata$departement_code == "57" |  humdata$departement_code == "67" | humdata$departement_code == "68" | humdata$departement_code == "88")

# DSKdataAL <- subset(DSKdata, DSKdata$departement_code == "08" | DSKdata$departement_code == "10" | DSKdata$departement_code == "51" | DSKdata$departement_code == "52" | DSKdata$departement_code == "54" |  DSKdata$departement_code == "55" |  DSKdata$departement_code == "57" |  DSKdata$departement_code == "67" | DSKdata$departement_code == "68" | DSKdata$departement_code == "88")

anidataAL <- subset(anidata, anidata$departement_code == "08" | anidata$departement_code == "10" | anidata$departement_code == "51" | anidata$departement_code == "52" | anidata$departement_code == "54" |  anidata$departement_code == "55" |  anidata$departement_code == "57" |  anidata$departement_code == "67" | anidata$departement_code == "68" | anidata$departement_code == "88")

# anidatachat

# anidatachien


### Création des subset pour l'IDF


humdataIDF <- subset(humdata, humdata$departement_code == "75" |  humdata$departement_code == "77" |  humdata$departement_code == "78" |  humdata$departement_code == "91" |  humdata$departement_code == "92" | humdata$departement_code == "93" |  humdata$departement_code == "94" | humdata$departement_code == "95")

# DSKdataIDF <- subset(DSKdata, DSKdata$departement_code == "75" |  DSKdata$departement_code == "77" |  DSKdata$departement_code == "78" |  DSKdata$departement_code == "91" |  DSKdata$departement_code == "92" | DSKdata$departement_code == "93" |  DSKdata$departement_code == "94" | DSKdata$departement_code == "95")

anidataIDF <- subset(anidata, anidata$departement_code == "75" |  anidata$departement_code == "77" |  anidata$departement_code == "78" |  anidata$departement_code == "91" |  anidata$departement_code == "92" | anidata$departement_code == "93" |  anidata$departement_code == "94" | anidata$departement_code == "95")

# anidatachat

# anidatachien




## 2.1 Analyse du tableau de données

ls(humdata) ## liste les variables
# ls(DSKdata)
ls(anidata)


str(humdata) ##  a diagnostic function and an alternative to summary
# str(DSKdata)
str(anidata)

summary(humdata)
# summary(DSKdata)
summary(anidata)


### 2.2 Impression de la matrice avec la librairie FactoMineR
install.packages("FactoMineR") ## si pas déjà installé !
library(FactoMineR)


#### 2.2.2.1 export en txt
write.infile(anidataAL, "anidataAL.txt", sep = "\t")
write.infile(humdataAL, "humdataAL.txt", sep = "\t")

write.infile(anidataIDF, "anidataIDF.txt", sep = "\t")
write.infile(humdataIDF, "humdataIDF.txt", sep = "\t")


# 3. Analyse des dates de signalements  en fonction du temps

## 3.1. Convertion des dates de signalements

# Courbe exprimée en fonction du temps : https://sites.google.com/site/rgraphiques/realiser-des-graphiques-avec-le-logiciel-r/les-graphiques/autres-types-de-graphiques/donn%C3%A9es-calendaires-temporelles-avec-r/graphique-en-fonction-du-temps-avec-le-logiciel-r-exemple#h.p_ID_47

## voir en particulier :
## read in date info in format 'ddmmmyyyy'(nous yyy-mm-dd)
## This will give NA(s) in some non-English locales; setting the C locale (surtout si pas respect des Maj/min dans %Y-%m-%d)
## as in the commented lines will overcome this on most systems.

###§ 3.1.1 Humains

lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
humdata_dates <- strptime(humdata$date_piqure_saisie, "%Y-%m-%d")  # Pour accéder à d'autres formats de temps : https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/strptime.
Sys.setlocale("LC_TIME", lct)
# humdata_dates

# plot(humdata_dates,humdata$id,col="#AAFF00",pch=16,cex=2,type="o")

###§ 3.1.2 Chats

lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
anidatachat_dates = strptime(anidatachat$date_piqure_saisie, "%Y-%m-%d")  # Pour accéder à d'autres formats de temps : https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/strptime.
Sys.setlocale("LC_TIME", lct)
#anidatachat_dates


###§ 3.1.3 Chiens

lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
anidatachien_dates = strptime(anidatachien$date_piqure_saisie, "%Y-%m-%d")  # Pour accéder à d'autres formats de temps : https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/strptime.
Sys.setlocale("LC_TIME", lct)
# anidatachien_dates



## 3.2. Groupement des piqûres par dates


### 3.2.1 group_by pour humadata

## inspiré de :
## https://www.datanovia.com/en/lessons/ggplot-histogram/
## in exo_ggplot_Histo.R
## et
## https://www.datanovia.com/en/fr/blog/comment-creer-un-ggplot-contenant-plusieurs-lignes/
## in exo_ggplot2_plusieurs_lignes.R
## ainsi que :
## https://www.datanovia.com/en/fr/blog/ggplot-personnalisation-des-dates-sur-les-axes/

library("dplyr")
wdata = data.frame(humdata)

mu <- wdata %>%
        group_by(datepiq_YMD) %>%
        summarise(grp.sum = sum(nbr))
mu

p <- ggplot(mu, aes(x=datepiq_YMD)) +
        geom_line(aes(y = grp.sum), color = "darkred")

# Format : Semaine
p + scale_x_date(date_labels = "%U")

# Format : mois/année
p + scale_x_date(date_labels = "%b/%y")


# Définir les limites de l'axe c(min, max)
min <- as.Date("2017-07-15")
max <- NA
p + scale_x_date(limits = c(min, max), date_labels = "%b/%y")
p + scale_x_continuous(breaks = seq(min, max, by = "%b/%y"))


ggplot(humdata, aes(x=datepiq_YMD)) +
        geom_line(aes(y = nbr), color = "darkred") +
        geom_line(aes(y = nbr_tique), color="steelblue", linetype="twodash")


## http://forums.cirad.fr/logiciel-r/viewtopic.php?t=6814

par(las = 2) ## 2 = always perpendicular to the axis,

barplot(table(humdata$datepiq_YMD))

dotchart(as.numeric(table(humdata$datepiq_YMD)))

hist(humdata$datepiq_YMD, breaks = "days")

compt <- as.data.frame(table(humdata$datepiq_YMD))

compt[,1] <- as.Date(compt[,1])

plot(x=compt[,1], y=compt[,2], type = "b", pch = 19, las = 1, xlab = "Temps", ylab = "Effectif",
     ylim = c(0, max(y)))


## sources : https://www.datanovia.com/en/fr/blog/ggplot-personnalisation-des-dates-sur-les-axes/

        ## exo_ggplot2_date&time.R chap. 4 Définir les limites des axes














humDate <- ymd(humdata$date_piqure_saisie)
humDate
class(humDate)

df <- data.frame(
        date = humDate)
head(df)

## sauf que ici pas de frequency calculé = message d'erreur
p <- ggplot(data = df, aes(x = date, y = frequency)) +
        geom_line(color = "steelblue")
p  + scale_x_date(date_labels = "%B / %Y")+
        theme(axis.text.x = element_text(angle=45, hjust = 1))


### 3.2.2. Data preparation /!\ marche avec le sex et le nbr_tique /!\


wdatahum <- data.frame(
        sexpique = humdata$sex_pique,
        nbtique = humdata$nbr_tique,
        x = 1)

head(wdatahum, 4)

library("dplyr")
mu <- wdatahum %>%
        group_by(sexpique) %>%
        summarise(grp.mean = mean(nbtique))
mu

library(ggplot2)
theme_set(
        theme_classic() +
                theme(legend.position = "top")
)

a <- ggplot(wdatahum, aes(x = nbtique))

# Basic density plots
a + geom_histogram(bins = 30, color = "black", fill = "gray") +
        geom_vline(aes(xintercept = mean(nbtique)),
                   linetype = "dashed", size = 0.6)



L3 <- LETTERS[1:3]
fac <- sample(L3, 10, replace = TRUE)
(d <- data.frame(x = 1, y = 1:10, fac = fac))
## The "same" with automatic column names:
data.frame(1, 1:10, sample(L3, 10, replace = TRUE))


## essai avec données de l'exemple => https://webdevdesigner.com/q/understanding-dates-and-plotting-a-histogram-with-ggplot2-in-r-27253/ (à la fin)

        # dates <- read.csv("http://pastebin.com/raw.php?i=sDzXKFxJ", sep=",", header=T)


## Mises à jour basées sur les réponses de edgester et gauden (in : https://webdevdesigner.com/q/understanding-dates-and-plotting-a-histogram-with-ggplot2-in-r-27253/)

# dates <- read.csv("http://pastebin.com/raw.php?i=sDzXKFxJ", sep=",", header=T)
# head(dates)

library(scales)

freqs <- aggregate(humDate, by=list(humDate), FUN=length) # calcul la freq de humdate en prenant la liste de ts les humdate en fct de leur longueur (!)
freqs$names <- as.Date(freqs$Group.1, format="%Y-%m-%d")
head(freqs$names)
head(freqs$Group.1)


# https://stackoverflow.com/questions/59008974/why-is-stat-identity-necessary-in-geom-bar-in-ggplot
# si  geom_bar(stat="identity") sinon geom_col() fait même chose
# si geom_line


p <- ggplot(freqs, aes(x=names, y=x)) + geom_line() +
        scale_x_date(breaks="2 month", labels = date_format("%Y-%m"), # %m en chiffres, %b en lettres (français) abrégées, %B en lettres (français) in extenso
                     limits=c(as.Date("2017-07-15"),as.Date("2020-04-05"))) +
        ylab("Frequency") + xlab("Year and Month") +
        theme_bw()
p
p + theme(axis.text.x = element_text(angle=90)) ## https://www.datanovia.com/en/fr/blog/ggplot-graduations-des-axes-definir-et-pivoter-les-textes/

# Définir les limites de l'axe c(min, max) mais on perd les mois !
min <- as.Date("2017-07-15")
max <- NA

p + theme(axis.text.x = element_text(angle=90)) + scale_x_date(limits = c(min, max)) # ce "scale_x_date" annule le précédent

p + theme(axis.text.x = element_blank(), axis.ticks = element_blank()) # élimine les dates de l'axe des x !




## 3.3. Histogramme des dates de signalements # (méthode Alice Favre)

#### Après avoir :

#### vérifié que le nombre d'obs n'est pas égal entre les tableaux
length(humdata_dates)
#◘ [1] 12864
length(na.omit(humdata_dates)) ## pour tester si NA
#◘ [1] 12864

# length(DSKdata$date_releve)
# [1]
# length(na.omit(DSKdata$date_releve)) ## pour tester si NA
# [1] ????

length(anidata$datenum)
# [1] 4668
length(na.omit(anidata$datenum)) ## pour tester si NA
# [1] 4668

##### et que l'étendue n'est pas la même
range(humdata$datenum, na.rm = TRUE)
# [1]  220 1215
# [1] -1222  -228

range(DSKdata$date_releve, na.rm = TRUE)
# [1] "2017-07-15" "2020-04-05"

range(anidata$datenum, na.rm = TRUE)
# [1]  257 1215

range(anidatachat$datenum, na.rm = TRUE)
# [1]  271 1214

range(anidatachien$datenum, na.rm = TRUE)
# [1]  271 1215


### on calcule le nb de lignes des tableaux (pour l'habillage des histogrammes)
nrhum <- nrow(humdata_dates)
nrdsk <- nrow(DSKdata)
nrani <- nrow(anidata)
nranichat <- nrow(anidatachat)
nranichien <- nrow(anidatachien)


####### avec $datepiq_YMD

##### et que l'étendue n'est pas la même
range(humdata$datepiq_YMD, na.rm = TRUE)

## [1] "2017-08-10" "2020-04-05" /?\ /?\ /?\ /?\ /?\ /?\ /?\ /?\

range(anidata$datepiq_YMD, na.rm = TRUE)
# [1]  "2017-08-10" "2020-02-28"

range(anidatachat$datepiq_YMD, na.rm = TRUE)
# [1]  "2017-08-10" "2020-02-28"

range(anidatachien$datepiq_YMD, na.rm = TRUE)
# [1]  "2017-08-10" "2020-02-28"

### on calcule le nb de lignes des tableaux (pour l'habillage des histogrammes)
nrhum <- nrow(humdata)

nrani <- nrow(anidata)
nranichat <- nrow(anidatachat)
nranichien <- nrow(anidatachien)





#### on définit les breaks pour l'abscisse commune qui tient compte des deux distributions
BRt <- seq(from= 2017-08-10, to= 2020-04-05, by=1) ## à l'envers
#BRt <- seq(from= 200, to= 1250, by=5)

#### puis on fait l'histo en utilisant

### freq=F => des fréquences relatives et pas des effectifs

hist(humdata$datepiq_YMD,
     "week", # break
     axes=F,
     freq=T, # fréquences si True, densité si False
     col="grey",
     main = "Distribution of ticks' reporting \n (France, july 2017 - april 2020), 995 days",
     ylab = "Frequency",
     xlab = "Dates of reporting"
)

# Abscisses exprimant dans ses graduations uniquement le noms des mois

axis.POSIXct(2,
             at=seq(from=strptime("2017-08-10","%Y-%m-%d" ),
                to=strptime("2020-04-05","%Y-%m-%d"),
                by="years"), format="%Y", las=2) # force indiquant la date de chaque jour

# Ordonnées

axis(2,c(0:600))



### calcul des paramètres pour la fonction lines() à superposer à l'histo

HH2 <- hist(anidata$datepiq_YMD, "week",  plot=F)

#lines(HH2$mids, HH2$density, lwd = 2, col = "orange") ### courbe non lissée  ## SO
lines(density(humdata$datepiq_YMD, na.rm = TRUE), lwd = 2, col = "blue") ### courbe lissée, kernel

text(-750, 0.003, paste("Human"), cex = 1.2,  col = "black")
text(-700, 0.002, paste("Animals"), cex = 1.2,  col = "blue")


##─ 3.2 Test de wilcox pour vérifier la similitude des signalements humains vs animaux

## https://perso.univ-rennes1.fr/denis.poinsot/Statistiques_%20pour_statophobes/R%20pour%20les%20statophobes.pdf

wilcox.test(humdata$datenum , anidata$datenum) ## comparaison des médianes avec p-value = 7.532e-08 donc différences significatives




