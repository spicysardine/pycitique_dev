

# Une fonction dans R est un objet

func <- function(x, message){
    
    resultat=(x*1000)/854
    cat('le resultat du calcul est ', resultat, message)
    return(resultat)
}


resultat <- func(message='fin des operations', x=1)
resultat


# principe du scope ou Etendu

fonction(pos1, pos2,pos3,...)


for( name in names(humdata) ) {
  # print(name)
  # assign(name, humdata[name])
  # obj <- parse(text=name)
  # call("<-", NULL, as.name(name)  )
  rm(list = c(name))
}

ic_list <- list()

for(name in names(humdata) ){
  
  if (!grepl('time', name) & grepl('temperature|pressure|humid|uv|precip|visib|dew|win|uv|precip|visib|dew|wind', name) ){
    
    cat('------------------------\n\n')
    print(name)
    param <- humdata[,name]
    vect <- decile(param)
    print(vect)
    ic_list[[name]] <- vect
    ic_list <- as.data.frame(ic_list)
    rm(param)
  }
  
}

write.csv(ic_list, 'table_echantillonnage.csv')

ic_table[,'temp'] <- decile(humdata$temperature)
empty
  
  
alist <- list('first'='one', 'second'=2, 'third'='three')
  
alist['fifth'] <-  decile(humdata$apparenttemperaturehigh)

alist




# names(humdata)
# 
# humdata[,"temperature"]
decile(humdata$apparenttemperaturehigh)
# 
# # |pressure|humid|uv|precip|cloud|visib|dew|win|uv|precip|cloud|visib|dew|win
# index = sapply(humdata, is.numeric)
# index
# for (var in humdata[,index]){
# 
#   decile(var)
# }





empty_list <- list('un', 2, 'quatre', 45, T)
empty_list[[6]] <- vectornames

empty_list$six[3]

list_names <- c('un', 'deux', 'trois', 'quatre', 'cinq', 'six')
names(empty_list) <- list_names

empty <- humdata[, "temperaturehigh"]

quartile(empty)


quartile(humdata$temperature)

dim(humdata)
names(humdata)

# Dans l’indexation R on commence toujours par 1
for(i in 37:length(humdata)){

       name <- names(humdata[i])
       print(name)

}

vectornames


for( name in vectornames ){

        print(name)
        param <- humdata[,name]
        result <- quartile(param)
        print(result)

}

# sapply applique la fonction a droite au data frame cible en retournant un vecteur booléen
index <- sapply(humdata, is.numeric)

# boucle de calcule implementant la fonction quartile
# avec filtrage par le vecteur index sur le dataframe humdata
for (name in names(humdata[,index])  ){
  
  # isolation de la colonne cible pour chaque iteration de la boucle
  param <- humdata[,name]
  # affectation dans une variable intermediaire
  result <- quartile(param)
  # insertion du vecteru numeric resultant dans la liste
  ic_table[[name]] <- result
  
}






c1 <- c(0:999)*NA 


empty <- data.frame("centile_25"=c(0:999),"centile_50"=c(0:999),"centile_75"=c(0:999))*NA
View(empty)


c(paste('centile_', c(25,50,75)))


?data.frame
















