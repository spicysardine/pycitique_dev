

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