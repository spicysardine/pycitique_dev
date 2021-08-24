

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


for(name in names(humdata)){
  
  if (!grepl('time', name) ){
    
    print(name)
    
  }
}








