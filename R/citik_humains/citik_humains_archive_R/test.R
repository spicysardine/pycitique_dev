env <- function(pkg){
  
  environment(pkg)
  
}

env(jpeg)

pkglst <- list(read.table, data, matrix, jpeg)

for(pkg in pkglst){
  print(env(pkg))
}


pkgstring <- ls("package:graphics")
pkgstring

for(string in pkgstring){
  if(string == 'smoothScatter'){
    print("the function smoothScatter is in the package graphics")
  }
}

for (col in colnames(iris[,1:3])){
  p <- ggplot(iris, aes(x=iris[, col], fill=Species) )+
    geom_density(alpha=.7)+
    xlab(colnames(iris[, col]))
  cat('----\n')
  print(p)
}

names(iris[, 'Sepal.Length'])
names(iris)




bench <- benchmark_std()
class(bench)
typeof(bench)
names(bench)
plot(bench)

upload_results(bench)

ggplot(bench, aes(Rank))




for (region in c('idf', 'al', 'ra', 'france') ){
  print(datalist[[region]]$report)
  
}
  
  
  
