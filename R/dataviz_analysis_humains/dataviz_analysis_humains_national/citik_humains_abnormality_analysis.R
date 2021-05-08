################### Library calls and preliminary database connection ####################################
getwd()
setwd("./")

require(ggplot2)
require(RPostgreSQL)
require(MASS)
require(mgcv)

## Defining plotsaving Function Template
plotsave <- function(plotname, plotpath){
  
  ggsave(filename = paste(plotname,'.png', sep = ''), path = plotpath, width=16, height =9  , units = 'in', dpi = 182)
  
}

## Defining Plots General Theming
plotstyle <- theme(plot.title = element_text(hjust = .5, face = 'bold', size = 14))+
              theme(axis.title.x = element_text(face = 'bold', size = 12))+
              theme(axis.title.y = element_text(face = 'bold', size = 12))

####### Getting data from postgres database for citik experimentation

###### Connection parameters and main human reports dataset associated to weather conditions
drv <- PostgreSQL()
con <- dbConnect(drv, dbname= 'localbase10', user='beetroot', password='root', port=5432, host='localhost')
query <- 'SELECT * FROM citik.citik_humains_clean_weather_strict'

res <- dbSendQuery(con, query)
dataset <- fetch(res, n=-1)

### Selecting reports with less than 25 Collected Ticks by reporter’s sex.
### This subset will be used throughout the script.
exagerated_subset <- dataset[dataset$nbr_tique > 25 & dataset$sex_pique != '',]

nobs <- nrow(exagerated_subset)

############### Study of abnormaly high number of collected ticks cases ################ 

### Quick summary for entire dataset

mean(dataset$nbr_tique)
median(dataset$nbr_tique)
summary(dataset$nbr_tique)
# 
# ### Quick summary for study  data subset
# 
mean(exagerated_subset$nbr_tique)
median(exagerated_subset$nbr_tique)
summary(exagerated_subset$nbr_tique)

#################### Abnormal subset of reports ######################


p <- ggplot(exagerated_subset, aes(sex_pique))+
  geom_bar()+
  xlab(label = '-')+
  ylab(label = '-')+
  ggtitle('-')
  p+plotstyle

p <- ggplot(exagerated_subset, aes(nbr_tique, colour=sex_pique))+
  geom_histogram(binwidth = 5, alpha= .7)+
  xlab(label = '-')+
  ylab(label = '-')+
  ggtitle('-')
  p+plotstyle

p <- ggplot(exagerated_subset, aes(nbr_tique, colour=sex_pique))+
  geom_density()+
  xlab(label = '-')+
  ylab(label = '-')+
  ggtitle('-')
  p+plotstyle

p <- ggplot(exagerated_subset, aes(sex_pique, nbr_tique, colour=sex_pique))+
  geom_violin()+
  xlab(label = '-')+
  ylab(label = '-')+
  ggtitle('-')
  p+plotstyle

p <- ggplot(exagerated_subset, aes(sex_pique, nbr_tique, colour=sex_pique))+
  geom_boxplot()+
  xlab(label = '-')+
  ylab(label = '-')+
  ggtitle('-')
  p+plotstyle




