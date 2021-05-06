################## Library calls and preliminary database connection ####################################
getwd()
setwd("./")
plotpath <- paste(getwd(),'/plots', sep = '')

require(ggplot2)
require(RPostgreSQL)
require(MASS)
library(mgcv)

## Defining plotsaving Function

plotsave <- function(plotname){
  
    ggsave(filename = paste(plotname,'.png', sep = ''), path = plotpath, dpi = 'retina')
  
}

####### Getting data from postgres database for citik experimentation

###### Connection parameters and main human reports dataset associated to weather conditions
drv <- PostgreSQL()
con <- dbConnect(drv, dbname= 'localbase10', user='beetroot', password='root', port=5432, host='localhost')
query <- 'SELECT * FROM citik.citik_humains_clean_weather_strict'

res <- dbSendQuery(con, query)
dataset <- fetch(res, n=-1)


####################################################################################################################
####################################################################################################################
########################################### Reports data: associated to reports ####################################
####################################################################################################################


#----------------------- Summary of the Number of colleced ticks aggregated by sex -------------------------------#

query_aggnbr_tique <-"SELECT sex_pique,
                      sum(nbr_tique) as sum_nbr_tique 
                      FROM citik.citik_humains_clean_weather_strict
                      where sex_pique != '' group by sex_pique"

res <- dbSendQuery(con, query_aggnbr_tique)
aggnbr_tique <- fetch(res, n=-1)

ggplot(aggnbr_tique, aes(sex_pique, sum_nbr_tique))+
  geom_bar(stat = "identity")+
  xlab(label = 'Collector’s Sex')+
  ylab(label = 'Total Number of Ticks Collected')+
  ggtitle('Total Number of Ticks Collected \n (Breakdown by Sex)')+
  theme(plot.title = element_text(hjust = .5, face = 'bold', size = 14))+
  theme(axis.title.x = element_text(face = 'bold', size = 12))+
  theme(axis.title.y = element_text(face = 'bold', size = 12))
  plotsave('total_nbr_tick_col_by_sex')
  
#----------------------- Breakdown of number of ticks collected - Time Series Analysis --------------------------#

### Selecting reports with less than 25 ticks collected and reported sex.
datasubset <- dataset[dataset$nbr_tique < 25 & dataset$sex_pique != '',]

ggplot(datasubset, aes(sex_pique))+
  geom_bar()+
  xlab(label = 'Collector’s Sex')+
  ylab(label = 'Frequency of Reporting')+
  ggtitle('Frequency of Tick Bite Reporting \n (Breakdown by Sex)')+
  theme(plot.title = element_text(hjust = .5, face = 'bold', size = 14))+
  theme(axis.title.x = element_text(face = 'bold', size = 12))+
  theme(axis.title.y = element_text(face = 'bold', size = 12))
  plotsave('freq_tick_col_by_sex')

ggplot(datasubset, aes(nbr_tique))+
  geom_bar(color='red', alpha=.7)+
  xlab(label = 'Report Declaring x Number of Ticks Collected')+
  ylab(label = 'Frequency of reporting')+
  ggtitle('Frequency of Reports Declaring x Number of Ticks Collected')+
  theme(plot.title = element_text(hjust = .5, face = 'bold', size = 14))+
  theme(axis.title.x = element_text(face = 'bold', size = 12))+
  theme(axis.title.y = element_text(face = 'bold', size = 12))
  plotsave('freq_rep_by_nbr_tick')

ggplot(datasubset, aes(nbr_tique, color=sex_pique ))+
  geom_bar(alpha=.7)+
  xlab(label = 'Report Declaring x Number of Ticks Collected')+
  ylab(label = 'Frequency of reporting')+
  ggtitle('Frequency of Reports Declaring x Number of Ticks Collected \n (Breakdown by Sex)')+
  theme(plot.title = element_text(hjust = .5, face = 'bold', size = 14))+
  theme(axis.title.x = element_text(face = 'bold', size = 12))+
  theme(axis.title.y = element_text(face = 'bold', size = 12))+
  facet_wrap(~sex_pique)
  plotsave('freq_rep_by_nbr_tick_by_sex')

ggplot(datasubset, aes(nbr_tique))+
  geom_density(color='blue')+
  xlab(label = 'Report Declaring x Number of Ticks Collected')+
  ylab(label = 'Density of Reporting')+
  ggtitle('Density of Reports Declaring x Number of Ticks Collected')+
  theme(plot.title = element_text(hjust = .5, face = 'bold', size = 14))+
  theme(axis.title.x = element_text(face = 'bold', size = 12))+
  theme(axis.title.y = element_text(face = 'bold', size = 12))
  plotsave('dens_rep_by_nbr_tick')

ggplot(datasubset, aes(nbr_tique, colour=sex_pique))+
  geom_density()+
  xlab(label = 'Report Declaring x Number of Ticks Collected')+
  ylab(label = 'Density of Reporting')+
  ggtitle('Density of Reports Declaring x Number of Ticks Collected \n (Breakdown by Sex)')+
  theme(plot.title = element_text(hjust = .5, face = 'bold', size = 14))+
  theme(axis.title.x = element_text(face = 'bold', size = 12))+
  theme(axis.title.y = element_text(face = 'bold', size = 12))+
  facet_wrap(~sex_pique, ncol = 1)
  plotsave('dens_rep_by_nbr_tick_by_sex')

ggplot(datasubset, aes(sex_pique, nbr_tique, colour=sex_pique))+
  geom_violin(fill='grey', alpha=.7)+
  xlab(label = 'Collector’s Sex')+
  ylab(label = 'Density of Reporting per Number of Ticks Collected')+
  ggtitle('Density of Reports Declaring x Number of Ticks Collected \n (Breakdown by Sex - Violin Plot)')+
  theme(plot.title = element_text(hjust = .5, face = 'bold', size = 14))+
  theme(axis.title.x = element_text(face = 'bold', size = 12))+
  theme(axis.title.y = element_text(face = 'bold', size = 12))
  plotsave('violplot_dens_rep_by_nbr_tick_by_sex')

ggplot(datasubset, aes(sex_pique, nbr_tique, colour=sex_pique))+
  geom_boxplot()+
  xlab(label = 'Collector’s Sex')+
  ylab(label = 'Distribution of Reports per Number of Ticks Collected')+
  ggtitle('Distribution of Reports Declaring x Number of Ticks Collected \n (Breakdown by Sex)')+
  theme(plot.title = element_text(hjust = .5, face = 'bold', size = 14))+
  theme(axis.title.x = element_text(face = 'bold', size = 12))+
  theme(axis.title.y = element_text(face = 'bold', size = 12))
  plotsave('boxplot_dens_rep_by_nbr_tick_by_sex')


################ Time series analysis using point distribution : Number of ticks collected in time ###############

# ggplot(datasubset, aes(date_piqure_saisie, nbr_tique))+
#   geom_line(color='blue')

ggplot(datasubset, aes(date_piqure_saisie, nbr_tique, colour=sex_pique))+
  geom_point(size=.2)+
  xlab(label = 'Date')+
  ylab(label = 'Number of Ticks Collected')+
  ggtitle('Variation in Time of the Number of Ticks \n Collected per Report \n (Unjitterd)')+
  theme(plot.title = element_text(hjust = .5, face = 'bold', size = 14))+
  theme(axis.title.x = element_text(face = 'bold', size = 12))+
  theme(axis.title.y = element_text(face = 'bold', size = 12))
  plotsave('ts_nbr_tick_nojitter')

ggplot(datasubset, aes(date_piqure_saisie, nbr_tique, colour=sex_pique))+
  geom_jitter(aes(size=nbr_tique), alpha=.4)+
  xlab(label = 'Date')+
  ylab(label = 'Number of Ticks Collected')+
  # ylim(1,NA)+
  ggtitle('Variation in Time of the Number of Ticks \n Collected per Report \n (Jitterd & Size Proportional)')+
  theme(plot.title = element_text(hjust = .5, face = 'bold', size = 14))+
  theme(axis.title.x = element_text(face = 'bold', size = 12))+
  theme(axis.title.y = element_text(face = 'bold', size = 12))+
  geom_smooth(aes(weight = nbr_tique), method = lm, size = 1)
  plotsave('ts_nbr_tick_by_sex_jitter_proportional')

ggplot(datasubset, aes(date_piqure_saisie, nbr_tique, colour=sex_pique))+
  geom_jitter(aes(size=nbr_tique), alpha=.4)+
  xlab(label = 'Date')+
  ylab(label = 'Number of Ticks Collected')+
  ggtitle('Breakdown by French Region of the Variation in Time of the Number of Ticks \n Collected per Report (Jitterd & Size Proportional)')+
  theme(plot.title = element_text(hjust = .5, face = 'bold', size = 12))+
  theme(axis.title.x = element_text(face = 'bold', size = 10))+
  theme(axis.title.y = element_text(face = 'bold', size = 10))+
  facet_wrap(~region)
  plotsave('ts_nbr_tick_by_region_by_sex_jitter_proportional')

ggplot(datasubset, aes(date_piqure_saisie, nbr_tique, colour=sex_pique))+
  geom_point(size=.2)+
  xlab(label = 'Date')+
  ylab(label = 'Number of Ticks Collected')+
  ggtitle('Breakdown by French Departement of the Variation in Time of the Number of Ticks Collected per Report \n (Jitterd & Size Proportional)')+
  theme(plot.title = element_text(hjust = .5, face = 'bold', size = 12))+
  theme(axis.title.x = element_text(face = 'bold', size = 10))+
  theme(axis.title.y = element_text(face = 'bold', size = 10))+
  facet_wrap(~departement)
  plotsave('ts_nbr_tick_by_dpt_by_sex_jitter_proportional')

ggplot(datasubset, aes(date_piqure_saisie, nbr_tique, colour=sex_pique))+
  geom_point(size=.2)+
  xlab(label = 'Date')+
  ylab(label = 'Number of Ticks Collected')+
  ggtitle('Breakdown by Incident Environment of the Variation in Time of the Number of Ticks Collected per Report \n (Jitterd & Size Proportional)')+
  theme(plot.title = element_text(hjust = .5, face = 'bold', size = 12))+
  theme(axis.title.x = element_text(face = 'bold', size = 10))+
  theme(axis.title.y = element_text(face = 'bold', size = 10))+
  facet_wrap(~environnement)
  plotsave('ts_nbr_tick_by_environ_by_sex_jitter_proportional')

ggplot(datasubset, aes(date_piqure_saisie, nbr_tique, colour=sex_pique))+
  geom_jitter(size=.5)+
  xlab(label = 'Date')+
  ylab(label = 'Number of Ticks Collected')+
  ggtitle('Breakdown by Reason of Presence at Incident location of the Variation in Time of the Number of Ticks Collected per Report \n (Jitterd & Size Proportional)')+
  theme(plot.title = element_text(hjust = .5, face = 'bold', size = 12))+
  theme(axis.title.x = element_text(face = 'bold', size = 10))+
  theme(axis.title.y = element_text(face = 'bold', size = 10))+
  facet_wrap(~raison_presence)
  plotsave('ts_nbr_tick_by_presence_by_sex_jitter_proportional')

ggplot(datasubset, aes(date_piqure_saisie, nbr_tique, colour=sex_pique))+
  geom_point(size=.2)+
  xlab(label = 'Date')+
  ylab(label = 'Number of Ticks Collected')+
  ggtitle('Breakdown by Weather Summary of the Variation in Time of the Number of Ticks Collected per Report \n (Jitterd & Size Proportional)')+
  theme(plot.title = element_text(hjust = .5, face = 'bold', size = 12))+
  theme(axis.title.x = element_text(face = 'bold', size = 10))+
  theme(axis.title.y = element_text(face = 'bold', size = 10))+
  facet_wrap(~icon)
  plotsave('ts_nbr_tick_by_weathersummary_by_sex_jitter_proportional')


################## Time series analysis using density distributions: Frequency of reporting days #################

ggplot(datasubset, aes(date_piqure_saisie))+
  geom_density(alpha=.1, fill= 'red', color='red')+
  xlab(label = 'Date')+
  ylab(label = 'Reporting Density')+
  ggtitle('Variation in Time of Reporting Density')+
  theme(plot.title = element_text(hjust = .5, face = 'bold', size = 14))+
  theme(axis.title.x = element_text(face = 'bold', size = 12))+
  theme(axis.title.y = element_text(face = 'bold', size = 12))
  plotsave('ts_report_density')

ggplot(datasubset, aes(date_piqure_saisie, colour= sex_pique, fill= sex_pique))+
  geom_density(alpha=.1)+
  xlab(label = 'Date')+
  ylab(label = 'Reporting Density')+
  ggtitle('Variation in Time of Reporting Date Density \n (Breakdown by Collector’s Sex)')+
  theme(plot.title = element_text(hjust = .5, face = 'bold', size = 14))+
  theme(axis.title.x = element_text(face = 'bold', size = 12))+
  theme(axis.title.y = element_text(face = 'bold', size = 12))+
  facet_wrap(~sex_pique, ncol = 1)
  plotsave('ts_report_date_dens_by_sex')

ggplot(datasubset, aes(date_piqure_saisie))+
  geom_freqpoly(binwidth = 1, color='blue')+
  xlab(label = 'Date')+
  ylab(label = 'Reporting Frequency')+
  ggtitle('Variation in Time of Reporting Date Frequency')+
  theme(plot.title = element_text(hjust = .5, face = 'bold', size = 14))+
  theme(axis.title.x = element_text(face = 'bold', size = 12))+
  theme(axis.title.y = element_text(face = 'bold', size = 12))
  plotsave('ts_report_date_freq')

ggplot(datasubset, aes(date_piqure_saisie, colour= sex_pique, fill= sex_pique))+
  geom_freqpoly(binwidth = 1)+
  xlab(label = 'Date')+
  ylab(label = 'Reporting Frequency')+
  ggtitle('Variation in Time of Reporting Date Frequency \n (Breakdown by Collector’s Sex)')+
  theme(plot.title = element_text(hjust = .5, face = 'bold', size = 14))+
  theme(axis.title.x = element_text(face = 'bold', size = 12))+
  theme(axis.title.y = element_text(face = 'bold', size = 12))+
  facet_wrap(~sex_pique, ncol = 1)
  plotsave('ts_report_date_freq_by_sex')

ggplot(datasubset, aes(date_piqure_saisie))+
  geom_histogram(binwidth = 1, alpha=.7, colour='blue')+
  xlab(label = 'Report Date')+
  ylab(label = 'Report Count')+
  ggtitle('Hisotgram of Report Date Frequency (Bin Width = 1 day)')+
  theme(plot.title = element_text(hjust = .5, face = 'bold', size = 14))+
  theme(axis.title.x = element_text(face = 'bold', size = 12))+
  theme(axis.title.y = element_text(face = 'bold', size = 12))
  plotsave('ts_histogram_report_date_freq_one_day')

ggplot(datasubset, aes(date_piqure_saisie))+
  geom_histogram(binwidth = 5, alpha=.7, colour='red')+
  xlab(label = 'Report Date')+
  ylab(label = 'Report Count')+
  ggtitle('Hisotgram of Report Date Frequency (Bin Width = 5 days)')+
  theme(plot.title = element_text(hjust = .5, face = 'bold', size = 14))+
  theme(axis.title.x = element_text(face = 'bold', size = 12))+
  theme(axis.title.y = element_text(face = 'bold', size = 12))
  plotsave('ts_histogram_report_date_freq_five_days')


ggplot(datasubset, aes(date_piqure_saisie, colour=sex_pique))+
  geom_histogram(binwidth = 5, alpha=.7)+
  xlab(label = 'Report Date')+
  ylab(label = 'Report Count')+
  ggtitle('Hisotgram of Report Date Frequency \n (Breakdown by Collector’s Sex & Bin Width equals 5 days)')+
  theme(plot.title = element_text(hjust = .5, face = 'bold', size = 14))+
  theme(axis.title.x = element_text(face = 'bold', size = 12))+
  theme(axis.title.y = element_text(face = 'bold', size = 12))
  facet_wrap(~sex_pique, ncol = 1)
  plotsave('ts_histogram_report_date_freq_by_sex_five_days')

ggplot(datasubset, aes(date_piqure_saisie, colour=sex_pique))+
  geom_histogram(binwidth = 5, alpha=.4)+
  xlab(label = 'Report Date')+
  ylab(label = 'Report Count')+
  ggtitle('Hisotgram of Report Date Frequency \n (Breakdown by French Region & Bin Width equals 5 days)')+
  theme(plot.title = element_text(hjust = .5, face = 'bold', size = 14))+
  theme(axis.title.x = element_text(face = 'bold', size = 12))+
  theme(axis.title.y = element_text(face = 'bold', size = 12))+
  facet_wrap(~region)
  plotsave('ts_histogram_report_date_freq_by_region_five_days')

ggplot(datasubset, aes(date_piqure_saisie, colour=sex_pique))+
  geom_histogram(binwidth = 5, alpha=.7)+
  xlab(label = 'Report Date')+
  ylab(label = 'Report Count')+
  ggtitle('Hisotgram of Report Date Frequency \n (Breakdown by French Departement & Bin Width equals 5 days)')+
  theme(plot.title = element_text(hjust = .5, face = 'bold', size = 14))+
  theme(axis.title.x = element_text(face = 'bold', size = 12))+
  theme(axis.title.y = element_text(face = 'bold', size = 12))+
  facet_wrap(~departement)
  plotsave('ts_histogram_report_date_freq_by_dpt_five_days')

ggplot(datasubset, aes(date_piqure_saisie, colour=sex_pique))+
  geom_histogram(binwidth = 5, alpha=.4)+
  xlab(label = 'Report Date')+
  ylab(label = 'Report Count')+
  ggtitle('Hisotgram of Report Date Frequency \n (Breakdown by Reason of Presence at incident Date & Bin Width equals 5 days)')+
  theme(plot.title = element_text(hjust = .5, face = 'bold', size = 14))+
  theme(axis.title.x = element_text(face = 'bold', size = 12))+
  theme(axis.title.y = element_text(face = 'bold', size = 12))+
  facet_wrap(~raison_presence)
  plotsave('ts_histogram_report_date_freq_by_reason_five_days')

ggplot(datasubset, aes(date_piqure_saisie, colour=sex_pique))+
  geom_histogram(binwidth = 5, alpha=.4)+
  xlab(label = 'Report Date')+
  ylab(label = 'Report Count')+
  ggtitle('Hisotgram of Report Date Frequency \n (Breakdown by Collector’s Age & Bin Width equals 5 days)')+
  theme(plot.title = element_text(hjust = .5, face = 'bold', size = 14))+
  theme(axis.title.x = element_text(face = 'bold', size = 12))+
  theme(axis.title.y = element_text(face = 'bold', size = 12))+
  facet_wrap(~age)
plotsave('ts_histogram_report_date_freq_by_age_five_days')

ggplot(datasubset, aes(date_piqure_saisie, colour=sex_pique))+
  geom_histogram(binwidth = 5, alpha=.4)+
  xlab(label = 'Report Date')+
  ylab(label = 'Report Count')+
  ggtitle('Hisotgram of Report Date Frequency \n (Breakdown by Report Positional Accuracy & Bin Width equals 5 days)')+
  theme(plot.title = element_text(hjust = .5, face = 'bold', size = 14))+
  theme(axis.title.x = element_text(face = 'bold', size = 12))+
  theme(axis.title.y = element_text(face = 'bold', size = 12))+
  facet_wrap(~precision_geo)
  plotsave('ts_histogram_report_date_freq_by_precisiongeo_five_days')

ggplot(datasubset, aes(date_piqure_saisie, colour=sex_pique))+
  geom_histogram(binwidth = 5, alpha=.4)+
  xlab(label = 'Report Date')+
  ylab(label = 'Report Count')+
  ggtitle('Hisotgram of Report Date Frequency \n (Breakdown by Incident’s Environment & Bin Width equals 5 days)')+
  theme(plot.title = element_text(hjust = .5, face = 'bold', size = 14))+
  theme(axis.title.x = element_text(face = 'bold', size = 12))+
  theme(axis.title.y = element_text(face = 'bold', size = 12))+
  facet_wrap(~environnement)
  plotsave('ts_histogram_report_date_freq_by_environ_five_days')

ggplot(datasubset, aes(environnement, date_piqure_saisie, colour=sex_pique))+
  geom_violin()+
  xlab(label = 'Environment Type')+
  ylab(label = 'Date')+
  ggtitle('Density of Report Date Frequency Per Incident’s Environment Type')+
  theme(plot.title = element_text(hjust = .5, face = 'bold', size = 14))+
  theme(axis.title.x = element_text(face = 'bold', size = 12))+
  theme(axis.title.y = element_text(face = 'bold', size = 12))
  plotsave('violinplot_report_date_freq_by_environ')

ggplot(datasubset, aes(environnement, date_piqure_saisie, colour=sex_pique))+
  geom_boxplot()+
  xlab(label = 'Environment Type')+
  ylab(label = 'Date')+
  ggtitle('Distribution of Report Date Frequency Per Incident’s Environment Type')+
  theme(plot.title = element_text(hjust = .5, face = 'bold', size = 14))+
  theme(axis.title.x = element_text(face = 'bold', size = 12))+
  theme(axis.title.y = element_text(face = 'bold', size = 12))
  plotsave('boxplot_report_date_freq_by_environ')

############### Study of regional effect and weather parameters on Frequency of reporting days ###################

# names(datasubset)

##### Without Regional Variation
for ( i in 37:length(datasubset) ){
  
  param <- names(datasubset[i])
  timecolumn <- grepl('time', param )
  datasourcecolumn <- grepl('datasource', param )
  cloudcovererrcolumn <- grepl('cloudcovererror', param )
  
  if(timecolumn | datasourcecolumn | cloudcovererrcolumn) {
    print('Skipping time variable')
  }else{
    print(param)
    plot <- ggplot( datasubset, aes(date_piqure_saisie, datasubset[,i]) )+
      geom_point(size=.2, color= 'blue', alpha = .4)+
      geom_smooth(span=1, color='black')+
      xlab(label = 'Date')+
      ylab(label=paste(param, '( International System. Unit)'))+
      ggtitle(paste('Variation of ',param,' at Report Date between January 2017 & April 2020'))+
      theme(plot.title = element_text(hjust = .5, face = 'bold', size = 14))+
      theme(axis.title.x = element_text(face = 'bold', size = 12))+
      theme(axis.title.y = element_text(face = 'bold', size = 12))
    
    print(plot)
  }
  
}

##### Without Regional Variation Breakdown by Sex Jittered and Weighted by number of ticks
for ( i in 37:length(datasubset) ){
  
  param <- names(datasubset[i])
  timecolumn <- grepl('time', param )
  datasourcecolumn <- grepl('datasource', param )
  cloudcovererrcolumn <- grepl('cloudcovererror', param )
  
  if(timecolumn | datasourcecolumn | cloudcovererrcolumn) {
    print('Skipping time variable')
  }else{
    print(param)
    plot <- ggplot( datasubset, aes(date_piqure_saisie, datasubset[,i], color = sex_pique) )+
      geom_jitter( aes(size=nbr_tique), alpha = .3)+
      geom_smooth(span=1)+
      xlab(label = 'Date')+
      ylab(label=paste(param, '( International System. Unit)'))+
      ggtitle(paste('Variation of ',param,' at Report Date between January 2017 & April 2020 \n Breakdown by Sex Jittered and Weighted by nbr of Ticks Collected'))+
      theme(plot.title = element_text(hjust = .5, face = 'bold', size = 14))+
      theme(axis.title.x = element_text(face = 'bold', size = 12))+
      theme(axis.title.y = element_text(face = 'bold', size = 12))
    
    print(plot)
  }
  
}


##### With Regional Variation
for ( i in 37:length(datasubset) ){
  
  param <- names(datasubset[i])
  timecolumn <- grepl('time', param )
  datasourcecolumn <- grepl('datasource', param )
  cloudcovererrcolumn <- grepl('cloudcovererror', param )

  if(timecolumn | datasourcecolumn | cloudcovererrcolumn) {
    print('Skipping time variable')
  }else{
      print(param)
      plot <- ggplot(datasubset, aes(date_piqure_saisie, datasubset[,i], colour=sex_pique))+
        geom_point(size=.2)+
        geom_smooth(span=1)+
        facet_wrap(~region)+
        xlab(label = 'Date')+
        ylab(label=paste(param, ' IS. Unit'))+
        ggtitle(paste('Regional Variation of ',param,' at Report Date between January 2017 & April 2020'))+
        theme(plot.title = element_text(hjust = .5, face = 'bold', size = 14))+
        theme(axis.title.x = element_text(face = 'bold', size = 12))+
        theme(axis.title.y = element_text(face = 'bold', size = 12))
    
      print(plot)
    }
  
}

####################################################################################################################
####################################################################################################################
########################################### Weather data: without reports ##########################################
####################################################################################################################

#############################  Average weather data analysis Mainland France & Corsica ##############################

### Creating weather plotting function

weatherplot <- function(weatherframe, wframename){
  
  for( i in 3:length(weatherframe) ) {
    
    param <- names(weatherframe[i])
    plotpath <- paste(getwd(),'/plots', sep = '')
    
    ## The second column of each dframe is the date frame
    plot <- ggplot(weatherframe, aes(weatherframe[,2], weatherframe[,i]) )+
      geom_line(color='blue')+
      geom_smooth(color='black')+
      xlab(label = 'Date')+
      ylab(label=paste(param, ' IS. Unit'))+
      ggtitle(paste('Daily Variation of ',param,'. \n Mainland France & Corsica between January 2017 & April 2020 \n',wframename))+
      theme(plot.title = element_text(hjust = .5, face = 'bold', size = 12))+
      theme(axis.title.x = element_text(face = 'bold', size = 9))+
      theme(axis.title.y = element_text(face = 'bold', size = 9))
    
    print(plot)
    
    ggsave(
      filename = paste(param,wframename,'.png', sep = '_'),
      plot = plot,
      device = "png",
      path = plotpath,
      dpi = 'retina'
    )
    
  }
  
}

#############################  dsk synop 42  ##############################
query_dsk42 <- 'SELECT * FROM meteo.darksky_synop42_avg'
res <- dbSendQuery(con, query_dsk42)
dataset_dsk42 <- fetch(res, n=-1)

weatherplot(dataset_dsk42, 'Darksky 42 Synoptic Stations.')

#############################  meteo france synop 42  ##############################

query_mf42 <- 'SELECT *, floor(humidite) as humidity_rounded FROM meteo.mf_synop42_avg'
res <- dbSendQuery(con, query_mf42)
dataset_mf42 <- fetch(res, n=-1)

weatherplot(dataset_mf42, '(Meteo-France 42 Synoptic Stations)')

#############################  dsk synop 700 ##############################
query_dsk700 <- 'SELECT * FROM meteo.darksky_maille_700_avg'
res <- dbSendQuery(con, query_dsk700)
dataset_dsk700 <- fetch(res, n=-1)

weatherplot(dataset_dsk700, '(Darksky 700 Synoptic Stations)')




################ Study of abnormaly high number of collected ticks cases ################ 

### Quick summary for entire dataset

# mean(dataset$nbr_tique)
# median(dataset$nbr_tique)
# summary(dataset$nbr_tique)
# 
# ### Quick summary for study  data subset
# 
# mean(datasubset$nbr_tique)
# median(datasubset$nbr_tique)
# summary(datasubset$nbr_tique)

#################### Abnormal subset of reports ######################

# exagerated_subset <- dataset[dataset$nbr_tique > 25 & dataset$sex_pique != '',]
# 
# ggplot(exagerated_subset, aes(sex_pique))+geom_bar()
# 
# ggplot(exagerated_subset, aes(nbr_tique, colour=sex_pique))+
#   geom_histogram(binwidth = 5, alpha= .7)
# 
# ggplot(exagerated_subset, aes(nbr_tique, colour=sex_pique))+
#   geom_density()
# 
# ggplot(exagerated_subset, aes(sex_pique, nbr_tique, colour=sex_pique))+
#   geom_violin()
# 
# ggplot(exagerated_subset, aes(sex_pique, nbr_tique, colour=sex_pique))+
#   geom_boxplot()


# xlab(label = '-')+
#   ylab(label = '-')+
#   ggtitle('-')+
#   theme(plot.title = element_text(hjust = .5, face = 'bold', size = 14))+
#   theme(axis.title.x = element_text(face = 'bold', size = 12))+
#   theme(axis.title.y = element_text(face = 'bold', size = 12))
