################## Library calls and preliminary database connection ####################################
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
datasubset <- dataset[dataset$nbr_tique < 25 & dataset$sex_pique != '', -5]

nobs <- nrow(datasubset)

###########################################################################################################
###########################################################################################################
################################### Reports data: associated to reports ###################################
###########################################################################################################

#------------ Quantitative study: Breakdown of the number of Collected Ticks - Time Series Analysis -----------------#

#--- Summary of the Number of colleced ticks aggregated by sex ---#


############ Section Plot Path ############## 
plotpath_quant_study <- paste(getwd(),'/plots/quantitative_study', sep = '')
dir.create(plotpath_quant_study)
############################################# 

query_aggnbr_tique <-"SELECT sex_pique,
                      sum(nbr_tique) as sum_nbr_tique 
                      FROM citik.citik_humains_clean_weather_strict
                      where sex_pique != '' group by sex_pique"

res <- dbSendQuery(con, query_aggnbr_tique)
aggnbr_tique <- fetch(res, n=-1)

p <- ggplot(aggnbr_tique, aes(sex_pique, sum_nbr_tique))+
  geom_bar(stat = "identity")+
  xlab(label = 'Collector’s Sex')+
  ylab(label = 'Total Number of Collected Ticks')+
  ggtitle('Total Number of Collected Ticks \n (Breakdown by Sex)')
p+plotstyle
plotsave('total_nbr_tick_col_by_sex', plotpath_quant_study)

p <- ggplot(datasubset, aes(sex_pique))+
  geom_bar()+
  xlab(label = 'Collector’s Sex')+
  ylab(label = 'Frequency of Reporting')+
  ggtitle('Frequency of Tick Bite Reporting \n (Breakdown by Sex)')
p+plotstyle
plotsave('freq_tick_col_by_sex', plotpath_quant_study)

p <- ggplot(datasubset, aes(nbr_tique))+
  geom_bar(color='red', alpha=.7)+
  xlab(label = 'Report Declaring x Number of Collected Ticks')+
  ylab(label = 'Frequency of reporting')+
  ggtitle('Frequency of Reports Declaring x Number of Collected Ticks')
p+plotstyle
plotsave('freq_rep_by_nbr_tick', plotpath_quant_study)

p <- ggplot(datasubset, aes(nbr_tique, color=sex_pique ))+
  geom_bar(alpha=.7)+
  xlab(label = 'Report Declaring x Number of Collected Ticks')+
  ylab(label = 'Frequency of reporting')+
  ggtitle('Frequency of Reports Declaring x Number of Collected Ticks \n (Breakdown by Sex)')+
  facet_wrap(~sex_pique)
p+plotstyle
plotsave('freq_rep_by_nbr_tick_by_sex', plotpath_quant_study)

p <- ggplot(datasubset, aes(nbr_tique))+
  geom_density(color='blue')+
  xlab(label = 'Report Declaring x Number of Collected Ticks')+
  ylab(label = 'Density of Reporting')+
  ggtitle('Density of Reports Declaring x Number of Collected Ticks')
p+plotstyle
plotsave('dens_rep_by_nbr_tick', plotpath_quant_study)

p <- ggplot(datasubset, aes(nbr_tique, colour=sex_pique))+
  geom_density()+
  xlab(label = 'Report Declaring x Number of Collected Ticks')+
  ylab(label = 'Density of Reporting')+
  ggtitle('Density of Reports Declaring x Number of Collected Ticks \n (Breakdown by Sex)')+
  facet_wrap(~sex_pique, ncol = 1)
p+plotstyle
plotsave('dens_rep_by_nbr_tick_by_sex', plotpath_quant_study)

p <- ggplot(datasubset, aes(sex_pique, nbr_tique, colour=sex_pique))+
  geom_violin(fill='grey', alpha=.7)+
  xlab(label = 'Collector’s Sex')+
  ylab(label = 'Density of Reporting per Number of Collected Ticks')+
  ggtitle('Density of Reports Declaring x Number of Collected Ticks \n (Breakdown by Sex - Violin Plot)')
p+plotstyle
plotsave('violplot_dens_rep_by_nbr_tick_by_sex', plotpath_quant_study)

p <- ggplot(datasubset, aes(sex_pique, nbr_tique, colour=sex_pique))+
  geom_boxplot()+
  xlab(label = 'Collector’s Sex')+
  ylab(label = 'Distribution of Reports per Number of Collected Ticks')+
  ggtitle('Distribution of Reports Declaring x Number of Collected Ticks \n (Breakdown by Sex Whisker Boxplot)')
p+plotstyle
plotsave('boxplot_dens_rep_by_nbr_tick_by_sex', plotpath_quant_study)

### Destruction of plot Handler object for safety purposes
rm(p);

################ Time series analysis using point distribution : Number of Collected Ticks in time ###############

############ Section Plot Path ############## 
plotpath_ts_pt_distro <- paste(getwd(),'/plots/time_series_analysis_point_distribution', sep = '')
dir.create(plotpath_ts_pt_distro)
############################################# 

# ggplot(datasubset, aes(date_piqure_saisie, nbr_tique))+
#   geom_line(color='blue')
p <- ggplot(datasubset, aes(date_piqure_saisie, nbr_tique))+
  geom_point(size=.2, color='blue', alpha=.4)+
  xlab(label = 'Date')+
  ylab(label = 'Number of Collected Ticks')+
  ggtitle('Variation in Time of the Number of Ticks \n Collected per Report \n (Unjitterd)')
p+plotstyle
plotsave('ts_nbr_tick_nojitter_noprop', plotpath_ts_pt_distro)

p <- ggplot(datasubset, aes(date_piqure_saisie, nbr_tique, colour=sex_pique))+
  geom_point(size=.2)+
  xlab(label = 'Date')+
  ylab(label = 'Number of Collected Ticks')+
  ggtitle('Variation in Time of the Number of Ticks \n Collected per Report \n (Unjitterd)')
p+plotstyle
plotsave('ts_nbr_tick_by_sex_nojitter_noprop', plotpath_ts_pt_distro)

p <- ggplot(datasubset, aes(date_piqure_saisie, nbr_tique, colour=sex_pique))+
  geom_jitter(aes(size=nbr_tique), alpha=.4)+
  xlab(label = 'Date')+
  ylab(label = 'Number of Collected Ticks')+
  # ylim(1,NA)+
  ggtitle('Variation in Time of the Number of Ticks \n Collected per Report \n (Jitterd & Size Proportional)')
p+plotstyle
# geom_smooth(aes(weight = nbr_tique), method = lm, size = 1)
plotsave('ts_nbr_tick_by_sex_jitter_proportional', plotpath_ts_pt_distro)

p <- ggplot(datasubset, aes(date_piqure_saisie, nbr_tique, colour=sex_pique))+
  geom_jitter(aes(size=nbr_tique), alpha=.4)+
  xlab(label = 'Date')+
  ylab(label = 'Number of Collected Ticks')+
  ggtitle('Breakdown by French Region of the Variation in Time of the Number of Ticks 
           \n Collected per Report (Jitterd & Size Proportional)')+
  facet_wrap(~region)
p+plotstyle
plotsave('ts_nbr_tick_by_region_by_sex_jitter_proportional', plotpath_ts_pt_distro)

p <- ggplot(datasubset, aes(date_piqure_saisie, nbr_tique, colour=sex_pique))+
  geom_point(size=.2)+
  xlab(label = 'Date')+
  ylab(label = 'Number of Collected Ticks')+
  ggtitle('Breakdown by French Departement of the Variation in Time of the Number of Collected Ticks per Report')+
  facet_wrap(~departement)
p+plotstyle
plotsave('ts_nbr_tick_by_dpt_by_sex_jitter_proportional', plotpath_ts_pt_distro)

p <- ggplot(datasubset, aes(date_piqure_saisie, nbr_tique, colour=sex_pique))+
  geom_point(size=.2)+
  xlab(label = 'Date')+
  ylab(label = 'Number of Collected Ticks')+
  ggtitle('Breakdown by Incident Environment of the Variation in Time of the Number of Collected Ticks per Report')+
  facet_wrap(~environnement)
p+plotstyle
plotsave('ts_nbr_tick_by_environ_by_sex_jitter_proportional', plotpath_ts_pt_distro)

p <- ggplot(datasubset, aes(date_piqure_saisie, nbr_tique, colour=sex_pique))+
  geom_jitter(size=.5)+
  xlab(label = 'Date')+
  ylab(label = 'Number of Collected Ticks')+
  ggtitle('Breakdown by Reason of Presence at Incident location \n
          of the Variation in Time of the Number of Collected Ticks per Report \n (Jitterd)')+
  facet_wrap(~raison_presence)
p+plotstyle
plotsave('ts_nbr_tick_by_presence_by_sex_jitter_proportional', plotpath_ts_pt_distro)

p <- ggplot(datasubset, aes(date_piqure_saisie, nbr_tique, colour=sex_pique))+
  geom_point(size=.2)+
  xlab(label = 'Date')+
  ylab(label = 'Number of Collected Ticks')+
  ggtitle('Breakdown by Weather Summary of the Variation in Time of the Number of Collected Ticks per Report')+
  facet_wrap(~icon)
p+plotstyle
plotsave('ts_nbr_tick_by_weathersummary_by_sex_jitter_proportional', plotpath_ts_pt_distro)

### Destruction of plot Handler object for safety purposes
rm(p);

################## Time series analysis using density distributions: Frequency of reporting days #################

############ Section Plot Path ############## 
plotpath_ts_dens_distro <- paste(getwd(),'/plots/time_series_analysis_density_distribution', sep = '')
dir.create(plotpath_ts_dens_distro)
############################################# 


p <- ggplot(datasubset, aes(date_piqure_saisie))+
  geom_density(alpha=.1, fill= 'red', color='red')+
  xlab(label = 'Date')+
  ylab(label = 'Reporting Density')+
  ggtitle('Variation in Time of Reporting Date Density')
p+plotstyle
plotsave('ts_report_date_dens', plotpath_ts_dens_distro)

p <- ggplot(datasubset, aes(date_piqure_saisie, colour= sex_pique, fill= sex_pique))+
  geom_density(alpha=.1)+
  xlab(label = 'Date')+
  ylab(label = 'Reporting Density')+
  ggtitle('Variation in Time of Reporting Date Density \n (Breakdown by Collector’s Sex)')+
  facet_wrap(~sex_pique, ncol = 1)
p+plotstyle
plotsave('ts_report_date_dens_by_sex', plotpath_ts_dens_distro)

p <- ggplot(datasubset, aes(date_piqure_saisie))+
  geom_freqpoly(binwidth = 1, color='blue')+
  xlab(label = 'Date')+
  ylab(label = 'Reporting Frequency')+
  ggtitle('Variation in Time of Reporting Date Frequency')
p+plotstyle
plotsave('ts_report_date_freq', plotpath_ts_dens_distro)

p <- ggplot(datasubset, aes(date_piqure_saisie, colour= sex_pique, fill= sex_pique))+
  geom_freqpoly(binwidth = 1)+
  xlab(label = 'Date')+
  ylab(label = 'Reporting Frequency')+
  ggtitle('Variation in Time of Reporting Date Frequency \n (Breakdown by Collector’s Sex)')+
  facet_wrap(~sex_pique, ncol = 1)
p+plotstyle
plotsave('ts_report_date_freq_by_sex', plotpath_ts_dens_distro)

p <- ggplot(datasubset, aes(date_piqure_saisie))+
  geom_histogram(binwidth = 1, alpha=.7, colour='blue')+
  xlab(label = 'Report Date')+
  ylab(label = 'Report Date Count')+
  ggtitle('Hisotgram of Report Date Frequency (Bin Width = 1 day)')
p+plotstyle
plotsave('ts_histogram_report_date_freq_one_day', plotpath_ts_dens_distro)

p <- ggplot(datasubset, aes(date_piqure_saisie))+
  geom_histogram(binwidth = 2, alpha=.7, colour='red')+
  xlab(label = 'Report Date')+
  ylab(label = 'Report Date Count')+
  ggtitle('Hisotgram of Report Date Frequency (Bin Width two days)')
p+plotstyle
plotsave('ts_histogram_report_date_freq_two_days', plotpath_ts_dens_distro)

p <- ggplot(datasubset, aes(date_piqure_saisie))+
  geom_histogram(binwidth = 5, alpha=.7, colour='red')+
  xlab(label = 'Report Date')+
  ylab(label = 'Report Date Count')+
  ggtitle('Hisotgram of Report Date Frequency (Bin Width = 5 days)')
p+plotstyle
plotsave('ts_histogram_report_date_freq_five_days', plotpath_ts_dens_distro)


p <- ggplot(datasubset, aes(date_piqure_saisie, colour=sex_pique))+
  geom_histogram(binwidth = 5, alpha=.7)+
  xlab(label = 'Report Date')+
  ylab(label = 'Report Date Count')+
  ggtitle('Hisotgram of Report Date Frequency \n (Breakdown by Collector’s Sex & Bin Width equals 5 days)')+
  facet_wrap(~sex_pique, ncol = 1)
p+plotstyle
plotsave('ts_histogram_report_date_freq_by_sex_five_days', plotpath_ts_dens_distro)

p <- ggplot(datasubset, aes(date_piqure_saisie, colour=sex_pique))+
  geom_histogram(binwidth = 5, alpha=.4)+
  xlab(label = 'Report Date')+
  ylab(label = 'Report Date Count')+
  ggtitle('Hisotgram of Report Date Frequency \n (Breakdown by French Region & Bin Width equals 5 days)')+
  facet_wrap(~region)
p+plotstyle
plotsave('ts_histogram_report_date_freq_by_region_five_days', plotpath_ts_dens_distro)

p <- ggplot(datasubset, aes(date_piqure_saisie, colour=sex_pique))+
  geom_histogram(binwidth = 5, alpha=.7)+
  xlab(label = 'Report Date')+
  ylab(label = 'Report Date Count')+
  ggtitle('Hisotgram of Report Date Frequency \n (Breakdown by French Departement & Bin Width equals 5 days)')+
  facet_wrap(~departement)
p+plotstyle
plotsave('ts_histogram_report_date_freq_by_dpt_five_days', plotpath_ts_dens_distro)

p <- ggplot(datasubset, aes(date_piqure_saisie, colour=sex_pique))+
  geom_histogram(binwidth = 5, alpha=.4)+
  xlab(label = 'Report Date')+
  ylab(label = 'Report Date Count')+
  ggtitle('Hisotgram of Report Date Frequency \n (Breakdown by Reason of Presence at incident Date & Bin Width equals 5 days)')+
  facet_wrap(~raison_presence)
p+plotstyle
plotsave('ts_histogram_report_date_freq_by_reason_five_days', plotpath_ts_dens_distro)

p <- ggplot(datasubset, aes(date_piqure_saisie, colour=sex_pique))+
  geom_histogram(binwidth = 5, alpha=.4)+
  xlab(label = 'Report Date')+
  ylab(label = 'Report Date Count')+
  ggtitle('Hisotgram of Report Date Frequency \n (Breakdown by Collector’s Age & Bin Width equals 5 days)')+
  facet_wrap(~age)
p+plotstyle
plotsave('ts_histogram_report_date_freq_by_age_five_days', plotpath_ts_dens_distro)

p <- ggplot(datasubset, aes(date_piqure_saisie, colour=sex_pique))+
  geom_histogram(binwidth = 5, alpha=.4)+
  xlab(label = 'Report Date')+
  ylab(label = 'Report Date Count')+
  ggtitle('Hisotgram of Report Date Frequency \n (Breakdown by Report Positional Accuracy & Bin Width equals 5 days)')+
  facet_wrap(~precision_geo)
p+plotstyle
plotsave('ts_histogram_report_date_freq_by_precisiongeo_five_days', plotpath_ts_dens_distro)

p <- ggplot(datasubset, aes(date_piqure_saisie, colour=sex_pique))+
  geom_histogram(binwidth = 5, alpha=.4)+
  xlab(label = 'Report Date')+
  ylab(label = 'Report Date Count')+
  ggtitle('Hisotgram of Report Date Frequency \n (Breakdown by Incident’s Environment & Bin Width equals 5 days)')+
  facet_wrap(~environnement)
p+plotstyle
plotsave('ts_histogram_report_date_freq_by_environ_five_days', plotpath_ts_dens_distro)

p <- ggplot(datasubset, aes(environnement, date_piqure_saisie, colour=sex_pique))+
  geom_violin()+
  xlab(label = 'Environment Type')+
  ylab(label = 'Date')+
  ggtitle('Density of Report Date Frequency Per Incident’s Environment Type')
p+plotstyle
plotsave('violinplot_report_date_freq_by_environ', plotpath_ts_dens_distro)

p <- ggplot(datasubset, aes(environnement, date_piqure_saisie, colour=sex_pique))+
  geom_boxplot()+
  xlab(label = 'Environment Type')+
  ylab(label = 'Date')+
  ggtitle('Distribution of Report Date Frequency Per Incident’s Environment Type')
p+plotstyle
plotsave('boxplot_report_date_freq_by_environ', plotpath_ts_dens_distro)

### Destruction of plot Handler object for safety purposes
rm(p);

############### Study of weather effect on Frequency of reporting dates using point geometries ###################

############ Section Plot Path ############## 
plotpath_ts_reportdate_weather <- paste(getwd(),'/plots/time_series_reportdate_weather', sep = '')
dir.create(plotpath_ts_reportdate_weather)
#############################################

for ( i in 37:length(datasubset) ) {
  
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
      ggtitle(paste('Variation of ',param,' at Report Date between January 2017 & April 2020'))
    plot+plotstyle
    plotname <- paste('ts_',param,'_no_jitter', sep = '')
    plotsave(plotname, plotpath_ts_reportdate_weather)  
    # print(plot)
  }
  
}

############ Section Plot Path ############## 
plotpath_ts_reportdate_weather_by_sex <- paste(getwd(),'/plots/time_series_reportdate_weather_by_sex', sep = '')
dir.create(plotpath_ts_reportdate_weather_by_sex)
#############################################

##### Without Regional Variation Breakdown by Sex Jittered and Weighted by number of ticks
for ( i in 37:length(datasubset) ) {
  
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
      ggtitle(paste('Variation of ',param,' at Report Date between January 2017 & April 2020 
                     \n Breakdown by Sex Jittered and Weighted by nbr of Collected Ticks'))
    plot+plotstyle
    plotname <- paste('ts_',param,'_by_sex_jitter_proportional', sep = '')
    plotsave(plotname, plotpath_ts_reportdate_weather_by_sex)    
    # print(plot)
  }
  
}

############ Section Plot Path ############## 
plotpath_ts_reportdate_weather_by_sex_by_region <- paste(getwd(),'/plots/time_series_reportdate_weather_by_sex_by_region', sep = '')
dir.create(plotpath_ts_reportdate_weather_by_sex_by_region)
#############################################

############### Study of regional effect and weather parameters on Frequency of reporting days ###################
##### With Regional Variation
for ( i in 37:length(datasubset) ) {
  
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
      ggtitle(paste('Regional Variation of ',param,' at Report Date between January 2017 & April 2020'))
    plot+plotstyle
    plotname <- paste('ts_',param,'_with_region_var', sep = '')
    plotsave(plotname, plotpath_ts_reportdate_weather_by_sex_by_region)
    # print(plot)
  }
  
}

####################################################################################################################
####################################################################################################################
########################################### Weather data: without reports ##########################################
####################################################################################################################

###############  Average weather analysis: Mainland France & Corsica using line geometries ####################

### Creating weather plotting function

weatherplot <- function(weatherframe, wframename, plotpath){
  
  for( i in 3:length(weatherframe) ) {
    
    param <- names(weatherframe[i])
    
    ## The second column of each dframe is the date frame
    plot <- ggplot(weatherframe, aes(weatherframe[,2], weatherframe[,i]) )+
      geom_line(color='blue')+
      geom_smooth(color='black')+
      xlab(label = 'Date')+
      ylab(label=paste(param, ' IS. Unit'))+
      ggtitle(paste('Daily Variation of ',param,'. 
                    \n Mainland France & Corsica between January 2017 & April 2020 
                    \n (',wframename,')'))
    plot+plotstyle
    plotname <- paste(param, wframename, sep = '_')
    plotsave(plotname, plotpath)
    # print(plot)
    
  }
  
}

#############################  dsk synop 42  ############
query_dsk42 <- 'SELECT * FROM meteo.darksky_synop42_avg'
res <- dbSendQuery(con, query_dsk42)
dataset_dsk42 <- fetch(res, n=-1)

#### Plot Path
plotpath_ts_weather_dsk42 <- paste(getwd(),'/plots/time_series_weather_darksky42', sep = '')
dir.create(plotpath_ts_weather_dsk42)

weatherplot(dataset_dsk42, 'Darksky_42_Synoptic_Stations.', plotpath_ts_weather_dsk42)

#############################  meteo france synop 42  ################################
query_mf42 <- 'SELECT *, floor(humidite) as humidity_rounded FROM meteo.mf_synop42_avg'
res <- dbSendQuery(con, query_mf42)
dataset_mf42 <- fetch(res, n=-1)

#### Plot Path  
plotpath_ts_weather_mf42 <- paste(getwd(),'/plots/time_series_weather_mf42', sep = '')
dir.create(plotpath_ts_weather_mf42)

weatherplot(dataset_mf42, 'Meteo-France-42_Synoptic_Stations', plotpath_ts_weather_mf42)

#############################  dsk synop 700 ##################
query_dsk700 <- 'SELECT * FROM meteo.darksky_maille_700_avg'
res <- dbSendQuery(con, query_dsk700)
dataset_dsk700 <- fetch(res, n=-1)

### Plot Path
plotpath_ts_weather_dsk700 <- paste(getwd(),'/plots/time_series_weather_darksky700', sep = '')
dir.create(plotpath_ts_weather_dsk700)

weatherplot(dataset_dsk700, 'Darksky_700_Synoptic_Stations', plotpath_ts_weather_dsk700)


