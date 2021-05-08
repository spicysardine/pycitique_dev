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

### Selecting reports with less than 25 Collected Ticks by reporter’s sex. Île-de-France
### This subset will be used throughout the script.
query <- " SELECT 
              *,
              	CASE
              		WHEN departement_code ~ '26|07|42|69D|69M|01|74|73|38' THEN  'Rhone-Alpes'
              		WHEN departement_code ~ '54|55|57|67|68|88' THEN 'Alsace-Lorraine'
              		ELSE  'Île-de-France' 
              	END as study_area
              
              FROM citik.citik_humains_clean_weather_strict
              WHERE ( sex_pique != '' AND nbr_tique < 25 )
              -- Departements of Île-de-France
              AND ( region = 'Île-de-France'
              -- Departements of Rhone-Alpes
              OR departement_code ~* '26|07|42|69D|69M|01|74|73|38'
              -- -- Departements of Alsace-Lorraine
              OR departement_code ~* '54|55|57|67|68|88' ) "

res <- dbSendQuery(con, query)
datasubset <- fetch(res, n=-1)

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
                      WHERE ( sex_pique != '' AND nbr_tique < 25 )
                      AND ( region = 'Île-de-France'
                      OR departement_code ~* '26|07|42|69D|69M|01|74|73|38'
                      OR departement_code ~* '54|55|57|67|68|88') group by sex_pique"

res <- dbSendQuery(con, query_aggnbr_tique)
aggnbr_tique <- fetch(res, n=-1)

p <- ggplot(aggnbr_tique, aes(sex_pique, sum_nbr_tique))+
  geom_bar(stat = "identity")+
  xlab(label = 'Reporter’s Sex')+
  ylab(label = 'Total Number of Collected Ticks')+
  ggtitle('Total Number of Collected Ticks \n (Breakdown by Sex)')
  p+plotstyle
plotsave('total_nbr_tick_col_by_sex', plotpath_quant_study)

p <- ggplot(datasubset, aes(sex_pique))+
  geom_bar()+
  xlab(label = 'Reporter’s Sex')+
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
  xlab(label = 'Reporter’s Sex')+
  ylab(label = 'Density of Reporting per Number of Collected Ticks')+
  ggtitle('Density of Reports Declaring x Number of Collected Ticks \n (Breakdown by Sex - Violin Plot)')
  p+plotstyle
plotsave('violplot_dens_rep_by_nbr_tick_by_sex', plotpath_quant_study)

p <- ggplot(datasubset, aes(sex_pique, nbr_tique, colour=sex_pique))+
  geom_boxplot()+
  xlab(label = 'Reporter’s Sex')+
  ylab(label = 'Distribution of Reports per Number of Collected Ticks')+
  ggtitle('Distribution of Reports Declaring x Number of Collected Ticks \n (Breakdown by Sex Whisker - Boxplot)')
  p+plotstyle
plotsave('boxplot_dens_rep_by_nbr_tick_by_sex', plotpath_quant_study)

### Destruction of plot Handler object for safety purposes
rm(p);

################ Time series analysis using point distribution : Number of Collected Ticks in time ###############

############ Section Plot Path ############## 
plotpath_ts_pt_distro <- paste(getwd(),'/plots/time_series_analysis_point_distribution', sep = '')
dir.create(plotpath_ts_pt_distro)


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
  ggtitle('Variation in Time of the Number of Ticks \n Collected per Report (Jitterd - Size Proportional)')
  p+plotstyle
# geom_smooth(aes(weight = nbr_tique), method = lm, size = 1)
plotsave('ts_nbr_tick_by_sex_jitter_proportional', plotpath_ts_pt_distro)

p <- ggplot(datasubset, aes(date_piqure_saisie, nbr_tique, colour=sex_pique))+
  geom_jitter(aes(size=nbr_tique), alpha=.4)+
  xlab(label = 'Date')+
  ylab(label = 'Number of Collected Ticks')+
  ggtitle('Breakdown by Study Area of the Variation in Time of the Number of Ticks 
           Collected per Report (Jitterd - Size Proportional)')+
  facet_wrap(~study_area, ncol = 1)
  p+plotstyle
plotsave('ts_nbr_tick_by_region_by_sex_jitter_proportional_vertlyo', plotpath_ts_pt_distro)

p <- ggplot(datasubset, aes(date_piqure_saisie, nbr_tique, colour=sex_pique))+
  geom_jitter(aes(size=nbr_tique), alpha=.4)+
  xlab(label = 'Date')+
  ylab(label = 'Number of Collected Ticks')+
  ggtitle('Breakdown by Study Area of the Variation in Time of the Number of Ticks Collected per Report \n (Jitterd - Size Proportional - Horizontal Layout)')+
  facet_wrap(~study_area)
  p+plotstyle
  plotsave('ts_nbr_tick_by_region_by_sex_jitter_proportional_hzlyo', plotpath_ts_pt_distro)

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
  ggtitle('Breakdown by Reason of Presence at Incident location
          of the Variation in Time of the Number of Collected Ticks per Report (Jitterd)')+
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
  ggtitle('Variation in Time of Reporting Date Density \n (Breakdown by ’s Sex)')+
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
  ggtitle('Variation in Time of Reporting Date Frequency \n (Breakdown by Reporter’s Sex)')+
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
  ggtitle('Hisotgram of Report Date Frequency \n (Breakdown by Reporter’s Sex - Bin Width equals 5 days)')+
  facet_wrap(~sex_pique, ncol = 1)
  p+plotstyle
  plotsave('ts_histogram_report_date_freq_by_sex_five_days', plotpath_ts_dens_distro)

p <- ggplot(datasubset, aes(date_piqure_saisie, colour=sex_pique))+
  geom_histogram(binwidth = 5, alpha=.4)+
  xlab(label = 'Report Date')+
  ylab(label = 'Report Date Count')+
  ggtitle('Hisotgram of Report Date Frequency \n (Breakdown by Study Area - Bin Width equals 5 days)')+
  facet_wrap(~study_area)
  p+plotstyle
  plotsave('ts_histogram_report_date_freq_by_region_five_days_hztlyo', plotpath_ts_dens_distro)

p <- ggplot(datasubset, aes(date_piqure_saisie, colour=sex_pique))+
  geom_histogram(binwidth = 5, alpha=.4)+
  xlab(label = 'Report Date')+
  ylab(label = 'Report Date Count')+
  ggtitle('Hisotgram of Report Date Frequency \n (Breakdown by Study Area - Bin Width equals 5 days. Vertical Layout)')+
  facet_wrap(~study_area, ncol = 1)
  p+plotstyle
  plotsave('ts_histogram_report_date_freq_by_region_five_days_vertlyo', plotpath_ts_dens_distro)

p <- ggplot(datasubset, aes(date_piqure_saisie, colour=sex_pique))+
  geom_histogram(binwidth = 5, alpha=.7)+
  xlab(label = 'Report Date')+
  ylab(label = 'Report Date Count')+
  ggtitle('Hisotgram of Report Date Frequency \n (Breakdown by French Departement - Bin Width equals 5 days)')+
  facet_wrap(~departement)
  p+plotstyle
  plotsave('ts_histogram_report_date_freq_by_dpt_five_days', plotpath_ts_dens_distro)

p <- ggplot(datasubset, aes(date_piqure_saisie, colour=sex_pique))+
  geom_histogram(binwidth = 5, alpha=.4)+
  xlab(label = 'Report Date')+
  ylab(label = 'Report Date Count')+
  ggtitle('Hisotgram of Report Date Frequency \n (Breakdown by Reason of Presence at incident Date - Bin Width equals 5 days)')+
  facet_wrap(~raison_presence)
  p+plotstyle
  plotsave('ts_histogram_report_date_freq_by_reason_five_days', plotpath_ts_dens_distro)

p <- ggplot(datasubset, aes(date_piqure_saisie, colour=sex_pique))+
  geom_histogram(binwidth = 5, alpha=.4)+
  xlab(label = 'Report Date')+
  ylab(label = 'Report Date Count')+
  ggtitle('Hisotgram of Report Date Frequency \n (Breakdown by Reporter’s Age - Bin Width equals 5 days)')+
  facet_wrap(~age)
  p+plotstyle
  plotsave('ts_histogram_report_date_freq_by_age_five_days', plotpath_ts_dens_distro)

p <- ggplot(datasubset, aes(date_piqure_saisie, colour=sex_pique))+
  geom_histogram(binwidth = 5, alpha=.4)+
  xlab(label = 'Report Date')+
  ylab(label = 'Report Date Count')+
  ggtitle('Hisotgram of Report Date Frequency \n (Breakdown by Report Positional Accuracy - Bin Width equals 5 days)')+
  facet_wrap(~precision_geo)
  p+plotstyle
  plotsave('ts_histogram_report_date_freq_by_precisiongeo_five_days', plotpath_ts_dens_distro)

p <- ggplot(datasubset, aes(date_piqure_saisie, colour=sex_pique))+
  geom_histogram(binwidth = 5, alpha=.4)+
  xlab(label = 'Report Date')+
  ylab(label = 'Report Date Count')+
  ggtitle('Hisotgram of Report Date Frequency \n (Breakdown by Incident’s Environment - Bin Width equals 5 days)')+
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

#### Section Plot Path
plotpath_ts_reportdate_weather <- paste(getwd(),'/plots/time_series_reportdate_weather', sep = '')
dir.create(plotpath_ts_reportdate_weather)


for ( i in 37:length(datasubset) ) {
  
  param <- names(datasubset[i])
  timecolumn <- grepl('time', param )
  datasourcecolumn <- grepl('datasource', param )
  cloudcovererrcolumn <- grepl('cloudcovererror', param )
  study_area <- grepl('study_area', param )
  
  if(timecolumn | datasourcecolumn | cloudcovererrcolumn | study_area ) {
    print('Skipping time variable')
  }else{
    print(param)
    plot <- ggplot( datasubset, aes(date_piqure_saisie, datasubset[,i]) )+
      geom_point(size=.2, color= 'blue', alpha = .4)+
      geom_smooth(span=1, color='black')+
      xlab(label = 'Date')+
      ylab(label=paste(param, '( International System. Unit)'))+
      ggtitle(paste('Variation of ',param,' at Report Date between January 2017 - April 2020'))
    plot+plotstyle
    plotname <- paste('ts_reportdate_',param, sep = '')
    plotsave(plotname, plotpath_ts_reportdate_weather)  
  }
  
}

### Section Plot Path
plotpath_ts_reportdate_weather_by_sex <- paste(getwd(),'/plots/time_series_reportdate_weather_by_sex', sep = '')
dir.create(plotpath_ts_reportdate_weather_by_sex)

##### Without Regional Variation Breakdown by Sex Jittered and Weighted by number of ticks
for ( i in 37:length(datasubset) ) {
  
  param <- names(datasubset[i])
  timecolumn <- grepl('time', param )
  datasourcecolumn <- grepl('datasource', param )
  cloudcovererrcolumn <- grepl('cloudcovererror', param )
  study_area <- grepl('study_area', param )
  
  if(timecolumn | datasourcecolumn | cloudcovererrcolumn | study_area) {
    print('Skipping time variable')
  }else{
    print(param)
    plot <- ggplot( datasubset, aes(date_piqure_saisie, datasubset[,i], color = sex_pique) )+
      geom_jitter( aes(size=nbr_tique), alpha = .3)+
      geom_smooth(span=1)+
      xlab(label = 'Date')+
      ylab(label=paste(param, '( International System. Unit)'))+
      ggtitle(paste('Variation of ',param,' at Report Date between January 2017 - April 2020 \n Breakdown by Sex Jittered and Weighted by nbr of Collected Ticks'))
    plot+plotstyle
    plotname <- paste('ts_reportdate_',param,'_by_sex_jitter_proportional', sep = '')
    plotsave(plotname, plotpath_ts_reportdate_weather_by_sex)    
    # print(plot)
  }
  
}

### Section Plot Path
plotpath_ts_reportdate_weather_by_region <- paste(getwd(),'/plots/time_series_reportdate_weather_regional_variation', sep = '')
dir.create(plotpath_ts_reportdate_weather_by_region)

############### Study of regional effect and weather parameters on Frequency of reporting days ###################

##### With Regional Variation
for ( i in 37:length(datasubset) ) {
  
  param <- names(datasubset[i])
  timecolumn <- grepl('time', param )
  datasourcecolumn <- grepl('datasource', param )
  cloudcovererrcolumn <- grepl('cloudcovererror', param )
  study_area <- grepl('study_area', param )
  
  if(timecolumn | datasourcecolumn | cloudcovererrcolumn | study_area ) {
    print('Skipping time variable')
  }else{
    print(param)
    plot <- ggplot(datasubset, aes(date_piqure_saisie, datasubset[,i]) )+
      geom_point(size=.2, color= 'blue', alpha = .4)+
      geom_smooth(span=1, color='black')+
      facet_wrap(~study_area)+
      xlab(label = 'Date')+
      ylab(label=paste(param, ' IS. Unit'))+
      ggtitle(paste('Regional Variation of ',param,' at Report Date between January 2017 - April 2020'))
    plot+plotstyle
    plotname <- paste('ts_reportdate_',param,'_regional_variation', sep = '')
    plotsave(plotname, plotpath_ts_reportdate_weather_by_region)
    # print(plot)
  }
  
}

############### Study of regional effect and weather parameters on Frequency of reporting days vertical layout ###################


### Section Plot Path
plotpath_ts_reportdate_weather_by_region_vlayout <- paste(getwd(),'/plots/time_series_reportdate_weather_regional_variation_vlayout', sep = '')
dir.create(plotpath_ts_reportdate_weather_by_region_vlayout)

##### With Regional Variation vertical layout 
for ( i in 37:length(datasubset) ) {
  
  param <- names(datasubset[i])
  timecolumn <- grepl('time', param )
  datasourcecolumn <- grepl('datasource', param )
  cloudcovererrcolumn <- grepl('cloudcovererror', param )
  study_area <- grepl('study_area', param )
  
  if(timecolumn | datasourcecolumn | cloudcovererrcolumn | study_area ) {
    print('Skipping time variable')
  }else{
    print(param)
    plot <- ggplot(datasubset, aes(date_piqure_saisie, datasubset[,i]) )+
      geom_point(size=.2, color= 'blue', alpha = .4)+
      geom_smooth(span=1, color='black')+
      facet_wrap(~study_area, ncol = 1)+
      xlab(label = 'Date')+
      ylab(label=paste(param, ' IS. Unit'))+
      ggtitle(paste('Regional Variation of ',param,' at Report Date between January 2017 - April 2020 - Vertical Layout'))
    plot+plotstyle
    plotname <- paste('ts_reportdate_',param,'_regional_variation_vlayout', sep = '')
    plotsave(plotname, plotpath_ts_reportdate_weather_by_region_vlayout)
    # print(plot)
  }
  
}
