################## Library calls and preliminary database connection ####################################
getwd()
setwd("./")

require(ggplot2)
require(RPostgreSQL)
require(MASS)
require(mgcv)


ggplot(humdata, aes(date_piqure_saisie))+
  geom_jitter(aes(y=temperature), color='blue', size=1, alpha=.7)+
  geom_smooth(aes(y=temperature), color='black', size=.5)+
  geom_line( data = DSKdata_700avg, aes(date_releve, temperature), color='green')+
  geom_smooth(data = DSKdata_700avg, color='red', aes(date_releve, temperature), size=.5 )+
  ggtitle('Comparaison between vartiations in time of signaling vs witness temperatures')+
  geom_line(y = 0, colour='grey50', linetype='dotdash')+
  xlab(label = 'Date')+
  ylab(label=' Temperature degrees Celsius')+
  # ylim(min(DSKdata_700avg$temperature), max(DSKdata_700avg$temperature))+
  # scale_y_continuous(breaks = seq(ceiling(min(DSKdata_700avg$temperature)+5),ceiling(max(DSKdata_700avg$temperature)+5), by=5) )+
  scale_y_continuous(breaks = c(seq(-10,40, by=5)) )+
  scale_x_date(expand = c(0,0), limits = as.Date(c('2017-03-31','2020-04-01')))+
  plotstyle
# plotsave('Comparaison between vartiations in time of signaling vs witness temperatures', '.')

ggplot(datasubset, aes(date_piqure_saisie))+
  geom_point(aes(y=temperature), color='blue', size=.2, alpha=.7)+
  geom_smooth(aes(y=temperature), color='black')+
  geom_line( data = dataset_dsk700, aes(date_releve, temperature), color='#00CC00')+
  geom_smooth(data=dataset_dsk700, color='red', mapping = aes(date_releve, temperature))+
  ggtitle('Comparaison between vartiations in time of signaling vs witness temperatures \n from 2018-03-01 to 2018-10-01')+
  xlab(label = 'Date')+
  ylab(label=' Temperature degrees Celsius')+
  xlim( as.Date('2018-03-01'), as.Date('2018-10-01') )+
  plotstyle
plotsave('Comparaison between vartiations in time of signaling vs witness temperatures spring summer 2018', '.')

ggplot(datasubset, aes(date_piqure_saisie))+
  geom_point(aes(y=humidity), color='blue', size=.2, alpha=.7)+
  geom_smooth(aes(y=humidity), color='black')+
  geom_line( data = dataset_dsk700, aes(date_releve, humidity), color='green')+
  geom_smooth(data=dataset_dsk700, color='red', mapping = aes(date_releve, humidity))+
  ggtitle('Comparaison between vartiations in time of signaling vs witness humidity')+
  xlab(label = 'Date')+
  ylab(label=' Humidity %')+
  plotstyle
plotsave('omparaison between vartiations in time of signaling vs witness humidity', '.')

ggplot(datasubset, aes(date_piqure_saisie))+
  geom_point(aes(y=dewpoint), color='blue', size=.2, alpha=.7)+
  geom_smooth(aes(y=dewpoint), color='black')+
  geom_line( data = dataset_dsk700, aes(date_releve, dewpoint), color='green')+
  geom_smooth(data=dataset_dsk700, color='red', mapping = aes(date_releve, dewpoint))+
  ggtitle('Comparaison between vartiations in time of signaling vs witness dew point')+
  xlab(label = 'Date')+
  ylab(label='Dew point degrees Celsius')+
  plotstyle
plotsave('Comparaison between vartiations in time of signaling vs witness dew point', '.')

ggplot(datasubset, aes(date_piqure_saisie))+
  geom_point(aes(y=uvindex), color='blue', size=.2, alpha=.7)+
  geom_smooth(aes(y=uvindex), color='black')+
  geom_line( data = dataset_dsk700, aes(date_releve, uvindex), color='green')+
  geom_smooth(data=dataset_dsk700, color='red', mapping = aes(date_releve, uvindex))+
  ggtitle('Comparaison between vartiations in time of signaling vs witness Ultra violet index')+
  xlab(label = 'Date')+
  ylab(label='Ultra violet index scale 0 to 10')+
  plotstyle
plotsave('Comparaison between vartiations in time of signaling vs witness Ultra violet index', '.')


ggplot(datasubset, aes(date_piqure_saisie))+
  geom_point(aes(y=temperaturehigh), color='blue', size=.2, alpha=.7)+
  geom_smooth(aes(y=temperaturehigh), color='black')+
  geom_line( data = dataset_dsk700, aes(date_releve, temperaturehigh), color='green')+
  geom_smooth(data=dataset_dsk700, color='red', mapping = aes(date_releve, temperaturehigh))+
  ggtitle('Comparaison between vartiations in time of signaling vs witness day temperatures')+
  xlab(label = 'Date')+
  ylab(label=' Temperature degrees Celsius')+
  plotstyle
plotsave('Comparaison between vartiations in time of signaling vs witness day temperatures', '.')


ggplot(datasubset, aes(date_piqure_saisie))+
  geom_point(aes(y=temperaturelow), color='blue', size=.2, alpha=.7)+
  geom_smooth(aes(y=temperaturelow), color='black')+
  geom_line( data = dataset_dsk700, aes(date_releve, temperaturelow), color='green')+
  geom_smooth(data=dataset_dsk700, color='red', mapping = aes(date_releve, temperaturelow))+
  ggtitle('Comparaison between vartiations in time of signaling vs witness night temperatures')+
  xlab(label = 'Date')+
  ylab(label=' Temperature degrees Celsius')+
  plotstyle
plotsave('Comparaison between vartiations in time of signaling vs witness night temperatures', '.')


ggplot(datasubset, aes(date_piqure_saisie))+
  geom_point(aes(y=pressure), color='blue', size=.2, alpha=.7)+
  geom_smooth(aes(y=pressure), color='black')+
  geom_line( data = dataset_dsk700, aes(date_releve,y=pressure), color='green')+
  geom_smooth(data=dataset_dsk700, color='red', mapping = aes(date_releve,y=pressure))+
  ggtitle('Comparaison between vartiations in time of signaling vs witness pressure')+
  xlab(label = 'Date')+
  ylab(label=' pressure hPa')+
  plotstyle
plotsave('Comparaison between vartiations in time of signaling vs witness pressure', '.')

## Etude de l’Effet regional

ggplot(datasubset, aes(date_piqure_saisie))+
  geom_point(aes(y=temperature), color='blue', size=.2, alpha=.7)+
  geom_smooth(aes(y=temperature), color='black')+
  geom_line( data = dataset_dsk700, aes(date_releve, temperature), color='green')+
  geom_smooth(data=dataset_dsk700, color='red', mapping = aes(date_releve, temperature))+
  ggtitle('Regional Comparaison between vartiations in time of signaling vs witness temperatures')+
  xlab(label = 'Date')+
  ylab(label=' Temperature degrees Celsius')+
  facet_wrap(~study_area, ncol = 2)+
  plotstyle
plotsave('Regional Comparaison between vartiations in time of signaling vs witness temperatures', '.')

ggplot(datasubset, aes(date_piqure_saisie))+
  geom_point(aes(y=temperature), color='blue', size=.2, alpha=.7)+
  geom_smooth(aes(y=temperature), color='black')+
  geom_line( data = dataset_dsk700, aes(date_releve, temperature), color='#00CC00')+
  geom_smooth(data=dataset_dsk700, color='red', mapping = aes(date_releve, temperature))+
  ggtitle('Regional Comparaison between vartiations in time of signaling vs witness temperatures \n from 2018-03-01 to 2018-10-01')+
  xlab(label = 'Date')+
  ylab(label=' Temperature degrees Celsius')+
  xlim( as.Date('2018-03-01'), as.Date('2018-10-01') )+
  facet_wrap(~study_area, ncol = 2)+
  plotstyle
plotsave('Regional Comparaison between vartiations in time of signaling vs witness temperatures spring summer 2018', '.')

ggplot(datasubset, aes(date_piqure_saisie))+
  geom_point(aes(y=humidity), color='blue', size=.2, alpha=.7)+
  geom_smooth(aes(y=humidity), color='black')+
  geom_line( data = dataset_dsk700, aes(date_releve, humidity), color='green')+
  geom_smooth(data=dataset_dsk700, color='red', mapping = aes(date_releve, humidity))+
  ggtitle('Regional Comparaison between vartiations in time of signaling vs witness humidity')+
  xlab(label = 'Date')+
  ylab(label=' Humidity %')+
  facet_wrap(~study_area, ncol = 2)+
  plotstyle
plotsave('Regional Comparaison between vartiations in time of signaling vs witness humidity', '.')

ggplot(datasubset, aes(date_piqure_saisie))+
  geom_point(aes(y=dewpoint), color='blue', size=.2, alpha=.7)+
  geom_smooth(aes(y=dewpoint), color='black')+
  geom_line( data = dataset_dsk700, aes(date_releve, dewpoint), color='green')+
  geom_smooth(data=dataset_dsk700, color='red', mapping = aes(date_releve, dewpoint))+
  ggtitle('Regional Comparaison between vartiations in time of signaling vs witness dew point')+
  xlab(label = 'Date')+
  ylab(label='Dew point degrees Celsius')+
  facet_wrap(~study_area, ncol = 2)+
  plotstyle
plotsave('Regional Comparaison between vartiations in time of signaling vs witness dew point', '.')

ggplot(datasubset, aes(date_piqure_saisie))+
  geom_point(aes(y=uvindex), color='blue', size=.2, alpha=.7)+
  geom_smooth(aes(y=uvindex), color='black')+
  geom_line( data = dataset_dsk700, aes(date_releve, uvindex), color='green')+
  geom_smooth(data=dataset_dsk700, color='red', mapping = aes(date_releve, uvindex))+
  ggtitle('Regional Regional Comparaison between vartiations in time of signaling vs witness Ultra violet index')+
  xlab(label = 'Date')+
  ylab(label='Ultra violet index scale 0 to 10')+
  facet_wrap(~study_area, ncol = 2)+
  plotstyle
plotsave('Regional Comparaison between vartiations in time of signaling vs witness Ultra violet index', '.')


ggplot(datasubset, aes(date_piqure_saisie))+
  geom_point(aes(y=temperaturehigh), color='blue', size=.2, alpha=.7)+
  geom_smooth(aes(y=temperaturehigh), color='black')+
  geom_line( data = dataset_dsk700, aes(date_releve, temperaturehigh), color='green')+
  geom_smooth(data=dataset_dsk700, color='red', mapping = aes(date_releve, temperaturehigh))+
  ggtitle('Regional Comparaison between vartiations in time of signaling vs witness day temperatures')+
  xlab(label = 'Date')+
  ylab(label=' Temperature degrees Celsius')+
  facet_wrap(~study_area, ncol = 2)+
  plotstyle
plotsave('Regional Comparaison between vartiations in time of signaling vs witness day temperatures', '.')


ggplot(datasubset, aes(date_piqure_saisie))+
  geom_point(aes(y=temperaturelow), color='blue', size=.2, alpha=.7)+
  geom_smooth(aes(y=temperaturelow), color='black')+
  geom_line( data = dataset_dsk700, aes(date_releve, temperaturelow), color='green')+
  geom_smooth(data=dataset_dsk700, color='red', mapping = aes(date_releve, temperaturelow))+
  ggtitle('Regional Comparaison between vartiations in time of signaling vs witness night temperatures')+
  xlab(label = 'Date')+
  ylab(label=' Temperature degrees Celsius')+
  facet_wrap(~study_area, ncol = 2)+
  plotstyle
plotsave('Regional Comparaison between vartiations in time of signaling vs witness night temperatures', '.')


ggplot(datasubset, aes(date_piqure_saisie))+
  geom_point(aes(y=pressure), color='blue', size=.2, alpha=.7)+
  geom_smooth(aes(y=pressure), color='black')+
  geom_line( data = dataset_dsk700, aes(date_releve,y=pressure), color='green')+
  geom_smooth(data=dataset_dsk700, color='red', mapping = aes(date_releve,y=pressure))+
  ggtitle('Regional Comparaison between vartiations in time of signaling vs witness pressure')+
  xlab(label = 'Date')+
  ylab(label=' pressure hPa')+
  facet_wrap(~study_area, ncol = 2)+
  plotstyle
plotsave('Regional Comparaison between vartiations in time of signaling vs witness pressure', '.')

