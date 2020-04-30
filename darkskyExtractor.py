#coding:utf-8

### darkskyExtractor.py ################################################
#												#
#									  			#
#Author: Hilami Khaldoune								#
#Organism: CNRS										#
#Year: 2018											#
#License: This script is propriety of CNRS					#  
#Descriptions: This script retrives weather data for darksky.net website#
#												#
########################################################################

###library IMPORTS###

from darksky  import forecast
from datetime import date, timedelta
from csv import reader, writer, QUOTE_ALL
from re import search
from itertools import islice
from os import chdir, getcwd, listdir
chdir("/home/beetroot/Developer/python/CNRS/projetNominatim")

APIkey= '865f840cdde4ab359bce9a5adee70f84'
missingObjectCount=0
loop=0

with open('./.csv', 'r') as weatherDataInputFile:

	filestream = reader(weatherDataInputFile, delimiter=';')
	# Header Construction
	for line in islice(filestream,0,1):
		headerlist=line
	headerlist+=['timezone','time','summary','icon','sunriseTime','sunsetTime','moonPhase',
				'precipIntensity','precipIntensityMax','precipIntensityMaxTime','precipAccumulation',
					'precipProbability','temperatureHigh','temperatureHighTime','temperatureLow','temperatureLowTime',
						'dewPoint','humidity','pressure','visibility','windGust','windGustTime','windSpeed','windBearing',
							'cloudCover','cloudCoverError','uvIndex','uvIndexTime','temperatureMin','temperatureMinTime','temperatureMax',
								'temperatureMaxTime','apparentTemperatureMin','apparentTemperatureMinTime','apparentTemperatureMax','apparentTemperatureMaxTime',
									'apparentTemperatureHigh','apparentTemperatureHighTime','apparentTemperatureLow','apparentTemperatureLowTime','DataSource']

	print("File headers construction is complete...\n")
	print("...\n")

	with open('./.csv', 'a') as weatherDataOutputFile:
		streamwriter = writer( weatherDataOutputFile, delimiter = ';', quotechar = "'", quoting=QUOTE_ALL   )
		# Header writing to the file
		streamwriter.writerow(headerlist)
		print("File headers writing is complete. Beginning  data extraction precess...\n")
		print("...\n")
		# Main extraction loop
		for row in filestream:

			# field[9] is the latittude of the point; field[10] is the positional argument of lognitude
			# Set of Object existance verification, might be unnecessary
			with forecast(APIkey, field[9], field[10], lang='en', units='si', time=field[7], exclude='currently,minutely,hourly,alerts' ) as location:
				if hasattr(location.daily[0], 'timezone')==False: location.daily[0].timezone='NA'
				if hasattr(location.daily[0], 'time')==False: location.daily[0].time='NA'
				if hasattr(location.daily[0], 'summary')==False: location.daily[0].summary='NA'
				if hasattr(location.daily[0], 'icon')==False: location.daily[0].icon='NA'
				if hasattr(location.daily[0], 'sunriseTime')==False: location.daily[0].sunriseTime='NA'
				if hasattr(location.daily[0], 'sunsetTime')==False: location.daily[0].sunsetTime='NA'
				if hasattr(location.daily[0], 'moonPhase')==False: location.daily[0].moonPhase='NA'
				if hasattr(location.daily[0], 'precipIntensity')==False: location.daily[0].precipIntensity='NA'
				if hasattr(location.daily[0], 'precipIntensityMax')==False: location.daily[0].precipIntensityMax='NA'
				if hasattr(location.daily[0], 'precipIntensityMaxTime')==False: location.daily[0].precipIntensityMaxTime='NA'
				if hasattr(location.daily[0], 'precipAccumulation')==False: location.daily[0].precipAccumulation='NA'
				if hasattr(location.daily[0], 'precipProbability')==False: location.daily[0].precipProbability='NA'
				if hasattr(location.daily[0], 'temperatureHigh')==False: location.daily[0].temperatureHigh='NA'
				if hasattr(location.daily[0], 'temperatureHighTime')==False: location.daily[0].temperatureHighTime='NA'
				if hasattr(location.daily[0], 'temperatureLow')==False: location.daily[0].temperatureLow='NA'
				if hasattr(location.daily[0], 'temperatureLowTime')==False: location.daily[0].temperatureLowTime='NA'
				if hasattr(location.daily[0], 'dewPoint')==False: location.daily[0].dewPoint='NA'
				if hasattr(location.daily[0], 'humidity')==False: location.daily[0].humidity='NA'
				if hasattr(location.daily[0], 'pressure')==False: location.daily[0].pressure='NA'
				if hasattr(location.daily[0], 'visibility')==False: location.daily[0].visibility='NA'
				if hasattr(location.daily[0], 'windGust')==False: location.daily[0].windGust='NA'
				if hasattr(location.daily[0], 'windGustTime')==False: location.daily[0].windGustTime='NA'
				if hasattr(location.daily[0], 'windSpeed')==False: location.daily[0].windSpeed='NA'
				if hasattr(location.daily[0], 'windBearing')==False: location.daily[0].windBearing='NA'
				if hasattr(location.daily[0], 'cloudCover')==False: location.daily[0].cloudCover='NA'
				if hasattr(location.daily[0], 'cloudCoverError')==False: location.daily[0].cloudCoverError='NA'
				if hasattr(location.daily[0], 'uvIndex')==False: location.daily[0].uvIndex='NA'
				if hasattr(location.daily[0], 'uvIndexTime')==False: location.daily[0].uvIndexTime='NA'
				if hasattr(location.daily[0], 'temperatureMin')==False: location.daily[0].temperatureMin='NA'
				if hasattr(location.daily[0], 'temperatureMinTime')==False: location.daily[0].temperatureMinTime='NA'
				if hasattr(location.daily[0], 'temperatureMax')==False: location.daily[0].temperatureMax='NA'
				if hasattr(location.daily[0], 'temperatureMaxTime')==False: location.daily[0].temperatureMaxTime='NA'
				if hasattr(location.daily[0], 'apparentTemperatureMin')==False: location.daily[0].apparentTemperatureMin='NA'
				if hasattr(location.daily[0], 'apparentTemperatureMinTime')==False: location.daily[0].apparentTemperatureMinTime='NA'
				if hasattr(location.daily[0], 'apparentTemperatureMax')==False: location.daily[0].apparentTemperatureMax='NA'
				if hasattr(location.daily[0], 'apparentTemperatureMaxTime')==False: location.daily[0].apparentTemperatureMaxTime='NA'
				if hasattr(location.daily[0], 'apparentTemperatureHigh')==False: location.daily[0].apparentTemperatureHigh='NA'
				if hasattr(location.daily[0], 'apparentTemperatureHighTime')==False: location.daily[0].apparentTemperatureHighTime='NA'
				if hasattr(location.daily[0], 'apparentTemperatureLow')==False: location.daily[0].apparentTemperatureLow='NA'
				if hasattr(location.daily[0], 'apparentTemperatureLowTime')==False: location.daily[0].apparentTemperatureLowTime='NA'

			row += [location.timezone, location.daily[0].time, location.daily[0].summary, location.daily[0].icon, 
				location.daily[0].sunriseTime, location.daily[0].sunsetTime, location.daily[0].moonPhase, location.daily[0].precipIntensity,
				location.daily[0].precipIntensityMax, location.daily[0].precipIntensityMaxTime, location.daily[0].precipAccumulation, location.daily[0].precipProbability,
				location.daily[0].temperatureHigh, location.daily[0].temperatureHighTime, location.daily[0].temperatureLow, location.daily[0].temperatureLowTime, location.daily[0].dewPoint, 
				location.daily[0].humidity, location.daily[0].pressure, location.daily[0].visibility, location.daily[0].windGust, location.daily[0].windGustTime, location.daily[0].windSpeed, 
				location.daily[0].windBearing, location.daily[0].cloudCover, location.daily[0].cloudCoverError, location.daily[0].uvIndex, location.daily[0].uvIndexTime, location.daily[0].temperatureMin, 
				location.daily[0].temperatureMinTime, location.daily[0].temperatureMax, location.daily[0].temperatureMaxTime, location.daily[0].apparentTemperatureMin, location.daily[0].apparentTemperatureMinTime, 
				location.daily[0].apparentTemperatureMax, location.daily[0].apparentTemperatureMaxTime, location.daily[0].apparentTemperatureHigh, location.daily[0].apparentTemperatureHighTime, 
				location.daily[0].apparentTemperatureLow, location.daily[0].apparentTemperatureLowTime, location.flags.sources]
			# Conversion of the list to string
			row = [str(field or '') for field in row]
 
					try:
						streamwriter.writerow(row)
						loop += 1
						print('Object ', loop,' Written\n')
					except Exception, AttributeError, ValueError:
						print('Could not convert string to float!')
						missingObjectCount+=1
						print('The program will continue...')

if (missingObjectCount>0):
	print("There was", missingObjectCount, "missing dataPoint objects during the extraction process.")
else:
	print("Congratulations! Extraction finished without missing dataPoint objects! You may check your file.")



################################################ EOS #########################################################################################################################################################
