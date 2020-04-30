### conding: utf-8 ###

from csv 			import DictReader, DictWriter, reader, writer, QUOTE_ALL
from numpy 			import mean
from time 			import sleep
from psycopg2 		import connect
from psycopg2.extras 	import DictCursor
import warnings

class MFDayMeaner:

	def __init__( self ):

		self.meanlist00 =	[]
		self.meanlist03 =	[]
		self.meanlist06 =	[]
		self.meanlist09 =	[]
		self.meanlist12 =	[]
		self.meanlist15 =	[]
		self.meanlist18 =	[]
		self.meanlist21 =	[]

	def computeDayMean(self, offset, offsetStatus, absoluTemp):

		mean00 = mean(self.meanlist00)
		mean03 = mean(self.meanlist03)
		mean06 = mean(self.meanlist06)
		mean09 = mean(self.meanlist09)
		mean12 = mean(self.meanlist12)
		mean15 = mean(self.meanlist15)
		mean18 = mean(self.meanlist18)
		mean21 = mean(self.meanlist21)

		dayMeanlsit = [mean00,
						mean03,
							mean06,
								mean09,
									mean12,
										mean15,
											mean18,
												mean21]

		dayMean = mean( dayMeanlsit )

		if absoluTemp == True:
			kelvin = 273.15
		else :
			kelvin = 0

		if offsetStatus == False:
			result = dayMean - kelvin
		else:
			result = (dayMean - kelvin) + offset

		result = round( result, 2 )

		self.meanlist00.clear()
		self.meanlist03.clear()
		self.meanlist06.clear()
		self.meanlist09.clear()
		self.meanlist12.clear()
		self.meanlist15.clear()
		self.meanlist18.clear()
		self.meanlist21.clear()

		return result


	def getDayMean(self, paramHeader, offset, offsetStatus, absoluTemp, outputfile):

		conn = connect(host='localhost', database='localbase10', user='beetroot', password='root', port=5432)
		print('Connection to database')
		curs = conn.cursor(cursor_factory = DictCursor)
		print('Creating DictCursor')
		sqlweather = "SELECT date, {} FROM meteo.synop_meteo_france_select AS mfs ORDER BY mfs.date asc;".format(paramHeader)
		curs.execute(sqlweather)
		print('Executing query...Data is being fetched...')
		wdata = curs.fetchall()
		print('Data is fetched dictionary is constructed.')
		print('File creation is beginning.')

		with open(outputfile, 'w+') as outputfilestream:
			print('output file ', outputfile, ' is open for writing')

			header = '{};{}\n'.format( 'date', paramHeader )
			outputfilestream.write( header )
			quartercheck21 = ''

			try:
				for row in wdata :
					if   (row['date'][8:] == '030000') & (row[paramHeader] != '') :
						self.meanlist03.append( round( float(row[paramHeader]), 2) )

					elif (row['date'][8:] == '060000') & (row[paramHeader] != '') :
						self.meanlist06.append( round( float(row[paramHeader]), 2) )

					elif (row['date'][8:] == '090000') & (row[paramHeader] != '') :
						self.meanlist09.append( round( float(row[paramHeader]), 2) )

					elif (row['date'][8:] == '120000') & (row[paramHeader] != '') :
						self.meanlist12.append( round( float(row[paramHeader]), 2) )

					elif (row['date'][8:] == '150000') & (row[paramHeader] != '') :
						self.meanlist15.append( round( float(row[paramHeader]), 2) )

					elif (row['date'][8:] == '180000') & (row[paramHeader] != '') :
						self.meanlist18.append( round( float(row[paramHeader]), 2) )

					elif (row['date'][8:] == '210000') & (row[paramHeader] != '') :
						self.meanlist21.append( round( float(row[paramHeader]), 2) )

						quartercheck21 = row['date'][8:10]
						day   = row['date'][6:8]
						month = row['date'][4:6]
						year  = row['date'][0:4]

					elif (row['date'][8:] == '000000') and (row[paramHeader] != '') :

						if (quartercheck21 == '21') and ( row['date'][8:10] == '00' ) :

							average = self.computeDayMean(offset, offsetStatus, absoluTemp)
							dayaverage = '{}/{}/{};{}\n'.format( day, month, year, average )
							outputfilestream.write( dayaverage )
							
							if (day+month == '3112') or (day+month+year == '12042020'):
								print('Year ', year, ' is done.')
							
							del day
							del month
							del year
							del average
							quartercheck21 = ''
							
							self.meanlist00.append( round( float(row[paramHeader]), 2) )
							continue
						else:
							self.meanlist00.append( round( float(row[paramHeader]), 2) )
							continue
					else:
						continue

			except RuntimeWarning as error:
				print('Runtime warning! :', error)

		print('All averages written to ', outputfile)
		self.meanlist00.clear()
		self.meanlist03.clear()
		self.meanlist06.clear()
		self.meanlist09.clear()
		self.meanlist12.clear()
		self.meanlist15.clear()
		self.meanlist18.clear()
		self.meanlist21.clear()
		del wdata
		print('Cleaning up finished...')
		curs.close()
		conn.close()
		print('Connextion to database is closed.')


############################################################ END OF CLASS ##################################################################
outputfolder = '/home/beetroot/Developer/R/projetArticleR/param/'

## Instanciation de l’objet
MFDayMeaner = MFDayMeaner()

### température
#MFDayMeaner.getDayMean( paramHeader='temperature', offset=0, offsetStatus=False, absoluTemp=True, outputfile = outputfolder+'tmpDB.csv' )

### température + offset
#MFDayMeaner.getDayMean( paramHeader='temperature', offset=2, offsetStatus=True, absoluTemp=True, outputfile = outputfolder+'tmpOffset2DB.csv' )

#### Humidité
#MFDayMeaner.getDayMean( paramHeader='humidite', offset=0, offsetStatus=False, absoluTemp=False, outputfile = outputfolder+'humidityDB.csv' )

#### Humidité + offset
#MFDayMeaner.getDayMean( paramHeader='humidite', offset=2, offsetStatus=True, absoluTemp=False, outputfile = outputfolder+'humidityOffse2tDB.csv' )

### ptRosée
#MFDayMeaner.getDayMean( paramHeader='point_rose', offset=0, offsetStatus=False, absoluTemp=True, outputfile = outputfolder+'ptroseDB.csv' )

### ptRosée + offset
#MFDayMeaner.getDayMean( paramHeader='point_rose', offset=2, offsetStatus=True, absoluTemp=True, outputfile = outputfolder+'ptroseOffset2DB.csv' )

### pression atmosphérique
#MFDayMeaner.getDayMean( paramHeader='press_sta', offset=0, offsetStatus=False, absoluTemp=False, outputfile = outputfolder+'pressionDB.csv' )

### vvent
#MFDayMeaner.getDayMean( paramHeader='vvent', offset=0, offsetStatus=False, absoluTemp=False, outputfile = outputfolder+'vventDB.csv' )

### visibilite
#MFDayMeaner.getDayMean( paramHeader='visibilite', offset=0, offsetStatus=False, absoluTemp=False, outputfile = outputfolder+'visibiliteDB.csv' )

### nebulosité relative
#MFDayMeaner.getDayMean( paramHeader='nebulosite', offset=0, offsetStatus=False, absoluTemp=False, outputfile = outputfolder+'nebulositeDB.csv' )

### rafale de vent les 10 dernières minuites
#MFDayMeaner.getDayMean( paramHeader='rafale_10min', offset=0, offsetStatus=False, absoluTemp=False, outputfile = outputfolder+'rafal_10minDB.csv' )

### précipitation en mm toutes les heures 
#MFDayMeaner.getDayMean( paramHeader='precip_01h', offset=0, offsetStatus=False, absoluTemp=False, outputfile = outputfolder+'precip_01hDB.csv' )

### précipitation en mm toutes les 24 heures 
#MFDayMeaner.getDayMean( paramHeader='precip_24h', offset=0, offsetStatus=False, absoluTemp=False, outputfile = outputfolder+'precip_24hDB.csv' )

### Température ressentie sur 24h
#MFDayMeaner.getDayMean( paramHeader='temp_max_24', offset=0, offsetStatus=False, absoluTemp=True, outputfile = outputfolder+'tempmax24DB.csv' )

### Température ressentie sur 24h + offset
#MFDayMeaner.getDayMean( paramHeader='temp_max_24', offset=2, offsetStatus=True, absoluTemp=True, outputfile = outputfolder+'tempmax24Offset2DB.csv' )

### Température ressentie sur 24h
#MFDayMeaner.getDayMean( paramHeader='temp_min_24', offset=0, offsetStatus=False, absoluTemp=True, outputfile = outputfolder+'tempmin24DB.csv' )

### Température ressentie sur 24h + offset
#MFDayMeaner.getDayMean( paramHeader='temp_min_24', offset=2, offsetStatus=True, absoluTemp=True, outputfile = outputfolder+'tempmin24Offset2DB.csv' )




