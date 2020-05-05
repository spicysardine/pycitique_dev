### conding: utf-8 ###

from csv 			import DictReader, DictWriter, reader, writer, QUOTE_ALL
from numpy 			import mean, isnan
from time 			import sleep
from psycopg2 		import connect
from psycopg2.extras 	import DictCursor
from psycopg2.errors	import UndefinedColumn
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
		
		self.outputschema = 'meteo'

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


	def getDayMean(self, paramHeader, offset, offsetStatus, absoluTemp, outputable):

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
		createschemasql = "CREATE SCHEMA IF NOT EXISTS {}".format(self.outputschema)
		curs.execute(createschemasql)
		droptablesql = "DROP TABLE IF EXISTS {}.{} ;".format(self.outputschema, outputable)
		curs.execute(droptablesql)
		createtablesql = "CREATE TABLE {}.{} (date date, {} float);".format( self.outputschema, outputable, outputable)
		curs.execute(createtablesql)
		conn.commit()
		print("Table creation is complete. Beginning  data extraction precess...\n")
		print("...\n")
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
						
						if isnan(average):
							pass
						else:
							date = '{}/{}/{}'.format(day, month, year)
							insertintotablesql = "INSERT INTO {}.{} VALUES ('{}', {}) ;".format(self.outputschema, outputable, date, average)
							curs.execute( insertintotablesql )
							conn.commit()
							
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

		except ( RuntimeWarning, UndefinedColumn ) as error:
			print('Runtime warning! :', error)

		print('All averages written to ', outputable)
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
		print('Connextion to database is closed.\n')


############################################################ END OF CLASS ##################################################################

## Instanciation de l’objet
MFDayMeaner = MFDayMeaner()

### température
MFDayMeaner.getDayMean( paramHeader='temperature', offset=0, offsetStatus=False, absoluTemp=True, outputable = 'tmpDB' )

### température + offset
MFDayMeaner.getDayMean( paramHeader='temperature', offset=2, offsetStatus=True, absoluTemp=True, outputable = 'tmpOffset2DB' )

#### Humidité
MFDayMeaner.getDayMean( paramHeader='humidite', offset=0, offsetStatus=False, absoluTemp=False, outputable = 'humidityDB' )

### ptRosée
MFDayMeaner.getDayMean( paramHeader='point_rose', offset=0, offsetStatus=False, absoluTemp=True, outputable = 'ptroseDB' )

### ptRosée + offset
MFDayMeaner.getDayMean( paramHeader='point_rose', offset=2, offsetStatus=True, absoluTemp=True, outputable = 'ptroseOffset2DB' )

### pression atmosphérique
MFDayMeaner.getDayMean( paramHeader='press_sta', offset=0, offsetStatus=False, absoluTemp=False, outputable = 'pressionDB' )

### vvent
MFDayMeaner.getDayMean( paramHeader='vvent', offset=0, offsetStatus=False, absoluTemp=False, outputable = 'vventDB' )

### visibilite
MFDayMeaner.getDayMean( paramHeader='visibilite', offset=0, offsetStatus=False, absoluTemp=False, outputable = 'visibiliteDB' )

### nebulosité relative
MFDayMeaner.getDayMean( paramHeader='nebulosite', offset=0, offsetStatus=False, absoluTemp=False, outputable = 'nebulositeDB' )

### rafale de vent les 10 dernières minutes
MFDayMeaner.getDayMean( paramHeader='rafale_10min', offset=0, offsetStatus=False, absoluTemp=False, outputable = 'rafal_10minDB' )

### précipitation en mm toutes les heures 
MFDayMeaner.getDayMean( paramHeader='precip_01h', offset=0, offsetStatus=False, absoluTemp=False, outputable = 'precip_01hDB' )

### précipitation en mm toutes les 24 heures 
MFDayMeaner.getDayMean( paramHeader='precip_24h', offset=0, offsetStatus=False, absoluTemp=False, outputable = 'precip_24hDB' )

#### Température ressentie sur 24h
#MFDayMeaner.getDayMean( paramHeader='temp_max_12', offset=0, offsetStatus=False, absoluTemp=True, outputable = 'tempmax12DB' )

#### Température ressentie sur 24h + offset
#MFDayMeaner.getDayMean( paramHeader='temp_max_12', offset=2, offsetStatus=True, absoluTemp=True, outputable = 'tempmax12Offset2DB' )

#### Température ressentie sur 24h
#MFDayMeaner.getDayMean( paramHeader='temp_min_12', offset=0, offsetStatus=False, absoluTemp=True, outputable = 'tempmin12DB' )

#### Température ressentie sur 24h + offset
#MFDayMeaner.getDayMean( paramHeader='temp_min_12', offset=2, offsetStatus=True, absoluTemp=True, outputable = 'tempmin12Offset2DB' )



