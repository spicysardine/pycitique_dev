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
		self.meanlist21 =	[]

	def computeDayMean(self, offset, offsetStatus, absoluTemp):

		mean00 = mean(self.meanlist00)
		mean03 = mean(self.meanlist03)
		mean21 = mean(self.meanlist21)


		dayMeanlsit = [mean00,
						mean03,
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
		self.meanlist21.clear()

		return result


	def getDayMean(self, paramHeader, offset, offsetStatus, absoluTemp, outputfile):

		conn = connect(host='localhost', database='localbase10', user='beetroot', password='root', port=5432)
		print('Connection to database')
		curs = conn.cursor(cursor_factory = DictCursor)
		print('Creating DictCursor')
		sqlweather = "SELECT date, {} FROM meteo.synop_meteo_france_select AS mfs where substring(mfs.date, 9, 2) !~ '(06|09|12|15|18)' ORDER BY mfs.date asc;".format(paramHeader)
#		sqlweather = "SELECT date, {} FROM meteo.synop_meteo_france_select AS mfs ORDER BY mfs.date asc;".format(paramHeader)
		curs.execute(sqlweather)
		print('Executing query...Data is being fetched...')
		wdata = curs.fetchall()
		print('Data is fetched dictionary is constructed.')
		print('File creation is beginning.')

		with open(outputfile, 'w+') as outputfilestream:
			print('output file ', outputfile, ' is open for writing')

			header = '{};{}\n'.format( 'date', paramHeader )
			outputfilestream.write( header )
			quarterchecker = ''

			try:
				for row in wdata :
					if   (row['date'][8:] == '000000') & (row[paramHeader] != '') :
						self.meanlist00.append( round( float(row[paramHeader]), 2) )

					elif (row['date'][8:] == '030000') & (row[paramHeader] != '') :
						self.meanlist03.append( round( float(row[paramHeader]), 2) )
						quarterchecker = row['date'][8:10]
						day   = row['date'][6:8]
						month = row['date'][4:6]
						year  = row['date'][0:4]
						
					elif (row['date'][8:] == '210000') & (row[paramHeader] != '') :
						
						if (quarterchecker == '03') and ( row['date'][8:10] == '21' ) :

							average = self.computeDayMean(offset, offsetStatus, absoluTemp)
							dayaverage = '{}/{}/{};{}\n'.format( day, month, year, average )
							outputfilestream.write( dayaverage )
							
							if (day+month == '3112') or (day+month+year == '12042020'):
								print('Year ', year, ' is done.')
							
							del day
							del month
							del year
							del average
							quarterchecker = ''
							
							self.meanlist21.append( round( float(row[paramHeader]), 2) )
							continue
						else:
							self.meanlist21.append( round( float(row[paramHeader]), 2) )
							continue
					else:
						continue

			except RuntimeWarning as warning:
				print('Runtime warning! :', warning)

		print('All averages written to ', outputfile)
		self.meanlist00.clear()
		self.meanlist03.clear()
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


### Température diurne ( de 6 à 21h )
MFDayMeaner.getDayMean( paramHeader='temperature', offset=0, offsetStatus=False, absoluTemp=True, outputfile = outputfolder+'tempnightDB.csv' )

### Température diurne ( de 6 à 21h ) + offset
MFDayMeaner.getDayMean( paramHeader='temperature', offset=2, offsetStatus=True, absoluTemp=True, outputfile = outputfolder+'tempnightOffset2DB.csv' )

