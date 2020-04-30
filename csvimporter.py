#coding:utf-8

### csvimporter.py ################################################
#								  #
#								  #
#Author: Hilami Khaldoune					  #
#Organism: CNRS							  #
#Year: 2020							  #
#License: This script is propriety of CNRS			  #
#Descriptions: * This module takes a folder in the system and	  # 
#looks for csv files. If it finds none, it exits. If it finds any #
#it imports them to the specified schema into the database	  #
#								  #
###################################################################

### library IMPORTS csv inserter ###
from psycopg2		import connect, Error
from psycopg2.extras 	import DictCursor
from psycopg2		import errors as PgError
###library IMPORTS pyreverser ###
from geocoder	import osm
### library IMPORTS darkskyextractorCSV ###
from darksky	import forecast
from datetime	import datetime as dt
### other library IMPORTS ###
from re		import search
from csv		import reader, writer, QUOTE_ALL, DictReader, DictWriter
from itertools	import islice
from os		import chdir, getcwd, listdir
from collections	import OrderedDict
chdir("/home/beetroot/Developer/python/CNRS/projetNominatim/")


class Csvimporter:

	def __init__(self, inputdir, outputdir, outputschema, delimiter, quotechar, server):

		chdir(self.inputdir)
		self.conn = connect(host='localhost', database='localbase10', user='beetroot', password='root', port=5432)
		self.inputdir=inputdir
		self.outputdir=outputdir
		self.outputschema=outputschema
		self.delimiter=delimiter # must be the same as the file delimiter
		self.quotechar=quotechar
		self.server=server #'http://localhost/nominatim/'
		self.count = 0
		self.APIkey= '865f840cdde4ab359bce9a5adee70f84'
		self.rglist = ['village','ville','chef_lieu', 'departement', 'departement_code','region','code_postal','pays','code_pays','adresse_complette']
		self.wobjlist = ['timezone','time','summary','icon','sunriseTime','sunsetTime','moonPhase',
					'precipIntensity','precipIntensityMax','precipIntensityMaxTime','precipAccumulation',
						'precipProbability','temperatureHigh','temperatureHighTime','temperatureLow','temperatureLowTime',
							'dewPoint','humidity','pressure','visibility','windGust','windGustTime','windSpeed','windBearing',
								'cloudCover','cloudCoverError','uvIndex','uvIndexTime','temperatureMin','temperatureMinTime','temperatureMax',
									'temperatureMaxTime','apparentTemperatureMin','apparentTemperatureMinTime','apparentTemperatureMax','apparentTemperatureMaxTime',
										'apparentTemperatureHigh','apparentTemperatureHighTime','apparentTemperatureLow','apparentTemperatureLowTime','DataSource']


	def csvinserter(self, inputfile):

		try:

			curs = self.conn.cursor()

			with open(self.inputdir+inputfile+'.csv', 'r') as DataInputFile:
				filestream = reader(DataInputFile, delimiter=self.delimiter , quotechar=self.quotechar)
				createschemasql = "CREATE SCHEMA IF NOT EXISTS {}".format(self.outputschema)
				curs.execute(createschemasql)
				droptablesql = "DROP TABLE IF EXISTS {}.{} ;".format(self.outputschema, inputfile)
				curs.execute(droptablesql)
				for line in islice(filestream,0,1):
					headerlist=line
				sqlformat = ''
				for col in headerlist:
					sqlformat += col+' varchar,'
				sqlformat = sqlformat[:-1]
				createtablesql = "CREATE TABLE {}.{} ({});".format( self.outputschema, inputfile, sqlformat )
				curs.execute(createtablesql)
				self.conn.commit()
				loop = 0
				for row in filestream:
					values = ''
					for field in row:
						values += "'"+field.replace("'", "’")+"',"
					values = values[:-1]
					insertintotablesql = 'INSERT INTO {}.{} VALUES ({}) ;'.format(self.outputschema, inputfile, values)
					loop +=1
					curs.execute( insertintotablesql )
					self.conn.commit()
					del values
				print("The Table "+inputfile+" is written to the database with ",loop," rows")
				self.count += loop

			curs.close()

		except (Exception, Error) as error:
			print ("Error while connecting to PostgreSQL", error)

	def tableinsert(self):

		inputfilelist=listdir(self.inputdir)
		for inputfile in inputfilelist:
			if search(r'\.[cC][sS][vV]$', inputfile) is not None :
				inputfile=inputfile[0:-4]
				self.csvinserter(inputfile)
		print("\nAll tables written successfully to database. Operation completed.")
		print("\nA total of ", self.count," rows is inserted to database.")
		self.conn.close()
		print("\nPostgreSQL connection is closed.\n")


	def reversegeocoderCSV(self, inputfile):

		with open(self.inputdir+inputfile+'.csv', 'r') as DataInputFile:

			filestream = reader(DataInputFile, delimiter=self.delimiter , quotechar=self.quotechar )
			# Selecting only the first line of the file, usualy containing the deader
			# Converting the first line to a python list, in preparation of the iteration
			for line in islice(filestream,0,1):
				headerlist=line
			# Appending reverse geocoding fields to header list
			headerlist+=self.rglist
			with open(self.outputdir+inputfile+'_revgeocoded.csv', 'a') as DataOutputFile:
				streamwriter = writer( DataOutputFile, delimiter = self.delimiter, quotechar = "'", quoting=QUOTE_ALL   )
				# Header writing to the file
				streamwriter.writerow(headerlist)
				print("File headers writing is complete. Beginning  data extraction precess...\n")
				print("...\n")
				missingObjectCount=0
				loop=0
				row=''
				for field in filestream:
					loop+=1
					lat = float(field[1])
					lng = float(field[2])
					location = osm( [ lat , lng ], method='reverse', url=self.server)
					if location.address is not None: location.address=location.address.replace( ',' ,';')
					if location.address is not None: location.address=location.address.replace( "'" ,'’')
					if location.state   is not None: location.state = location.state.replace( "'" ,'’')
					if location.postal  is not None  and len(location.postal) < 5: location.postal = '0'+location.postal
					if location.address is not None and location.postal is not None:
						if   search(r';\sLyon;', location.address)    is not None: department_code = '69M'
						elif search('Corse-du-Sud', location.address) is not None: department_code = '2A'
						elif search('Haute-Corse' , location.address) is not None: department_code = '2B'
						elif search('Villefranche-sur-Saône', location.address) is not None: department_code = '69D'
						else: department_code = location.postal[0:-3]
					else: department_code = ''
					department_geocoded = osm( department_code , url=self.server )
					department = department_geocoded.county
					if department_code  == '14': department = 'Calvados'
					if department_code  == '27': department = 'Eure'
					if department is not None: department = department.replace( "'" ,'’')
					
					field+=[location.village, location.town, location.county, department, department_code, location.state, 
													location.postal, location.country, location.country_code, location.address]
					field = [str(i or '') for i in field]
					try:
						streamwriter.writerow(field) 
						print('Object '+str(loop)+' Written\n')
					except ValueError:
						print('Could not convert string to float!')
						missingObjectCount+=1
						print('The program will continue...')
			self.count += loop

		if (missingObjectCount>0):
			print('There was', missingObjectCount, 'missing dataPoint objects during the extraction process.\n')
		else:
			print('Congratulations! Extraction finished without missing dataPoint objects! You may check your file.\n')
			print(str(loop)+' Objects Written to the last file\n')

	def revgfilemaker(self):

		inputfilelist=listdir(self.inputdir)
		for inputfile in inputfilelist:
			if search(r'\.[cC][sS][vV]$', inputfile) is not None :
				inputfile=inputfile[0:-4]
				self.reversegeocoderCSV(inputfile)
		print("\nA total of ", self.count," rows is written\n.")
		del self.count


	def darkskyextractorCSV(self, inputfile):

		missingObjectCount=0
		loop=0
		headerlist = []
		valdict = {}

		with open(self.inputdir+inputfile+'.csv', 'r') as weatherDataInputFile:

			filestream = DictReader(weatherDataInputFile, delimiter=self.delimiter, quotechar = self.quotechar)
			# Header Construction
			for header in islice(filestream,0,1):
				for fieldname in header.keys():
					headerlist.append(fieldname)

			headerlist += self.wobjlist

			print("File headers construction is complete...\n")
			print("...\n")

			with open(self.outputdir+inputfile+'_weather.csv', 'a') as weatherDataOutputFile:

				streamwriter = DictWriter( weatherDataOutputFile, fieldnames = headerlist, delimiter = self.delimiter, quotechar = "'", quoting=QUOTE_ALL)
				# Header writing to the file
				streamwriter.writeheader()
				print("File headers writing is complete. Beginning  data extraction precess...\n")
				print("...\n")
				# Main extraction loop
				for row in filestream:
					try:
						date_piqure = row['date_piqure_saisie']

						if search(r'^\s*(0?[1-9]|1[0-9]|2[0-9]|3[0-1])?\s*/\s*(0?[1-9]|1[0-2])?\s*/\s*(19|20)[0-9][0-9]\s*$', date_piqure) :
							date = date_piqure
							datelist = date.split('/')
							timep = dt(int(datelist[2]), int(datelist[1]), int(datelist[0]) ).isoformat()
							lat = float(row['lat'])
							lon = float(row['lon'])

							location = forecast(self.APIkey, lat, lon, lang='en', units='si', time=timep, exclude='currently,minutely,hourly,alerts' )

							data = location['daily']
							datalist = data.get('data', None)
							weatherdict = datalist[0]
							for key, value in weatherdict.items():
								if search(r'[tT]ime$' , key ) is not None:
									weatherdict[key] = dt.utcfromtimestamp(value).strftime('%d-%m-%y %H:%M:%S')
								else: weatherdict[key] = value
							for wobj in self.wobjlist:
								valdict[wobj] = weatherdict.get(wobj, '')
							valdict['timezone']   = location.timezone
							valdict['DataSource'] = location.flags.sources
							for key, value in valdict.items():
								row[key] = value
							streamwriter.writerow(row)
							loop += 1
							print('Object ', loop,' Written\n')
							del date
							del date_piqure
						else:
							print('Date format is illegal...row will be skipped')
							missingObjectCount+=1
							pass
					except (Exception, AttributeError, ValueError, KeyError, TypeError, IndexError ) as error:
						missingObjectCount+=1
						print('The object is missing!', error)
						print('Weather Object is missing...row will be skipped')
						print('Extraction will continue...')

		# Input file is closed at this point

		if (missingObjectCount>0):
			print("There was", missingObjectCount, "missing dataPoint objects during the extraction process.")
		else:
			print("Congratulations! Extraction finished without missing dataPoint objects! You may check your file.")

	def darkskyfilemaker(self):

		inputfilelist=listdir(self.inputdir)
		for inputfile in inputfilelist:
			if search(r'\.[cC][sS][vV]$', inputfile) is not None :
				inputfile=inputfile[0:-4]
				self.darkskyextractorCSV(inputfile)
		print("\nA total of ", self.count," rows is written\n.")

	def reversegeocoderDB(self, inputable):

		missingObjectCount=0
		loop=0
		headerlist=self.rglist

		print('Connection to database')
		curs = self.conn.cursor()
		print('Creating DictCursor')
		inputsql = "select id, lat, lon, geom from gis.france_maille_500;" #.format(inputfields, inputschema, inputable, orderfield)
		curs.execute(inputsql)
		print('Executing query...Data is being fetched...')
		revdata = curs.fetchall()
		print('Data dictionary is fetched.')
		print('File creation is beginning.')

		droptablesql = "DROP TABLE IF EXISTS {}.{} ;".format(self.outputschema, inputable)
		curs.execute(droptablesql)
		fieldnames = 'id integer, lat float, lon float, position geometry, '
		for field in headerlist:
			fieldnames += field+' varchar,'
		fieldnames = fieldnames[:-1]
		createtablesql = "CREATE TABLE {}.{} ({});".format( self.outputschema, inputable, fieldnames )
		curs.execute(createtablesql)
		self.conn.commit()

		for row in revdata:

			try:

				loop+=1
				lat = row[1]
				lon = row[2]

				location = osm( [ lat , lon ], method='reverse', url=self.server)

				if location.address is not None and location.postal is not None:
					if   search(r',\sLyon,', location.address)    is not None: department_code = '69M'
					elif search('Corse-du-Sud', location.address) is not None: department_code = '2A'
					elif search('Haute-Corse' , location.address) is not None: department_code = '2B'
					elif search('Villefranche-sur-Saône', location.address) is not None: department_code = '69D'
					else: department_code = location.postal[0:-3]
				else: department_code = ''
				department_geocoded = osm( department_code , url=self.server )
				department = department_geocoded.county

				row += (location.village, location.town, location.county, department, department_code, location.state, 
												location.postal, location.country, location.country_code, location.address)
				row = (str(i or '') for i in row)
				values= ''
				for field in row :
					values += "'"+field.replace("'", "’")+"',"
				values = values[:-1]
				insertintotablesql = 'INSERT INTO {}.{} VALUES ({}) ;'.format(self.outputschema, inputable, values)
				curs.execute( insertintotablesql )
				self.conn.commit()
				del values

				print('Row '+str(loop)+' Written\n')

			except ValueError:
				print('Could not convert string to float!')
				missingObjectCount+=1
				print('The program will continue...')

		if (missingObjectCount>0):
			print('There was', missingObjectCount, 'missing dataPoint objects during the extraction process.\n')
		else:
			print('Congratulations! Reverse-Geocoding finished without missing any dataPoint objects! You may check your table.\n')
			print(str(loop)+' Objects Written to the table\n')
		curs.close()
		self.conn.close()


	def darkskyextractorDB(self, inputable):


		missingObjectCount=0
		loop=0
		valdict = OrderedDict()

		print('Connection to database')
		curs = self.conn.cursor(cursor_factory = DictCursor)
		print('Creating DictCursor')
		inputsql = "select * from citik.citik_animaux_clean ;" #.format(inputfields, inputschema, inputable, orderfield)
		curs.execute(inputsql)
		print('Executing query...Data is being fetched...')
		wdata = curs.fetchall()
		print('Data dictionary is fetched.')
		print('File creation is beginning.')

		createschemasql = "CREATE SCHEMA IF NOT EXISTS {}".format(self.outputschema)
		curs.execute(createschemasql)

		droptablesql = "DROP TABLE IF EXISTS {}.{} ;".format(self.outputschema, inputable)
		curs.execute(droptablesql)
		fieldnames = ''
		for row in islice(wdata,0,1):
			for key, values in row.items():
				fieldnames += key+' varchar,'
#		fieldnames = 'id integer, lat float, lon float, date_piqure_saisie date, '
		for field in self.wobjlist:
			fieldnames += field+' varchar,'
		fieldnames = fieldnames[:-1]

		createtablesql = "CREATE TABLE {}.{} ({});".format( self.outputschema, inputable, fieldnames )
		curs.execute(createtablesql)

		self.conn.commit()
		print("Table creation is complete. Beginning  data extraction precess...\n")
		print("...\n")

		for row in wdata:

			try:

				date_piqure = row['date_piqure_saisie']

				if search(r'^\s*(0?[1-9]|1[0-9]|2[0-9]|3[0-1])?\s*/\s*(0?[1-9]|1[0-2])?\s*/\s*(19|20)[0-9][0-9]\s*$', date_piqure) :

					date = date_piqure
					datelist = date.split('/')
					year = datelist[2]; month = datelist[1] ; day = datelist[0]
					timep = dt(int(year), int(month), int(day) ).isoformat()
					lat = float(row['lat'])
					lon = float(row['lon'])

					location = forecast(self.APIkey, lat, lon, lang='en', units='si', time=timep, exclude='currently,minutely,hourly,alerts' )

					data = location['daily']
					datalist = data.get('data', None)
					weatherdict = datalist[0]

					for key, value in weatherdict.items():
						if search(r'[tT]ime$' , key ) is not None:
							weatherdict[key] = dt.utcfromtimestamp(value).strftime('%d-%m-%y %H:%M:%S')
						else: weatherdict[key] = value
					for wobj in self.wobjlist:
						valdict[wobj] = weatherdict.get(wobj, '')
					valdict['timezone']   = location.timezone
					valdict['DataSource'] = location.flags.sources
					row = OrderedDict(row)
					row.update(valdict)
					values= ''
					for key, value in row.items() :
						value = str(value)
						values += "'"+value.replace("'", "’")+"',"
					values = values[:-1]
					insertintotablesql = 'INSERT INTO {}.{} VALUES ({}) ;'.format(self.outputschema, inputable, values)
					curs.execute( insertintotablesql )
					self.conn.commit()
					loop += 1
					print('Object ', loop,' Written\n')
					del date
					del date_piqure
					del values

				else:
					print('Date format is illegal...row will be skipped')
					missingObjectCount+=1
					pass

			except (Exception, AttributeError, ValueError, KeyError, TypeError, IndexError, PgError.DatetimeFieldOverflow) as error:
				missingObjectCount+=1
				print('\nThere was an error!', error)
				print('Weather Object is missing... the row will be skipped but extraction will continue...\n')

		# Input file is closed at this point

		if (missingObjectCount>0):
			print("There was", missingObjectCount, "missing dataPoint objects during the extraction process.")
			print(str(loop)+' Objects Written to the table', inputable,'\n')
		else:
			print("Congratulations! Extraction finished without missing dataPoint objects! You may check your table.")
			print(str(loop)+' Objects Written to the table', inputable,'\n')
		curs.close()
		self.conn.close()



######################################################## EOC #################################################################

#inputdir='/home/beetroot/Developer/python/CNRS/projetNominatim/inputest/'
#outputdir='/home/beetroot/Developer/python/CNRS/projetNominatim/outputest/'
#server='http://localhost/nominatim/'
#delimiter=',' # must be the same as the file delimiter
#quotechar='"' # must be the same as the file quote character

#reverser=Csvimporter(inputdir, outputdir, outputschema, delimiter, quotechar, server)

############################################## DB darkskyextraction  ####################################################
#inputdir = '.'
#outputdir = '.'
#outputschema = 'citik'
#delimiter=';' 
#quotechar="'"
#server='http://localhost/nominatim/'
#reverser=Csvimporter(inputdir, outputdir, outputschema, delimiter, quotechar, server)
##reverser.reversegeocoderDB('dsktabletest')
#reverser.darkskyextractorDB('citik_animaux_clean_weather_v2')

############################################# reverse geocoding ##############################################################
#reverser.revgfilemaker()
#reverser.reversegeocoderCSV('citik_onf_2020')

############################################## weather extraction ############################################################
#del reverser
#quotechar="'" # must be the same as the file quote characte
#reverser=Csvimporter(outputdir, outputdir, outputschema, delimiter, quotechar, server)
#reverser.darkskyfilemaker()
#reverser.darkskyextractorCSV('citik_onf_2020_revgeocoded')

############################################## DB insertion ##################################################################
#inputdir = '/home/beetroot/Developer/python/CNRS/projetNominatim/outputest/'
#outputdir = ''
#outputschema = 'citik'
#delimiter=';' 
#quotechar="'"
#reverser=Csvimporter(inputdir, outputdir, outputschema, delimiter, quotechar, server)
#reverser.tableinsert()
#reverser.csvinserter('citik_animaux_clean_weather')
#reverser.csvinserter('citik_onf_2020_revgeocoded_weather')

############################################## DB insertion meteteo france ####################################################
#inputdir = '/home/beetroot/Developer/python/CNRS/meteoFrance/MFoutput/MFinput/'
#outputdir = ''
#outputschema = 'meteo'
#delimiter=';' 
#quotechar="'"
#reverser=Csvimporter(inputdir, outputdir, outputschema, delimiter, quotechar, server)
#reverser.csvinserter('synop_meteo_france')

############################################## DB insertion meteteo france  stations ####################################################
#inputdir = '/home/beetroot/Developer/python/CNRS/meteoFrance/MFoutput/MFinput/'
#outputdir = ''
#outputschema = 'meteo'
#delimiter=';' 
#quotechar="'"
#reverser=Csvimporter(inputdir, outputdir, outputschema, delimiter, quotechar, server)
#reverser.tableinsert()
#reverser.csvinserter('liste_stations_mf_synop')






