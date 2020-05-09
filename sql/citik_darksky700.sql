/*  Script SQL de préparation de la donnée comparative darksky sur une maille de 726 points répartis sur la métropole + Corse*/

-- début de l'extraction de la maille 700 mercredi 06 mai 2020 à 01h07 du matin par le script python pycitique
SELECT
round(percentage(count(id_station), 830538)::numeric, 2) || ' %' as extraction_progress,
count(id_station) as count
FROM meteo.darksky_maille_700_extraction
;
-- fin de l'extraction samedi 09 mai 2020 à 17h50
-- There was 34128 missing dataPoint objects during the extraction process.
-- 830538 Objects Written to the table darksky_maille_700_extraction 
-- la taille théorique de la table est de 864666 enregistrements

-- Définition des types de données des colonnes pour la table darksky prevenant de la maille à 726 points
alter table meteo.darksky_maille_700_extraction

alter column date_releve set data type date using date_releve::date,
alter column lat set data type float using lat::float,
alter column lon set data type float using lon::float,
alter column time set data type timestamp using nullif(time, '')::timestamp,
alter column sunsettime set data type timestamp using nullif(sunsettime, '')::timestamp,
alter column sunrisetime set data type timestamp using nullif(sunrisetime, '')::timestamp,
alter column temperaturehightime set data type timestamp using nullif(temperaturehightime, '')::timestamp,
alter column temperaturelowtime set data type timestamp using nullif(temperaturelowtime, '')::timestamp,
alter column windgusttime set data type timestamp using nullif(windgusttime, '')::timestamp,
alter column uvindextime set data type timestamp using nullif(uvindextime, '')::timestamp,
alter column temperaturemintime set data type timestamp using nullif(temperaturemintime, '')::timestamp,
alter column temperaturemaxtime set data type timestamp using nullif(temperaturemaxtime, '')::timestamp,
alter column apparenttemperaturemintime set data type timestamp using nullif(apparenttemperaturemintime, '')::timestamp,
alter column apparenttemperaturemaxtime set data type timestamp using nullif(apparenttemperaturemaxtime, '')::timestamp,
alter column apparenttemperaturehightime set data type timestamp using nullif(apparenttemperaturehightime, '')::timestamp,
alter column apparenttemperaturelowtime set data type timestamp using nullif(apparenttemperaturelowtime, '')::timestamp,
alter column precipIntensity set data type float using nullif( precipIntensity,'')::float,
alter column precipIntensityMax	set data type float using nullif( precipIntensityMax,'')::float,
alter column precipAccumulation	set data type float using nullif( precipAccumulation,'')::float,
alter column precipProbability	set data type float using nullif( precipProbability,'')::float,
alter column temperatureHigh	set data type float using nullif( temperatureHigh,'')::float,
alter column temperatureLow	set data type float using nullif(temperatureLow ,'')::float,
alter column dewPoint	set data type float using nullif(dewPoint ,'')::float,
alter column humidity	set data type float using nullif( humidity,'')::float,
alter column pressure	set data type float using nullif(pressure ,'')::float,
alter column visibility	set data type float using nullif( visibility,'')::float,
alter column windGust	set data type float using nullif( windGust,'')::float,
alter column windSpeed	set data type float using nullif( windSpeed,'')::float,
alter column windBearing	set data type float using nullif( windBearing,'')::float,
alter column cloudCover	set data type float using nullif( cloudCover,'')::float,
alter column cloudCoverError	set data type float using nullif( cloudCoverError,'')::float,
alter column uvIndex	set data type float using nullif( uvIndex,'')::float,
alter column temperatureMin	set data type float using nullif(temperatureMin ,'')::float,
alter column temperatureMax	set data type float using nullif( temperatureMax,'')::float,
alter column apparentTemperatureMin set data type float using nullif( apparentTemperatureMin,'')::float,
alter column apparentTemperatureMax	set data type float using nullif( apparentTemperatureMax,'')::float,
alter column apparentTemperatureHigh	set data type float using nullif( apparentTemperatureHigh,'')::float,
alter column apparentTemperatureLow	set data type float using nullif(apparentTemperatureLow,'')::float
;


---- darksky maille à 726 points table des moyennes ----
--drop table if exists meteo.darksky_maille_700_avg;
create table meteo.darksky_maille_700_avg as (

	select
		row_number() over() as id,
		date_releve,
		round( cast(avg(humidity) as numeric) ,2) as 		humidity,
		round( cast(avg(dewpoint) as numeric) ,2) as 		dewpoint,
		round( cast(avg(pressure) as numeric) ,2) as 		pressure,
		round( cast(avg(windspeed) as numeric) ,2) as 		windspeed,
		round( cast(avg(visibility) as numeric) ,2) as 		visibility,
		round( cast(avg(cloudcover) as numeric) ,2) as 		cloudcover,
		round( cast(avg(windgust) as numeric) ,2) as 		windgust,
		round( cast(avg(uvindex) as numeric) ,2) as 		uvindex,
		round( cast(avg(precipintensity) as numeric) ,2) as precipintensity,
		round( cast(avg(precipintensitymax) as numeric) ,2) as precipintensitymax,
		round( cast(avg(temperaturehigh) as numeric) ,2) as temperaturehigh,
		round( cast(avg(temperaturelow) as numeric) ,2)  as temperaturelow,
		round( cast(avg(temperaturehigh+2) as numeric) ,2) as temperaturehighoffset2,
		round( cast(avg(temperaturelow+2) as numeric) ,2)  as temperaturelowoffset2

	FROM meteo.darksky_maille_700_extraction as ex42
	group by date_releve
	order by date_releve asc
	
);


-- vérification de la dernière journée extraite.
select
* 
from meteo.darksky_maille_700_extraction
order by date_releve desc
limit 726
;

