/**********************************    Traitement météo -15 j  humains  *****************************************

-- Humains
select
	id,
	lat,
	lon,
	date_piqure_saisie,
	annee_extract
From citik.citik_humains_clean
where date_piqure_saisie between '01/01/2017' and '05/04/2020'
order by date_piqure_saisie
;



-- Série temporlle des extractions
SELECT
jm0.id,
jm0.date_piqure_saisie,
jm0.humidity as humidite_jm0,
jm1.humidity as humidite_jm1,
jm2.humidity as humidite_jm2,
jm3.humidity as humidite_jm3,
jm4.humidity as humidite_jm4,
jm5.humidity as humidite_jm5,
jm6.humidity as humidite_jm6,
jm7.humidity as humidite_jm7,
jm8.humidity as humidite_jm8,
jm9.humidity as humidite_jm9,
jm10.humidity as humidite_jm10,
jm11.humidity as humidite_jm11,
jm12.humidity as humidite_jm12,
jm13.humidity as humidite_jm13,
jm14.humidity as humidite_jm14,
jm15.humidity as humidite_jm15

from citik.citik_humains_clean_weather_strict_raw as jm0

left	join meteo.citik_humains_clean_weather_strict_raw_jm1 as jm1
			on jm0.id = jm1.id
left	join meteo.citik_humains_clean_weather_strict_raw_jm2 as jm2
			on jm1.id = jm2.id
left	join meteo.citik_humains_clean_weather_strict_raw_jm3 as jm3
			on jm1.id = jm3.id
left	join meteo.citik_humains_clean_weather_strict_raw_jm4 as jm4
			on jm1.id = jm4.id
left	join meteo.citik_humains_clean_weather_strict_raw_jm5 as jm5
			on jm1.id = jm5.id
left	join meteo.citik_humains_clean_weather_strict_raw_jm6 as jm6
			on jm1.id = jm6.id
left	join meteo.citik_humains_clean_weather_strict_raw_jm7 as jm7
			on jm1.id = jm7.id
left	join meteo.citik_humains_clean_weather_strict_raw_jm8 as jm8
			on jm1.id = jm8.id
left	join meteo.citik_humains_clean_weather_strict_raw_jm9 as jm9
			on jm1.id = jm9.id
left	join meteo.citik_humains_clean_weather_strict_raw_jm10 as jm10
			on jm1.id = jm10.id
left	join meteo.citik_humains_clean_weather_strict_raw_jm11 as jm11
			on jm1.id = jm11.id
left	join meteo.citik_humains_clean_weather_strict_raw_jm12 as jm12
			on jm1.id = jm12.id
left	join meteo.citik_humains_clean_weather_strict_raw_jm13 as jm13
			on jm1.id = jm13.id
left	join meteo.citik_humains_clean_weather_strict_raw_jm14 as jm14
			on jm1.id = jm14.id
left	join meteo.citik_humains_clean_weather_strict_raw_jm15 as jm15
			on jm1.id = jm15.id
;



-- Définition des types de données des colonnes 

alter table meteo.citik_animaux_clean_weather_strict_raw_jm15

alter column date_piqure_saisie_jm15 set data type date using date_piqure_saisie_jm15::date,
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

*/

--select round(cloudcover::numeric*100, 2) from meteo.citik_humains_clean_weather_strict_raw_jm9

-- create table meteo.citik_animaux_clean_weather_strict_raw_jm1_bkp as (

-- 	select * from meteo.citik_animaux_clean_weather_strict_raw_jm1_bkp
-- )

update meteo.citik_humains_clean_weather_strict_raw_jm15
set
humidity = round(humidity::numeric*100, 2),
cloudcover = round(cloudcover::numeric*100, 2)


select * from meteo.citik_animaux_clean_weather_strict_raw_jm5
;
