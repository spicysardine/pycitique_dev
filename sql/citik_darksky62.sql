----------------------------   Téléchargement des stations  -------------------------
/*
create table meteo.liste_stations_synop_metropole as (
	
	select * from meteo.liste_stations_synop
	where id !~ '^6|^7|^8'
	
);


select * from meteo.liste_stations_synop
;

alter table meteo.testrev
alter column altitude set data type integer using altitude::int,
alter column latitude  set data type float using latitude::float,
alter column longitude set data type float using longitude::float,
alter column geom set data type geometry using geom::geometry
;

select * from meteo.liste_stations_synop_metropole
order by id
;
update meteo.liste_stations_synop_metropole
set ville = 'Saint-Laurent-du-Var'
where id = '07690'

select distinct
pg_typeof(id),
pg_typeof(nom),
pg_typeof(latitude),
pg_typeof(longitude),
pg_typeof(altitude),
pg_typeof(geom)

from meteo.liste_stations_synop_metropole
;

select column_name, data_type from information_schema.columns
where table_name = 'liste_stations_synop_metropole';

-- delete from meteo.darksky_synop42_extraction
-- where date_releve = '2017-07-18'
-- ;

drop view if exists meteo.synop_dates ;
create view meteo.synop_dates as (

select distinct substring(date, 0, 9) as date from meteo.synop_meteo_france_select

);

drop view if exists meteo.synop_dates_detail ;
create view meteo.synop_dates_detail as (
	
select
row_number() over() as id,
(substring(date, 7 , 2)||'-'||substring(date, 5 , 2)||'-'||substring(date, 0 , 5))::date as date,
substring(date, 0 , 5) as year,
substring(date, 5 , 2) as month,
substring(date, 7 , 2) as day
	
from meteo.synop_dates
order by (substring(date, 7 , 2)||'-'||substring(date, 5 , 2)||'-'||substring(date, 0 , 5))::date
);

create table meteo.synop_dates_dsk as (
	select * from meteo.synop_dates_detail
);

select * from meteo.liste_stations_synop_metropole
--order by altitude
order by id::int
;

select * from meteo.liste_stations_500
--order by altitude
order by id::int
;

select cast(date as text) as date_releve from meteo.synop_dates_dsk
--where date between '31/03/2017' and '31/03/2019'
where date > '2017-07-17'

SELECT cast(date as text) as date_releve FROM meteo.synop_dates_dsk limit 1;

select * from meteo.darksky_maille_500_extraction_test

-- Définition des types de données des colonnes pour la table darksky à 42 station en métropole

alter table meteo.darksky_synop42_extraction

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




select percentage(count(id_station), 50022) from meteo.darksky_synop42_extraction
;


select * from meteo.darksky_synop42_extraction
order by date_releve desc
limit 100
;

select avg(nullif(pressure, '')::float) from meteo.darksky_synop42_extraction
where nom_station ~* 'abbeville'
;





---- darksky 42 stations weather averages table ----
drop table if exists meteo.darksky_synop42_avg;
create table meteo.darksky_synop42_avg as (

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

from meteo.darksky_synop42_extraction as ex42
group by date_releve
order by date_releve asc
	
);

select
*
--date_releve 
from meteo.darksky_maille_700_extraction
order by date_releve desc
limit 100
;

*/

-- début de l'extraction de la maille 700 mercredi 06 mai 2020 à 01h07 du matin
select round(percentage(count(id_station), 864666)::numeric, 2) || ' %' as extraction_progress from meteo.darksky_maille_700_extraction
;








