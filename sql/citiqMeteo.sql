/*********************************************  Citik script de preparatoin de la donnée météo  *******************************************************/

-- ALTER TABLE france_maille_hex01
--   ALTER COLUMN geom TYPE geometry(MULTIPOLYGON, 4326)
--     USING ST_transform(geom,4326);

--SELECT UpdateGeometrySRID('gis','bufferhex','geom',4326);

-- select column_name from information_schema.columns where table_schema = 'meteo' and table_name = 'synop_meteo_france';

-- creation de la table des station avec la postition géographique
create table if not exists meteo.liste_stations_mf_synop_geom as (
SELECT

id,
nom,
latitude,
longitude,
altitude,
st_setsrid( st_makepoint(longitude::float, latitude::float), 4326) as geom
from meteo.liste_stations_mf_synop

);


/*-**********************    Traitement de la donnée météo France       **************************-*/

-- select distinct numer_sta from meteo.synop_meteo_france order by numer_sta asc;
select * from meteo.synop_meteo_france order by date asc;

SELECT numer_sta,
date,
(coalesce(nullif(t, '')::float, 0.0) - 273.15)  as t ,
u 
from meteo.synop_meteo_france
--where t is not null
--where date ~ '20170129150000'
--where date ~ '20171113'
order by date
--limit 30000;
--where numer_sta = '07015'
;


--- creation de la table météo avec les paramètres sélectionnés

--DROP TABLE IF EXISTS meteo.synop_meteo_france_select ;

CREATE TABLE IF NOT EXISTS meteo.synop_meteo_france_select as (

	SELECT
		numer_sta as id_station,
		date,
		t as temperature,
		u as humidite,
		tn12 as temp_min_12,
		tn24 as temp_min_24,
		tx12 as temp_max_12,
		tx24 as temp_max_24,
		rr1 as precip_01h,
		rr3 as precip_03h,
		rr6 as precip_06h,
		rr12 as precip_12h,
		rr24 as precip_24h,
		ht_neige as haut_neige,
		tend24 as var_pression_24,
		tend as var_pression_03,
		n as nebulosite,
		pres as press_sta,
		pmer as press_mer,
		td as point_rose,
		raf10 as rafale_10min,
		ff as vvent,
		vv as visibilite
		from meteo.synop_meteo_france
		order by date asc
	
);

select * from meteo.synop_meteo_france_select limit 20;

select distinct date from meteo.synop_meteo_france_select order by date asc;

select substring('20170101210000', 9, 2)

-- températures diurnes 
SELECT
id_station,
date,
temperature as temp_day

FROM meteo.synop_meteo_france_select as mfs
where substring(mfs.date, 9, 2) ~ '(06|09|12|15|18)'
order by date asc
;

-- températures nocturnes 
SELECT
id_station,
date,
temperature as temp_night

from meteo.synop_meteo_france_select as mfs
where substring(mfs.date, 9, 2) !~ '(06|09|12|15|18)'
order by date asc
;

-----------------------------------------------------------------

SELECT * FROM meteo.synop_meteo_france as mf order by mf.date asc;
-----------------------------------------------------------------
show datestyle;
show timezone;
set datestyle to 'DMY';


SELECT temperaturelow, temperaturelowtime from citik.citik_humains_clean_weather;

SELECT 
temperaturelow,
nullif(temperaturelowtime, '')::timestamp as templowtime
from citik.citik_humains_clean_weather
where nullif(temperaturelowtime, '')::timestamp between '01-01-2017 00:00:00'::timestamp and '12-04-2020 00:00:00'::timestamp
order by nullif(temperaturelowtime, '')::timestamp desc;

SELECT
min( nullif(temperaturelowtime, '')::timestamp ) mintime,
max( nullif(temperaturelowtime, '')::timestamp ) maxtime
from citik.citik_humains_clean_weather
where nullif(temperaturelowtime, '')::timestamp between '01-01-2017 00:00:00'::timestamp and '01-04-2020 00:00:00'::timestamp
;

-- sunrise and sunset times

-- sunrise
SELECT 
nullif(sunrisetime, '')::timestamp as sunrisetime
from citik.citik_humains_clean_weather as cw
where nullif(sunrisetime, '')::timestamp between '01-01-2017 00:00:00'::timestamp and '12-04-2020 00:00:00'::timestamp
order by nullif(sunrisetime, '')::timestamp desc
;

-- sunset
SELECT
cw.date_piqure_saisie,
nullif(sunsettime, '')::timestamp as sunsettime
from citik.citik_humains_clean_weather as cw
where nullif(sunsettime, '')::timestamp between '01-01-2017 00:00:00'::timestamp and '12-04-2020 00:00:00'::timestamp
and date_piqure_saisie ~ '^\s*(0?[1-9]|1[0-9]|2[0-9]|3[0-1])?\s*[/]\s*(0?[1-9]|1[0-2])?\s*[/]\s*(19|20)[0-9][0-9]\s*$'
order by nullif(date_piqure_saisie, '')::date desc
;


-----------------------------

SELECT
--id,
--nullif(date_piqure_saisie, '')::date as date_piqure_saisie,
max(nullif(temperaturemin,'')) as max_min,
max(nullif(temperaturemax,'')) as max_max,
min(nullif(temperaturemin,'')) as min_min,
min(nullif(temperaturemax,'')) as min_max
FROM
citik.citik_humains_clean_weather
where date_piqure_saisie ~ '^\s*(0?[1-9]|1[0-9]|2[0-9]|3[0-1])?\s*[/]\s*(0?[1-9]|1[0-2])?\s*[/]\s*(19|20)[0-9][0-9]\s*$'
and nullif(date_piqure_saisie, '')::date between '01/01/2017'::date and '05/04/2020'::date
--order by nullif(date_piqure_saisie, '')::date
;

---------------------- Taitement Météo France -----------------------
select distinct date_piqure_saisie from citik.citik_animaux_clean_weather_strict order by date_piqure_saisie --desc;

select distinct date_piqure_saisie from citik.citik_humains_clean_weather_strict order by date_piqure_saisie --desc;

select * from meteo.humidityoffse2tdb as tmp
order by tmp.date --desc
;


select * from meteo.liste_stations_synop
order by id::int desc
;

-- DELETE FROM meteo.synop_meteo_france_select
-- where id_station ~ '^6|^7|^8'
-- ;

SELECT COUNT(*) - 154336 FROM meteo.synop_meteo_france_select;


SELECT id_station, date, temperature, temp_min_24, temp_min_12 FROM meteo.synop_meteo_france_select
where temp_min_12 !~* '^$'

SELECT id_station, date, temperature, temp_max_24, temp_max_12 FROM meteo.synop_meteo_france_select
where temp_max_12 !~* '^$'

SELECT numer_sta, date, tn24, tn12 FROM meteo.synop_meteo_france
where tn12 !~* '^$'

SELECT numer_sta, date, tx24, tx12 FROM meteo.synop_meteo_france
where tx12 !~* '^$'

SELECT count(numer_sta) FROM meteo.synop_meteo_france
where substring(date, 0, 9) ~ '^202004(13|12|11|10|09|08|07|06)'
and numer_sta ~ '^6|^7|^8'
;

SELECT numer_sta, date, substring(date, 0, 9) FROM meteo.synop_meteo_france
where substring(date, 0, 9) ~ '^202004(13|12|11|10|09|08|07|06)'
and numer_sta ~ '^6|^7|^8'
ORDER by date asc
;

-- DELETE FROM meteo.synop_meteo_france_select
-- where substring(date, 0, 9) ~ '^202004(13|12|11|10|09|08|07|06)'
-- ;

select (count(id_station)+2603+154336 = 554776) as evaluation FROM meteo.synop_meteo_france_select;

-- drop view if exists meteo.synop_dates ;
create view meteo.synop_dates as (

select distinct substring(date, 0, 9) as date from meteo.synop_meteo_france_select

);

-- drop view if exists meteo.synop_dates_detail ;
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

select * from meteo.synop_dates_dsk
where date between '31/03/2017' and '31/03/2019'

select * from meteo.darksky_synop42_extraction
order by date_releve desc
;


----- Préparation des stations de la maille 500 -----------

select * from gis.france_maille_500

select * from meteo.liste_stations_500


--------------- Calcul des moyennes de données MF ----------------------

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










