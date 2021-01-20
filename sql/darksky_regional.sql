/**** donnée Darksky à l'échelle régionale: IDF plus Alsace pré-réforme ****/


select * from geography.france_regions_prereforme_20140306_5m as reg
where reg.nom ~* 'île-de-france|alsace'
;

select dex.* from meteo.darksky_maille_700_extraction as dex limit 44;

select * from meteo.liste_stations_700;

create table meteo.darksky_maille_700_extraction_idf_alsace as 
select
dex.*
from
meteo.darksky_maille_700_extraction as dex
inner join meteo.liste_stations_700 as ls
	on dex.id_station::int = ls.id
inner join geography.france_regions_prereforme_20140306_5m as reg
	on st_contains(reg.geom, ls.geom)
where reg.nom ~* 'île-de-france|alsace'
order by sunrisetime --desc

create table meteo.darksky_maille_700_extraction_idf_alsace_bkp as 
(select * from meteo.darksky_maille_700_extraction_idf_alsace )
;

---- darksky 700 stations regional weather averages table for idf and alsace ----
--drop table if exists meteo.darksky_maille_700_extraction_idf_alsace_avg_raw;

create table meteo.darksky_maille_700_idf_alsace_avg_raw as (

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

from meteo.darksky_maille_700_extraction_idf_alsace as ext
group by date_releve
order by date_releve asc
	
);

create table meteo.darksky_maille_700_idf_alsace_avg as 
select * from  meteo.darksky_maille_700_idf_alsace_avg_raw
;



create table meteo.darksky_maille_700_idf_alsace_avg as 
select * from  meteo.darksky_maille_700_idf_alsace_avg_raw
order by date_releve
;


alter table meteo.darksky_maille_700_idf_alsace_avg
add column temperature numeric;

alter table meteo.darksky_maille_700_idf_alsace_avg
add column temperatureoffset2 numeric;

update  meteo.darksky_maille_700_idf_alsace_avg
set temperature =  round( ((temperaturehigh+temperaturelow) / 2 ), 2)
;

update  meteo.darksky_maille_700_idf_alsace_avg
set temperatureoffset2 = round((((temperaturehigh+temperaturelow)/2)+2), 2)
;

select * from  meteo.darksky_maille_700_idf_alsace_avg
order by date_releve

























