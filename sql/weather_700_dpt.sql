SELECT * FROM citik.citik_humains_clean_weather_strict

SELECT * FROM meteo.liste_stations_700

SELECT * FROM meteo.darksky_maille_700_extraction_dpt 
where temperaturehigh is not null
order by temperaturehigh desc
limit 44

-- drop table if exists meteo.darksky_maille_700_extraction_dpt
create table meteo.darksky_maille_700_extraction_dpt as (

SELECT 
extra.*,
departement_code,
departement
FROM meteo.darksky_maille_700_extraction as extra
join meteo.liste_stations_700 as sta
	on sta.id = extra.id_station
-- limit 999
)	



alter table meteo.liste_stations_700
alter column id set data type varchar(5) using id::varchar(5)



alter table meteo.darksky_maille_700_extraction_dpt
add column temperature numeric
;

update meteo.darksky_maille_700_extraction_dpt
set temperature = (temperaturehigh+temperaturelow)/2
;

update meteo.darksky_maille_700_extraction_dpt
set temperature = round(temperature,2)
;

update meteo.darksky_maille_700_extraction_dpt
set humidity = humidity*100
;

update meteo.darksky_maille_700_extraction_dpt
set cloudcover = cloudcover*100
;



















































































