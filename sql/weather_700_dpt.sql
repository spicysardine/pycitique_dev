SELECT * FROM citik.citik_humains_clean_weather_strict

SELECT * FROM meteo.liste_stations_700

SELECT * FROM meteo.darksky_maille_700_extraction 
limit 44


create table meteo.darksky_maille_700_extraction_dpt as (

SELECT 
extra.*,
round( (temperaturehigh+temperaturelow/2)::numeric, 2 ) as temperature,
departement_code,
departement
FROM meteo.darksky_maille_700_extraction as extra
join meteo.liste_stations_700 as sta
	on sta.id = extra.id_station
-- limit 999
)	



alter table meteo.liste_stations_700
alter column id set data type varchar(5) using id::varchar(5)






























































































