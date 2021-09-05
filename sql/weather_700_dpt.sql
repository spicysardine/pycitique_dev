SELECT * FROM citik.citik_humains_clean_weather_strict

SELECT * FROM meteo.liste_stations_700

SELECT * FROM meteo.darksky_maille_700_extraction_dpt 
-- where temperaturehigh is not null
-- order by temperaturehigh desc
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


SELECT 
count(id_station) as dsk_winter_long
FROM meteo.darksky_maille_700_extraction_dpt 
where date_releve between '2017-10-01' and '2018-03-31'
or date_releve between '2018-10-01' and '2019-03-31'
or date_releve between '2019-10-01' and '2020-03-31'

UNION

SELECT 
count(id) as humdata_winter_long
FROM citik.citik_humains_clean_weather_strict 
where date_piqure_saisie between '2017-10-01' and '2018-03-31'
or date_piqure_saisie between '2018-10-01' and '2019-03-31'
or date_piqure_saisie between '2019-10-01' and '2020-03-31'

SELECT 
count(id_station) as dsk_winter_short
FROM meteo.darksky_maille_700_extraction_dpt 
where date_releve between '2017-11-01' and '2018-02-28'
or date_releve between '2018-11-01' and '2019-02-28'
or date_releve between '2019-11-01' and '2020-02-28'

UNION

SELECT 
count(id) as humdata_winter_short
FROM citik.citik_humains_clean_weather_strict 
where date_piqure_saisie between '2017-11-01' and '2018-02-28'
or date_piqure_saisie between '2018-11-01' and '2019-02-28'
or date_piqure_saisie between '2019-11-01' and '2020-02-28'


--------

select * from meteo.darksky_synop42_avg
where id = 250
order by id asc




DO
$$
DECLARE rec record;
BEGIN
	for rec in (select code_insee from datasample) loop
		raise notice '%', rec;
	end loop;
END
$$



































































