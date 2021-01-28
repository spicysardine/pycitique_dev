/******************************  Aggrégation donnée météo france   **********************************/

/*

alter table meteo.synop_meteo_france_select
alter column date set data type date using nullif(date, '')::date,
alter column temperature set data type float using nullif(temperature, '')::float,
alter column humidite set data type float using nullif(humidite, '')::float,
alter column temp_min_12 set data type float using nullif(temp_min_12, '')::float,
alter column temp_min_24 set data type float using nullif(temp_min_24, '')::float,
alter column temp_max_12 set data type float using nullif(temp_max_12, '')::float,
alter column temp_max_24 set data type float using nullif(temp_max_24, '')::float,
alter column precip_01h set data type float using nullif(precip_01h, '')::float,
alter column precip_03h set data type float using nullif(precip_03h, '')::float,
alter column precip_06h set data type float using nullif(precip_06h, '')::float,
alter column precip_12h set data type float using nullif(precip_12h, '')::float,
alter column precip_24h set data type float using nullif(precip_24h, '')::float,
alter column haut_neige set data type float using nullif(haut_neige, '')::float,
alter column var_pression_24 set data type float using nullif(var_pression_24, '')::float,
alter column var_pression_03 set data type float using nullif(var_pression_03, '')::float,
alter column nebulosite set data type float using nullif(nebulosite, '')::float,
alter column press_sta set data type float using nullif(press_sta, '')::float,
alter column press_mer set data type float using nullif(press_mer, '')::float,
alter column point_rose set data type float using nullif(point_rose, '')::float,
alter column rafale_10min set data type float using nullif(rafale_10min, '')::float,
alter column vvent set data type float using nullif(vvent, '')::float,
alter column visibilite set data type float using nullif(visibilite, '')::float
;

*/

------ création de la table MF avec des données converties en celsius et autre données détaillées

drop table if exists meteo.synop_meteo_france_select_celsius ;
create table meteo.synop_meteo_france_select_celsius as (

	select
	id_station,
	date,
	date_iso,
	substring(date, 9 ) as quarter,
	substring(date, 0 , 5) as year,
	substring(date, 5 , 2) as month,
	substring(date, 7 , 2) as day,
	humidite,
	round(( temperature - 273.15)::numeric, 2) as temperature,
	round(( temp_min_12 - 273.15)::numeric, 2) as temp_min_12,
	round(( temp_min_24 - 273.15)::numeric, 2) as temp_min_24,
	round(( temp_max_12 - 273.15)::numeric, 2) as temp_max_12,
	round(( temp_max_24 - 273.15)::numeric, 2) as temp_max_24,
	precip_01h,
	precip_03h,
	precip_06h,
	precip_12h,
	precip_24h,
	haut_neige,
	var_pression_24,
	var_pression_03,
	nebulosite,
	press_sta,
	press_mer,
	round(( point_rose - 273.15)::numeric, 2) as point_rose,,
	rafale_10min,
	vvent,
	visibilite
	
	from meteo.synop_meteo_france_select
	order by date
	
);





select * from meteo.synop_meteo_france_select_celsius as mfs
order by date
limit 1000
;

-- création des colonnes températures diurnes et nocturnes
alter table meteo.synop_meteo_france_select_celsius
add column temperature_diurne   float,
add column temperature_nocturne float
;

-- inspection des temperatures diurnes
select * from meteo.synop_meteo_france_select_celsius
where substring(date, 9, 2) ~ '(06|09|12|15|18)'  -- diurne
;
-- inspection des temperatures nocturnes
select * from meteo.synop_meteo_france_select_celsius
where substring(date, 9, 2) !~ '(06|09|12|15|18)' -- nocturn
;

-- remplissage des températures diurnes
update  meteo.synop_meteo_france_select_celsius
set temperature_diurne = temperature
where substring(date, 9, 2) ~ '(06|09|12|15|18)'  -- diurne
;

-- remplissage des températures nocturnes
update  meteo.synop_meteo_france_select_celsius
set temperature_nocturne = temperature
where substring(date, 9, 2) !~ '(06|09|12|15|18)' -- nocturn
;

-- Création d'une vue contenant les moyennes par quart

-- drop view if exists meteo.synop_meteo_france_celsius_avg;

create view meteo.synop_meteo_france_celsius_avg as (

	select
	row_number() over() as id,
	date,
	(substring(date, 7 , 2)||'-'||substring(date, 5 , 2)||'-'||substring(date, 0 , 5))::date as date_iso,
	substring(date, 0 , 5) as year,
	substring(date, 5 , 2) as month,
	substring(date, 7 , 2) as day,
	substring(date, 9 ) as quarter,
	round( cast(avg(humidite) as numeric) ,2)        as humidite,
	round( cast(avg(point_rose) as numeric) ,2)      as point_rose,
	round( cast(avg(press_sta) as numeric) ,2)       as press_sta,
	round( cast(avg(press_mer) as numeric) ,2)       as press_mer,
	round( cast(avg(vvent) as numeric) ,2)           as vvent,
	round( cast(avg(visibilite) as numeric) ,2)      as visibilite,
	round( cast(avg(nebulosite) as numeric) ,2)      as nebulosite,
	round( cast(avg(rafale_10min) as numeric) ,2)    as rafale_10min,
	round( cast(avg(precip_24h) as numeric) ,2)      as precip_24h,
	round( cast(avg(precip_01h) as numeric) ,2)      as precip_01h,
	round( cast(avg(temperature) as numeric) ,2)     as temperature,
	round( cast(avg(temperature+2) as numeric) ,2)   as temperature_offset2,
	round( cast(avg(temperature_diurne) as numeric) ,2)     as temperature_diurne,
	round( cast(avg(temperature_diurne+2) as numeric) ,2)   as temperature_diurne_offset2,
	round( cast(avg(temperature_nocturne) as numeric) ,2)   as temperature_nocturne,
	round( cast(avg(temperature_nocturne+2) as numeric) ,2) as temperature_nocturne_offset2

	from meteo.synop_meteo_france_select_celsius
	group by date
	order by date asc

);

select * from meteo.synop_meteo_france_celsius_avg
;

create table meteo.mf_synop42_avg as (
	select
	row_number() over() as id,
	date_iso,
	round( cast(avg(humidite) as numeric) ,2)        as humidite,
	round( cast(avg(point_rose) as numeric) ,2)      as point_rose,
	round( cast(avg(press_sta) as numeric) ,2)       as press_sta,
	round( cast(avg(press_mer) as numeric) ,2)       as press_mer,
	round( cast(avg(vvent) as numeric) ,2)           as vvent,
	round( cast(avg(visibilite) as numeric) ,2)      as visibilite,
	round( cast(avg(nebulosite) as numeric) ,2)      as nebulosite,
	round( cast(avg(rafale_10min) as numeric) ,2)    as rafale_10min,
	round( cast(avg(precip_24h) as numeric) ,2)      as precip_24h,
	round( cast(avg(precip_01h) as numeric) ,2)      as precip_01h,
	round( cast(avg(temperature) as numeric) ,2)     as temperature,
	round( cast(avg(temperature+2) as numeric) ,2)   as temperatureoffset2,
	round( cast(avg(temperature_diurne) as numeric) ,2)     as temperature_diurne,
	round( cast(avg(temperature_diurne+2) as numeric) ,2)   as temperature_diurne_offset2,
	round( cast(avg(temperature_nocturne) as numeric) ,2)   as temperature_nocturne,
	round( cast(avg(temperature_nocturne+2) as numeric) ,2) as temperature_nocturne_offset2

	from meteo.synop_meteo_france_celsius_avg
	group by date_iso
	order by date_iso desc
	
);


update meteo.synop_meteo_france_select_celsius
set point_rose = round(point_rose::numeric, 2)
;

select * from meteo.mf_synop42_avg
;