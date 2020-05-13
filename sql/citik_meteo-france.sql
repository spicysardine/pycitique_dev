/******************************  Aggrégation donnée météo france   **********************************/

/* Suite à l'insertion de la table brute par le script python pycitique.py 
	on sélectionne les les variables pertinente à l'étude en leur donnant un nom de colonn
	significatif
*/

--- creation de la table météo avec les paramètres sélectionnés

-- DROP TABLE IF EXISTS meteo.synop_meteo_france_select ;

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
	where substring(date, 0, 9) !~ '^202004(13|12|11|10|09|08|07|06)'
	and numer_sta !~ '^6|^7|^8'
	order by date asc
);

select count(*) from meteo.synop_meteo_france_select;

-- on vérifie si la somme des enregistrement restant plus les enregistrements DTOM plus les dates ultérieures au 05 avrile
-- est égale au nombre d'enregistrements de la table d'origine
select (count(id_station)+2603+154336 = 554776) as evaluation FROM meteo.synop_meteo_france_select;


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

-- Vérification de la table exprimée en celsius
select * from meteo.synop_meteo_france_select_celsius as mfs
order by date
limit 100
;

-- création des colonnes de températures diurnes et nocturnes
alter table meteo.synop_meteo_france_select_celsius
add column temperature_diurne   float,
add column temperature_nocturne float
;

-- inspection des temperatures diurnes de 06h à 21h
select * from meteo.synop_meteo_france_select_celsius
where substring(date, 9, 2) ~ '(06|09|12|15|18)'  -- diurne
;
-- inspection des temperatures nocturnes de 21h à 06h
select * from meteo.synop_meteo_france_select_celsius
where substring(date, 9, 2) !~ '(06|09|12|15|18)' -- nocturn
;

-- remplissage des températures diurnes de 06h à 21h
update  meteo.synop_meteo_france_select_celsius
set temperature_diurne = temperature
where substring(date, 9, 2) ~ '(06|09|12|15|18)'  -- diurne
;

-- remplissage des températures nocturnes de 21h à 06h
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

-- Vérification de la table des moyennes des quarts
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

select * from meteo.mf_synop42_avg
;

select * from meteo.darksky_synop42_avg
;

create table meteo.mf_synop42_avg as (

	select * from meteo.mf_synop42_avg_raw
);

------ Uniformisation des unités avec darksky  --------

-- update meteo.mf_synop42_avg
-- set
-- press_sta = round( (press_sta/100), 2),
-- press_mer = round( (press_mer/100), 2),
-- visibilite = round( (visibilite/1000), 2 )
-- ;

-- update meteo.darksky_synop42_avg
-- set humidite = humidite*100,
-- set nebulosite = nebulosite*100
-- ;






