/*----------------------------  Étapes prépartives du Téléchargement des signalements  -------------------------*/


/*############################  Étape spatiale: préparation de la maille des stations synoptiques ############*/

-- creation de la table des station avec la postition géographique
create table if not exists meteo.liste_stations_synop as (
	SELECT
		id,
		nom,
		latitude,
		longitude,
		altitude,
		st_setsrid( st_makepoint(longitude::float, latitude::float), 4326) as geom
	
	from meteo.liste_stations_mf_synop

);

drop table meteo.liste_stations_mf_synop

-- Création d'une table géolocalisée de stations synoptiques 
-- météo-france contenant uniquement les 42 staion en métropole + corse
create table meteo.liste_stations_synop_metropole as (
	
	select * from meteo.liste_stations_synop
	where id !~ '^6|^7|^8'
	
);

-- la table est ensuite reverse-géolocalisée par le script python pycitique.py
-- vérification de la table créée et r-géolocalisée
select * from meteo.liste_stations_synop_metropole
order by id
;

-- vérification des types de colonnes (optionnel)
-- méthode 1
select distinct
	pg_typeof(id),
	pg_typeof(nom),
	pg_typeof(latitude),
	pg_typeof(longitude),
	pg_typeof(altitude),
	pg_typeof(geom)
from meteo.liste_stations_synop_metropole
;

-- méthode 2
select column_name, data_type from information_schema.columns
where table_name = 'liste_stations_synop_metropole'
;

/*#################  Étape temporelle : préparation des dates d'extraction ###########*/


---------------------- vérification de l'étendue temporelle -----------------------
select distinct date_piqure_saisie from citik.citik_animaux_clean_weather_strict order by date_piqure_saisie --desc;

select distinct date_piqure_saisie from citik.citik_humains_clean_weather_strict order by date_piqure_saisie --desc;

/*-**********************   Cette Partie permet d'estimer la limite entre les températures diurnes et nocturenes ********************-*/
SELECT 
temperaturelow,
temperaturelowtime as templowtime
from citik.citik_humains_clean_weather
where temperaturelowtime between '01-01-2017 00:00:00' and '12-04-2020 00:00:00'
order by temperaturelowtime desc;

SELECT
'temperaturelowtime' as mintime,
max( temperaturelowtime ) maxtime
from citik.citik_humains_clean_weather
where temperaturelowtime between '01-01-2017 00:00:00' and '01-04-2020 00:00:00'
;

-- sunrise and sunset times

-- sunrise
SELECT 
sunrisetime as sunrisetime
from citik.citik_humains_clean_weather as cw
where sunrisetime between '01-01-2017 00:00:00' and '12-04-2020 00:00:00'
order by sunrisetime desc
;

-- sunset
SELECT
cw.date_piqure_saisie,
sunsettime as sunsettime
from citik.citik_humains_clean_weather as cw
where sunsettime between '01-01-2017 00:00:00' and '12-04-2020 00:00:00'
order by date_piqure_saisie desc
;


-- Création d'une table des dates uniques des signalements
-- c'est la date qui définit l'étendue temporelle de l'extraction
-- et aussi, servira à l'extraction de la donnée météo nationale comparative

-- drop view if exists meteo.synop_dates ;
create view meteo.synop_dates as (

	select distinct
	substring(date, 0, 9) as date
	from meteo.synop_meteo_france_select

);

-- drop view if exists meteo.synop_dates_detail;
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

