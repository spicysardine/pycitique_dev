/*********************************************  Citik Nominatim exploration  *******************************************************/

-- alter table geography.france_departements_wgs84
-- drop column geom;

-- alter table geography.france_departements_wgs84
-- rename column geometry to geom;

-- vérification des signalements frontaliers
select * from citik.citik_humains where code_pays !~* 'fr';
-- vérification des signalements en DOM
select * from citik.citik_humains where adresse_complette = 'France';

-- nombre de signalement par utilisateur
select utilisateur, count(utilisateur) from citik.citik_humains group by utilisateur order by count asc;

-- inspection des signalements extrêmes
SELECT * from citik.citik_humains 
where nbr_tique::integer > 50
order by nbr_tique::int desc;

--- contrôle des régions
SELECT distinct region from citik.citik_humains order by region desc;

SELECT id, geom, annee_extract, village, ville, chef_lieu, departement, departement_code, region 
from citik.citik_humains
where region ~* 'Auvergne-Rhône-Alpes'-- and departement ~* 'calvados'
order by departement;

--- contrôle des départements
SELECT distinct departement from citik.citik_humains order by departement;

--SELECT id, geom, annee_extract, nbr_tique, village, ville, chef_lieu, departement, departement_code, region 
select *
from citik.citik_humains
where departement ~* 'meurthe-et-moselle'-- and departement ~* 'calvados'
--where chef_lieu = 'Nancy'
order by nbr_tique::int desc;


-- Vérification de l'import 17166
select * from citik.citik_humains order by id::int asc;


--- nombre de tiques par région française
select
freg.nom,
st_centroid(freg.geom) as position,
ch.tique_region
from geography.france_metropole_regions as freg
left join (
		select
		ch.region,
		sum(ch.nbr_tique::int) as tique_region
		from citik.citik_humains as ch
		where ch.region !~* '^$|france métropolitaine'
		 group by region order by tique_region
) as  ch

on freg.nom = ch.region
order by ch.tique_region
;


--- nombre de tiques par département français
select
fdpt.nom,
ch.departement,
st_centroid(fdpt.geom) as position,
ch.tique_dpt
from geography.france_departements as fdpt
 join (
		select
		ch.departement,
		sum(ch.nbr_tique::int) as tique_dpt
		from citik.citik_humains as ch
		where ch.departement !~* '^$|mayotte|guyane|la réunion|martinique|guadeloupe'
		 group by departement order by departement asc
) as  ch

on fdpt.nom = ch.departement
order by ch.tique_dpt asc
;

-- contrôle du code départemental
select distinct
departement_code, code_postal,
departement_code = substring(code_postal, 1,2) as truth
from citik.citik_humains as ch
where departement_code = substring(code_postal, 1,2) is false
and departement_code != '69D'
and departement_code != '69M'
and departement_code != '2A'
and departement_code != '2B'
;

select * from citik.citik_humains
where char_length(departement_code) != 2
and char_length(departement_code) != 0
and (departement_code != '69D' and departement_code != '69M')
;

select distinct
code_pays--departement, departement_code, code_postal --, geom
from citik.citik_humains --where departement = 'Montpellier'
order by code_postal asc


select 
ch.id, ch.geom as pos, ch.annee_extract, ch.village, ch.ville, 
ch.chef_lieu, ch.departement, ch.departement_code, ch.code_postal
--fdpt.nom, fdpt.code_insee, fdpt.geom as geometry
from citik.citik_humains as ch
--left join geography.france_departements fdpt
	--on st_contains(fdpt.geom, ch.geom)
where char_length(ch.code_postal) = 0


update citik.citik_humains as ch
set departement      = fdpt.nom,
	departement_code = fdpt.code_insee
from
	geography.france_departements as fdpt
where
	st_contains(fdpt.geom, ch.geom)
and
	char_length(ch.departement_code) = 0
;


select 
*
from citik.citik_humains as ch
where char_length(ch.code_postal) = 0
order by departement_code asc

select distinct departement, departement_code
from citik.citik_humains
order by departement_code asc

select
ch.*
from citik.citik_humains ch
where ch.id::integer between 161938 and 177873
order by ch.id::int asc


select
ch.*
from citik.citik_humains ch
where date_server ~* '2018'
order by ch.id::int asc


--- visualisation des géométries citik du premier envoi de jfc
drop view if exists citik.citik_jfc_geom;

create view citik.citik_jfc_geom as (
select
split_part(localisation, '/', 1)::float as lat,
split_part(localisation, '/', 2)::float as lon,
st_setsrid(
	st_makepoint(
		(split_part(localisation, '/', 2))::float
		, 
		(split_part(localisation, '/', 1))::float 
	)
	,4326) as position,
jfc.*

from citik.citik_data_180820_jfc as jfc
where split_part(localisation, '/', 1) ~ '^[0-9]?[0-9]\.[0-9]?[0-9]{1,}$'
and split_part(localisation, '/', 2) ~ '^-?[0-9]?[0-9]\.[0-9]?[0-9]{1,}$'
);

select * from citik.citik_jfc_geom;

select
split_part(localisation, '/', 1) lat,
split_part(localisation, '/', 2) lon

from citik.citik_data_180820_jfc as jfc
where split_part(localisation, '/', 1) !~ '^[0-9]?[0-9]\.[0-9]?[0-9]{1,}$'
and split_part(localisation, '/', 2) !~ '^-?[0-9]?[0-9]\.[0-9]?[0-9]{1,}$'


select
split_part(localisation, '/', 1) lat,
split_part(localisation, '/', 2) lon

from citik.citik_data_180820_jfc as jfc
where split_part(localisation, '/', 1) ~ '^$'
or split_part(localisation, '/', 2) ~ '^$'

-- Complétion de la date saisie de 2017 et 2018
-- vérification et comparaison
SELECT
ch18.id,
jfc.id,
ch18.annee_extract,
jfc.position,
ch18.date_piqure_saisie,
jfc.date_saisie
from citik.citik_humains_2018_revgeocoded as ch18
 join citik.citik_jfc_geom as jfc
 on jfc.id = ch18.id
ch18.date_piqure_saisie = ''
order by ch18.id::int asc
;

update citik.citik_humains_2017_revgeocoded as ch17
set date_piqure_saisie = jfc.date_saisie
from citik.citik_jfc_geom as jfc
where jfc.id = ch17.id
and ch17.date_piqure_saisie ~ '^$'


select id, annee_extract, date_piqure_saisie from citik.citik_humains_2018_revgeocoded
where date_piqure_saisie = ''

drop table if exists citik.citik_humains_clean;
create table citik.citik_humains_clean as (
-- creation de la table propre
SELECT * 
from citik.citik_humains as ch
where ch.date_piqure_saisie !~ '^$'
and ch.date_piqure_saisie ~ '^\s*(0?[1-9]|1[0-9]|2[0-9]|3[0-1])?\s*[\s/\-:.\\]\s*(0?[1-9]|1[0-2])?\s*[\s/\-:.\\]\s*(19|20)[0-9][0-9]\s*$'
and ch.lat ~ '^[0-9]?[0-9]\.[0-9]?[0-9]{1,}$'
and ch.lon ~ '^-?[0-9]?[0-9]\.[0-9]?[0-9]{1,}$'
--and annee_extract = '2018'
order by ch.id::int asc
);


SELECT * 
from citik.citik_humains as ch

select regexp_replace(date_piqure_saisie, '[\s/\-:.\\]+', '/', 'g') as string
from citik.citik_humains as ch



-- creation de la table propre
SELECT * 
from citik.citik_humains as ch
where ch.date_piqure_saisie !~ '^$'
and ch.date_piqure_saisie ~ '^\s*(0?[1-9]|1[0-9]|2[0-9]|3[0-1])?\s*[\s/\-:.\\]\s*(0?[1-9]|1[0-2])?\s*[\s/\-:.\\]\s*[0-9][0-9]\s*$'
and ch.lat ~ '^[0-9]?[0-9]\.[0-9]?[0-9]{1,}$'
and ch.lon ~ '^-?[0-9]?[0-9]\.[0-9]?[0-9]{1,}$'
--and annee_extract = '2018'
order by ch.id::int asc
;
----
SELECT * 
from citik.citik_humains as ch
where char_length(ch.date_piqure_saisie) != 10
and ch.date_piqure_saisie !~ '^$'

----
-- adding april 2020 subset to humains_2019_revgeocoded

select *  from citik.citik_humains_2019_revgeocoded ch
order by ch.id::int desc

--
select * 
from citik.citik_humains_2019_04_subset_revgeocoded ch
order by ch.id::int desc;

-- humains 2019
ALTER TABLE citik.citik_humains_2019_04_subset_revgeocoded
add column annee_extract text;

update citik.citik_humains_2019_04_subset_revgeocoded
set annee_extract = '2019';

INSERT INTO citik.citik_humains_2019_revgeocoded
select *
from citik.citik_humains_2019_04_subset_revgeocoded;

select * from citik.citik_humains_clean as chc
order by chc.id::int asc
;

--############################################# Donnée météo propre pour les humains ####################################

select chw.* from citik.citik_animaux_clean_weather as chw;

select chw.* from citik.citik_humains_clean_weather_v2 as chw;

-- converting geom to geometry
-- converting date_piqure_saisie to date type
-- converting coordinates to float type
-- converting coordinates to float type
-- converting to timestamp type


alter table citik.citik_animaux_clean_weather
alter column nbr_tique set data type int using nullif(nbr_tique, '')::int,
alter column date_piqure_saisie set data type date using date_piqure_saisie::date,
alter column lat set data type float using lat::float,
alter column lon set data type float using lon::float,
alter column nbr_tique set data type int using nullif(nbr_tique, '')::int,
alter column geom set data type geometry using geom::geometry,
alter column date_server set data type timestamp using nullif(date_server, '')::timestamp,
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



-- select chw.* from citik.citik_humains_clean_weather_v2 as chw;

--############################################# pour les animaux ####################################


-- creation de la table propre
SELECT * 
from citik.citik_animaux as ch
where ch.date_piqure_saisie !~ '^$'
and ch.date_piqure_saisie !~ '^\s*(0?[1-9]|1[0-9]|2[0-9]|3[0-1])?\s*[\s/\-:.\\]\s*(0?[1-9]|1[0-2])?\s*[\s/\-:.\\]\s*(19|20)?[0-9][0-9]\s*$'
and ch.lat ~ '^[0-9]?[0-9]\.[0-9]?[0-9]{1,}$'
and ch.lon ~ '^-?[0-9]?[0-9]\.[0-9]?[0-9]{1,}$'
--and annee_extract = '2018'
order by ch.id::int asc

select count(id) from citik.citik_animaux_2017_revgeocoded where adresse_complette ~ '^France$';
select count(id) from citik.citik_animaux_2018_revgeocoded where adresse_complette ~ '^France$';
select count(id) from citik.citik_animaux_2019_revgeocoded where adresse_complette ~ '^France$';

select distinct precision_geo
from citik.citik_animaux;

-- creation de la table propre
SELECT st_buffer(geom::geography, 5000)
from citik.citik_humains_clean_weather as ch
where environnement  ~* 'forêt'
and raison_presence !~* 'lieu de résidence'
and precision_geo   = 'Précision > 5 km'
--and id = '148690'
;

SELECT 
chw.id,
lat,
lon,
date_piqure_saisie,
nbr_tique
--st_buffer(chw.geom::geography, 1000) as geom
from citik.citik_humains_clean_weather as chw, gis.france_idf_forest as frst
where environnement  ~* 'forêt'
and raison_presence !~* 'lieu de résidence'
and precision_geo   = 'Précision < 1 km'
and region ~* 'Île-de-France'
and st_intersects( st_buffer(chw.geom::geography, 1000) , frst.geom ) is false
;

----------------------------------------------


-- 

select id, date_piqure_saisie, annee_extract from citik.citik_humains
where id = ''
;

select id, date_piqure_saisie, annee_extract from citik.citik_animaux
where id = '165614'
;

select id, date_piqure_saisie, annee_extract from  citik.citik_animaux_2018_revgeocoded
where date_piqure_saisie = '31/09/2018'
;

--------------------------------

-- humains meteo
SELECT 
*
from citik.citik_humains_clean_weather as ch
where ch.date_piqure_saisie ~ '^\s*(0?[1-9]|1[0-9]|2[0-9]|3[0-1])?\s*/\s*(0?[1-9]|1[0-2])?\s*/\s*(19|20)?[0-9][0-9]\s*$'
order by date_piqure_saisie::date desc;
;

-- animaux avec meteo
SELECT 
*
from citik.citik_animaux_clean_weather as ch
where ch.date_piqure_saisie ~ '^\s*(0?[1-9]|1[0-9]|2[0-9]|3[0-1])?\s*/\s*(0?[1-9]|1[0-2])?\s*/\s*(19|20)?[0-9][0-9]\s*$'
order by date_piqure_saisie::date asc;
;

--------------------------------

-- humains propre
SELECT 
*
from citik.citik_humains_clean as ch
--where ch.date_piqure_saisie !~ '^$'
--and ch.date_piqure_saisie ~ '^\s*(0?[1-9]|1[0-9]|2[0-9]|3[0-1])?\s*/\s*(0?[1-9]|1[0-2])?\s*/\s*(19|20)?[0-9][0-9]\s*$'
order by date_piqure_saisie::date asc;

-- animaux propre
SELECT 
*
from citik.citik_animaux_clean as ch
--where ch.date_piqure_saisie ~ '^$'
--and ch.date_piqure_saisie ~ '^\s*(0?[1-9]|1[0-9]|2[0-9]|3[0-1])?\s*/\s*(0?[1-9]|1[0-2])?\s*/\s*(19|20)?[0-9][0-9]\s*$'
order by date_piqure_saisie::date asc;

----------------------------
/*
update citik.citik_animaux_clean_weather as ch
set qui_pique = ''
where qui_pique ~ '---'
;

update citik.citik_animaux_clean_weather as ch
set qui_pique = ''
where qui_pique ~ '- - - - -'
;
*/

/*
update citik.citik_animaux_clean_weather
set nbr_tique = '1' where nbr_tique = 'un';

update citik.citik_animaux_clean_weather
set nbr_tique = '3' where nbr_tique = '2 ou 3';

update citik.citik_animaux_clean_weather
set nbr_tique = '50' where nbr_tique = '50 par jour et par équidé';
*/

select  count(qui_pique) from citik.citik_animaux_clean_weather
where qui_pique != ''
--order by qui_pique
;


-- nombre moyen de tique prélevée

SELECT 
qui_pique as animal_pique,
sum( nbr_tique ) as nombre_tiques,
AVG( nbr_tique ) as moyenne_tiques,
MIN(nbr_tique) as nombre_tiques,
MAX(nbr_tique) as nombre_tiques,
count(qui_pique)  as decompte,
percentage(count(qui_pique), 4936)  as pourcentage
FROM citik.citik_animaux_clean_weather
group by qui_pique
order by moyenne_tiques asc
;

select *
from citik.citik_humains_clean_weather
where  date_piqure_saisie between '01/01/2017' and  '12/04/2020'
order by date_piqure_saisie desc
;

select *
from citik.citik_animaux_clean_weather
where  date_piqure_saisie between '01/01/2017' and  '12/04/2020'
order by date_piqure_saisie desc
;

select nbr_tique, date_piqure_saisie
from citik.citik_humains_clean_weather
where nbr_tique ~ '[[:alpha:]]'
order by char_length(nbr_tique) desc
;

create table citik.citik_humains_clean_weather_strict as
(select * from citik.citik_humains_clean_weather
where  date_piqure_saisie between '01/01/2017' and  '12/04/2020'
order by date_piqure_saisie asc
);

create table citik.citik_animaux_clean_weather_strict as
(select * from citik.citik_animaux_clean_weather
where  date_piqure_saisie between '01/01/2017' and  '12/04/2020'
order by date_piqure_saisie asc
);

/* du 17 janvier 2017 au 05 avril 2020 14657 enr. */
select * from citik.citik_humains_clean_weather_strict
order by date_piqure_saisie desc
;

/* du 13 janvier 2017 au 28 mars 2020  4926 enr. */
select distinct sex_animal from citik.citik_animaux_clean_weather_strict
order by date_piqure_saisie desc
;

