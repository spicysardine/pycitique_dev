/***************************************  Citik Nominatim  **************************************************/

/***************************************  table humain 2018 ***********************************************/
/*
-- Ajout de l'année d'extraction
-- animaux 2017
ALTER TABLE citik.citik_animaux_2017_revgeocoded
add column annee_extract text;

update citik.citik_animaux_2017_revgeocoded
set annee_extract = '2017';
-- animaux 2018
ALTER TABLE citik.citik_animaux_2018_revgeocoded
add column annee_extract text;

update citik.citik_animaux_2018_revgeocoded
set annee_extract = '2018';
-- animaux 2019
ALTER TABLE citik.citik_animaux_2019_revgeocoded
add column annee_extract text;

update citik.citik_animaux_2019_revgeocoded
set annee_extract = '2019';
*/

-- création de la table agrégée

DROP TABLE IF EXISTS citik.citik_animaux;
CREATE TABLE citik.citik_animaux as (
select
id,
lat,
lon,
date_piqure_saisie,
st_setsrid(st_makepoint(lon::float, lat::float), 4326) as geom,
annee_extract,
utilisateur,
email,
date_server,
valide,
qui_pique,
sex_animal,
nbr_tique,
organe_pique,
precision_geo,
environnement,
environnement_precision,
raison_presence,
village,
ville,
chef_lieu,
departement,
departement_code,
region,
code_postal,
pays,
code_pays,
adresse_complette,
autre_information
from citik.citik_animaux_2017_revgeocoded

UNION

select
id,
lat,
lon,
date_piqure_saisie,
st_setsrid(st_makepoint(lon::float, lat::float), 4326) as geom,
annee_extract,
utilisateur,
email,
date_server,
valide,
qui_pique,
sex_animal,
nbr_tique,
organe_pique,
precision_geo,
environnement,
environnement_precision,
raison_presence,
village,
ville,
chef_lieu,
departement,
departement_code,
region,
code_postal,
pays,
code_pays,
adresse_complette,
autre_information
from citik.citik_animaux_2018_revgeocoded

UNION

select
id,
lat,
lon,
date_piqure_saisie,
st_setsrid(st_makepoint(lon::float, lat::float), 4326) as geom,
annee_extract,
utilisateur,
email,
date_server,
valide,
qui_pique,
sex_animal,
nbr_tique,
organe_pique,
precision_geo,
environnement,
environnement_precision,
raison_presence,
village,
ville,
chef_lieu,
departement,
departement_code,
region,
code_postal,
pays,
code_pays,
adresse_complette,
autre_information
from citik.citik_animaux_2019_revgeocoded
);

---- nettoyage des tables

-- suppression des signalements frontaliers
DELETE FROM citik.citik_animaux
where code_pays !~* 'fr';

-- suppression des signalements en DOM
DELETE FROM citik.citik_animaux
where adresse_complette ~ '^France$';

-- rempalcement des nombres vides par un zéro
UPDATE citik.citik_animaux
set nbr_tique = '0'
where nbr_tique = '';

-- uniformisation de la précision de géolocalisation
update citik.citik_animaux
set   precision_geo = 'Précision > 5 km'
where precision_geo = 'Précision : > 5 Km';

update citik.citik_animaux
set   precision_geo = 'Précision entre 1 et 5 km'
where precision_geo = 'Précision : entre 1 et 5 Km';

update citik.citik_animaux
set   precision_geo = 'Précision entre 1 et 5 km'
where precision_geo = 'Précision entre 1 et 5 Km';

update citik.citik_animaux
set   precision_geo = 'Précision < 1 km'
where precision_geo = 'Précision < 1 Km';

update citik.citik_animaux
set   precision_geo = 'Précision entre 1 et 5 km'
where precision_geo = 'Précision entre 1 et Km';

update citik.citik_animaux
set   precision_geo = 'Précision > 5 km'
where precision_geo = 'Précision > 5Km';

-- Uniformisation des environnements
update citik.citik_animaux
set   environnement = 'Parc urbain'
where environnement = 'Parc Urbain';

-- suppression des triple tirets

-- uniformisation et correction des noms et codes départementaux
update citik.citik_animaux as ch
set precision_geo = ''
where precision_geo ~ '---'
;

update citik.citik_animaux as ch
set environnement = ''
where environnement ~ '---'
;

update citik.citik_animaux as ch
set sex_animal = ''
where sex_animal ~ '---'
;

update citik.citik_animaux as ch
set sex_animal = ''
where sex_animal ~ '- - - - -'
;


update citik.citik_animaux
set nbr_tique = regexp_replace(nbr_tique, '-', '', 'g')
;

-- correction de longitudes
update citik.citik_animaux as ch
set lon = regexp_replace(lon, 'E-[0-9]$', '')
;

-- complèter les départements et leurs code par une jointure spatiale
update citik.citik_animaux as ch
set departement      = fdpt.nom,
	departement_code = fdpt.code_insee
from
	geography.france_departements as fdpt
where
	st_contains(fdpt.geom, ch.geom)
and
	char_length(ch.departement_code) = 0
;

-- correction de date de piqure saisies
update citik.citik_animaux
set date_piqure_saisie = split_part(date_piqure_saisie, ' - ', 1)
;

update citik.citik_animaux
set date_piqure_saisie = '02/05/2018'
where id = '153264'
;

update citik.citik_animaux
set date_piqure_saisie = split_part(date_piqure_saisie, ' ', 1)
where annee_extract = '2018'
and date_piqure_saisie ~ '[0-9][0-9]/[0-9][0-9]/20[0-9][0-9]\s{1,}[0-9][0-9]:[0-9][0-9]:[0-9][0-9]'
;
-- 
update citik.citik_animaux
set date_piqure_saisie = regexp_replace(date_piqure_saisie, '[\s/\-:.\\]+', '/', 'g')
where date_piqure_saisie ~ '^\s*(0?[1-9]|1[0-9]|2[0-9]|3[0-1])?\s*[\s\-:.\\]\s*(0?[1-9]|1[0-2])?\s*[\s\-:.\\]\s*(19|20)?[0-9][0-9]\s*$'
;
update citik.citik_animaux
set date_piqure_saisie = split_part(date_piqure_saisie, ' ', 1)
where annee_extract ~ '2018|2017'
and date_piqure_saisie ~ '^\s*(0?[1-9]|1[0-9]|2[0-9]|3[0-1])?\s*[\s/\-:.\\]\s*(0?[1-9]|1[0-2])?\s*[\s/\-:.\\]\s*(19|20)?[0-9][0-9]\s*[a-zA-Z]+'
;
update citik.citik_animaux
set date_piqure_saisie = regexp_replace(date_piqure_saisie, '\s+juin\s+', '/06/', 'i')
where date_piqure_saisie ~ '^\s*(0?[1-9]|1[0-9]|2[0-9]|3[0-1])?\s*[\s/\-:.\\]\s*[a-zA-Z]+\s*[\s/\-:.\\]\s*(19|20)?[0-9][0-9]\s*$'
;
update citik.citik_animaux
set date_piqure_saisie = regexp_replace(date_piqure_saisie, '\s+juillet\s+', '/07/', 'i')
where date_piqure_saisie ~ '^\s*(0?[1-9]|1[0-9]|2[0-9]|3[0-1])?\s*[\s/\-:.\\]\s*[a-zA-Z]+\s*[\s/\-:.\\]\s*(19|20)?[0-9][0-9]\s*$'
;
update citik.citik_animaux
set date_piqure_saisie = regexp_replace(date_piqure_saisie, '\s+mai\s+', '/05/', 'i')
where date_piqure_saisie ~ '^\s*(0?[1-9]|1[0-9]|2[0-9]|3[0-1])?\s*[\s/\-:.\\]\s*[a-zA-Z]+\s*[\s/\-:.\\]\s*(19|20)?[0-9][0-9]\s*$'
;
update citik.citik_animaux
set date_piqure_saisie = regexp_replace(date_piqure_saisie, '\s+avril\s+', '/04/', 'i')
where date_piqure_saisie ~ '^\s*(0?[1-9]|1[0-9]|2[0-9]|3[0-1])?\s*[\s/\-:.\\]\s*[a-zA-Z]+\s*[\s/\-:.\\]\s*(19|20)?[0-9][0-9]\s*$'
;
update citik.citik_animaux
set date_piqure_saisie = regexp_replace(date_piqure_saisie, '\s+mars\s+', '/03/', 'i')
where date_piqure_saisie ~ '^\s*(0?[1-9]|1[0-9]|2[0-9]|3[0-1])?\s*[\s/\-:.\\]\s*[a-zA-Z]+\s*[\s/\-:.\\]\s*(19|20)?[0-9][0-9]\s*$'
;
update citik.citik_animaux
set date_piqure_saisie = regexp_replace(date_piqure_saisie, '\s+(aot|aout)\s+', '/08/', 'i')
where date_piqure_saisie ~ '^\s*(0?[1-9]|1[0-9]|2[0-9]|3[0-1])?\s*[\s/\-:.\\]\s*[a-zA-Z]+\s*[\s/\-:.\\]\s*(19|20)?[0-9][0-9]\s*$'
;
update citik.citik_animaux
set date_piqure_saisie = regexp_replace(date_piqure_saisie, '\s+octobre\s+', '/10/', 'i')
where date_piqure_saisie ~ '^\s*(0?[1-9]|1[0-9]|2[0-9]|3[0-1])?\s*[\s/\-:.\\]\s*[a-zA-Z]+\s*[\s/\-:.\\]\s*(19|20)?[0-9][0-9]\s*$'
;
update citik.citik_animaux
set date_piqure_saisie = regexp_replace(date_piqure_saisie, '/16\s*$', '/2016', 'g')
where date_piqure_saisie  ~ '^\s*(0?[1-9]|1[0-9]|2[0-9]|3[0-1])?\s*[\s/\-:.\\]\s*(0?[1-9]|1[0-2])?\s*[\s/\-:.\\]\s*[0-9][0-9]\s*$'
;
update citik.citik_animaux
set date_piqure_saisie = regexp_replace(date_piqure_saisie, '/17\s*$', '/2017', 'g')
where date_piqure_saisie  ~ '^\s*(0?[1-9]|1[0-9]|2[0-9]|3[0-1])?\s*[\s/\-:.\\]\s*(0?[1-9]|1[0-2])?\s*[\s/\-:.\\]\s*[0-9][0-9]\s*$'
;
update citik.citik_animaux
set date_piqure_saisie = regexp_replace(date_piqure_saisie, '/18\s*$', '/2018', 'g')
where date_piqure_saisie  ~ '^\s*(0?[1-9]|1[0-9]|2[0-9]|3[0-1])?\s*[\s/\-:.\\]\s*(0?[1-9]|1[0-2])?\s*[\s/\-:.\\]\s*[0-9][0-9]\s*$'
;
update citik.citik_animaux
set date_piqure_saisie = regexp_replace(date_piqure_saisie, '/19\s*$', '/2019', 'g')
where date_piqure_saisie  ~ '^\s*(0?[1-9]|1[0-9]|2[0-9]|3[0-1])?\s*[\s/\-:.\\]\s*(0?[1-9]|1[0-2])?\s*[\s/\-:.\\]\s*[0-9][0-9]\s*$'
;
update citik.citik_animaux
set date_piqure_saisie = regexp_replace(date_piqure_saisie, '/20\s*$', '/2020', 'g')
where date_piqure_saisie  ~ '^\s*(0?[1-9]|1[0-9]|2[0-9]|3[0-1])?\s*[\s/\-:.\\]\s*(0?[1-9]|1[0-2])?\s*[\s/\-:.\\]\s*[0-9][0-9]\s*$'
;

update citik.citik_animaux
set date_piqure_saisie = regexp_replace(date_piqure_saisie, '/2027\s*$', '/2017', 'g')
where date_piqure_saisie  ~ '^\s*(0?[1-9]|1[0-9]|2[0-9]|3[0-1])?\s*[\s/\-:.\\]\s*(0?[1-9]|1[0-2])?\s*[\s/\-:.\\]\s*(20)?[0-9][0-9]\s*$'
;

update citik.citik_animaux
set date_piqure_saisie = regexp_replace(date_piqure_saisie, '/2028\s*$', '/2018', 'g')
where date_piqure_saisie  ~ '^\s*(0?[1-9]|1[0-9]|2[0-9]|3[0-1])?\s*[\s/\-:.\\]\s*(0?[1-9]|1[0-2])?\s*[\s/\-:.\\]\s*(20)?[0-9][0-9]\s*$'
;


-- creation of the clean table
drop table if exists citik.citik_animaux_clean;
--
create table citik.citik_animaux_clean as (
-- 
	SELECT * 
	from citik.citik_animaux as ch
	where ch.date_piqure_saisie !~ '^$'
	and ch.date_piqure_saisie ~ '^\s*(0?[1-9]|1[0-9]|2[0-9]|3[0-1])?\s*[\s/\-:.\\]\s*(0?[1-9]|1[0-2])?\s*[\s/\-:.\\]\s*(19|20)[0-9][0-9]\s*$'
	and ch.lat ~ '^[0-9]?[0-9]\.[0-9]?[0-9]{1,}$'
	and ch.lon ~ '^-?[0-9]?[0-9]\.[0-9]?[0-9]{1,}$'
	--and annee_extract = '2018'
	order by ch.id::int asc
--
);

-- élimination des espaces restant
update citik.citik_animaux_clean
set date_piqure_saisie = regexp_replace(date_piqure_saisie, '\s*$', '', 'g')
;

-- recorrection des séparateurs irréguliers restants
update citik.citik_animaux_clean
set date_piqure_saisie = regexp_replace(date_piqure_saisie, '[\s\-:.\\]+', '/', 'g')
where date_piqure_saisie !~ '^\s*(0?[1-9]|1[0-9]|2[0-9]|3[0-1])?\s*/\s*(0?[1-9]|1[0-2])?\s*/\s*(19|20)?[0-9][0-9]\s*$'
;

-- élimination des espaces dans les  dates
update citik.citik_animaux_clean
set date_piqure_saisie = regexp_replace(date_piqure_saisie, '\s*', '', 'g')
;

update citik.citik_animaux_clean
set lat = regexp_replace(lat, '\s*', '', 'g')
;
update citik.citik_animaux_clean
set lon = regexp_replace(lon, '\s*', '', 'g')
;

-- recorrection des séparateurs irréguliers restants
update citik.citik_animaux_clean
set date_piqure_saisie = regexp_replace(date_piqure_saisie, '[\-:.\\]+', '/', 'g')
;
--------------------------------------------------------------------------

-- Définition des types de données des colonnes

alter table citik.citik_animaux_clean

alter column lat set data type float using lat::float,
alter column lon set data type float using lon::float,
alter column date_piqure_saisie set data type date using date_piqure_saisie::date,
alter column nbr_tique set data type int using nullif(nbr_tique, '')::int,
alter column geom set data type geometry using geom::geometry,
alter column date_server set data type timestamp using nullif(date_server, '')::timestamp
;

------------- Affichage du résultat --------------

select * from citik.citik_animaux_clean as chc
order by chc.id::int asc
;

/* -- Définition des types de données des colonnes

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

*/