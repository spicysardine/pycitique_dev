/***************************************  Citik Nominatim  **************************************************/

/***************************************  table humain 2018 ***********************************************/
/*
-- Ajout de l'année d'extraction
-- humains 2017
ALTER TABLE citik.citik_humains_2017_revgeocoded
add column annee_extract text;

update citik.citik_humains_2017_revgeocoded
set annee_extract = '2017';
-- humains 2018
ALTER TABLE citik.citik_humains_2018_revgeocoded
add column annee_extract text;

update citik.citik_humains_2018_revgeocoded
set annee_extract = '2018';
-- humains 2019
ALTER TABLE citik.citik_humains_2019_revgeocoded
add column annee_extract text;

update citik.citik_humains_2019_revgeocoded
set annee_extract = '2019';

*/

-- création de la table agrégée


DROP TABLE IF EXISTS citik.citik_humains;
CREATE TABLE citik.citik_humains as (
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
sex_pique,
age,
nbr_tique,
precision_geo,
environnement,
environnement_precision,
raison_presence,
activite,
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
autre_info
from citik.citik_humains_2017_revgeocoded

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
sex_pique,
age,
nbr_tique,
precision_geo,
environnement,
environnement_precision,
raison_presence,
activite,
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
autre_info
from citik.citik_humains_2018_revgeocoded

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
sex_pique,
age,
nbr_tique,
precision_geo,
environnement,
environnement_precision,
raison_presence,
activite,
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
autre_info
from citik.citik_humains_2019_revgeocoded
);

---- nettoyage des tables

-- suppression des signalements frontaliers
DELETE FROM citik.citik_humains
where code_pays !~* 'fr';

-- suppression des signalements en DOM
DELETE FROM citik.citik_humains
where adresse_complette ~ '^France$';

-- suppression de doublon 
delete from citik.citik_humains
where id = '163533';

delete from citik.citik_humains
where id = '146423'
;

-- rempalcement des nombres vides par un zéro
UPDATE citik.citik_humains
set nbr_tique = '0'
where nbr_tique = '';

-- uniformisation des tranches d'âge
update citik.citik_humains
set   age = 'De 0 à 5 ans'
where age = ' De 1 à 5 ans';

-- uniformisation de la précision de géolocalisation
update citik.citik_humains
set   precision_geo = 'Précision > 5 km'
where precision_geo = 'Précision : > 5 Km';

update citik.citik_humains
set   precision_geo = 'Précision entre 1 et 5 km'
where precision_geo = 'Précision : entre 1 et 5 Km';

update citik.citik_humains
set   precision_geo = 'Précision entre 1 et 5 km'
where precision_geo = 'Précision entre 1 et 5 Km';

update citik.citik_humains
set   precision_geo = 'Précision < 1 km'
where precision_geo = 'Précision < 1 Km';

-- Uniformisation des environnements
update citik.citik_humains
set   environnement = 'Parc urbain'
where environnement = 'Parc Urbain';

-- uniformisation et correction des noms et codes départementaux
update citik.citik_humains
set   departement_code = '67'
where departement_code = '67 ';

update citik.citik_humains
set   departement_code = '49'
where departement_code = '49000;49';

update citik.citik_humains
set   code_postal = '49000'
where code_postal = '49000;49100';

update citik.citik_humains
set   departement = 'Maine-et-Loire'
where departement = 'Angers';

update citik.citik_humains
set   departement = 'Nord'
where departement = 'Montpellier';

update citik.citik_humains
set   departement_code = '59'
where departement_code = '590';

update citik.citik_humains
set   code_postal = '59000'
where code_postal = '590000';

update citik.citik_humains
set   code_postal = '67100'
where code_postal = '67 100';

-- complèter les départements et leurs code par une jointure spatiale
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

update citik.citik_humains
set lon = '0.00'
where id = '176022';

--- correction des dates de piqure saisies
-- suppression de l'heure séparées par un tiret
update citik.citik_humains
set date_piqure_saisie = split_part(date_piqure_saisie, ' - ', 1)
;

-- suppression de l'heure séparée par un espace
update citik.citik_humains
set date_piqure_saisie = split_part(date_piqure_saisie, ' ', 1)
where annee_extract = '2018'
and date_piqure_saisie ~ '[0-9][0-9]/[0-9][0-9]/20[0-9][0-9]\s{1,}[0-9][0-9]:[0-9][0-9]:[0-9][0-9]'
;

-- remplacement de tous les caractères de séparation par un slash
update citik.citik_humains
set date_piqure_saisie = regexp_replace(date_piqure_saisie, '[\s\-:.\\]+', '/', 'g')
where date_piqure_saisie ~ '^\s*(0?[1-9]|1[0-9]|2[0-9]|3[0-1])?\s*[\s\-:.\\]\s*(0?[1-9]|1[0-2])?\s*[\s\-:.\\]\s*(19|20)?[0-9][0-9]\s*$'
;

-- extraction des dates dont la partie la plus à gauche est valide
update citik.citik_humains
set date_piqure_saisie = split_part(date_piqure_saisie, ' ', 1)
where annee_extract ~ '2018|2017'
and date_piqure_saisie ~ '^\s*(0?[1-9]|1[0-9]|2[0-9]|3[0-1])?\s*[\s/\-:.\\]\s*(0?[1-9]|1[0-2])?\s*[\s/\-:.\\]\s*(19|20)?[0-9][0-9]\s*[a-zA-Z]+'
;

--- remplacement des mois en lettre par un format valide
update citik.citik_humains
set date_piqure_saisie = regexp_replace(date_piqure_saisie, '\s+juin\s+', '/06/', 'i')
where date_piqure_saisie ~ '^\s*(0?[1-9]|1[0-9]|2[0-9]|3[0-1])?\s*[\s/\-:.\\]\s*[a-zA-Z]+\s*[\s/\-:.\\]\s*(19|20)?[0-9][0-9]\s*$'
;
--
update citik.citik_humains
set date_piqure_saisie = regexp_replace(date_piqure_saisie, '\s+juillet\s+', '/07/', 'i')
where date_piqure_saisie ~ '^\s*(0?[1-9]|1[0-9]|2[0-9]|3[0-1])?\s*[\s/\-:.\\]\s*[a-zA-Z]+\s*[\s/\-:.\\]\s*(19|20)?[0-9][0-9]\s*$'
;
--
update citik.citik_humains
set date_piqure_saisie = regexp_replace(date_piqure_saisie, '\s+mai\s+', '/05/', 'i')
where date_piqure_saisie ~ '^\s*(0?[1-9]|1[0-9]|2[0-9]|3[0-1])?\s*[\s/\-:.\\]\s*[a-zA-Z]+\s*[\s/\-:.\\]\s*(19|20)?[0-9][0-9]\s*$'
;
--
update citik.citik_humains
set date_piqure_saisie = regexp_replace(date_piqure_saisie, '\s+avril\s+', '/04/', 'i')
where date_piqure_saisie ~ '^\s*(0?[1-9]|1[0-9]|2[0-9]|3[0-1])?\s*[\s/\-:.\\]\s*[a-zA-Z]+\s*[\s/\-:.\\]\s*(19|20)?[0-9][0-9]\s*$'
;
--
update citik.citik_humains
set date_piqure_saisie = regexp_replace(date_piqure_saisie, '\s+mars\s+', '/03/', 'i')
where date_piqure_saisie ~ '^\s*(0?[1-9]|1[0-9]|2[0-9]|3[0-1])?\s*[\s/\-:.\\]\s*[a-zA-Z]+\s*[\s/\-:.\\]\s*(19|20)?[0-9][0-9]\s*$'
;
--
update citik.citik_humains
set date_piqure_saisie = regexp_replace(date_piqure_saisie, '\s+(aot|aout)\s+', '/08/', 'i')
where date_piqure_saisie ~ '^\s*(0?[1-9]|1[0-9]|2[0-9]|3[0-1])?\s*[\s/\-:.\\]\s*[a-zA-Z]+\s*[\s/\-:.\\]\s*(19|20)?[0-9][0-9]\s*$'
;
--
update citik.citik_humains
set date_piqure_saisie = regexp_replace(date_piqure_saisie, '\s+octobre\s+', '/10/', 'i')
where date_piqure_saisie ~ '^\s*(0?[1-9]|1[0-9]|2[0-9]|3[0-1])?\s*[\s/\-:.\\]\s*[a-zA-Z]+\s*[\s/\-:.\\]\s*(19|20)?[0-9][0-9]\s*$'
;

-- correction de l'année par l'ajout de '20' pour obtenir un format ISO
update citik.citik_humains
set date_piqure_saisie = regexp_replace(date_piqure_saisie, '/16\s*$', '/2016', 'g')
where date_piqure_saisie  ~ '^\s*(0?[1-9]|1[0-9]|2[0-9]|3[0-1])?\s*[\s/\-:.\\]\s*(0?[1-9]|1[0-2])?\s*[\s/\-:.\\]\s*[0-9][0-9]\s*$'
;
update citik.citik_humains
set date_piqure_saisie = regexp_replace(date_piqure_saisie, '/17\s*$', '/2017', 'g')
where date_piqure_saisie  ~ '^\s*(0?[1-9]|1[0-9]|2[0-9]|3[0-1])?\s*[\s/\-:.\\]\s*(0?[1-9]|1[0-2])?\s*[\s/\-:.\\]\s*[0-9][0-9]\s*$'
;
update citik.citik_humains
set date_piqure_saisie = regexp_replace(date_piqure_saisie, '/18\s*$', '/2018', 'g')
where date_piqure_saisie  ~ '^\s*(0?[1-9]|1[0-9]|2[0-9]|3[0-1])?\s*[\s/\-:.\\]\s*(0?[1-9]|1[0-2])?\s*[\s/\-:.\\]\s*[0-9][0-9]\s*$'
;
update citik.citik_humains
set date_piqure_saisie = regexp_replace(date_piqure_saisie, '/19\s*$', '/2019', 'g')
where date_piqure_saisie  ~ '^\s*(0?[1-9]|1[0-9]|2[0-9]|3[0-1])?\s*[\s/\-:.\\]\s*(0?[1-9]|1[0-2])?\s*[\s/\-:.\\]\s*[0-9][0-9]\s*$'
;
update citik.citik_humains
set date_piqure_saisie = regexp_replace(date_piqure_saisie, '/20\s*$', '/2020', 'g')
where date_piqure_saisie  ~ '^\s*(0?[1-9]|1[0-9]|2[0-9]|3[0-1])?\s*[\s/\-:.\\]\s*(0?[1-9]|1[0-2])?\s*[\s/\-:.\\]\s*[0-9][0-9]\s*$'
;

update citik.citik_humains
set date_piqure_saisie = regexp_replace(date_piqure_saisie, '/2027\s*$', '/2017', 'g')
where date_piqure_saisie  ~ '^\s*(0?[1-9]|1[0-9]|2[0-9]|3[0-1])?\s*[\s/\-:.\\]\s*(0?[1-9]|1[0-2])?\s*[\s/\-:.\\]\s*(20)?[0-9][0-9]\s*$'
;

update citik.citik_humains
set date_piqure_saisie = regexp_replace(date_piqure_saisie, '/2028\s*$', '/2018', 'g')
where date_piqure_saisie  ~ '^\s*(0?[1-9]|1[0-9]|2[0-9]|3[0-1])?\s*[\s/\-:.\\]\s*(0?[1-9]|1[0-2])?\s*[\s/\-:.\\]\s*(20)?[0-9][0-9]\s*$'
;

-- creation de la table propre
drop table if exists citik.citik_humains_clean;
--
create table citik.citik_humains_clean as (
-- 
	SELECT * 
	from citik.citik_humains as ch
	where ch.date_piqure_saisie !~ '^$'
	and ch.date_piqure_saisie ~ '^\s*(0?[1-9]|1[0-9]|2[0-9]|3[0-1])?\s*[\s/\-:.\\]\s*(0?[1-9]|1[0-2])?\s*[\s/\-:.\\]\s*(19|20)[0-9][0-9]\s*$'
	and ch.lat ~ '^[0-9]?[0-9]\.[0-9]?[0-9]{1,}$'
	and ch.lon ~ '^-?[0-9]?[0-9]\.[0-9]?[0-9]{1,}$'
	order by ch.id::int asc
--
);

-- élimination des espaces dans les fins des dates
update citik.citik_humains_clean
set date_piqure_saisie = regexp_replace(date_piqure_saisie, '\s*$', '', 'g')
;
-- recorrection des séparateurs irréguliers restants
update citik.citik_humains_clean
set date_piqure_saisie = regexp_replace(date_piqure_saisie, '[\s\-:.\\]+', '/', 'g')
where date_piqure_saisie !~ '^\s*(0?[1-9]|1[0-9]|2[0-9]|3[0-1])?\s*/\s*(0?[1-9]|1[0-2])?\s*/\s*(19|20)?[0-9][0-9]\s*$'
;

-- élimination des espaces dans les  dates
update citik.citik_humains_clean
set date_piqure_saisie = regexp_replace(date_piqure_saisie, '\s*', '', 'g')
;

update citik.citik_humains_clean
set lat = regexp_replace(lat, '\s*', '', 'g')
;
update citik.citik_humains_clean
set lon = regexp_replace(lon, '\s*', '', 'g')
;



--------------------------------------------------------------------------
-- Définition des types de données des colonnes
alter table citik.citik_humains_clean

alter column lat set data type float using lat::float,
alter column lon set data type float using lon::float,
alter column date_piqure_saisie set data type date using date_piqure_saisie::date,
alter column nbr_tique set data type int using nullif(nbr_tique, '')::int,
alter column geom set data type geometry using geom::geometry,
alter column date_server set data type timestamp using nullif(date_server, '')::timestamp
;
------------- Affichage du résultat --------------
select * from citik.citik_humains_clean as chc
order by chc.id::int asc
;

/* -- Définition des types de données des colonnes

alter table citik.citik_humains_clean_weather
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
