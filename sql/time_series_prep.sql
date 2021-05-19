select * from citik.citik_humains_clean_weather_strict
where id = '178062';

select * from citik.citik_humains_clean_weather_strict
where departement = '';

select distinct region 
from citik.citik_humains_clean_weather_strict 
where id = '178062'
order by region;

update citik.citik_humains_clean_weather_strict 
set region = 'Normandie'
where id = '178062';

SELECT *, ceil(humidite) FROM meteo.mf_synop42_avg;

delete from citik.citik_humains_clean_weather_strict
where id = '178062'

select sex_pique, sum(nbr_tique) from citik.citik_humains_clean_weather_strict where sex_pique != '' group by sex_pique

select distinct chef_lieu 
from citik.citik_humains_clean_weather_strict
order by chef_lieu;

select count(id) from citik.citik_humains_clean_weather_strict
where nbr_tique = 0;

select sum(nbr_tique) from citik.citik_humains_clean_weather_strict

select median(nbr_tique) from citik.citik_humains_clean_weather_strict

select count(id) from citik.citik_humains_clean_weather_strict
where sex_pique = '';




SELECT 
*,
	CASE
		WHEN departement_code ~ '26|07|42|69D|69M|01|74|73|38' THEN  'Rhone-Alpes'
		WHEN departement_code ~ '54|55|57|67|68|88' THEN 'Alsace-Lorraine'
		ELSE  'Île-de-France' 
	END as study_area

FROM citik.citik_humains_clean_weather_strict
WHERE (sex_pique != '' AND nbr_tique < 25)
-- Departements of Île-de-France
AND (region = 'Île-de-France'
-- Departements of Rhone-Alpes
OR departement_code ~* '26|07|42|69D|69M|01|74|73|38'
-- -- Departements of Alsace-Lorraine
OR departement_code ~* '54|55|57|67|68|88')


SELECT annee_extract as year, sex_pique sex, sum(nbr_tique) as sum
FROM citik.citik_humains_clean_weather_strict
WHERE (sex_pique != '' AND nbr_tique < 25)
group by annee_extract, sex_pique
order by annee_extract, sex_pique




select row_number() over() as gid,
	(st_dump(st_generatepoints(geom, 100))).geom as geom
from geography.france_hexagone
























;