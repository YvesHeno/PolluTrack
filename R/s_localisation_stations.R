library(hubeau)
library(sf)
library(mapview)
library(tidyverse)
library(stringr)
Carto_stations <- function(){
  if (!exists("Stations", environment())){
    Stations <- get_hydrobio_stations_hydrobio(code_region="53") 
  }

  colnames(Stations)[1] <- "cdstation"
  stations_geo <- Stations %>% 
    select(cdstation,
           libelle_station_hydrobio,
           coordonnee_x,
           coordonnee_y,
           libelle_cours_eau,
           libelle_masse_eau,
           code_masse_eau,
           libelle_masse_eau,
           code_departement) %>% 
    sf::st_as_sf(coords = c("coordonnee_x", "coordonnee_y"), crs = 2154)
  colnames(stations_geo)[1] <- "cdstation"
  
  mapview::mapview(stations_geo, 
                   label = "libelle_station_hydrobio",zcol="code_masse_eau",
                   homebutton = mapviewGetOption("homebutton"),legend='false')
}
Carto_stations()
