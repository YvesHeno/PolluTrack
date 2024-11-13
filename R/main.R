library(hubeau)
library(sf)
library(mapview)
library(tidyverse)
library(stringr)

#' Title
#'
#' @return
#' @export
#'une page web contenant les localisations des remontées de pollutions
#'et une caractérisation de celles ci
#'les pollutions snt bancarisées dans un fichier excel Data/Remqual2019-2023.xlsx
#'mais ce n'est pas l'ideal.
#'reflechir à d'autres facons de bancariser la donnée (ex avec grist ?)
#' @examples
#' Carto_pollutions()
#' 
Carto_pollutions <- function(){
  # pour ne pas se recoltiner l'appel à hubeau on charge une fois les stations DCE et 
  # on sauve le fichier
  if (!file.exists("Data/Stations.Rdata")){
    Stations <- get_hydrobio_stations_hydrobio(code_region="53") 
    save(Stations,file="Data/Stations.Rdata")
  }  else {
    load("Data/Stations.Rdata")
  }
  colnames(Stations)[1] <- "cdstation"
  Stations <- Stations[,1:11]
  stations_geo <- Stations %>% 
    select(cdstation,
           libelle_station_hydrobio,
           coordonnee_x,
           coordonnee_y) %>% 
    sf::st_as_sf(coords = c("coordonnee_x", "coordonnee_y"), crs = 2154)
  colnames(stations_geo)[1] <- "cdstation"
  
  
  #lecture du fichier excel contenant les signalements
  Pollutions <- readxl::read_xlsx('Data/Remqual2019-2023.xlsx',sheet=1)
  Acartographier <- left_join(stations_geo,Pollutions,by="cdstation")
  
  Stations_DCE <- stations_geo#juste pour avoir un nom de couche parlant dans la carto
  
  Pollutions_sans_station <- filter(Pollutions,Pollutions$cdstation=="0") %>% filter(X_L93!=0) %>% 
    sf::st_as_sf(coords = c("X_L93", "Y_L93"), crs = 2154) 
  #attribution de champ auto si pas de code station
  Pollutions_sans_station$cdstation <- paste("pas de code station","/", Pollutions_sans_station$nom_station,"/",Pollutions_sans_station$commune)
  
  #on integre les geometries issues des stations ou des lieux de pollution
  Acartographier <- dplyr::bind_rows(list(Acartographier,Pollutions_sans_station))
  
  #on trie
  Pollutions_2023 <- filter(Acartographier,!is.na(Acartographier$remarques2023)) %>% 
    select(cdstation,libelle_station_hydrobio,remarques2023)
  Pollutions_2024 <- filter(Acartographier,!is.na(Acartographier$remarques2024)) %>% 
    select(cdstation,libelle_station_hydrobio,remarques2024)
  
  #carto         
  mapviewOptions(basemaps = c( "OpenStreetMap.DE"))
 # mapview(Stations_DCE,col.regions="black",cex=1.5, label = "libelle_station_hydrobio")+
   
     mapview::mapview(Pollutions_2023, label = "libelle_station_hydrobio",col.regions='blue') +
    mapview::mapview(Pollutions_2024, label = "libelle_station_hydrobio",col.regions='green')

  
  
  #enregistrement de la carte
  #creer le dossier s'il n'existe pas
  if(!dir.exists("Cartographie")){
    dir.create("Cartographie")
  }
  mapshot(m, url = paste0(getwd(), "/Cartographie/map.html"))
  #affichage
  browseURL("Cartographie/map.html")
  
}


Carto_pollutions()

