library(hubeau)
library(sf)
library(mapview)
library(tidyverse)
library(stringr)
Carto_pollutions <- function(){
if (!exists("Stations", environment())){
      Stations <- get_hydrobio_stations_hydrobio(code_region="53") 
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

#mapview::mapview(stations_geo, label = "libelle_station_hydrobio")

Pollutions <- readxl::read_xlsx('Data/Remqual2019-2023.xlsx',sheet=1)
Acartographier <- left_join(stations_geo,Pollutions,by="cdstation")
Stations_DCE <- stations_geo
#traiter les cas où on a un x y et pas de code station
Pollutions_sans_station <- filter(Pollutions,Pollutions$cdstation=="0") %>% filter(X_L93!=0) %>% 
  sf::st_as_sf(coords = c("X_L93", "Y_L93"), crs = 2154) 
Pollutions_sans_station$cdstation <- paste("pas de code station","/", Pollutions_sans_station$nom_station,"/",Pollutions_sans_station$commune)
#on integre les geometries issues des stations ou des lieux de pollution
Acartographier <- dplyr::bind_rows(list(Acartographier,Pollutions_sans_station))

Pollutions_2023 <- filter(Acartographier,!is.na(Acartographier$remarques2023)) %>% 
  select(cdstation,libelle_station_hydrobio,remarques2023)
Pollutions_2024 <- filter(Acartographier,!is.na(Acartographier$remarques2024)) %>% 
  select(cdstation,libelle_station_hydrobio,remarques2024)

                                                 
  m =  mapview(Stations_DCE,col.regions="grey",cex=3,alpha=1, label = "libelle_station_hydrobio")+
  mapview::mapview(Pollutions_2023, label = "libelle_station_hydrobio",col.regions='blue') +
  mapview::mapview(Pollutions_2024, label = "libelle_station_hydrobio",col.regions='green')
  
 

  mapshot(m, url = paste0(getwd(), "/Cartographie/map.html"))
   browseURL("Cartographie/map.html")

}
Carto_pollutions()
