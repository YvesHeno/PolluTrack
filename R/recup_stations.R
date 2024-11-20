recup_stations <- function(){
  # pour ne pas se recoltiner l'appel à hubeau on charge une fois les stations DCE et 
  # on sauve le fichier
  if (!file.exists("Data/Stations.Rdata")){
    Stations <- get_hydrobio_stations_hydrobio(code_region="53") #53 pour la Bretagne
    save(Stations,file="Data/Stations.Rdata")
  }  else {
    load("Data/Stations.Rdata")}
  colnames(Stations)[1] <- "cdstation"
  Stations <- Stations[,1:11]
  stations_geo <- Stations %>% 
    select(cdstation,
           libelle_station_hydrobio,
           coordonnee_x,
           coordonnee_y) %>% 
    sf::st_as_sf(coords = c("coordonnee_x", "coordonnee_y"), crs = 2154)
  colnames(stations_geo)[1] <- "cdstation"
  return(stations_geo)
}
