adresses_to_geometry <- function(mydf){
  # retourne un tableau avec géométrie en fonction d'adresses
  # en entree un DF, adresses dans la colonne loc
  # en sortie une liste contenant le tableau avec géométries et
  # le tableau des adresses non converties
  # exemple d'utilisation
  #mydf <- tibble::tribble(
  #     ~name,                  ~loc,
  #     "maison",          "35, avenue des erables, le rheu",
  #     "parents", "sarzeau",     
  #     "lieu dit a vezin",         "pontchateau, 35132 vezin le coquet"                                  
  # )
  # points <- adresses_to_geometry(mydf) 
  # mapview(points$geometrie)
  # data.table::data.table(points$defaut)
  
  
  
  library(tidygeocoder)
  library(sf)
  library(dplyr)
  library(mapview)
  result_tidygeocoder=tidygeocoder::geocode(mydf,loc,method='osm')
  adresses_non_geocodees <- filter(result_tidygeocoder,is.na(lat))
  pointsresult <- st_as_sf(filter(result_tidygeocoder,!is.na(lat)),coords = c("long","lat"),crs=4326) 
  if (dim(pointsresult)[1]!=0){
    mapview(pointsresult)
  }
  return(list("geometrie"=pointsresult,"defaut"=adresses_non_geocodees))
                                                                                   
}

