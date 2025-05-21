recup_donnees56 <- function(){
  library(dplyr)
  library(sf)
  Tableaubrut <- readxl::read_xlsx('../Data/Pollutions organiques_yannick_chauvin sd56.xlsx') %>% 
    sf::st_as_sf(coords = c( "Lon (WGS84)","Lat (WGS84)"), crs = 4326)
  return(Tableaubrut)
  
}
