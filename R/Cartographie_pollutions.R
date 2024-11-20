Cartographie_pollutions <- function(Acartographier,Stations_DCE)
    {
      
      
     
      #on trie
      Pollutions_2023 <- filter(Acartographier,!is.na(Acartographier$remarques2023)) %>% 
        select(cdstation,libelle_station_hydrobio,remarques2023)
      Pollutions_2024 <- filter(Acartographier,!is.na(Acartographier$remarques2024)) %>% 
        select(cdstation,libelle_station_hydrobio,remarques2024)
      
      #carto         
      mapviewOptions(basemaps = c( "OpenStreetMap.DE"))
      m= mapview(Stations_DCE,col.regions="black",cex=1.5, label = "libelle_station_hydrobio")+
        # le rajout des stations DCE peut allourdir la carte, a voir
        mapview::mapview(Pollutions_2023, label = "libelle_station_hydrobio",col.regions='blue') +
        mapview::mapview(Pollutions_2024, label = "libelle_station_hydrobio",col.regions='green')
      
      #pour les popup rearder dans options de leaflet leafpop
      # qd on a un objet mapview noté m, alors m@map est un objet leaflet
      
      
      #enregistrement de la carte
      #creer le dossier s'il n'existe pas
      if(!dir.exists("Cartographie")){
        dir.create("Cartographie")
      }
      mapshot(m, url = paste0(getwd(), "/Cartographie/map.html"))
      #affichage
      browseURL("Cartographie/map.html")
      
    }