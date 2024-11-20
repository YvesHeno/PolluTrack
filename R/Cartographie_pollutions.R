Cartographie_pollutions <- function()
    {
      
      
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