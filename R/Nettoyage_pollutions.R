Nettoyage_pollutions <- function(Pollutions){
  Taille <- dim(Pollutions)
  Pollutions <- Pollutions[,3:Taille[2]-2] %>% 
    mutate(loc=paste(Adresse,ifelse(nchar(Adresse)==0,"",","),Commune))#le ifselse pour eviter une virgule si pas d'adresse mais juste commune
  Pollutions$Type_de_pollution <- unlist(Pollutions$Type_de_pollution)[2]#issu de grist on a une liste, on ne garde que l'info interessante
  
 return (Pollutions) 
}
