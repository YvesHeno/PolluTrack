convertir_en_DF <-  function(Tab){
  #convertir un format large liste recupere de python en DF
  #Tab est un fichier de N enregistrements à 14 champs
  Nli <- length(Tab)
  Ncol <- length(Tab[[1]])
  nomscol <- names(unlist(Tab[[1]]))
  Tab_DF <- NULL
  for (i in 1:Nli)
    {
    Ligne <- unlist(Tab[[i]])
    b <- NULL
    for(j in 3:Ncol-1)#positio 0 et 1 sont des élements contruits artificiellement par apel python
      {
    b <- c(b,Ligne[j])  
    }
    Tab_DF <- rbind(Tab_DF,b)
 
  }
  Tab_DF <- data.frame(Tab_DF)
  noms <- nomscol[3:Ncol-1]
  noms[2] <- "libelle"
  noms[3] <- 'commune'
  
colnames(Tab_DF) <- noms

  return(Tab_DF)
}
tabsortie <- convertir_en_DF(Tableau)

