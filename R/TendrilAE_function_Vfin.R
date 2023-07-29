TendrilAE <- function(baseEI, baseTr, baseDates, 
                      idvar, Termsvar, EIdatestart_var, ARMvar, tttdebdate_var,
                      coltype = NULL, caption=TRUE){
  #remplacement des noms de variables
  baseEI <- baseEI %>% rename("id_pat" = idvar,
                              "COD" = Termsvar,
                              "aedatestart" = EIdatestart_var)
  baseTr <- baseTr %>% rename("id_pat" = idvar,
                              "ARM" = ARMvar)
  baseDates <- baseDates %>% rename("id_pat" = idvar,
                                    "tttdebdate" = tttdebdate_var)
  
  #liste des groupes de traitement de la table df_Tr
  list_ARM <- unique(baseTr$ARM)
  #Liste des id patients dans le bras numéro 1
  list_pat1 <- unique(baseTr$id_pat[baseTr$ARM == list_ARM[1]]) 
  #Liste des id patients dans le bras numéro 2
  list_pat2 <- unique(baseTr$id_pat[baseTr$ARM == list_ARM[2]])
  #Ajouter une colonne ARM dans la table data en faisant correspondre les id_pat selon la liste où ils sont présents
  baseEI$ARM <- ifelse(baseEI$id_pat %in% list_pat1, "arm1", "arm2")
  
  #jointure pour récupérer la date de début de traitement de chaque individu
  baseEI2 <- left_join(baseEI %>% select(id_pat, ARM, COD, aedatestart),
                      baseDates %>% select(id_pat,tttdebdate), 
                      by="id_pat", multiple="all")
  
  #transformation en format date
  baseEI2$aedatestart <- as.Date(baseEI2$aedatestart,format = "%d/%m/%Y")
  baseEI2$tttdebdate <- as.Date(baseEI2$tttdebdate, format = "%d/%m/%Y")
  # soustraction des deux dates pour obtenir la variable Days
  baseEI2$Days <- as.numeric(baseEI2$aedatestart-baseEI2$tttdebdate)
  
  baseEI2 <- baseEI2[!is.na(baseEI2$Days),]
  #retrait des variable qui ne servent plus
  baseEI2 <- baseEI2 %>% select(-c(aedatestart,tttdebdate)) 
  #transformation en facteur de deux variable de la base
  # baseEI2$ARM <- as.factor(baseEI2$ARM)
  baseEI2$COD <- as.factor(baseEI2$COD)
  
  ##on remet mes valeurs d'origine pour les bras de traitement
  baseEI2$ARM <- ifelse(baseEI2$ARM=="arm1",as.character(list_ARM[1]),as.character(list_ARM[2]))
  
  # liste des patients avec leur bras de traitement
  SubjList <- baseEI %>% arrange(id_pat,ARM)
  SubjList$ARM <- ifelse(SubjList$ARM=="arm1",as.character(list_ARM[1]),as.character(list_ARM[2]))
  
  data.Tendril <- Tendril(mydata = baseEI2,
                          rotations = 4, #set the degree to which each event pulls a tendril in a direction
                          AEfreqThreshold = 5, #Change the number of occurrences required to be plotted
                          Tag = "COD",
                          Treatments = c(as.character(list_ARM[1]),as.character(list_ARM[2])),
                          Unique.Subject.Identifier = "id_pat",
                          Terms = "COD",
                          Treat = "ARM",
                          StartDay = "Days",
                          SubjList = SubjList,
                          SubjList.subject = "id_pat",
                          SubjList.treatment = "ARM")
  
  # vecteur pour les annotations
  # dans data.Tendril$data on prend les coordonnées x et y du dernier point de chaque Terms
  anno <- data.Tendril$data %>% group_by(Terms) %>% filter(StartDay==max(StartDay)) %>% arrange(Terms)
  anno <- anno %>% select(Terms,x,y)
  labcap <- "Pas de différence visuelle entre les bras de traitement (couleur ou autre), c’est l’interprétation du graphique qui permet de savoir si un EI est plus présent dans un bras que dans l’autre. 
  La distance entre les points est proportionnelle à l’intervalle de temps entre les évènements. 
  L'angle est dicté par un paramètre de la fonction Tendril(), fixé à rotations = 4. La branche se dirigera vers la droite si l'évènement à lieu dans le premier groupe et à gauche s'il à lieu dans l'autre groupe.
  L’évolution temporelle est représentée le long de chaque branche, c’est donc la forme qui est importante et qui porte l’information. 
  Les branches peuvent être colorés selon plusieurs variable, par exemple selon la p-value du chi-square de Pearson 
  ou selon le nombre d'évènement pour chaque type d'EI (branche) par un gradient de couleurs."
  
  if(is.null(coltype)){
    plot(data.Tendril) + 
      {if (caption==TRUE) labs(caption = labcap)} +
      geom_label(data=anno, aes(x=x,y=y, label=Terms), color="black",
                 min.segment.length = 0.1, force = 5,
                 max.overlaps = 10,
                 direction="y") 
  } else if (coltype %in% c("p.adj","fish","p","rdiff","OR","RR","FDR.tot","TermsCount")){
    plot(data.Tendril, coloring=coltype) + 
      {if (caption==TRUE) labs(caption = labcap)} +
      geom_label_repel(data=anno, aes(x=x,y=y, label=Terms), color="black",
                       min.segment.length = 0.1, force = 8,
                       max.overlaps = 30,
                       direction="y")
  } else return("Valeur non valide pour l'option coltype")
} 
