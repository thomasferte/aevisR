TendrilAETimeSeries <- function(baseEI,baseTr, baseDates,
                                idvar, Termsvar, EIdatestart_var, ARMvar, tttdebdate_var,
                                CODlist=NULL, listcol = NULL){
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
                      baseDates %>% select(id_pat,tttdebdate), by="id_pat")
  
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
  
  ##on remet les valeurs d'origine pour les bras de traitement
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
  
  #table de données pour le TimeSeries
  t <- plot_timeseries(data.Tendril) 
  
  if (!is.null(CODlist)){
    p2 <- ggplot(t$data,aes(x=StartDay, y=Net, group=Terms)) +
      geom_line(linewidth=1, aes(color = Terms)) +
      {if (!is.null(listcol)) scale_color_manual(breaks = CODlist, values = listcol)} +
      {if (is.null(listcol)) scale_color_manual(breaks = CODlist, values = rep("black",length(CODlist)))} +
      geom_point(size=2, aes(color = Terms)) +
      geom_hline(yintercept = 0, col="red",linetype=2, linewidth=1) +
      labs(x="Day since start of treatment", y=paste0("Net event - ",list_ARM[1]," over ",list_ARM[2])) +
      gghighlight::gghighlight(t$data$Terms %in% CODlist,
                               unhighlighted_params = list(linewidth = 1, colour = alpha("gray", 0.4))
      ) +
      theme(axis.text = element_text(size=12), #taille des labels des axes
            axis.title = element_text(size = 12), #taille des titres des axes
            panel.background = element_blank(), #pas d'arrière plan
            panel.grid.major = element_line(color = "gray60", linewidth = 0.5, linetype = 1), #grille de lecture de couleur grise et d'épaisseur 0.5
            axis.line = element_line(color="gray30"), #couleur de la ligne de chacun des axes
            legend.position = "none" #ne pas afficher la légende
      )
    
  } else  if (is.null(CODlist)) {
    anno <- t$data %>% group_by(Terms) %>% filter(StartDay==max(StartDay)) %>% arrange(Terms)
    anno <- anno %>% select(Terms,StartDay,Net)
    
    p2 <- ggplot(t$data,aes(x=StartDay, y=Net, group=Terms)) +
      geom_line(linewidth=1) + 
      geom_point(size=2) +
      geom_hline(yintercept = 0, col="red",linetype=2, linewidth=1) +
      labs(x="Day since start of treatment", y=paste0("Net event - ",list_ARM[1]," over ",list_ARM[2])) + 
      geom_label_repel(data=anno, aes(x=StartDay, y=Net, label=Terms), 
                       color="black",
                       min.segment.length = 0.1, force = 8,
                       max.overlaps = 30,
                       direction = "x") +
      theme(axis.text = element_text(size=12), #taille des labels des axes
            axis.title = element_text(size = 12), #taille des titres des axes
            panel.background = element_blank(), #pas d'arrière plan
            panel.grid.major = element_line(color = "gray60", linewidth = 0.5, linetype = 1), #grille de lecture de couleur grise et d'épaisseur 0.5
            axis.line = element_line(color="gray30"), #couleur de la ligne de chacun des axes
            legend.position = "none" #ne pas afficher la légende
      )
  }
  
  suppressWarnings(print(p2))
}
