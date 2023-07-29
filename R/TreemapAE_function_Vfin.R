TreemapAE <- function(baseEI,baseTr, 
                      idvar, Termsvar, SOCvar=NULL, gradevar, TTTYN = NULL, ARMvar,
                      choixSOC=NULL, listcol, ARMe=NULL, caption=TRUE){
  #remplacement des noms de variables
  baseEI <- baseEI %>% rename("id_pat" = idvar,
                              "COD" = Termsvar,
                              "Grade" = gradevar)
  if (!is.null(SOCvar)) baseEI <- baseEI %>% rename("SOC" = SOCvar)
  baseTr <- baseTr %>% rename("id_pat" = idvar,
                              "ARM" = ARMvar)
  if (!is.null(TTTYN)) baseTr <- baseTr %>% rename("TTTYN" = TTTYN)
  
  #on récupère le nombre de modalité de la variable Grade pour les échelles de couleurs des graphiques
  vect_grade <- sort(unique((baseEI$Grade)))
  
  list_ARM <- unique(baseTr$ARM)
  if (is.null(ARMe)) {
    #Liste des id patients dans le bras numéro 1
    list_pat1 <- unique(baseTr$id_pat[baseTr$ARM == list_ARM[1]]) 
    #Liste des id patients dans le bras numéro 2
    list_pat2 <- unique(baseTr$id_pat[baseTr$ARM == list_ARM[2]])
  } else if (!is.null(ARMe)){
    l2 <- unique(baseTr$ARM) 
    if (!(ARMe %in% l2)) return("Nom de bras de traitement non correct ou non présent dans la base.") 
    list_ARM[1] <- ARMe
    list_ARM[2] <- l2[l2 != ARMe]
    
    #Liste des id patients dans le bras numéro 1
    list_pat1 <- unique(baseTr$id_pat[baseTr$ARM == list_ARM[1]]) 
    #Liste des id patients dans le bras numéro 2
    list_pat2 <- unique(baseTr$id_pat[baseTr$ARM == list_ARM[2]])
  }
  #Ajouter une colonne ARM dans la table data en faisant correspondre les id_pat selon la liste où ils sont présents
  baseEI$ARM <- ifelse(baseEI$id_pat %in% list_pat1, "armH", "armB")
  baseTr$ARM <- ifelse(baseTr$ARM==list_ARM[1], "armH","armB")
  
  if (is.null(TTTYN)){
    df_Tr2 <- baseTr
  } else {
    df_Tr2  <- baseTr[baseTr$TTTYN=="Yes",]
  }
  USU_distinct  <- df_Tr2  %>% select(id_pat, ARM) %>% distinct(id_pat, ARM)
  frq2 <- data.frame(xtabs(~ ARM, data=USU_distinct))
  
  if(!is.null(SOCvar)){
    df_AE3 <- baseEI %>% 
      select(SOC,COD,ARM,Grade) %>%
      group_by(SOC,COD,ARM) %>% summarise(count=n()) 
    df_gmax2 <- baseEI %>% 
      select(SOC,COD,ARM,Grade) %>%
      group_by(SOC,COD,ARM) %>% dplyr::filter(Grade == max(Grade))
    
    df_gmax2 <- df_gmax2 %>% distinct(SOC, COD, ARM, Grade)
    data_plot <- merge(df_AE3,df_gmax2, by=c("SOC","COD","ARM"))
  } else {
    df_AE3 <- baseEI %>% 
      select(COD,ARM,Grade) %>%
      group_by(COD,ARM) %>% summarise(count=n()) 
    df_gmax2 <- baseEI %>% 
      select(COD,ARM,Grade) %>%
      group_by(COD,ARM) %>% dplyr::filter(Grade == max(Grade))
    
    df_gmax2 <- df_gmax2 %>% distinct(COD, ARM, Grade)
    data_plot <- merge(df_AE3,df_gmax2, by=c("COD","ARM"))
  }
  
  
  #pour afficher le pourcentage de patient atteint en plus dans la Treemap (notamment dans les version avec 1 SOC selctionnée)
  #on doit d'abord récupérer le nombre de patient atteint
  #en effet la variable count de df_AE3 correspont au nombre total d'EI on peut donc avoir plusieurs occurrence
  #pour calculer le pourcentage de patients on ne doit garder qu'une occurrence par patient
  df_AE4 <- baseEI %>%
    select(COD, ARM ,id_pat) %>% distinct(COD,id_pat, ARM) %>% group_by(COD, ARM) %>% summarise(count=n())
  
  df_AE4$pctPat <- round(ifelse(df_AE4$ARM=="armH", df_AE4$count/frq2$Freq[frq2$ARM=="armH"],
                                df_AE4$count/frq2$Freq[frq2$ARM=="armB"])*100,1)
  
  #on merge avec la table précédent pour attribuer les pourcentages
  data_plot <- merge(data_plot,df_AE4 %>% select(COD,ARM,pctPat), by=c("COD","ARM"))
  
  #caption
  labcap <- "Chacun des rectangles principaux (1 par groupe de taitement) représente 100% des patients présents dans le groupe.
  Ce rectangle est divisé selon les SOC, puis selon Terms (PT ou LLT)  en de petits rectangles représentant la part de patients ayant eu chaque EI.
  Les sous-rectangles sont colorés selon le grade max enregistré pour chaque type d'EI selon la grille de couleurs choisie.
  A noter, que les rectangles des SOC sont triés du coin inférieur gauche au coin supérieur droit du pourcentage plus plus grand au plus petit (idem pour les EIs à l'intérieur de chaque SOC)"
  
  if (!is.null(choixSOC)){
    if (!(choixSOC %in% unique(data_plot$SOC))) return("SOC choisie non présente dans la base")
    data_plot <- data_plot[data_plot$SOC==choixSOC,]
    data_plot$pctEvents <- round((data_plot$count / sum(data_plot$count))*100,1)
    
    pH <- ggplot(data_plot %>% filter(ARM=="armH"), 
                 aes(area= count, fill=as.factor(Grade),
                     label= paste(COD,"\n",count,"(",pctEvents,"% of events)","\n",pctPat,"% of patient"))) +
      geom_treemap() +
      {if (!is.null(SOCvar)) geom_treemap_subgroup_border(aes(subgroup=SOC),color="gray20")} +
      {if (!is.null(SOCvar)) geom_treemap_subgroup_text(aes(subgroup=SOC),place="topleft", reflow = T, alpha=0.7, colour = "gray25",
                                 min.size=0, padding.y = grid::unit(-50,"mm"))} +
      geom_treemap_text(colour="black", place="centre", size=18) +
      scale_fill_manual(name="Grade max atteint", breaks = vect_grade,
                        values = listcol) +
      ggtitle(paste0(choixSOC,"\n",list_ARM[1])) +
      theme(legend.position = "bottom",
            legend.text = element_text(size=10),
            legend.title = element_text(size = 10))
    
    pB <- ggplot(data_plot %>% filter(ARM=="armB"), 
                 aes(area= count, fill=as.factor(Grade),
                     label= paste(COD,"\n",count,"(",pctEvents,"% of events)","\n",pctPat,"% of patient"))) +
      geom_treemap() +
      {if(!is.null(SOCvar)) geom_treemap_subgroup_border(aes(subgroup=SOC),color="gray20")} +
      {if(!is.null(SOCvar)) geom_treemap_subgroup_text(aes(subgroup=SOC),place="topleft", reflow = T, alpha=0.7, colour = "gray25",
                                 min.size=0, padding.y = grid::unit(-50,"mm"))} +
      geom_treemap_text(colour="black", place="centre", size=18) +
      scale_fill_manual(name="Grade max atteint", breaks = vect_grade,
                        values = listcol) +
      ggtitle(list_ARM[2]) +
      {if (caption==TRUE) labs(caption=labcap)} +
      theme(legend.position = "bottom",
            legend.text = element_text(size=10),
            legend.title = element_text(size = 10))
  } else if (is.null(choixSOC)){
    ### pH
    if (!is.null(SOCvar)){
      pH <- ggplot(data_plot %>% filter(ARM=="armH"), 
                   aes(area= count, fill=as.factor(Grade), subgroup=SOC,
                       label= stringr::str_wrap(paste(COD,count,sep="\n"), 2))) + 
        geom_treemap() +
        geom_treemap_subgroup_border(color="gray20") +
        geom_treemap_subgroup_text(place="centre", reflow = T, alpha=0.7, colour = "gray25", 
                                   min.size=0)
    } else {
      pH <- ggplot(data_plot %>% filter(ARM=="armH"), 
                   aes(area= count, fill=as.factor(Grade),
                       label= stringr::str_wrap(paste(COD,count,sep="\n"), 2))) + 
        geom_treemap()
    }
    pH <- pH +
      geom_treemap_text(colour="white", place="centre", size=15) +
      scale_fill_manual(name="Grade max atteint", breaks = vect_grade,
                        values = listcol) +
      ggtitle(list_ARM[1]) +
      theme(legend.position = "bottom")
    
    ###pB
    if (!is.null(SOCvar)){
    pB <- ggplot(data_plot %>% filter(ARM=="armB"), 
                 aes(area= count, fill=as.factor(Grade), subgroup=SOC, 
                     label= stringr::str_wrap(paste(COD,count,sep="\n"), 2))) + 
      geom_treemap() +
      geom_treemap_subgroup_border(color="gray20") +
      geom_treemap_subgroup_text(place="centre", reflow = T, alpha=0.7, colour = "gray25", 
                                 min.size=0)
    }else {
      pB <- ggplot(data_plot %>% filter(ARM=="armB"), 
                   aes(area= count, fill=as.factor(Grade), 
                       label= stringr::str_wrap(paste(COD,count,sep="\n"), 2))) + 
        geom_treemap()
    }
      pB <- pB +
      geom_treemap_text(colour="white", place="centre", size=15) +
      scale_fill_manual(name="Grade max atteint", breaks = vect_grade,
                        values = listcol) +
      ggtitle(list_ARM[2]) +
      {if (caption==TRUE) labs(caption=labcap)} +
      theme(legend.position = "bottom")
  }
  if(caption==TRUE){
    plot_grid(pH,pB,nrow = 2,rel_heights = c(0.45,0.55))
  } else {
    plot_grid(pH,pB,nrow = 2,rel_heights = c(0.5,0.5))
  }
}