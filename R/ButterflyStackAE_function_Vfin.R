ButterflyStackAE <- function(baseEI, baseTr, 
                             idvar, Termsvar, SOCvar, gradevar, TTTYN=NULL,ARMvar,
                             SOCchoix, ARMe=NULL, listcol, gsup=TRUE, grprk=NULL, rk="pctgrade", order="Desc"){
  #remplacement des noms de variables
  baseEI <- baseEI %>% rename("id_pat" = idvar,
                              "COD" = Termsvar,
                              "SOC" = SOCvar,
                              "Grade" = gradevar)
  baseTr <- baseTr %>% rename("id_pat" = idvar,
                              "ARM" = ARMvar)
  if (!is.null(TTTYN)) baseTr <- baseTr %>% rename("TTTYN" = TTTYN)
  
  #on récupère le nombre de modalité de la variable Grade pour les échelles de couleurs des graphiques
  vect_grade <- sort(unique((baseEI$Grade)))
  
  #### table EI 
  #liste des groupes de traitement de la table baseTr
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
    
    #Liste des id patients dans le bras numéro 1 : GC
    list_pat1 <- unique(baseTr$id_pat[baseTr$ARM == list_ARM[1]]) 
    #Liste des id patients dans le bras numéro 2 : GC + Avelumab
    list_pat2 <- unique(baseTr$id_pat[baseTr$ARM == list_ARM[2]])
  }
  #Ajouter une colonne ARM dans la table data en faisant correspondre les id_pat selon la liste où ils sont présents
  baseEI$ARM <- ifelse(baseEI$id_pat %in% list_pat1, "armG", "armD")
  baseTr$ARM <- ifelse(baseTr$ARM==list_ARM[1], "armG","armD")
  
  
  #On créé une table de fréquence **frq2** qui contient le nombre de patient dans chaque bras de traitement en tenant compte du fait qu’ils doivent avoir pris le traitement et donc être considérés comme "à risque"
  if (is.null(TTTYN)){
    df_Tr2 <- baseTr
  } else {
    df_Tr2  <- baseTr[baseTr$TTTYN=="Yes",]
  }
  USU_distinct  <- df_Tr2  %>% select(id_pat, ARM) %>% distinct(id_pat, ARM)
  frq2 <- data.frame(xtabs(~ ARM, data=USU_distinct))
  
  df2 <- baseEI %>% select(id_pat, ARM, SOC, COD, Grade) %>% arrange(id_pat, ARM,SOC,COD,desc(Grade))  
  
  #Selection des variables d'interêt
  if (!(SOCchoix %in% df2$SOC)) return("SOC choisit non présent dans la base de donnée")
  df2_SOC <- df2[df2$SOC == SOCchoix,]
  df2_SOC <- df2_SOC[!is.na(df2_SOC$Grade),] #on ne garde pas les EIs pour lesquels ont a pas l'info sur le grade (normalement pas présents la base doit être complète et nettoyée avant)
  
  
  ### Selectionner les lignes avec le grade max
  df3_SOC <- df2_SOC %>% distinct(id_pat,ARM,SOC,COD,Grade) %>%
    group_by(id_pat,SOC,COD) %>% dplyr::filter(Grade==max(Grade))
  
  df4_SOC <- df3_SOC %>% group_by(ARM, SOC, COD, Grade) %>% summarise(Count=n())
  
  ## Calculer le pourcentage par COD pour chaque groupe
  df4_SOC$pct <- ifelse(df4_SOC$ARM == "armG" ,round((df4_SOC$Count / frq2$Freq[frq2$ARM=="armG"])*100,1),
                        round((df4_SOC$Count / frq2$Freq[frq2$ARM=="armD"])*100,1))
  
  ### Division de cette sous table selon les deux bras de traitement
  df4_SOC_1 <- as.data.frame(df4_SOC[df4_SOC$ARM == "armG",])
  df4_SOC_2 <- as.data.frame(df4_SOC[df4_SOC$ARM == "armD",])
  
  
  ######################### Liste des COD ###############################
  # Car tous les COD doivent être présents dans les deux bases sinon problème pour le plot
  list_COD <- unique(df4_SOC$COD) #list des PT pour cette SOC dans toute la base (deux groupes confondus)
  list_COD_1 <- unique(df4_SOC_1$COD) #liste des PT déjà présents dans la base du groupe 1
  list_COD_2 <- unique(df4_SOC_2$COD) #liste des PT déjà présents dans la base du groupe 2
  
  ## ajout groupe 1
  add_1 <- list_COD[!(list_COD %in% list_COD_1)]  #liste des PT à ajouter dans la base du groupe1
  if (length(add_1)>0){
    for (i in 1:length(add_1)){
      new_line = list(list_ARM[1], SOCchoix, add_1[i], 1, 0, 0) #nouvelle ligne avec la SOC manquante
      df4_SOC_1 = rbind(df4_SOC_1,new_line) #ajout à la base du groupe 1
    }
  }  
  names(df4_SOC_1) <-  c("ARM", "SOC","COD", "Grade", "Count","pct")
  
  ## ajout groupe 2
  add_2 <- list_COD[!(list_COD %in% list_COD_2)]  #liste des PT à ajouter dans la base du groupe 2 
  if (length(add_2)>0){
    for (i in 1:length(add_2)){
      new_line = list(list_ARM[2], SOCchoix, add_2[i], 1, 0, 0)
      df4_SOC_2 = rbind(df4_SOC_2,new_line)
    }
  }
  names(df4_SOC_2) <-  c("ARM", "SOC","COD", "Grade", "Count","pct")
  
  if (is.null(grprk)) {table_ref <- df4_SOC_1
  } else if (grprk==list_ARM[1]) {table_ref <- df4_SOC_1
  } else if (grprk==list_ARM[2]) {table_ref <- df4_SOC_2
  }
  
  if (rk=="pcttot") {
    ################################# Ranking 1 pour le graph ##############################
    rk1 <- table_ref %>% group_by(COD) %>% summarise(Count=sum(Count))
    rk1 <- rk1[order(-rk1$Count),]
    rk1$rank <- 1:nrow(rk1)
    
    #attribution des rangs créés aux PT (ou LLT) des deux tables
    df4_SOC_2 <- left_join(df4_SOC_2,rk1[,-2], by="COD")
    df4_SOC_1 <- left_join(df4_SOC_1,rk1[,-2], by="COD")
    
  } else if (rk=="pctgrade") {
    ################################# Ranking 2 pour le graph ##############################
    rk2 <- table_ref %>% select(COD, Grade, Count) %>%
      arrange(desc(Grade), desc(Count))
    rk2$rank <- 1:nrow(rk2)
    rk2bis <- rk2 %>% select(COD, rank) %>% 
      group_by(COD) %>%
      filter(rank == min(rank)) 
    
    #attribution des rangs créés aux PT (ou LLT) des deux tables
    df4_SOC_2 <- left_join(df4_SOC_2,rk2bis, by="COD")
    df4_SOC_1 <- left_join(df4_SOC_1,rk2bis, by="COD")
  } else return("Valeur non valide pour l'option rk")
  
  
  ##### calculs pour la hauteur de chaque graphique top/bottom afin qu'elle s'adapte au nombre de PT affichées pour chaque SOC
  df <- baseEI %>% distinct(SOC,COD)
 
  df <- df %>% group_by(SOC) %>% summarise(Count=n()) %>% arrange(desc(Count))
  max_NPT <- max(df$Count) 
  
  #SOC choisie
  df_SOC <- df4_SOC %>% distinct(SOC,COD)
  df_SOC <- df_SOC %>% group_by(SOC) %>% summarise(Count=n())
  NPT <- df_SOC$Count
  
  #calculs pour la hauteur
  ht1 <- NPT/max_NPT-0.2 #0.15 est la hauteur fixe de la partie supérieure du graphique
  ht2 <- 1-ht1-0.2
  
  ##### calculs pour la largeur de chaque graph (top droit/gauche) afin qu'elle s'adapte à la largeur du plus grand label de PT
  lmax <- max(nchar(df4_SOC$COD))/170
  lgD <- 0.5+(lmax/2)
  lgG <- 1-lgD
  
  ################################### Plots ############################################## 
  tb_sum1 <- df4_SOC_1 %>% group_by(COD) %>% summarise(Count=sum(Count))
  max1 <- max(tb_sum1$Count)/frq2$Freq[frq2$ARM=="armG"]
  tb_sum2 <- df4_SOC_2 %>% group_by(COD) %>% summarise(Count=sum(Count))
  max2 <- max(tb_sum2$Count)/frq2$Freq[frq2$ARM=="armD"]
  max_lim <- max(max1,max2)
  
  lim <- ceiling(max_lim*10)*10 #arrondi à la dizaine supérieur (0.86 devient 0.90 et 90%)
  seq_by <- ifelse(lim==100 | lim==80,20,ifelse(lim==90 | lim==60,15,ifelse(lim<=30,5,10)))
  
  p1 <- ggplot(df4_SOC_1, aes(fill=as.factor(Grade), x=pct, y=reorder(COD,desc(rank)))) +
    {if(order=="Desc") geom_bar(position = position_stack(), stat = "identity")} +
    {if(order=="Inc") geom_bar(position = position_stack(reverse = TRUE), stat = "identity")} +
    scale_x_reverse(name = "Percent of patient", limits=c(lim,0), breaks = seq(0,lim, by=seq_by)) +
    scale_fill_manual(name="Grade max atteint", breaks = vect_grade,
                      values = listcol) +
    ggtitle(list_ARM[1]) +
    theme(axis.text.y = element_blank(),
          axis.text.x = element_text(size=10),
          axis.ticks.y = element_blank(),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_blank(),
          panel.background = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = "grey", linewidth = 1),
          legend.position = "none") 
  
  p2 <- ggplot(df4_SOC_2, aes(fill=as.factor(Grade), x=pct, y=reorder(COD,desc(rank)))) +
    {if(order=="Desc") geom_bar(position = position_stack(), stat = "identity")} +
    {if(order=="Inc") geom_bar(position = position_stack(reverse = TRUE), stat = "identity")} +
    scale_x_continuous(name="Percent of patient", limits = c(0,lim), breaks = seq(0,lim, by=seq_by)) +
    scale_fill_manual(name="Grade max atteint", breaks = vect_grade,
                      values = listcol) +
    ggtitle(list_ARM[2]) +
    ylab(paste0("Adverse events for ", SOCchoix))+
    theme(axis.text.y = element_text(hjust = 0.5, size = 12),
          axis.text.x = element_text(size=10),
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = 12),
          axis.ticks.y = element_blank(),
          panel.background = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.position = "none",
          panel.grid.major.x = element_line(color = "grey", linewidth = 1))
  
  if (gsup==TRUE){
    ########## Répartition des grades pour cette SOC
    ################## Creation graph ALL a ajouter au plot ######################
    ## Une barre empilée pour chaque bras de traitement en dessous du graph
    #groupe 1
    data_ALL_1 <- df4_SOC_1 %>% 
      group_by(Grade) %>% summarise(Cnt=sum(Count)) 
    tot_1 <- sum(data_ALL_1$Cnt) #nombre total d'EI dans le groupe 1 pour cette SOC
    data_ALL_1$pct <- round((data_ALL_1$Cnt / tot_1)*100,1)  
    data_ALL_1$ARM <- list_ARM[1]
    
    #groupe 2
    data_ALL_2 <- df4_SOC_2 %>% 
      group_by(Grade) %>% summarise(Cnt=sum(Count)) 
    tot_2 <- sum(data_ALL_2$Cnt)
    data_ALL_2$pct <- round((data_ALL_2$Cnt / tot_2)*100,1)  
    data_ALL_2$ARM <- list_ARM[2]
    
    data_ALL <- rbind(data_ALL_2, data_ALL_1)
    
    #Calcul pour placer les labels pct au centre de chaque portion
    if (order=="Desc"){
      data_ALL <- data_ALL %>% group_by(ARM) %>% arrange(ARM,desc(Grade)) %>%
        mutate(label_x = round(cumsum(pct) - 0.5 * pct,0))
    } else if (order=="Inc"){
      data_ALL <- data_ALL %>% group_by(ARM) %>% arrange(ARM,Grade) %>%
        mutate(label_x = round(cumsum(pct) - 0.5 * pct,0))
    }
    
    # si portions trop petites alors on affiche pas le label
    data_ALL$label_x[data_ALL$pct<4.5] <- NA
    
    p3 <- ggplot(data_ALL, aes(fill=as.factor(Grade), x=pct, y="")) +
      {if(order=="Desc") geom_bar(position = position_stack(), stat = "identity")} +
      {if(order=="Inc") geom_bar(position = position_stack(reverse = TRUE), stat = "identity")} +      
      scale_x_continuous(name = "Percent of EI", limits=c(0,101)) +
      {if(order=="Desc") scale_fill_manual(name="Grade max atteint", breaks = sort(vect_grade, decreasing = TRUE),
                                           values = list.reverse(listcol))} +
      {if(order=="Inc") scale_fill_manual(name="Grade max atteint", breaks = sort(vect_grade, decreasing = FALSE),
                                          values = listcol)} +
      geom_text(aes(label=paste0(pct,"%"), x=label_x),
                color = "white",
                fontface = "bold", size=5) +
      ggtitle(paste0("Adverse events for ", SOCchoix))+
      facet_grid(ARM ~ ., switch = "y") +
      theme(panel.background = element_blank(),
            panel.grid.major.x = element_blank(),
            axis.text = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_text(size = 12),
            axis.ticks = element_blank(),
            plot.title = element_text(size=12),
            legend.position = "bottom",
            legend.justification = "left",
            legend.text = element_text(size=10),
            legend.title = element_text(size=10),
            strip.text.y.left = element_text(angle=0, size=12))
    
    bottom_row <- plot_grid(p1,  p2, labels = NULL,nrow = 1,rel_widths = c(lgG, lgD))
    p<-plot_grid(p3, bottom_row, labels = NULL,nrow = 3,rel_heights = c(0.2,ht1,ht2))
  } else {
    p<-plot_grid(p1,  p2, labels = NULL,nrow = 1,rel_widths = c(lgG, lgD))
  }
  suppressWarnings(print(p))
}

