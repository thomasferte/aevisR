# fichiers contenant toutes les fonctions pour le shiny

# StackedBarChart <- function(data,SOC, grprk ,rk="pctgrade")
ButterflyStackAE <- function(baseEI, baseTr, SOCchoix, ARMe=NULL, CODvar="PT_COD", listcol, gsup=TRUE, grprk=NULL, rk="pctgrade", order="Desc"){
  #on récupère le nombre de modalité de la variable AE_GRADE pour les échelles de couleurs des graphiques
  vect_grade <- sort(unique((baseEI$AE_GRADE)))
  
  #### table EI 
  #liste des groupes de traitement de la table baseTr
  list_ARM <- unique(baseTr$ARM)
  if (is.null(ARMe)) {
    #Liste des id patients dans le bras numéro 1
    list_pat1 <- unique(baseTr$USUBJID[baseTr$ARM == list_ARM[1]]) 
    #Liste des id patients dans le bras numéro 2
    list_pat2 <- unique(baseTr$USUBJID[baseTr$ARM == list_ARM[2]])
  } else if (!is.null(ARMe)){
    l2 <- unique(baseTr$ARM) 
    if (!(ARMe %in% l2)) return("Nom de bras de traitement non correct ou non présent dans la base.") 
    list_ARM[1] <- ARMe
    list_ARM[2] <- l2[l2 != ARMe]
    
    #Liste des id patients dans le bras numéro 1 : GC
    list_pat1 <- unique(baseTr$USUBJID[baseTr$ARM == list_ARM[1]]) 
    #Liste des id patients dans le bras numéro 2 : GC + Avelumab
    list_pat2 <- unique(baseTr$USUBJID[baseTr$ARM == list_ARM[2]])
  }
  #Ajouter une colonne ARM dans la table data en faisant correspondre les USUBJID selon la liste où ils sont présents
  baseEI$ARM <- ifelse(baseEI$USUBJID %in% list_pat1, "armG", "armD")
  baseTr$ARM <- ifelse(baseTr$ARM==list_ARM[1], "armG","armD")
  
  
  #On créé une table de fréquence **frq2** qui contient le nombre de patient dans chaque bras de traitement en tenant compte du fait qu’ils doivent avoir pris le traitement et donc être considérés comme "à risque"
  df_Tr2  <- baseTr[baseTr$TTT_IND=="Yes",]
  USU_distinct  <- df_Tr2  %>% select(USUBJID, ARM, TTT_IND) %>% distinct(USUBJID, ARM)
  frq2 <- data.frame(xtabs(~ ARM, data=USU_distinct))
  
  if (CODvar == "PT_COD"){
    df2 <- baseEI %>% select(USUBJID, ARM, SOC_COD, PT_COD, AE_GRADE) %>% arrange(USUBJID, ARM,SOC_COD,PT_COD,desc(AE_GRADE))  
  } else if (CODvar == "LLT_COD"){
    df2 <- baseEI %>% select(USUBJID, ARM, SOC_COD, LLT_COD, AE_GRADE) %>% arrange(USUBJID, ARM,SOC_COD,LLT_COD,desc(AE_GRADE))  
  } else return("Valeur non correcte pour l'option CODvar")
  names(df2) <- c("USUBJID","ARM","SOC_COD","COD","AE_GRADE")
  
  
  #Selection des variables d'interêt
  if (!(SOCchoix %in% df2$SOC_COD)) return("SOC choisit non présent dans la base de donnée")
  df2_SOC <- df2[df2$SOC_COD == SOCchoix,]
  df2_SOC <- df2_SOC[!is.na(df2_SOC$AE_GRADE),] #on ne garde pas les EIs pour lesquels ont a pas l'info sur le grade (normalement pas présents la base doit être complète et nettoyée avant)
  
  
  ### Selectionner les lignes avec le grade max
  df3_SOC <- df2_SOC %>% distinct(USUBJID,ARM,SOC_COD,COD,AE_GRADE) %>%
    group_by(USUBJID,SOC_COD,COD) %>% dplyr::filter(AE_GRADE==max(AE_GRADE))
  
  df4_SOC <- df3_SOC %>% group_by(ARM, SOC_COD, COD, AE_GRADE) %>% summarise(Count=n())
  
  ## Calculer le pourcentage par PT_COD pour chaque groupe
  df4_SOC$pct <- ifelse(df4_SOC$ARM == "armG" ,round((df4_SOC$Count / frq2$Freq[frq2$ARM=="armG"])*100,1),
                        round((df4_SOC$Count / frq2$Freq[frq2$ARM=="armD"])*100,1))
  
  ### Division de cette sous table selon les deux bras de traitement
  df4_SOC_1 <- as.data.frame(df4_SOC[df4_SOC$ARM == "armG",])
  df4_SOC_2 <- as.data.frame(df4_SOC[df4_SOC$ARM == "armD",])
  
  
  ######################### Liste des COD ###############################
  # Car tous les PT_COD doivent être présents dans les deux bases sinon problème pour le plot
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
  names(df4_SOC_1) <-  c("ARM", "SOC_COD","COD", "AE_GRADE", "Count","pct")
  
  ## ajout groupe 2
  add_2 <- list_COD[!(list_COD %in% list_COD_2)]  #liste des PT à ajouter dans la base du groupe 2 
  if (length(add_2)>0){
    for (i in 1:length(add_2)){
      new_line = list(list_ARM[2], SOCchoix, add_2[i], 1, 0, 0)
      df4_SOC_2 = rbind(df4_SOC_2,new_line)
    }
  }
  names(df4_SOC_2) <-  c("ARM", "SOC_COD","COD", "AE_GRADE", "Count","pct")
  
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
    rk2 <- table_ref %>% select(COD, AE_GRADE, Count) %>%
      arrange(desc(AE_GRADE), desc(Count))
    rk2$rank <- 1:nrow(rk2)
    rk2bis <- rk2 %>% select(COD, rank) %>% 
      group_by(COD) %>%
      filter(rank == min(rank)) 
    
    #attribution des rangs créés aux PT (ou LLT) des deux tables
    df4_SOC_2 <- left_join(df4_SOC_2,rk2bis, by="COD")
    df4_SOC_1 <- left_join(df4_SOC_1,rk2bis, by="COD")
  } else return("Valeur non valide pour l'option rk")
  
  
  ##### calculs pour la hauteur de chaque graphique top/bottom afin qu'elle s'adapte au nombre de PT affichées pour chaque SOC
  if(CODvar=="PT_COD"){
    df <- baseEI %>% distinct(SOC_COD,PT_COD)
  } else if (CODvar =="LLT_COD"){
    df <- baseEI %>% distinct(SOC_COD,LLT_COD)
  }
  names(df) <- c("SOC_COD","COD")
  df <- df %>% group_by(SOC_COD) %>% summarise(Count=n()) %>% arrange(desc(Count))
  max_NPT <- max(df$Count) 
  
  #SOC choisie
  # NPT <- df$Count[df$SOC_COD == SOCchoix]
  #SOC choisie
  df_SOC <- df4_SOC %>% distinct(SOC_COD,COD)
  df_SOC <- df_SOC %>% group_by(SOC_COD) %>% summarise(Count=n())
  NPT <- df_SOC$Count
  
  #calculs pour la hauteur
  ht1 <- NPT/max_NPT-0.2 #0.15 est la hauteur fixe de la partie supérieure du graphique
  ht2 <- 1-ht1-0.2
  
  ##### calculs pour la largeur de chaque graph (top droit/gauche) afin qu'elle s'adapte à la largeur du plus grand label de PT
  lmax <- max(nchar(df4_SOC$COD))/120
  lgD <- 0.5+(lmax/2)
  lgG <- 1-lgD
  
  ################################### Plots ############################################## 
  tb_sum1 <- df4_SOC_1 %>% group_by(COD) %>% summarise(Count=sum(Count))
  max1 <- max(tb_sum1$Count)/frq2$Freq[frq2$ARM=="armG"]
  tb_sum2 <- df4_SOC_2 %>% group_by(COD) %>% summarise(Count=sum(Count))
  max2 <- max(tb_sum2$Count)/frq2$Freq[frq2$ARM=="armD"]
  max_lim <- max(max1,max2)
  
  lim <- ceiling(max_lim*10)*10 #arriondi à la dizaine supérieur (0.86 devient 0.90 et 90%)
  seq_by <- ifelse(lim==100 | lim==80,20,ifelse(lim==90 | lim==60,15,ifelse(lim<=30,5,10)))
  
  p1 <- ggplot(df4_SOC_1, aes(fill=as.factor(AE_GRADE), x=pct, y=reorder(COD,desc(rank)))) +
    {if(order=="Desc") geom_bar(position = position_stack(), stat = "identity")} +
    {if(order=="Inc") geom_bar(position = position_stack(reverse = TRUE), stat = "identity")} +
    scale_x_reverse(name = "Percent of patient", limits=c(lim,0), breaks = seq(0,lim, by=seq_by)) +
    scale_fill_manual(name="Grade max atteint", breaks = vect_grade,
                      values = listcol) +
    ggtitle(list_ARM[1]) +
    theme(axis.text.y = element_blank(),
          axis.text.x = element_text(size=rel(1.3)),
          axis.ticks.y = element_blank(),
          axis.title.x = element_text(size = rel(1.5)),
          axis.title.y = element_blank(),
          panel.background = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = "grey", linewidth = 1),
          legend.position = "none") 
  
  p2 <- ggplot(df4_SOC_2, aes(fill=as.factor(AE_GRADE), x=pct, y=reorder(COD,desc(rank)))) +
    {if(order=="Desc") geom_bar(position = position_stack(), stat = "identity")} +
    {if(order=="Inc") geom_bar(position = position_stack(reverse = TRUE), stat = "identity")} +
    scale_x_continuous(name="Percent of patient", limits = c(0,lim), breaks = seq(0,lim, by=seq_by)) +
    scale_fill_manual(name="Grade max atteint", breaks = vect_grade,
                      values = listcol) +
    ggtitle(list_ARM[2]) +
    ylab(paste0("Adverse events for ", SOCchoix))+
    theme(axis.text.y = element_text(hjust = 0.5, size = rel(1.5)),
          axis.text.x = element_text(size=rel(1.3)),
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = rel(1.5)),
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
      group_by(AE_GRADE) %>% summarise(Cnt=sum(Count)) 
    tot_1 <- sum(data_ALL_1$Cnt) #nombre total d'EI dans le groupe 1 pour cette SOC
    data_ALL_1$pct <- round((data_ALL_1$Cnt / tot_1)*100,1)  
    data_ALL_1$ARM <- list_ARM[1]
    
    #groupe 2
    data_ALL_2 <- df4_SOC_2 %>% 
      group_by(AE_GRADE) %>% summarise(Cnt=sum(Count)) 
    tot_2 <- sum(data_ALL_2$Cnt)
    data_ALL_2$pct <- round((data_ALL_2$Cnt / tot_2)*100,1)  
    data_ALL_2$ARM <- list_ARM[2]
    
    data_ALL <- rbind(data_ALL_2, data_ALL_1)
    
    #Calcul pour placer les labels pct au centre de chaque portion
    if (order=="Desc"){
      data_ALL <- data_ALL %>% group_by(ARM) %>% arrange(ARM,desc(AE_GRADE)) %>%
        mutate(label_x = round(cumsum(pct) - 0.5 * pct,0))
    } else if (order=="Inc"){
      data_ALL <- data_ALL %>% group_by(ARM) %>% arrange(ARM,AE_GRADE) %>%
        mutate(label_x = round(cumsum(pct) - 0.5 * pct,0))
    }
    
    # si portions trop petites alors on affiche pas le label
    data_ALL$label_x[data_ALL$pct<4.5] <- NA
    
    p3 <- ggplot(data_ALL, aes(fill=as.factor(AE_GRADE), x=pct, y="")) +
      {if(order=="Desc") geom_bar(position = position_stack(), stat = "identity")} +
      {if(order=="Inc") geom_bar(position = position_stack(reverse = TRUE), stat = "identity")} +      
      scale_x_continuous(name = "Percent of EI", limits=c(0,101)) +
      {if(order=="Desc") scale_fill_manual(name="Grade max atteint", breaks = sort(vect_grade, decreasing = TRUE),
                                           values = list.reverse(listcol))} +
      {if(order=="Inc") scale_fill_manual(name="Grade max atteint", breaks = sort(vect_grade, decreasing = FALSE),
                                          values = listcol)} +
      geom_text(aes(label=paste0(pct,"%"), x=label_x),
                color = "white",
                fontface = "bold", size=rel(5)) +
      ggtitle(paste0("Adverse events for ", SOCchoix))+
      facet_grid(ARM ~ ., switch = "y") +
      theme(panel.background = element_blank(),
            panel.grid.major.x = element_blank(),
            axis.text = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_text(size = rel(1.5)),
            axis.ticks = element_blank(),
            plot.title = element_text(size=rel(1.5)),
            legend.position = "bottom",
            legend.justification = "left",
            legend.text = element_text(size=rel(1.2)),
            legend.title = element_text(size=rel(1.2)),
            strip.text.y.left = element_text(angle=0, size=rel(1.5)))
    
    bottom_row <- plot_grid(p1,  p2, labels = NULL,nrow = 1,rel_widths = c(lgG, lgD))
    p<-plot_grid(p3, bottom_row, labels = NULL,nrow = 3,rel_heights = c(0.2,ht1,ht2))
  } else {
    p<-plot_grid(p1,  p2, labels = NULL,nrow = 1,rel_widths = c(lgG, lgD))
  }
  suppressWarnings(print(p))
}

#a modifier car pas dans le HTML (pour l'instant)
#ajout à partir de la fonction du script fonctionsShiny_V2_RMD
#pour afficher les SOC et non plus les PT en ligne dans le butterfly (version plus générale)
StackedBarChartSOC <- function(baseEI, baseTr, grprk=NULL, rk="pctgrade", ARMe=NULL, listcol, gsup=TRUE, order="Desc"){
  #on récupère le nombre de modalité de la variable AE_GRADE pour les échelles de couleurs des graphiques
  vect_grade <- sort(unique((baseEI$AE_GRADE)))
  
  #liste des groupes de traitement de la table baseTr
  list_ARM <- unique(baseTr$ARM)
  if (is.null(ARMe)) {
    #Liste des id patients dans le bras numéro 1
    list_pat1 <- unique(baseTr$USUBJID[baseTr$ARM == list_ARM[1]]) 
    #Liste des id patients dans le bras numéro 2
    list_pat2 <- unique(baseTr$USUBJID[baseTr$ARM == list_ARM[2]])
  } else if (!is.null(ARMe)){
    l2 <- unique(baseTr$ARM) 
    if (!(ARMe %in% l2)) return("Nom de bras de traitement non correct ou non présent dans la base.") 
    list_ARM[1] <- ARMe
    list_ARM[2] <- l2[l2 != ARMe]
    
    #Liste des id patients dans le bras numéro 1 : GC
    list_pat1 <- unique(baseTr$USUBJID[baseTr$ARM == list_ARM[1]]) 
    #Liste des id patients dans le bras numéro 2 : GC + Avelumab
    list_pat2 <- unique(baseTr$USUBJID[baseTr$ARM == list_ARM[2]])
  }
  #Ajouter une colonne ARM dans la table data en faisant correspondre les USUBJID selon la liste où ils sont présents
  baseEI$ARM <- ifelse(baseEI$USUBJID %in% list_pat1, "armG", "armD")
  baseTr$ARM <- ifelse(baseTr$ARM==list_ARM[1], "armG","armD")
  
  #On créé une table de fréquence **frq2** qui contient le nombre de patient dans chaque bras de traitement en tenant compte du fait qu’ils doivent avoir pris le traitement et donc être considérés comme "à risque"
  df_Tr2  <- baseTr[baseTr$TTT_IND=="Yes",]
  USU_distinct  <- df_Tr2  %>% select(USUBJID, ARM, TTT_IND) %>% distinct(USUBJID, ARM)
  frq2 <- data.frame(xtabs(~ ARM, data=USU_distinct))
  
  #Selection des variables d'interêt
  df2_SOC <- baseEI %>% select(USUBJID, ARM, SOC_COD, AE_GRADE) %>% arrange(USUBJID, ARM,SOC_COD,desc(AE_GRADE))
  
  ### Selectionner les lignes avec le grade max par SOC
  df3_SOC <- df2_SOC %>% distinct(USUBJID,ARM,SOC_COD,AE_GRADE) %>%
    group_by(USUBJID,SOC_COD) %>% dplyr::filter(AE_GRADE==max(AE_GRADE))
  
  df4_SOC <- df3_SOC %>% group_by(ARM, SOC_COD, AE_GRADE) %>% summarise(Count=n())
  
  ## Calculer le pourcentage par SOC pour chaque groupe
  df4_SOC$pct <- ifelse(df4_SOC$ARM == "armG" ,round((df4_SOC$Count / frq2$Freq[frq2$ARM=="armG"])*100,1),
                        round((df4_SOC$Count / frq2$Freq[frq2$ARM=="armD"])*100,1))
  
  ### Division de cette sous table selon les deux bras de traitement
  df4_SOC_1 <- as.data.frame(df4_SOC[df4_SOC$ARM == "armG",])
  df4_SOC_2 <- as.data.frame(df4_SOC[df4_SOC$ARM == "armD",])
  
  
  ######################### Liste des SOC ###############################
  # Car tous les SOC doivent être présents dans les deux bases sinon problème pour le plot
  list_S <- unique(df4_SOC$SOC_COD)
  list_S_1 <- unique(df4_SOC_1$SOC_COD) #liste des SOC déjà présents dans la base du groupe 1
  list_S_2 <- unique(df4_SOC_2$SOC_COD) #liste des SOC déjà présents dans la base du groupe 2
  
  ## ajout groupe 1
  add_1 <- list_S[!(list_S %in% list_S_1)]  #liste des PT à ajouter dans la base du groupe1
  if (length(add_1)>0){
    for (i in 1:length(add_1)){
      new_line = list(list_ARM[1], add_1[i], 1, 0, 0) #nouvelle ligne avec la SOC manquante
      df4_SOC_1 = rbind(df4_SOC_1,new_line) #ajout à la base du groupe 1
    }
  }
  
  ## ajout groupe 2
  add_2 <- list_S[!(list_S %in% list_S_2)]  #liste des PT à ajouter dans la base du groupe 2
  if (length(add_2)>0){
    for (i in 1:length(add_2)){
      new_line = list(list_ARM[2], add_2[i], 1, 0, 0)
      df4_SOC_2 = rbind(df4_SOC_2,new_line)
    }
  }
  
  if (is.null(grprk)) {table_ref <- df4_SOC_1
  } else if (grprk==list_ARM[1]) {table_ref <- df4_SOC_1
  } else if (grprk==list_ARM[2]) {table_ref <- df4_SOC_2
  }
  
  if (rk=="pcttot") {
    ################################# Ranking 1 pour le graph ##############################
    #pctTot
    tb_rk <- table_ref %>% group_by(SOC_COD) %>% summarise(Count=sum(Count))
    tb_rk <- tb_rk[order(-tb_rk$Count),]
    tb_rk$rank <- 1:nrow(tb_rk)
    
    #attribution des rangs créés aux PT (ou LLT) des deux tables
    df4_SOC_2 <- left_join(df4_SOC_2,tb_rk[,-2], by="SOC_COD")
    df4_SOC_1 <- left_join(df4_SOC_1,tb_rk[,-2], by="SOC_COD")
    
  } else if (rk=="pctgrade") {
    ################################# Ranking 2 pour le graph ##############################
    #pctgrade
    tb_rk <- table_ref %>% select(SOC_COD, AE_GRADE, Count) %>%
      arrange(desc(AE_GRADE), desc(Count))
    tb_rk$rank <- 1:nrow(tb_rk)
    tb_rkbis <- tb_rk %>% select(SOC_COD, rank) %>%
      group_by(SOC_COD) %>%
      filter(rank == min(rank))
    
    #attribution des rangs créés aux PT (ou LLT) des deux tables
    df4_SOC_2 <- left_join(df4_SOC_2,tb_rkbis, by="SOC_COD")
    df4_SOC_1 <- left_join(df4_SOC_1,tb_rkbis, by="SOC_COD")
  }
  
  ##### calculs pour la largeur de chaque graph (top droit/gauche) afin qu'elle s'adapte à la largeur du plus grand label de PT
  lmax <- max(nchar(df4_SOC$SOC_COD))/120
  lgD <- 0.5+(lmax/2)
  lgG <- 1-lgD
  
  ################################### Plots ##############################################
  tb_sum1 <- df4_SOC_1 %>% group_by(SOC_COD) %>% summarise(Count=sum(Count))
  max1 <- max(tb_sum1$Count)/frq2$Freq[frq2$ARM=="armG"]
  tb_sum2 <- df4_SOC_2 %>% group_by(SOC_COD) %>% summarise(Count=sum(Count))
  max2 <- max(tb_sum2$Count)/frq2$Freq[frq2$ARM=="armD"]
  max_lim <- max(max1,max2)
  
  lim <- ceiling(max_lim*10)*10 #arriondi à la dizaine supérieur (0.86 devient 0.90 et 90%)
  seq_by <- ifelse(lim==100 | lim==80,20,ifelse(lim==90 | lim==60,15,ifelse(lim<=30,5,10)))
  
  p1 <- ggplot(df4_SOC_1, aes(fill=as.factor(AE_GRADE), x=pct, y=reorder(SOC_COD,desc(rank)))) +
    {if(order=="Desc") geom_bar(position = position_stack(), stat = "identity")} +
    {if(order=="Inc") geom_bar(position = position_stack(reverse = TRUE), stat = "identity")} +
    scale_x_reverse(name = "Percent of patient", limits=c(lim,0), breaks = seq(0,lim, by=seq_by)) +
    scale_fill_manual(name="Grade max atteint", breaks = vect_grade,
                      values = listcol) +
    ggtitle(list_ARM[1]) +
    theme(axis.text.y = element_blank(),
          axis.text.x = element_text(size=rel(1.3)),
          axis.ticks.y = element_blank(),
          axis.title.x = element_text(size = rel(1.5)),
          axis.title.y = element_blank(),
          panel.background = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = "grey", linewidth = 1),
          legend.position = "none")
  
  p2 <- ggplot(df4_SOC_2, aes(fill=as.factor(AE_GRADE), x=pct, y=reorder(SOC_COD,desc(rank)))) +
    {if(order=="Desc") geom_bar(position = position_stack(), stat = "identity")} +
    {if(order=="Inc") geom_bar(position = position_stack(reverse = TRUE), stat = "identity")} +    
    scale_x_continuous(name="Percent of patient", limits = c(0,lim), breaks = seq(0,lim, by=seq_by)) +
    scale_fill_manual(name="Grade max atteint", breaks = vect_grade,
                      values = listcol) +
    ggtitle(list_ARM[2]) +
    theme(axis.text.y = element_text(hjust = 0.5, size = rel(1.5)),
          axis.text.x = element_text(size=rel(1.3)),
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = rel(1.5)),
          axis.ticks.y = element_blank(),
          panel.background = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.position = "none",
          panel.grid.major.x = element_line(color = "grey", linewidth = 1))
  
  
  if (gsup==TRUE){
    ########## Répartition des grades pour tous le graph
    ################## Creation graph ALL a ajouter au plot ######################
    ## Une barre empilée pour chaque bras de traitement en dessous du graph
    #groupe 1 : GC
    data_ALL_1 <- df4_SOC_1 %>%
      group_by(AE_GRADE) %>% summarise(Cnt=sum(Count))
    tot_1 <- sum(data_ALL_1$Cnt) #nombre total d'EI dans le groupe 1 pour cette SOC
    data_ALL_1$pct <- round((data_ALL_1$Cnt / tot_1)*100,1)
    data_ALL_1$ARM <- list_ARM[1]
    
    #groupe 2 : GC + Avelumab
    data_ALL_2 <- df4_SOC_2 %>%
      group_by(AE_GRADE) %>% summarise(Cnt=sum(Count))
    tot_2 <- sum(data_ALL_2$Cnt)
    data_ALL_2$pct <- round((data_ALL_2$Cnt / tot_2)*100,1)
    data_ALL_2$ARM <- list_ARM[2]
    
    data_ALL <- rbind(data_ALL_2, data_ALL_1)
    
    #Calcul pour placer les labels pct au centre de chaque portion
    if (order=="Desc"){
      data_ALL <- data_ALL %>% group_by(ARM) %>% arrange(ARM,desc(AE_GRADE)) %>%
        mutate(label_x = round(cumsum(pct) - 0.5 * pct,0))
    } else if (order=="Inc"){
      data_ALL <- data_ALL %>% group_by(ARM) %>% arrange(ARM,AE_GRADE) %>%
        mutate(label_x = round(cumsum(pct) - 0.5 * pct,0))
    }
    
    # si portions trop petites alors on affiche pas le label
    data_ALL$label_x[data_ALL$pct<4.5] <- NA
    
    p3 <- ggplot(data_ALL, aes(fill=as.factor(AE_GRADE), x=pct, y="")) +
      {if(order=="Desc") geom_bar(position = position_stack(), stat = "identity")} +
      {if(order=="Inc") geom_bar(position = position_stack(reverse = TRUE), stat = "identity")} +      
      scale_x_continuous(name = "Percent of EI", limits=c(0,101)) +
      {if(order=="Desc") scale_fill_manual(name="Grade max atteint", breaks = sort(vect_grade, decreasing = TRUE),
                                           values = list.reverse(listcol))} +
      {if(order=="Inc") scale_fill_manual(name="Grade max atteint", breaks = sort(vect_grade, decreasing = FALSE),
                                          values = listcol)} +
      geom_text(aes(label=paste0(pct,"%"), x=label_x),
                color = "white",
                fontface = "bold", size=rel(5)) +
      facet_grid(ARM ~ ., switch = "y") +
      theme(panel.background = element_blank(),
            panel.grid.major.x = element_blank(),
            axis.text = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_text(size = rel(1.3)),
            axis.ticks = element_blank(),
            legend.position = "bottom",
            legend.justification = "left",
            legend.text = element_text(size=rel(1.2)),
            legend.title = element_text(size=rel(1.2)),
            strip.text.y.left = element_text(angle=0, size=rel(1.5)))
    
    bottom_row <- plot_grid(p1,  p2, labels = NULL,nrow = 1,rel_widths = c(lgG, lgD))
    p<-plot_grid(p3, bottom_row, labels = NULL,nrow = 2,rel_heights = c(0.2,0.8))
  } else {
    p<-plot_grid(p1,  p2, labels = NULL,nrow = 1,rel_widths = c(lgG, lgD))
  }
  suppressWarnings(print(p))
}

#fonction qui renvoie le plot ($plot) et sa hauteur ($height) qui est conditionnée au nombre de lignes affichées dans le plot
# PatientChart <- function(data, usu,suivi=TRUE)
PatientSpanAE <- function(baseEI, baseTr, baseDates, baseBio, choixUSU, suivi=TRUE, CODvar="PT_COD", listcol, bio=FALSE){
  #liste des groupes de traitement de la table df_Tr
  list_ARM <- unique(baseTr$ARM)
  
  #on récupère le nombre de modalité de la variable AE_GRADE pour les échelles de couleurs des graphiques
  vect_grade <- sort(unique((baseEI$AE_GRADE)))
  # On stock son bras de traitement pour l'afficher dans le graphique
  ARMp <- unique(baseTr$ARM[baseTr$USUBJID == choixUSU])
  
  ############ table patient ########## 
  df_pat <- subset(baseEI, baseEI$USUBJID==choixUSU)
  
  screendate <- as.Date(baseDates$SCREEN_DAT[baseDates$USUBJID==choixUSU],format="%d/%m/%Y")
  suidate <- as.Date(baseDates$EI_SUI_DAT[baseDates$USUBJID==choixUSU],format="%d/%m/%Y")
  debttt <- as.Date(baseDates$C1D1_DAT[baseDates$USUBJID==choixUSU],format="%d/%m/%Y")
  randodate <- unique(as.Date(df_pat$RANDO_DAT,format="%d/%m/%Y"))
  df_pat <- df_pat %>% select(-c(AE_INIT_NUM,AE_INIT))
  
  # si date de début commençant par ND ou NK on remplace par 01 du mois
  # ou si < rando_dat et même mois que rando_dat alors rando_dat
  # si < rando_dat mais pas même mois alors on regarde si < screening (ensuite idem que rando_dat)
  l1 <- grep(pattern = "ND/",df_pat$AE_DAT_START,value=FALSE)
  l1 <- c(l1,grep(pattern = "NK/",df_pat$AE_DAT_START,value=FALSE))
  # provisoirement on met le premier du mois en ensuite on fait le vérifications avec randodate et screendate
  df_pat$AE_DAT_START <- str_replace(df_pat$AE_DAT_START,"ND/","01/")
  df_pat$AE_DAT_START <- str_replace(df_pat$AE_DAT_START,"NK/","01/")
  df_pat$AE_DAT_START <- as.Date(df_pat$AE_DAT_START, format = "%d/%m/%Y")
  df_pat$AE_DAT_END <- as.Date(df_pat$AE_DAT_END, format = "%d/%m/%Y")
  #verifications
  if (length(l1)>0){
    for (l in 1:length(l1)){
      if(df_pat$AE_DAT_START[l1[l]]<randodate & 
         lubridate::month(df_pat$AE_DAT_START[l1[l]])==lubridate::month(randodate)) df_pat$AE_DAT_START[l1[l]]<-randodate
      if(df_pat$AE_DAT_START[l1[l]]<screendate &
         lubridate::month(df_pat$AE_DAT_START[l1[l]])==lubridate::month(screendate)) df_pat$AE_DAT_START[l1[l]]<-screendate
    }
  }
  
  # si pas de date de fin et AE_ONGO yes alors on met la date la plus lointaine pour ce patient
  # on indiquera par une flèche sur le graphique que cet EI est en cours
  # si pas de date de fin et AE_ONGO yes alors on met la date de début
  df_pat$AE_DAT_END[is.na(df_pat$AE_DAT_END) & df_pat$AE_ONGO=="Yes"] <- df_pat$AE_DAT_START[is.na(df_pat$AE_DAT_END) & df_pat$AE_ONGO=="Yes"]
  
  
  # si on ne veut pas le suivi
  if (suivi==FALSE){
    #si date de fin sup mais pas date de début alors on change la date de fin avec la date de suivi
    for (i in 1:nrow(df_pat)){
      if(df_pat$AE_DAT_START[i] < suidate & df_pat$AE_DAT_END[i] > suidate & !is.na(df_pat$AE_DAT_END[i])){
        df_pat$AE_DAT_END[i]<-suidate
        df_pat$AE_ONGO[i]<-"Yes"
        df_pat$AE_OUT[i]<-NA
      } 
    }
    #on ne garde que ceux avec date de début inf à date de suivi et date de fin inf ou égale à la date de suivi
    df_pat <- subset(df_pat,(df_pat$AE_DAT_END <= suidate & df_pat$AE_DAT_START <= suidate) | is.na(df_pat$AE_DAT_END))
  }
  
  # transformer les dates en jours depuis la date de début de traitement
  df_pat$aestartdate <- as.numeric(df_pat$AE_DAT_START - debttt)
  df_pat$aeenddate<- as.numeric(df_pat$AE_DAT_END - debttt)
  df_pat <- df_pat %>% select(-c(AE_DAT_START,AE_DAT_END, USUBJID))
  
  # ranking pour le graph
  if (CODvar=="PT_COD") {table_rk <- df_pat %>% arrange(SOC_COD, AE_GRADE, PT_COD) %>% distinct(SOC_COD, PT_COD,AE_GRADE)
  } else if (CODvar == "LLT_COD") {
    table_rk <- df_pat %>% arrange(SOC_COD, AE_GRADE, LLT_COD) %>% distinct(SOC_COD, LLT_COD,AE_GRADE)
  } else return("Valeur non valide pour l'option CODvar")
  names(table_rk) <- c("SOC_COD", "COD","AE_GRADE")
  
  # liste des SOC et ajout à la table pat
  l_SOC <- unique(df_pat$SOC_COD)
  l_SOC2 <- paste0(" ",l_SOC," ------")
  table_SOC <- cbind(l_SOC,l_SOC2,NA)
  colnames(table_SOC) <- c("SOC_COD", "COD", "AE_GRADE")  
  table_rk <- rbind(table_rk,table_SOC)
  table_rk$AE_GRADE <- as.integer(table_rk$AE_GRADE)
  table_rk <- table_rk %>% arrange(SOC_COD, COD, AE_GRADE)
  table_rk$rank <- 6:(nrow(table_rk)+5) #à partir du rang 6 car avant on a les traitements (x4) et la bio (traités par la suite)
  
  if (CODvar == 'PT_COD'){
    df_pat <- df_pat %>% select(-LLT_COD)
    df_pat <- df_pat %>% rename("COD" = "PT_COD")
  } else if (CODvar=="LLT_COD"){
    df_pat <- df_pat %>% select(-PT_COD)
    df_pat <- df_pat %>% rename("COD" = "LLT_COD")
  } else return("Valeur non valide pour l'option CODvar")
  
    df_pat <- full_join(df_pat %>% select(-SOC_COD), table_rk %>% select(-SOC_COD), by=c("COD","AE_GRADE"), multiple="all") 
  
  
  ############ table traitement ########## 
  # comme pour la table EI on selectionne les traitements associés au patients choisi
  df_Trpat <- subset(baseTr, df_Tr$USUBJID==choixUSU)
  # calcul en jour depuis la rando pour chaque traitement
  df_Trpat$TTT_DAT <- as.Date(df_Trpat$TTT_DAT, format = "%d/%m/%Y")
  df_Trpat$tttdat <- as.numeric(df_Trpat$TTT_DAT - debttt)
  
  df_Trpat <- df_Trpat %>% select(VISITNUM, TTT_LAB, tttdat)
  df_Trpat <- df_Trpat[!is.na(df_Trpat$tttdat),] #on retire les lignes avec NA
  
  #rank de chacun des traitements
  tb_rank <- as.data.frame(unique(df_Trpat$TTT_LAB))
  colnames(tb_rank)<-"TTT_LAB"
  tb_rank$rank <- 1:nrow(tb_rank)
  df_Trpat <- merge(df_Trpat,tb_rank, by="TTT_LAB")
  
  # vecteur de jours pour l'axe x
  vect2 <- unique(df_Trpat$tttdat)
  
  if (bio==TRUE){
    ############ table biologie / Hemo ########## 
    df_Biopat <- subset(baseBio, baseBio$USUBJID==choixUSU)
    df_Biopat$BIO_DAT <- as.Date(df_Biopat$BIO_DAT, format = "%d/%m/%Y")
    # if (suivi==FALSE)  df_Biopat <- subset(df_Biopat,df_Biopat$BIO_DAT <= suidate)
    df_Biopat$biodays <- as.numeric(df_Biopat$BIO_DAT - debttt) 
    df_Biopat$rank<-5 #rang 5 car avant il y a les 4 traitements
    df_Biopat$col <- ifelse(df_Biopat$BIO_RES <85, 2, ifelse(df_Biopat$BIO_RES <100,1,0))
  }
  
  ######################################################################## 
  ## combinaison des trois tables avec renommage des colonnes
  #Creation d'une table pour accueilir toutes les données nécessaires
  dfxpat <- data.frame(aeterm=df_pat$COD,
                       AE_GRADE=df_pat$AE_GRADE,
                       AE_ONGO=df_pat$AE_ONGO,
                       AE_OUT=df_pat$AE_OUT,
                       SAE=df_pat$SAE,
                       aestartdate=df_pat$aestartdate,
                       aeenddate=df_pat$aeenddate,
                       tttdat=NA,biodat=NA,biores=NA,colbio=NA,
                       AE_MOL1 = df_pat$AE_MOL1,
                       AE_MOL2 = df_pat$AE_MOL2,
                       rank=df_pat$rank)
  dfxttt <- data.frame(aeterm=df_Trpat$TTT_LAB,
                       AE_GRADE=NA,AE_ONGO=NA,AE_OUT=NA,SAE=NA,
                       aestartdate=NA,aeenddate=NA,
                       tttdat=df_Trpat$tttdat,biodat=NA,biores=NA,
                       colbio=NA,AE_MOL1 = NA,AE_MOL2 = NA,
                       rank=df_Trpat$rank)
  if (bio==TRUE){
    dfxbio <- data.frame(aeterm="Bio / Hemo",
                         AE_GRADE=NA,AE_ONGO=NA,AE_OUT=NA,SAE=NA,
                         aestartdate=NA,aeenddate=NA,tttdat=NA,
                         biodat=df_Biopat$biodays,
                         biores=df_Biopat$BIO_RES,
                         colbio=df_Biopat$col,
                         AE_MOL1 = NA,AE_MOL2 = NA,
                         rank=df_Biopat$rank)
    df_plot <- rbind(dfxpat, dfxttt,dfxbio)
  } else if (bio==FALSE){
    df_plot <- rbind(dfxpat, dfxttt)
  }
  
  ## ajout d'une colonne bcol 1 ou 0 selon si le fond doit être blanc ou gris
  df_num <- df_plot %>% select(aeterm,rank) %>% arrange(rank)
  cpt=0
  for (i in 1:nrow(df_num)){
    if(str_detect(df_num$aeterm[i],"----") == TRUE)  cpt=cpt+1
    df_num$num[i]=cpt
  }
  
  #A chaque numéro on récupère le min et le max qui seront donc les limites des rectangles pour l'axes des ordonnées (représenté par les PT_COD)
  df_rect <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("grp", "aeterm","aeterm_start", "aeterm_end"))
  for(i in unique(df_num$num)){
    dfx_num <- subset(df_num, df_num$num==i)
    
    dfx = data.frame(grp=ifelse(i%%2==0,0,1), #0 si pair, 1 si impair
                     aeterm = unique(dfx_num$aeterm),
                     aeterm_start = unique(dfx_num$aeterm[dfx_num$rank==min(dfx_num$rank)]),
                     aeterm_end = unique(dfx_num$aeterm[dfx_num$rank==max(dfx_num$rank)]))
    df_rect <- rbind(df_rect,dfx)
  }
  #on merge avec les 3 tables qui servent à construire le graphique (df_p1,df_p2,df_p3)
  df_plot <- merge(df_plot,df_rect, by="aeterm")
  
  #limite de l'axe x en fonction de la valeur max dans la variable aeenddate
  max <- max(df_plot$aeenddate[!is.na(df_plot$aeenddate)]) 
  sup <- ceiling(max/20)
  max <- 20*sup
  
  
  
  ##### plot
  df_plot$AE_GRADE <- as.factor(df_plot$AE_GRADE)
  seqx <-seq(0,max,by=ifelse(max>200,40,20))
  seqdate <- as.character(as.Date(seqx,origin=debttt))
  ltyp <- ifelse(df_plot$AE_MOL1=="No" & (df_plot$AE_MOL2=="No" | df_plot$AE_MOL2=="Not applicable"),"black",
                 ifelse(df_plot$AE_MOL1=="Yes" & (df_plot$AE_MOL2 == "No" | df_plot$AE_MOL2=="Not applicable"), "deepskyblue2",
                        ifelse(df_plot$AE_MOL1=="No" & df_plot$AE_MOL2 == "Yes","red","purple")))
  
  df_plot$ltype=ifelse(df_plot$AE_MOL1=="No" & (df_plot$AE_MOL2=="No" | df_plot$AE_MOL2=="Not applicable"),"B",
                       ifelse(df_plot$AE_MOL1=="Yes" & (df_plot$AE_MOL2 == "No" | df_plot$AE_MOL2=="Not applicable"), "D",
                              ifelse(df_plot$AE_MOL1=="No" & df_plot$AE_MOL2 == "Yes","R","P")))
  
  if (bio==TRUE){
    ## on retire les résultats de biologie après le dernier EI
    datlim <- seqx[length(seqx)]
    df_plot <- df_plot[!(df_plot$biodat > datlim & !is.na(df_plot$biodat > datlim)),]
  }
  
  timeline <- ggplot(df_plot, aes(x=reorder(aeterm,desc(rank)))) +
    #fond rectangles pour séparer les SOC
    geom_rect(aes(xmin = aeterm_start,xmax = aeterm_end), 
              ymin = -Inf, ymax = Inf,
              colour=ifelse(df_plot$grp==1, "gray91","white"),
              fill=ifelse(df_plot$grp==1, "gray91","white"),
              linewidth=7,show.legend = F) +
    
    #lignes verticales tous les 20 jours
    geom_hline(yintercept = seqx, color="grey85", lty=1) + 
    
    #ligne verticale pour Screening
    geom_hline(yintercept = as.numeric(screendate - debttt), color="gray50", lty=1, linewidth=0.5) +
    geom_text(x=-Inf,y=as.numeric(screendate - debttt),label = "Screening", col="gray50", size=4, vjust=-0.3, hjust=-1, angle=90) +
    
    #ligne verticale pour la date de rando
    geom_hline(yintercept = as.numeric(randodate - debttt), color="gray50", lty=1, linewidth=0.5) + 
    geom_text(x=-Inf,y=as.numeric(randodate - debttt),label = "Rando", col="gray50", size=4, vjust=-0.3, hjust=-0.1, angle=90) +
    
    #ligne verticale pour la date de suivi (fin de traitement)
    geom_hline(yintercept = as.numeric(suidate - debttt), color="red", lty=1, linewidth=0.5) +
    geom_text(x=Inf,y=as.numeric(suidate - debttt),label = "Suivi", col="red", size=4, vjust=-0.3, hjust=1.5, angle=90) +
    
    #ligne verticale pleine noir pour le jours 0 : début du traitement
    geom_hline(yintercept = 0, color="gray20", lty=1, linewidth=0.5) +
    
    #segments représentants chaque EI (la couleur représentant le grade)
    geom_segment(aes(y=aestartdate, yend=aeenddate,
                     x=aeterm,xend=aeterm,
                     colour=AE_GRADE),
                 linewidth=6) +
    scale_colour_manual(name="Grade", breaks = vect_grade,labels = vect_grade,
                        values = listcol) +
    new_scale_color() +
    #ligne dans le segment principal s'il sagit d'un SAE
    geom_segment(data=df_plot %>% filter(SAE=="Yes"),
                 aes(y=aestartdate, yend=aeenddate,x=aeterm,xend=aeterm, colour=SAE),linewidth=1) +
    scale_colour_manual(name="",breaks = "Yes",labels = "Serious",values = "black") +
    
    #points pour le début de l'EI (la couleur représantant le liens avec les traitements)
    geom_point(data=df_plot %>% filter(ltype!="B"),
               aes(y=aestartdate,x=reorder(aeterm,desc(rank)),fill=ltype),
               color="black",size=3,shape=21)+
    scale_fill_manual(name="Related", breaks = c("D","R","P"),
                      c(list_ARM[1],list_ARM[2],"Both"),
                      values = c('deepskyblue2', 'red', 'purple')) +
    
    #point pour la fin de l'EI (la forme représentant l'outcome de l'EI)
    geom_point(aes(y=aeenddate, x=reorder(aeterm,desc(rank)), shape = factor(AE_OUT)),size=3, color="black")+
    scale_shape_manual(breaks = c("Recovered", "Recovered with sequelea", "Worsening", "Not recovered", "Death"),
                       values = c("Recovered" = 1, "Recovered with sequelea" = 10,"Worsening" = 0, "Not recovered" = 7, "Death"=8)) +
    
    #flèches pour representer les EIs qui sont ongoing
    geom_segment(data=df_plot %>% filter(AE_ONGO=="Yes"),
                 aes(x=aeterm,xend=aeterm,y=aeenddate,yend=aeenddate+7),
                 arrow = arrow(length=unit(0.2,"cm"), type = "closed"),color="black",linewidth=0.5) +
    
    #points représentant le moment de chaque prise de chaque traitement
    geom_point(aes(y=tttdat,x=aeterm),color='black',
               size=3,shape=20,show.legend = F) +
    
    #lignes verticales pour chaque prise de traitement (pointillés noir)
    geom_hline(yintercept =vect2, lty=3, linewidth = 0.5, color = "gray20") +
    
    # rectangle blanc par dessus les lignes pour faire un fond aux résultats de bio (plus lisible)
    {if (bio==TRUE) geom_rect(xmin = "Bio / Hemo", xmax = "Bio / Hemo", ymin = -Inf,ymax = Inf,
                              colour="white",fill="white",linewidth=10,show.legend = F)} +
    
    #labels pour les résultats d'analyse
    {if (bio==TRUE) geom_text(aes(y=biodat, x=aeterm, label=biores),size=4, angle=45,fontface="bold",
                              colour=ifelse(df_plot$colbio==2,"red",ifelse(df_plot$colbio==1,'red3','black')))} +
    
    #axes verticaux x2 : un pour les jours depuis la rando, un pour les dates brutes de ce patient
    scale_y_continuous(name="Days since rando",breaks = seqx,
                       sec.axis = dup_axis(trans=~.,name="", labels = seqdate)) +
    guides(colour = guide_legend(nrow = 2, byrow = TRUE)) +
    labs(x=NULL, y="Days since rando", shape = "Outcome") + 
    coord_flip() +
    ggtitle(paste0("Span chart for patient ",choixUSU, " (",ARMp,")")) + 
    theme(plot.title = element_text(size=rel(2)),
          panel.background = element_rect(fill="#ffffff", colour=NA),
          axis.title.x=element_text(vjust=-0.2), axis.title.y=element_text(vjust=1.5),
          panel.border = element_blank(), axis.line=element_blank(),
          panel.grid.minor=element_blank(),
          panel.grid.major.y = element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_text(size=rel(1.5)),
          strip.text=element_text(size=rel(1.5)),
          axis.text.y = element_text(size=rel(1.5)),
          axis.text.x.top = element_text(size=rel(1.3), angle=30, vjust=0.5),
          axis.text.x.bottom = element_text(size=rel(1.3)),
          axis.line.x = element_line(color="gray70"),
          axis.ticks.x = element_line(color="gray70"),
          strip.background=element_rect(fill="#ffffff", colour=NA),
          panel.spacing.y=unit(1.5, "lines"),
          legend.position="bottom",
          legend.justification = "left",
          legend.box = "vertical",
          legend.box.just = "left",
          legend.key = element_blank(),
          legend.text = element_text(size=rel(1.2)),
          legend.title = element_text(size=rel(1.2), family = "bold"),
          legend.spacing.y = unit(0.05,'cm'))
  #suppressWarnings(print(timeline))
  res <- list(plot = timeline, height = 170 +length(unique(df_plot$aeterm))*25)
  return(res)
}


# DumbbellAE <- function(data,nbplot,colnbAE=FALSE, caption=TRUE)
DumbbellAE <- function(baseEI, baseTr,CODvar="PT_COD",nbplot=1, nbEvents=FALSE, TriSOC="D", TriEI="D", SOC=TRUE, caption=TRUE){  
  #liste des groupes de traitement de la table df_Tr
  list_ARM <- unique(baseTr$ARM)
  #Liste des id patients dans le bras numéro 1
  list_pat1 <- unique(baseTr$USUBJID[baseTr$ARM == list_ARM[1]]) 
  #Liste des id patients dans le bras numéro 2
  list_pat2 <- unique(baseTr$USUBJID[baseTr$ARM == list_ARM[2]])
  
  #Ajouter une colonne ARM dans la table data en faisant correspondre les USUBJID selon la liste où ils sont présents
  baseEI$ARM <- ifelse(baseEI$USUBJID %in% list_pat1, "arm1", "arm2")
  baseTr$ARM <- ifelse(baseTr$ARM==list_ARM[1], "arm1","arm2")
  
  ############################################################################
  ######################## Construction table dumbbell #######################
  ###################
  # Calcul des freq
  ###################
  ## AE with events
  if (CODvar == "PT_COD") {
    data_distinct <- baseEI %>% select(USUBJID,ARM,PT_COD) %>% distinct(USUBJID, ARM, PT_COD)
    frq1 <- data.frame(xtabs(~ PT_COD + ARM, data = data_distinct))
  } else if (CODvar == "LLT_COD") {
    data_distinct <- baseEI %>% select(USUBJID,ARM,LLT_COD) %>% distinct(USUBJID, ARM, LLT_COD)
    frq1 <- data.frame(xtabs(~ LLT_COD + ARM, data = data_distinct))
  } else return("Option non valide pour CODvar")
  
  #pour ne pas avoir à ajouter un if pour l'option CODvar par la suite on change le nom de la var en COD (générique par rapport aux deux options)
  names(frq1) <- c("COD","ARM","Freq")
  
  ## Total subjects in each ARM
  df_Tr2 <- baseTr[baseTr$TTT_IND=="Yes",] 
  USU_distinct <- df_Tr2 %>% select(USUBJID, ARM, TTT_IND) %>% distinct(USUBJID, ARM) 
  frq2 <- data.frame(xtabs(~ ARM, data=USU_distinct)) 
  #Merged
  frq3 <- merge(frq1, frq2, by="ARM") 
  
  frq4 <- frq3 %>%
    mutate(no = Freq.y - Freq.x, # total - # with event
           yes = Freq.x) %>% # all with AE
    arrange(COD, ARM) %>%
    select(c(ARM, COD, yes, no)) 
  # frq4 : pour chaque type d'EI et par bras, nombre de patients ayant eu cet EI et nombre de patients ne l'ayant pas eu
  
  
  ########### fonction pour le calcul du RD (pris de fmsb::riskdifference) : modif -> pas de print
  RDfunct <- function (a, b, N1, N0, CRC = FALSE, conf.level = 0.95) {
    .M <- a + b
    .T <- N1 + N0
    .R1 <- a/N1
    .R0 <- b/N0
    .RT <- .M/.T
    norm.pp <- qnorm(1 - (1 - conf.level)/2)
    if (CRC) {
      .RC1 <- norm.pp * sqrt(a * (N1 - a)/(N1^3))
      .RC0 <- norm.pp * sqrt(b * (N0 - b)/(N0^3))
      .RCT <- norm.pp * sqrt(.M * (.T - .M)/(.T^3))
      .MAT <- matrix(c(a, b, .M, N1, N0, .T, .R1, .R0, .RT, 
                       .R1 - .RC1, .R0 - .RC0, .RT - .RCT, .R1 + .RC1, 
                       .R0 + .RC0, .RT + .RCT), 3, 5)
      colnames(.MAT) <- c("Cases", "People at Risk", "Risk", 
                          "Lower CL", "Upper CL")
      rownames(.MAT) <- c("Exposed", "Unexposed", "Total")
    }
    else {
      .MAT <- matrix(c(a, b, .M, N1, N0, .T, .R1, .R0, .RT), 
                     3, 3)
      colnames(.MAT) <- c("Cases", "People at risk", "Risk")
      rownames(.MAT) <- c("Exposed", "Unexposed", "Total")
    }
    class(.MAT) <- "table"
    ################ print(.MAT) ####################
    ESTIMATE <- .R1 - .R0
    .CHI <- ESTIMATE/sqrt(a * (N1 - a)/(N1^3) + b * (N0 - b)/(N0^3))
    p.v <- 2 * (1 - pnorm(abs(.CHI)))
    RDL <- ESTIMATE - norm.pp * sqrt(a * (N1 - a)/(N1^3) + b * 
                                       (N0 - b)/(N0^3))
    RDU <- ESTIMATE + norm.pp * sqrt(a * (N1 - a)/(N1^3) + b * 
                                       (N0 - b)/(N0^3))
    CINT <- c(RDL, RDU)
    attr(CINT, "conf.level") <- conf.level
    RVAL <- list(p.value = p.v, conf.int = CINT, estimate = ESTIMATE, 
                 method = "Risk difference and its significance probability (H0: The difference equals to zero)", 
                 data.name = paste(deparse(substitute(a)), deparse(substitute(b)), 
                                   deparse(substitute(N1)), deparse(substitute(N0))))
    class(RVAL) <- "htest"
    return(RVAL)
  }
  
  ################### calcul de RD par PT #####################
  #Creation d'une table pour accueilir toutes les données nécessaires
  dfx_all <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("Freq_Total", "COD","CI_1", "CI_2", "RD"))  
  
  for (p in unique(frq4$COD)){
    #pour chaque PT tableau permettant de calculer la p-value ainsi que le ratio
    df1 <- subset(frq4, COD == p, select = (c(-COD, -ARM)))
    
    dfx <- data.frame(COD = p,
                      frqTot = frq4$yes[frq4$COD == p][1] + frq4$yes[frq4$COD == p][2],
                      RD = RDfunct(df1[[1]][1], df1[[1]][2], frq2$Freq[frq2$ARM=="arm1"], 
                                   frq2$Freq[frq2$ARM=="arm2"], CRC=TRUE)$estimate,
                      CI_1 = RDfunct(df1[[1]][1], df1[[1]][2], frq2$Freq[frq2$ARM=="arm1"], 
                                     frq2$Freq[frq2$ARM=="arm2"], CRC=TRUE)$conf.int[1],
                      CI_2 = RDfunct(df1[[1]][1], df1[[1]][2], frq2$Freq[frq2$ARM=="arm1"], 
                                     frq2$Freq[frq2$ARM=="arm2"], CRC=TRUE)$conf.int[2],
                      row.names = NULL)
    # A chaque PT concatenation des tables
    dfx_all <- rbind(dfx_all,dfx)
  }
  
  ## ranking selon l'option TriEI
  if (TriEI=="D") {dfx_all <- dfx_all %>% arrange(desc(RD))
  } else if (TriEI=="C") {dfx_all <- dfx_all %>% arrange(RD)
  } else if (TriEI=="alpha") {dfx_all <- dfx_all %>% arrange(COD)
  } else return("Valeur non valide pour l'option TriEI")
  dfx_all$rkCOD <- 1:nrow(dfx_all)
  
  # on ajoute le RD  à la table utilisée pour le p1 pour le rank des SOC
  df_p1 <- merge(frq4,dfx_all %>% select(COD,rkCOD), by="COD")
  df_p1$pct <- ifelse(df_p1$ARM=="arm1",df_p1$yes/frq2$Freq[frq2$ARM=="arm1"],df_p1$yes/frq2$Freq[frq2$ARM=="arm2"]) 
  
  if (SOC==TRUE){
    ################# calcul RD par SOC (pour le tri des SOC) ###################
    SOC_dis <- baseEI %>% select(USUBJID,ARM,SOC_COD) %>% distinct(USUBJID, ARM, SOC_COD) 
    frq5 <- data.frame(xtabs(~ SOC_COD + ARM, data = SOC_dis))
    #Merged
    frq6 <- merge(frq5, frq2, by="ARM") 
    frq7 <- frq6 %>%
      mutate(no = Freq.y - Freq.x,
             yes = Freq.x) %>%
      arrange(SOC_COD, ARM) %>%
      select(c(ARM, SOC_COD, yes, no))
    
    dfx_SOC <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("SOC_COD","RD_SOC"))
    for (s in unique(frq7$SOC_COD)){
      df1 <- subset(frq7, SOC_COD == s, select = (c(-SOC_COD, -ARM)))
      dfx <- data.frame(SOC_COD = s,
                        RD_SOC = RDfunct(df1[[1]][1], df1[[1]][2], frq2$Freq[frq2$ARM=="arm1"], 
                                         frq2$Freq[frq2$ARM=="arm2"], CRC=TRUE)$estimate,
                        row.names = NULL)
      # A chaque PT concatenation des tables
      dfx_SOC <- rbind(dfx_SOC,dfx)
    }
    ## ranking selon l'option TriSOC
    if (TriSOC=="D") {dfx_SOC <- dfx_SOC %>% arrange(RD_SOC)
    } else if (TriSOC=="C") {dfx_SOC <- dfx_SOC %>% arrange(desc(RD_SOC))
    } else if (TriSOC=="alpha") {dfx_SOC <- dfx_SOC %>% arrange(desc(SOC_COD))
    } else return("Valeur non valide pour l'option TriSOC")
    dfx_SOC$rkSOC <- 1:nrow(dfx_SOC)
    
    #######################################################
  }
  
  ### calcul des chiffres brutes pour les deux colonnes supp (option nbEvents)
  if (CODvar=="PT_COD") {
    df_AE3 <- baseEI %>% select(USUBJID,PT_COD,ARM)
  } else if (CODvar=="LLT_COD") df_AE3 <- baseEI %>% select(USUBJID,LLT_COD,ARM)
  #idem on renomme la variable LLT_COD ou PT_COD en COD pour ne plus avoir à utiliser de if et rendre plus générique la base df_AE3
  names(df_AE3) <- c("USUBJID","COD","ARM")
  
  frq8 <- data.frame(xtabs(~COD + ARM, data = df_AE3)) 
  frq8 <- pivot_wider(frq8, names_from = ARM, values_from = Freq)
  
  if (SOC==TRUE){
    #On ajoute les SOC_COD correspondants aux PT_COD avec une jointure
    if (CODvar=="PT_COD") {
      tab_SOC <- baseEI %>% select(SOC_COD, PT_COD) %>% group_by(SOC_COD,PT_COD) %>% distinct(SOC_COD,PT_COD)
    } else if (CODvar=="LLT_COD")   tab_SOC <- baseEI %>% select(SOC_COD, LLT_COD) %>% group_by(SOC_COD,LLT_COD) %>% distinct(SOC_COD,LLT_COD)
    #idem on renomme la variable LLT_COD ou PT_COD en COD pour ne plus avoir à utiliser de if et rendre plus générique la base df_AE3
    names(tab_SOC) <- c("SOC_COD","COD")
    
    df_p1 <-left_join(df_p1 %>% select(-no),tab_SOC,by="COD",multiple="all")
    # on ajoute le RD de chaque SOC à la table utilisée pour le p1 pour le rank des SOC
    df_p1 <- merge(df_p1,dfx_SOC, by="SOC_COD")
    df_p2 <- left_join(dfx_all %>% select(-frqTot),tab_SOC, by="COD", multiple="all")
    df_p3 <- left_join(frq8,tab_SOC, by="COD", multiple="all")
  } else {
    df_p2 <- dfx_all %>% select(-frqTot)
    df_p3 <- merge(frq8, df_p1 %>% select(COD, rkCOD), by="COD")
  }
  
  # mettre en évidence par des labels (geom_text) les EIs PT_COD significatifs
  df_p2$test <- ""
  df_p2$test[(df_p2$CI_2<0 & df_p2$CI_1<0) | (df_p2$CI_2>0 & df_p2$CI_1>0)]<-"*"
  
  #### variable rang pour ordonner les modalités
  if (SOC==FALSE){
    names(df_p1) <- c("COD","ARM","yes","no","rk","pct")
    names(df_p2) <- c("COD","RD","CI_1","CI_2","rk","test")
    names(df_p3) <- c("COD", "arm1","arm2","rk")
  } else if (SOC==TRUE){
    tb_rank <- df_p1 %>% distinct(SOC_COD, COD, rkCOD, rkSOC) %>% arrange(desc(rkSOC), desc(rkCOD), COD)
    tb_rank$rk <- 1:nrow(tb_rank)
    #on merge la table tb_rank avec chacune des trois table pour les 3 parties du graphique
    df_p2 <- merge(df_p2, tb_rank %>% select(-rkCOD, -rkSOC, -SOC_COD), by="COD")
    df_p1 <- merge(df_p1, tb_rank %>% select(-rkCOD, -rkSOC, -SOC_COD), by="COD")
    df_p3 <- merge(df_p3, tb_rank %>% select(-rkCOD, -rkSOC, -SOC_COD), by="COD")
    
    # création d'une variable bcol (1 si doit être en gris et 0 sinon)
    df_num <- df_p2 %>% arrange(rk) %>% distinct(SOC_COD)
    df_num$num <- 1:nrow(df_num)
    
    df_num <- merge(df_num, df_p1 %>% select(SOC_COD,COD,rk), by="SOC_COD")
    #A chaque numéro on récupère le min et le max qui seront donc les limites des rectangles pour l'axes des ordonnées (représenté par les PT_COD)
    df_rect <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("grp", "SOC_COD","COD_start", "COD_end"))
    for(i in unique(df_num$num)){
      dfx_num <- subset(df_num, df_num$num==i)
      
      dfx = data.frame(grp=ifelse(i%%2==0,0,1), #0 si pair, 1 si impair
                       SOC_COD = unique(dfx_num$SOC_COD),
                       COD_start = unique(dfx_num$COD[dfx_num$rk==min(dfx_num$rk)]),
                       COD_end = unique(dfx_num$COD[dfx_num$rk==max(dfx_num$rk)]))
      df_rect <- rbind(df_rect,dfx)
    }
    #on merge avec les 3 tables qui servent à construire le graphique (df_p1,df_p2,df_p3)
    df_p1 <- merge(df_p1,df_rect, by="SOC_COD")
    df_p2 <- merge(df_p2,df_rect, by="SOC_COD")
    df_p3 <- merge(df_p3,df_rect, by="SOC_COD")
  }
  
  #on renomme la colonne ARM avec les noms d'origine (tromqué à 8 caractères)
  df_p1$ARM <- ifelse( df_p1$ARM=="arm1", substr(list_ARM[1],start=1, stop=8), substr(list_ARM[2],start=1, stop=8))
  
  ## limite pour le graph
  limpct <- ceiling(max(df_p1$pct)*10) #*10 car on veut prendre à la dizaine supérieure
  limpct <- limpct/10
  
  ###################################################################################
  if (SOC==TRUE){
    levels_SOC <- df_p1 %>% select(SOC_COD,rk) %>% arrange(rk) %>% distinct(SOC_COD)
    levels_SOC <- as.vector(levels_SOC$SOC_COD)
  }
  
  DBplot <- function(data1,data2,data3){
    if (SOC==TRUE){
      data1$SOC_COD <- factor(data1$SOC_COD, levels = c(levels_SOC))
      data2$SOC_COD <- factor(data2$SOC_COD, levels = c(levels_SOC))
      data3$SOC_COD <- factor(data3$SOC_COD, levels = c(levels_SOC))
    } 
    
    p1 <- ggplot(data1,aes(x=pct,y=reorder(COD,rk))) + 
      {if (SOC==TRUE) geom_rect(aes(ymin = COD_start, 
                                    ymax = COD_end), 
                                xmin = -Inf, 
                                xmax = Inf,
                                colour= ifelse(data1$grp==1,"gray91","white"),
                                fill=ifelse(data1$grp==1, "grey91","white"),
                                linewidth=5, show.legend = F)} +
      geom_hline(aes(yintercept = COD), color = "darkgray", linewidth = 0.05, lty=3) +
      geom_vline(xintercept = seq(0,limpct,by=0.25), color = "gray70", linewidth = 0.05, lty=1) +
      geom_point(size=2,aes(colour = factor(ARM), shape = factor(ARM))) +
      xlab('Percentage') + ylab('') +
      scale_colour_manual(values = c("deepskyblue2", "red"), name = "Treatment :") +
      scale_shape_manual(values = c(17,19), name = "Treatment :") +
      scale_x_continuous(name = "Percent of patient",breaks = seq(0,limpct,by=0.25), 
                         labels = scales::label_percent(), limits = c(0,limpct)) +
      {if(SOC==TRUE) facet_grid(SOC_COD ~ ., scales = "free", space = "free", switch = "y")} +
      theme(legend.position="bottom",
            legend.justification = c("right","top"),
            legend.box.just = "right",
            legend.margin = margin(6,6,6,6),
            legend.text = element_text(size=10),
            axis.line.x = element_line(color = "black", linetype = 1),
            axis.line.y = element_line(color = "black", linetype = 1),
            axis.ticks.y = element_blank(),
            axis.text.y = element_text(size = 10),
            axis.text.x = element_text(hjust = 1, size = 10),
            axis.title.x = element_text(size=12),
            panel.background = element_blank(),
            strip.text.y.left = element_text(angle = 0, hjust = 1, size = 10),
            strip.placement = "outside",
            panel.spacing.y = unit(3,"pt"))
    
    q <- ggplotGrob(p1)
    lg <- linesGrob(x=unit(c(1,1),"npc"), y=unit(c(0,1),"npc"), 
                    gp=gpar(col="gray5", lwd=1))
    for (k in grep("strip-l",q$layout$name)) {
      q$grobs[[k]]$grobs[[1]]$children[[1]] <- lg
    }
    
    p2 <- ggplot(data=data2, aes(x=reorder(COD,rk), y=RD, ymin=CI_2, ymax=CI_1)) +
      {if (SOC==TRUE) geom_rect(aes(xmin = COD_start, 
                                    xmax = COD_end), 
                                ymin = -Inf, 
                                ymax = Inf,
                                colour=ifelse(data2$grp==1, "gray91","white"),
                                fill=ifelse(data2$grp==1, "grey91","white"),
                                linewidth=5, show.legend = F)} +
      geom_vline(aes(xintercept = COD), color = "darkgray", linewidth = 0.05, lty=3) +
      geom_point(aes(shape="RD"), size=1.5) +
      scale_shape_manual(values = c("RD"=19)) +
      geom_errorbar(aes(color="IC"),width=0.5, linewidth=0.5) +
      scale_color_manual(values = c("IC"="black")) +
      geom_hline(yintercept=0, lty=2, colour = "red", linewidth = 1) +  # add a dotted line at x=1 after flip
      geom_text(aes(x=COD, label=test,y=CI_2), col="red", size=6,  hjust=-1, vjust=0.6) +
      scale_y_continuous(name = "Risk Difference with 95% CI")+
      coord_flip() +
      {if (SOC==TRUE) facet_grid(SOC_COD ~ ., scales = "free", space = "free", switch = "y")} +
      theme(axis.text.y = element_blank(),
            axis.text.x = element_text(size=10),
            axis.title.y = element_blank(),
            axis.title.x = element_text(size=12),
            panel.background = element_blank(),
            panel.grid.major.y = element_line(color = "darkgray", linewidth = 0.1, linetype = 3),
            axis.ticks.y = element_blank(),
            axis.ticks.x = element_line(linewidth = 1, colour = "black"),
            axis.line.x = element_line(color = "black", linetype = 1),
            legend.position="bottom",
            legend.title=element_blank(),
            strip.text.y.left = element_blank(),
            strip.placement = "none",
            panel.spacing.y = unit(3,"pt"))
    
    #deux col avec les chiffres brutes, ne seront ajoutés que si nbEvents est TRUE
    if (nbEvents==TRUE){
      p3 <-ggplot(data3,aes(x="",y=reorder(COD,rk))) +
        {if (SOC==TRUE) geom_rect(aes(ymin = COD_start, ymax = COD_end), 
                                  xmin = -Inf,  xmax = Inf,
                                  colour=ifelse(data3$grp==1, "gray91","white"),
                                  fill=ifelse(data3$grp==1, "grey91","white"),
                                  linewidth=5, show.legend = F)} +
        geom_text(aes(label=arm2, colour="red"), size=3.5, fontface="bold") +
        scale_colour_manual(values=c("red"="red")) +
        xlab("NbTot") +
        {if (SOC==TRUE) facet_grid(SOC_COD ~ ., scales = "free", space = "free", switch = "y")} +
        guides(colour = guide_legend(title=list_ARM[2],label=FALSE)) +
        theme(axis.text.y = element_blank(),
              axis.title.y = element_blank(),
              panel.background = element_blank(),
              strip.text.y.left = element_blank(),
              strip.placement = "none",
              legend.position = "bottom",
              axis.line.x = element_line(color = "black", linetype = 1),
              axis.ticks.y = element_blank(),
              axis.ticks.x = element_blank())
      
      p4 <-ggplot(data3,aes(x="",y=reorder(COD,rk))) +
        {if (SOC==TRUE) geom_rect(aes(ymin = COD_start,  ymax = COD_end), 
                                  xmin = -Inf, xmax = Inf,
                                  colour=ifelse(data3$grp==1, "gray91","white"),
                                  fill=ifelse(data3$grp==1, "grey91","white"),
                                  linewidth=5, show.legend = F)} +
        geom_text(aes(label=arm1, colour="deepskyblue"), size=3.5, fontface="bold") +
        scale_colour_manual(values=c("deepskyblue"="deepskyblue"))+
        xlab("NbTot") +
        {if (SOC==TRUE) facet_grid(SOC_COD ~ ., scales = "free", space = "free", switch = "y")} +
        guides(colour = guide_legend(title=list_ARM[1],label=FALSE)) +
        theme(axis.text.y = element_blank(),
              axis.title.y = element_blank(),
              panel.background = element_blank(),
              strip.text.y.left = element_blank(),
              strip.placement = "none",
              legend.position = "bottom",
              axis.line.x = element_line(color = "black", linetype = 1),
              axis.ticks.y = element_blank(),
              axis.ticks.x = element_blank())
      
      if (SOC==TRUE){
        plt <- plot_grid(q,  p2, p3 , p4, labels = NULL,nrow = 1,rel_widths = c(0.7, 0.2,0.05,0.05))
      } else {
        plt <- plot_grid(q,  p2, p3 , p4, labels = NULL,nrow = 1,rel_widths = c(0.5, 0.4,0.05,0.05))
      }
      
      labcap <- "
      Visual representation of AE data, Dumbbell plot + forest plot for adverse events between two treatment arms. 
      The left side of the figure displays the percentage of participants experiencing an adverse event (labelled on the y-axis) in the testing arm with a red circle and control arm with a blue triangle. 
      The middle part of the figure displays the Risk Difference and corresponding 95% confidence interval.
      The left side displays two columns with the absolute number of AE in each groups (multiple episodes are counted).
      SOC are sorted by their RD and inside PT are sorted by their RD, each in increasing order." 
    } else {
      if (SOC==TRUE){
        plt <- plot_grid(q,  p2, labels = NULL,nrow = 1,rel_widths = c(0.8, 0.3))
      } else {
        plt <- plot_grid(q,  p2, labels = NULL,nrow = 1,rel_widths = c(0.6, 0.4))
      }
      
      labcap <- "
      Visual representation of AE data, Dumbbell plot + forest plot for adverse events between two treatment arms. 
      The left side of the figure displays the percentage of participants experiencing an adverse event (labelled on the y-axis) in the testing arm with a red circle and control arm with a blue triangle. 
      The right side of the figure displays the Risk Difference and corresponding 95% confidence interval.
      SOC are sorted by their RD and inside PT are sorted by their RD, each in increasing order."
    }
    
    if (caption==TRUE){
      plt <- add_sub(plt,labcap,x=0, hjust=0, size=11)
    }
    ggdraw(plt)
  }
  
  #### séparation du graph en nbplot car trop de PT_ affichées
  #On fait des listes de SOC à peu près égales et on diviser les tables et les graphs selon ces deux listes
  if (SOC==TRUE) {
    nb_SOC <- length(levels_SOC)
    lim <- floor(nb_SOC/nbplot)
    res <- list()
    for (i in 1:nbplot){
      if (i==1) l_SOC <- levels_SOC[1:lim]
      else if (i==nbplot) {
        a <- (lim*(i-1))+(i-1)
        l_SOC <- levels_SOC[a:nb_SOC]
      } else {
        a <- (lim*(i-1))+(i-1)
        b <- (lim*i)+(i-1)
        l_SOC <- levels_SOC[a:b]
      }
      
      df_p1i <- subset(df_p1, SOC_COD %in% l_SOC)
      df_p2i <- subset(df_p2, SOC_COD %in% l_SOC)
      df_p3i <- subset(df_p3, SOC_COD %in% l_SOC)
      
      res[[i]] <- DBplot(df_p1i,df_p2i,df_p3i)
    }
  } else if (SOC==FALSE){
    levels_COD <- unique(df_p1$COD)
    nb_COD <- length(levels_COD)
    lim <- floor(nb_COD/nbplot)
    res <- list()
    
    for (i in 1:nbplot){
      if (i==1) l_COD <- levels_COD[1:lim]
      else if (i==nbplot) {
        a <- (lim*(i-1))+(i-1)
        l_COD <- levels_COD[a:nb_COD]
      } else {
        a <- (lim*(i-1))+(i-1)
        b <- (lim*i)+(i-1)
        l_COD <- levels_COD[a:b]
      }
      
      df_p1i <- subset(df_p1, COD %in% l_COD)
      df_p2i <- subset(df_p2, COD %in% l_COD)
      df_p3i <- subset(df_p3, COD %in% l_COD)
      res[[i]] <- DBplot(df_p1i,df_p2i,df_p3i)
    }
  }
  
  if (nbplot==1){
    res <- list(plot = DBplot(df_p1,df_p2,df_p3), height = 200+length(unique(dfx_all$COD))*20)
    return(res)
  } else if (nbplot>1){
    return(res) 
  }
}


Dumbbell3grp_rel <- function(data, format="dumbbell"){
  #Liste des id patients dans le bras GC
  list_pat_GC <- unique(df_Tr$USUBJID[df_Tr$ARM == "GC"]) #23 patients
  #Liste des id patients dans le bras GC + Avelumab
  list_pat_AVE <- unique(df_Tr$USUBJID[df_Tr$ARM == "GC + Avelumab"]) #42 patients
  #Ajouter une colonne ARM dans la table data
  data$ARM <- ifelse(data$USUBJID %in% list_pat_GC, "GC", "GC + Avelumab")
  
  
  ###################################################
  ### Subgroup Related GC ou Avelumab
  ###################################################
  dfGCAVE <- data %>% select(USUBJID, ARM, PT_COD, AE_GC, AE_AVE) %>% distinct(USUBJID, ARM,PT_COD, AE_GC, AE_AVE)
  dfGCAVE <- subset(dfGCAVE, AE_AVE == "Yes")
  dfGCAVE <- dfGCAVE %>% select(-AE_GC, -AE_AVE)
  frq1 <- data.frame(xtabs(~ PT_COD + ARM, data = dfGCAVE)) #frequence de chaque type d'EI dans chacun des bras de traitement
  frq2 <- df_Tr[df_Tr$TTT_IND=="Yes",] #on récupère une sous table avec seulement les patients ayant eu au moins 1 fois le traitement
  frq2 <- frq2 %>% select(USUBJID, ARM, TTT_IND) %>% distinct(USUBJID, ARM) # liste des patients avec leur bras de traitement
  frq2 <- data.frame(xtabs(~ ARM, data=frq2)) # compte le nombre de patient dans chacun des bras
  #Merged
  frq3 <- merge(frq1, frq2, by="ARM")
  frq4 <- frq3 %>%
    mutate(no = Freq.y - Freq.x, #total - # with event
           yes = Freq.x) %>% # all with AE
    arrange(PT_COD, ARM) %>%
    select(c(ARM, PT_COD, yes, no))
  
  ########### fontion pour le calcul du RD (pris de fmsb::riskdifference) : modif -> pas de print
  # RDfunct
  
  ################### calcul de RD #####################
  #Creation d'une table pour accueilir toutes les données nécessaires
  dfx_GCAVE <- setNames(data.frame(matrix(ncol = 6, nrow = 0)), c("Freq_Total", "PT_COD","CI_1", "CI_2", "RD"))
  
  for (p in unique(frq4$PT_COD)){
    #pour chaque PT tableau permettant de calculer la p-value ainsi que le ratio
    df1 <- subset(frq4, PT_COD == p, select = (c(-PT_COD, -ARM)))
    
    dfx <- data.frame(PT_COD = p,
                      subgroup="RelatedAll",
                      frqTot = 0 + frq4$yes[frq4$PT_COD == p][1],
                      frqGC = 0,
                      frqAVE = frq4$yes[frq4$PT_COD == p & frq4$ARM=="GC + Avelumab"],
                      RD = RDfunct(0, df1[[1]][1], frq2$Freq[frq2$ARM=="GC"], frq2$Freq[frq2$ARM=="GC + Avelumab"], CRC=TRUE)$estimate,
                      CI_1 = RDfunct(0, df1[[1]][1], frq2$Freq[frq2$ARM=="GC"], frq2$Freq[frq2$ARM=="GC + Avelumab"], CRC=TRUE)$conf.int[1],
                      CI_2 = RDfunct(0, df1[[1]][1], frq2$Freq[frq2$ARM=="GC"], frq2$Freq[frq2$ARM=="GC + Avelumab"], CRC=TRUE)$conf.int[2],
                      row.names = NULL)
    # A chaque PT concatenation des tables
    dfx_GCAVE <- rbind(dfx_GCAVE,dfx)
  }
  dfx_GCAVE$pct = round(dfx_GCAVE$frqTot / sum(frq2$Freq),2)
  dfx_GCAVE$pctGC = round(dfx_GCAVE$frqGC / frq2$Freq[1],2)
  dfx_GCAVE$pctAVE = round(dfx_GCAVE$frqAVE / frq2$Freq[2],2)
  
  ###################################################
  ### Subgroup Related GC (group GC ou GC + Avelumab)
  ###################################################
  dfGC <- data %>% select(USUBJID, ARM, PT_COD, AE_GC) %>% distinct(USUBJID, ARM,PT_COD, AE_GC)
  dfGC <- subset(dfGC, AE_GC == "Yes")
  dfGC <- dfGC %>% select(-AE_GC)
  frq1 <- data.frame(xtabs(~ PT_COD + ARM, data = dfGC)) #frequence de chaque type d'EI dans chacun des bras de traitement
  #Merged
  frq3 <- merge(frq1, frq2, by="ARM")
  frq4 <- frq3 %>%
    mutate(no = Freq.y - Freq.x, #total - # with event
           yes = Freq.x) %>% # all with AE
    arrange(PT_COD, ARM) %>%
    select(c(ARM, PT_COD, yes, no))
  
  ################### calcul de RD #####################
  #Creation d'une table pour accueilir toutes les données nécessaires
  dfx_GC <- setNames(data.frame(matrix(ncol = 6, nrow = 0)), c("Freq_Total", "PT_COD","CI_1", "CI_2", "RD"))
  
  for (p in unique(frq4$PT_COD)){
    #pour chaque PT tableau permettant de calculer la p-value ainsi que le ratio
    df1 <- subset(frq4, PT_COD == p, select = (c(-PT_COD, -ARM)))
    
    dfx <- data.frame(PT_COD = p,
                      subgroup="RelatedGC",
                      frqTot = frq4$yes[frq4$PT_COD == p][1] + frq4$yes[frq4$PT_COD == p][2],
                      frqGC = frq4$yes[frq4$PT_COD == p & frq4$ARM=="GC"],
                      frqAVE = frq4$yes[frq4$PT_COD == p & frq4$ARM=="GC + Avelumab"],
                      RD = RDfunct(df1[[1]][1], df1[[1]][2], frq2$Freq[frq2$ARM=="GC"], frq2$Freq[frq2$ARM=="GC + Avelumab"], CRC=TRUE)$estimate,
                      CI_1 = RDfunct(df1[[1]][1], df1[[1]][2], frq2$Freq[frq2$ARM=="GC"], frq2$Freq[frq2$ARM=="GC + Avelumab"], CRC=TRUE)$conf.int[1],
                      CI_2 = RDfunct(df1[[1]][1], df1[[1]][2], frq2$Freq[frq2$ARM=="GC"], frq2$Freq[frq2$ARM=="GC + Avelumab"], CRC=TRUE)$conf.int[2],
                      row.names = NULL)
    # A chaque PT concatenation des tables
    dfx_GC <- rbind(dfx_GC,dfx)
  }
  dfx_GC$pct = round(dfx_GC$frqTot / sum(frq2$Freq),2)
  dfx_GC$pctGC = round(dfx_GC$frqGC / frq2$Freq[1],2)
  dfx_GC$pctAVE = round(dfx_GC$frqAVE / frq2$Freq[2],2)
  
  
  #### table finale pour le plot
  tb_plot <- rbind(dfx_GCAVE, dfx_GC)
  
  ####
  if(format=="dumbbell") {
    tb_plot <- tb_plot %>%
      select(-frqTot, -frqGC, -frqAVE) %>%
      pivot_longer(cols = c("pctGC","pctAVE"),names_to = "ARM", values_to = "pctgrp")
  }
  else if (format=="text") {
    tb_plot <- tb_plot %>%
      select(-pctGC, -pctAVE, -pct) %>%
      pivot_longer(cols = c("frqGC","frqAVE"),names_to = "ARM", values_to = "frqgrp")
  }
  # tb_plot$grptxt <- paste0(tb_plot$subgroup,"_",tb_plot$ARM)
  
  
  ################# calcul RD par SOC (pour le tri des SOC) ###################
  frq5 <- data %>% select(USUBJID,ARM,SOC_COD) %>% distinct(USUBJID, ARM, SOC_COD)
  frq5 <- data.frame(xtabs(~ SOC_COD + ARM, data = frq5))
  #Merged
  frq6 <- merge(frq5, frq2, by="ARM")
  frq7 <- frq6 %>%
    mutate(no = Freq.y - Freq.x, #total - # with event
           yes = Freq.x) %>% # all with AE
    arrange(SOC_COD, ARM) %>%
    select(c(ARM, SOC_COD, yes, no))
  
  dfx_SOC <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("SOC_COD","RD_SOC"))
  for (s in unique(frq7$SOC_COD)){
    #pour chaque PT tableau permettant de calculer la p-value ainsi que le ratio
    df1 <- subset(frq7, SOC_COD == s, select = (c(-SOC_COD, -ARM)))
    dfx <- data.frame(SOC_COD = s,
                      RD_SOC = RDfunct(df1[[1]][1], df1[[1]][2], frq2$Freq[frq2$ARM=="GC"], frq2$Freq[frq2$ARM=="GC + Avelumab"], CRC=TRUE)$estimate,
                      row.names = NULL)
    # A chaque PT concatenation des tables
    dfx_SOC <- rbind(dfx_SOC,dfx)
  }
  #######################################################
  
  ## limite pour le graph
  # limpct <- ceiling(max(tb_plot$pct)*10) #*10 car on veut prendre à la dizaine supérieure
  # limpct <- limpct/10
  limpct=1
  
  ## On ajoute les SOC_COD correspondants aux PT_COD avec une jointure
  tab_SOC <- data %>% select(SOC_COD, PT_COD) %>% group_by(SOC_COD,PT_COD) %>% distinct(SOC_COD,PT_COD)
  tb_plot <- left_join(tb_plot,tab_SOC, by="PT_COD", multiple="all")
  # on ajoute le RD de chaque SOC à la table utilisée pour le p1 pour le rank des SOC
  tb_plot <- merge(tb_plot,dfx_SOC, by="SOC_COD")
  
  ## mettre en évidence par des labels (geom_text) les EIs PT_COD significatifs
  tb_plot$test <- ""
  tb_plot$test[tb_plot$CI_2<0 & tb_plot$CI_1<0]<-"*"
  tb_plot$test[tb_plot$CI_2>0 & tb_plot$CI_1>0]<-"*"
  
  ## variable rang pour ordonner les modalités
  #si PT présent en double alors on prend celui qui correspont à RelatedGC (automatique regarder comment choisir plutôt de garder RelatedAll)
  tb_plot_rk <- tb_plot %>% group_by(SOC_COD,PT_COD) %>% filter(!duplicated(PT_COD)) %>% ungroup()
  
  tb_rank <- tb_plot_rk %>% distinct(SOC_COD, PT_COD, RD, RD_SOC) %>% arrange(desc(RD_SOC), desc(RD), PT_COD)
  tb_rank$rk <- 1:nrow(tb_rank)
  tb_plot <- merge(tb_plot, tb_rank %>% select(-c(SOC_COD,RD, RD_SOC)), by="PT_COD")
  
  ## création d'une variable bcol (1 si doit être en gris et 0 sinon)
  df_pcol <- tb_plot %>% arrange(rk) %>% distinct(SOC_COD)
  df_pcol$bcol <- 0
  for (i in 1:nrow(df_pcol)){
    if (i %% 2 != 0){
      df_pcol$bcol[i] <- 1
    }
  }
  tb_plot <- merge(tb_plot,df_pcol, by="SOC_COD")
  
  ### création d'un data frame pour les emplacement des couleurs
  # variable start/end : PT_COD du début de la zone et PT_COD de la fin de la zone
  # variable colors pour la couleur
  tb_plot <- tb_plot %>% arrange(rk)
  cptS=2
  cptE=1
  SOC_COD = ""
  PT_start = ""
  PT_end = ""
  PT_start[1] <- tb_plot$PT_COD[1]
  SOC_COD[1] <- tb_plot$SOC_COD[1]
  for (i in 1:(nrow(tb_plot)-1)){
    if (tb_plot$bcol[i]==0 & tb_plot$bcol[i+1]==1){
      PT_start[cptS] <- tb_plot$PT_COD[i+1]
      SOC_COD[cptS] <- tb_plot$SOC_COD[i+1]
      cptS=cptS+1
    }
    if (tb_plot$bcol[i]==1 & tb_plot$bcol[i+1]==0){
      PT_end[cptE] <- tb_plot$PT_COD[i]
      cptE=cptE+1
    }
  }
  ##si nombre de SOC impaire
  if(length(unique(tb_plot$SOC_COD))%%2!=0)PT_end[cptE] <- tb_plot$PT_COD[nrow(tb_plot)]
  data_breaks1 <- as.data.frame(cbind(SOC_COD, PT_start, PT_end, col=1))
  cptS=1
  cptE=1
  SOC_COD = ""
  PT_start = ""
  PT_end = ""
  for (i in 1:(nrow(tb_plot)-1)){
    if (tb_plot$bcol[i]==1 & tb_plot$bcol[i+1]==0){
      PT_start[cptS] <- tb_plot$PT_COD[i+1]
      SOC_COD[cptS] <- tb_plot$SOC_COD[i+1]
      cptS=cptS+1
    }
    if (tb_plot$bcol[i]==0 & tb_plot$bcol[i+1]==1){
      PT_end[cptE] <- tb_plot$PT_COD[i]
      cptE=cptE+1
    }
  }
  ##si nombre de SOC paire
  if(length(unique(tb_plot$SOC_COD))%%2==0)PT_end[cptE]<-tb_plot$PT_COD[nrow(tb_plot)]
  data_breaks2 <- as.data.frame(cbind(SOC_COD, PT_start, PT_end, col=0))
  
  data_breaks <- rbind(data_breaks1, data_breaks2)
  data_breaks$SOC_COD <- as.factor(data_breaks$SOC_COD)
  tb_plot$SOC_COD <- as.factor(tb_plot$SOC_COD)
  tb_plot <- left_join(tb_plot,data_breaks, by="SOC_COD", multiple="all")
  
  levels_SOC <- tb_plot %>% select(SOC_COD,rk) %>% arrange(rk)
  levels_SOC <- levels_SOC %>% distinct(SOC_COD)
  levels_SOC <- as.vector(levels_SOC$SOC_COD)
  
  ## liste modalités
  lmod <- c("RelatedAll","RelatedGC")
  lcol <- c("#9A25E8","#179F75")
  
  
  #### plot
  DBplot <- function(data1){
    data1$SOC_COD <- factor(data1$SOC_COD, levels = c(levels_SOC))
    
    if (format=="dumbbell"){
      p1 <- ggplot(data1,aes(x=pctgrp,y=reorder(PT_COD,rk))) +
        geom_rect(aes(ymin = PT_start,  ymax = PT_end),
                  xmin = -Inf, xmax = Inf,
                  colour= ifelse(data1$col==1,"gray91","white"),
                  fill=ifelse(data1$col==1, "grey91","white"),
                  linewidth=5, show.legend = F) +
        geom_hline(aes(yintercept = PT_COD), color = "darkgray", linewidth = 0.05, lty=3) +
        geom_vline(xintercept = seq(0,1,by=0.2), color = "black", linewidth = 0.05, lty=1) +
        geom_point(size=2.5,aes(colour = factor(subgroup), shape = factor(ARM))) +
        scale_x_continuous(breaks = seq(0,limpct,by=0.2), limits = c(0,limpct)) +
        scale_color_manual(name="groupes percentages", values = lcol, breaks=lmod) +
        scale_shape_manual(name="groupes percentages", values = c(19,9),
                           breaks = c("pctGC","pctAVE"),labels=c("GC","GC+Ave")) +
        labs(x="Percent of patient") +
        facet_grid(SOC_COD ~ ., scales = "free", space = "free", switch = "y") +
        guides(colour = guide_legend(ncol = 1,nrow=2, byrow = TRUE),
               shape = guide_legend(ncol = 1,nrow=2, byrow = TRUE)) +
        theme(axis.line = element_line(color = "black", linetype = 1),
              axis.ticks.y = element_blank(),
              axis.text.y = element_text(size = rel(1.2)),
              axis.title.x = element_text(size=rel(1.5)),
              axis.title.y = element_blank(),
              axis.text.x = element_text(hjust = 1, size=rel(1.2)),
              panel.background = element_blank(),
              legend.position="bottom",
              legend.title = element_blank(),
              legend.text = element_text(size = rel(1.2)),
              panel.grid.major = element_line(linewidth = 0.5, color="gray90"),
              strip.text.y.left = element_text(angle = 0, hjust = 1, size = rel(1.2)),
              strip.placement = "outside",
              panel.spacing.y = unit(3,"pt"))
    } else if (format=="text"){
      p1 <- ggplot(data1,aes(x=factor(ARM),y=reorder(PT_COD,rk))) +
        geom_rect(aes(ymin = PT_start, ymax = PT_end),
                  xmin = -Inf,  xmax = Inf,
                  colour= ifelse(data1$col==1,"gray91","white"),
                  fill=ifelse(data1$col==1, "grey91","white"),
                  linewidth=5, show.legend = F) +
        geom_text(aes(label=frqgrp, color=factor(subgroup)), size=5) +
        scale_color_manual(name="groupes percentages", values = lcol, breaks=lmod) +
        facet_grid(SOC_COD ~ subgroup, scales = "free", space = "free", switch = "both") +
        theme(axis.line = element_line(color = "black", linetype = 1),
              axis.ticks.y = element_blank(),
              axis.text.y = element_text(size = rel(1.2)),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = element_text(hjust = 1,size=rel(1.3),angle=75),
              panel.background = element_blank(),
              legend.position="none",
              legend.text = element_text(size = rel(1.2)),
              panel.grid.major = element_line(linewidth = 0.5, color="gray90"),
              strip.text.y.left = element_text(angle = 0, hjust = 1, size = rel(1.2)),
              strip.text.x.bottom = element_text(angle = 0, hjust = 0.5, size = rel(1.5)),
              strip.placement = "outside",
              panel.spacing.y = unit(3,"pt"))
    }
    
    q <- ggplotGrob(p1)
    lg <- linesGrob(x=unit(c(1,1),"npc"), y=unit(c(0,1),"npc"),
                    gp=gpar(col="gray20", lwd=1))
    for (k in grep("strip-l",q$layout$name)) {
      q$grobs[[k]]$grobs[[1]]$children[[1]] <- lg
    }
    
    valXdodge=-0.8
    
    if (format=="dumbbell") nr <- 2
    else if (format == "text") nr<-1
    
    p2 <- ggplot(data1, aes(y=RD, ymin=CI_1, ymax=CI_2, x=reorder(PT_COD,rk))) +
      geom_rect(aes(xmin = PT_start,xmax = PT_end),
                ymin = -Inf,ymax = Inf,
                colour=ifelse(data1$col==1, "gray91","white"),
                fill=ifelse(data1$col==1, "grey91","white"),
                linewidth=5, show.legend = F) +
      geom_vline(aes(xintercept = PT_COD), color = "darkgray", linewidth = 0.05, lty=3) +
      geom_hline(yintercept=0, lty=2, colour = "black", linewidth = 0.5) +
      geom_point(size=2, shape=19, aes(colour= subgroup, shape=subgroup),
                 position = position_dodge(width = valXdodge)) +
      geom_errorbar(width=0, linewidth=1, aes(colour=subgroup),
                    position = position_dodge(width = valXdodge)) +
      geom_text(aes(x=PT_COD, label=test,y=CI_2), col="red", size=7,  hjust=-1, vjust=0.6) +
      scale_color_manual(values = lcol, breaks=lmod) +
      facet_grid(SOC_COD ~ ., scales = "free", space = "free", switch = "y") +
      coord_flip() +
      guides(colour = guide_legend(nrow = nr, byrow = TRUE)) +
      theme(axis.line.x = element_line(color = "black", linetype = 1),
            axis.line.y = element_line(color = "gray80", linetype = 1),
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_text(size=rel(1.2)),
            axis.text.x = element_text(hjust = 1, size=rel(1.2)),
            panel.background = element_blank(),
            panel.grid.major = element_line(linewidth = 0.5, color="gray90"),
            legend.title = element_blank(),
            legend.text = element_text(size=rel(1.2)),
            legend.position="bottom",
            strip.text.y.left = element_blank(),
            strip.placement = "none",
            panel.spacing.y = unit(3,"pt"))
    
    plot_grid(q,  p2, labels = NULL,nrow = 1,rel_widths = c(0.7, 0.3))
  }
  
  #### séparation du graph en nbplot car trop de PT_ affichées
  #On fait des listes de SOC à peu près égales et on diviser les tables et les graphs selon ces deux listes
  # nb_SOC <- length(levels_SOC)
  # lim <- floor(nb_SOC/nbplot)
  # res <- list()
  # for (i in 1:nbplot){
  #   if (i==1) l_SOC <- levels_SOC[1:lim]
  #   else if (i==nbplot) {
  #     a <- (lim*(i-1))+(i-1)
  #     l_SOC <- levels_SOC[a:nb_SOC]
  #   }
  #   else {
  #     a <- (lim*(i-1))+(i-1)
  #     b <- (lim*i)+(i-1)
  #     l_SOC <- levels_SOC[a:b]
  #   }
  #
  #   tb_ploti <- subset(tb_plot, SOC_COD %in% l_SOC)
  #
  #   res[[i]] <- DBplot(tb_ploti)
  # }
  
  plot <- DBplot(tb_plot)
  res <- list(plot = plot, height = 100+length(unique(tb_plot$PT_COD))*20)
  return(res)
}

Dumbbell3grp_epi <- function(data, format="dumbbell"){
  #Liste des id patients dans le bras GC
  list_pat_GC <- unique(df_Tr$USUBJID[df_Tr$ARM == "GC"]) #23 patients
  #Liste des id patients dans le bras GC + Avelumab
  list_pat_AVE <- unique(df_Tr$USUBJID[df_Tr$ARM == "GC + Avelumab"]) #42 patients
  #Ajouter une colonne ARM dans la table data
  data$ARM <- ifelse(data$USUBJID %in% list_pat_GC, "GC", "GC + Avelumab")
  
  frq1 <- data.frame(xtabs(~ PT_COD + ARM, data = data)) #frequence de chaque type d'EI dans chacun des bras de traitement
  frq2 <- df_Tr[df_Tr$TTT_IND=="Yes",] #on récupère une sous table avec seulement les patients ayant eu au moins 1 fois le traitement
  frq2 <- frq2 %>% select(USUBJID, ARM, TTT_IND) %>% distinct(USUBJID, ARM) # liste des patients avec leur bras de traitement
  frq2 <- data.frame(xtabs(~ ARM, data=frq2)) # compte le nombre de patient dans chacun des bras
  
  #######
  df <- data %>% select(USUBJID, ARM, PT_COD) %>% arrange(USUBJID, ARM,PT_COD)
  df <- df %>% group_by(USUBJID, ARM, PT_COD) %>% summarise(Count=n())
  df$SINGLE <- ifelse(df$Count==1, 1,0)
  df$MULTIPLE <- ifelse(df$Count>1, 1, 0)
  
  
  ###################################################
  ### Subgroup single
  ###################################################
  dfS <- df %>% select(-Count,-MULTIPLE)
  dfS <- dfS %>% group_by(ARM, PT_COD) %>% summarise(Count=sum(SINGLE))
  dfS <- dfS %>% pivot_wider(values_from = Count, names_from = ARM, values_fill = 0)
  dfS <- dfS %>% pivot_longer(names_to = "ARM", values_to = "Count", cols = c("GC", "GC + Avelumab"))
  frq3 <- merge(dfS, frq2, by="ARM")
  frq4 <- frq3 %>%
    mutate(no = Freq - Count, #total - # with event
           yes = Count) %>% # all with AE
    arrange(PT_COD, ARM) %>%
    select(c(ARM, PT_COD, yes, no))
  
  ########### fontion pour le calcul du RD (pris de fmsb::riskdifference) : modif -> pas de print
  # RDfunct
  
  ################### calcul de RD #####################
  #Creation d'une table pour accueilir toutes les données nécessaires
  dfx_Single <- setNames(data.frame(matrix(ncol = 6, nrow = 0)), c("Freq_Total", "PT_COD","CI_1", "CI_2", "RD"))
  
  for (p in unique(frq4$PT_COD)){
    #pour chaque PT tableau permettant de calculer la p-value ainsi que le ratio
    df1 <- subset(frq4, PT_COD == p, select = (c(-PT_COD, -ARM)))
    
    dfx <- data.frame(PT_COD = p,
                      subgroup="Single",
                      frqTot = frq4$yes[frq4$PT_COD == p][1] + frq4$yes[frq4$PT_COD == p][2],
                      frqGC = frq4$yes[frq4$PT_COD == p & frq4$ARM=="GC"],
                      frqAVE = frq4$yes[frq4$PT_COD == p & frq4$ARM=="GC + Avelumab"],
                      RD = RDfunct(df1[[1]][1], df1[[1]][2], frq2$Freq[frq2$ARM=="GC"], frq2$Freq[frq2$ARM=="GC + Avelumab"], CRC=TRUE)$estimate,
                      CI_1 = RDfunct(df1[[1]][1], df1[[1]][2], frq2$Freq[frq2$ARM=="GC"], frq2$Freq[frq2$ARM=="GC + Avelumab"], CRC=TRUE)$conf.int[1],
                      CI_2 = RDfunct(df1[[1]][1], df1[[1]][2], frq2$Freq[frq2$ARM=="GC"], frq2$Freq[frq2$ARM=="GC + Avelumab"], CRC=TRUE)$conf.int[2],
                      row.names = NULL)
    # A chaque PT concatenation des tables
    dfx_Single <- rbind(dfx_Single,dfx)
  }
  dfx_Single$pct = round(dfx_Single$frqTot / sum(frq2$Freq),2)
  dfx_Single$pctGC = round(dfx_Single$frqGC / frq2$Freq[1],2)
  dfx_Single$pctAVE = round(dfx_Single$frqAVE / frq2$Freq[2],2)
  
  ###################################################
  ### Subgroup Multiple
  ###################################################
  dfM <- df %>% select(-Count,-SINGLE)
  dfM <- dfM %>% group_by(ARM, PT_COD) %>% summarise(Count=sum(MULTIPLE))
  dfM <- dfM %>% pivot_wider(values_from = Count, names_from = ARM, values_fill = 0)
  dfM <- dfM %>% pivot_longer(names_to = "ARM", values_to = "Count", cols = c("GC", "GC + Avelumab"))
  frq3 <- merge(dfM, frq2, by="ARM")
  frq4 <- frq3 %>%
    mutate(no = Freq - Count, #total - # with event
           yes = Count) %>% # all with AE
    arrange(PT_COD, ARM) %>%
    select(c(ARM, PT_COD, yes, no))
  
  ################### calcul de RD #####################
  #Creation d'une table pour accueilir toutes les données nécessaires
  dfx_Multiple <- setNames(data.frame(matrix(ncol = 6, nrow = 0)), c("Freq_Total", "PT_COD","CI_1", "CI_2", "RD"))
  
  for (p in unique(frq4$PT_COD)){
    #pour chaque PT tableau permettant de calculer la p-value ainsi que le ratio
    df1 <- subset(frq4, PT_COD == p, select = (c(-PT_COD, -ARM)))
    
    dfx <- data.frame(PT_COD = p,
                      subgroup="Multiple",
                      frqTot = frq4$yes[frq4$PT_COD == p][1] + frq4$yes[frq4$PT_COD == p][2],
                      frqGC = frq4$yes[frq4$PT_COD == p & frq4$ARM=="GC"],
                      frqAVE = frq4$yes[frq4$PT_COD == p & frq4$ARM=="GC + Avelumab"],
                      RD = RDfunct(df1[[1]][1], df1[[1]][2], frq2$Freq[frq2$ARM=="GC"], frq2$Freq[frq2$ARM=="GC + Avelumab"], CRC=TRUE)$estimate,
                      CI_1 = RDfunct(df1[[1]][1], df1[[1]][2], frq2$Freq[frq2$ARM=="GC"], frq2$Freq[frq2$ARM=="GC + Avelumab"], CRC=TRUE)$conf.int[1],
                      CI_2 = RDfunct(df1[[1]][1], df1[[1]][2], frq2$Freq[frq2$ARM=="GC"], frq2$Freq[frq2$ARM=="GC + Avelumab"], CRC=TRUE)$conf.int[2],
                      row.names = NULL)
    # A chaque PT concatenation des tables
    dfx_Multiple <- rbind(dfx_Multiple,dfx)
  }
  dfx_Multiple$pct = round(dfx_Multiple$frqTot / sum(frq2$Freq),2)
  dfx_Multiple$pctGC = round(dfx_Multiple$frqGC / frq2$Freq[1],2)
  dfx_Multiple$pctAVE = round(dfx_Multiple$frqAVE / frq2$Freq[2],2)
  
  #### table finale pour le plot
  tb_plot <- rbind(dfx_Single, dfx_Multiple)
  
  ####
  if (format=="dumbbell") {
    tb_plot <- tb_plot %>%
      select(-frqTot,-frqAVE,-frqGC) %>%
      pivot_longer(cols = c("pctGC","pctAVE"),names_to = "ARM", values_to = "pctgrp")
  } else if (format=="text") {
    tb_plot <- tb_plot %>%
      select(-pct,-pctAVE,-pctGC) %>%
      pivot_longer(cols = c("frqGC","frqAVE"),names_to = "ARM", values_to = "frqgrp")
  }
  # tb_plot$grptxt <- paste0(tb_plot$subgroup,"_",tb_plot$ARM)
  
  
  ################# calcul RD par SOC (pour le tri des SOC) ###################
  frq5 <- data %>% select(USUBJID,ARM,SOC_COD) %>% distinct(USUBJID, ARM, SOC_COD)
  frq5 <- data.frame(xtabs(~ SOC_COD + ARM, data = frq5))
  #Merged
  frq6 <- merge(frq5, frq2, by="ARM")
  frq7 <- frq6 %>%
    mutate(no = Freq.y - Freq.x, #total - # with event
           yes = Freq.x) %>% # all with AE
    arrange(SOC_COD, ARM) %>%
    select(c(ARM, SOC_COD, yes, no))
  
  dfx_SOC <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("SOC_COD","RD_SOC"))
  for (s in unique(frq7$SOC_COD)){
    #pour chaque PT tableau permettant de calculer la p-value ainsi que le ratio
    df1 <- subset(frq7, SOC_COD == s, select = (c(-SOC_COD, -ARM)))
    dfx <- data.frame(SOC_COD = s,
                      RD_SOC = RDfunct(df1[[1]][1], df1[[1]][2], frq2$Freq[frq2$ARM=="GC"], frq2$Freq[frq2$ARM=="GC + Avelumab"], CRC=TRUE)$estimate,
                      row.names = NULL)
    # A chaque PT concatenation des tables
    dfx_SOC <- rbind(dfx_SOC,dfx)
  }
  #######################################################
  
  ## limite pour le graph
  if (format=="dumbbel"){
    limpct <- ceiling(max(tb_plot$pct)*10) #*10 car on veut prendre à la dizaine supérieure
    limpct <- limpct/10
  }
  
  ## On ajoute les SOC_COD correspondants aux PT_COD avec une jointure
  tab_SOC <- data %>% select(SOC_COD, PT_COD) %>% group_by(SOC_COD,PT_COD) %>% distinct(SOC_COD,PT_COD)
  tb_plot <- left_join(tb_plot,tab_SOC, by="PT_COD", multiple="all")
  # on ajoute le RD de chaque SOC à la table utilisée pour le p1 pour le rank des SOC
  tb_plot <- merge(tb_plot,dfx_SOC, by="SOC_COD")
  
  ## mettre en évidence par des labels (geom_text) les EIs PT_COD significatifs
  tb_plot$test <- ""
  tb_plot$test[tb_plot$CI_2<0 & tb_plot$CI_1<0]<-"*"
  tb_plot$test[tb_plot$CI_2>0 & tb_plot$CI_1>0]<-"*"
  
  ## variable rang pour ordonner les modalités
  tb_plot_rk <- tb_plot[tb_plot$subgroup=="Multiple",]
  tb_rank <- tb_plot_rk %>% distinct(SOC_COD, PT_COD, RD, RD_SOC) %>% arrange(desc(RD_SOC), desc(RD), PT_COD)
  tb_rank$rk <- 1:nrow(tb_rank)
  tb_plot <- merge(tb_plot, tb_rank %>% select(-c(SOC_COD,RD, RD_SOC)), by="PT_COD")
  
  ## création d'une variable bcol (1 si doit être en gris et 0 sinon)
  df_pcol <- tb_plot %>% arrange(rk) %>% distinct(SOC_COD)
  df_pcol$bcol <- 0
  for (i in 1:nrow(df_pcol)){
    if (i %% 2 != 0){
      df_pcol$bcol[i] <- 1
    }
  }
  tb_plot <- merge(tb_plot,df_pcol, by="SOC_COD")
  
  ### création d'un data frame pour les emplacement des couleurs
  # variable start/end : PT_COD du début de la zone et PT_COD de la fin de la zone
  # variable colors pour la couleur
  tb_plot <- tb_plot %>% arrange(rk)
  cptS=2
  cptE=1
  SOC_COD = ""
  PT_start = ""
  PT_end = ""
  PT_start[1] <- tb_plot$PT_COD[1]
  SOC_COD[1] <- tb_plot$SOC_COD[1]
  for (i in 1:(nrow(tb_plot)-1)){
    if (tb_plot$bcol[i]==0 & tb_plot$bcol[i+1]==1){
      PT_start[cptS] <- tb_plot$PT_COD[i+1]
      SOC_COD[cptS] <- tb_plot$SOC_COD[i+1]
      cptS=cptS+1
    }
    if (tb_plot$bcol[i]==1 & tb_plot$bcol[i+1]==0){
      PT_end[cptE] <- tb_plot$PT_COD[i]
      cptE=cptE+1
    }
  }
  ##si nombre de SOC impaire
  if(length(unique(tb_plot$SOC_COD))%%2!=0)PT_end[cptE] <- tb_plot$PT_COD[nrow(tb_plot)]
  data_breaks1 <- as.data.frame(cbind(SOC_COD, PT_start, PT_end, col=1))
  cptS=1
  cptE=1
  SOC_COD = ""
  PT_start = ""
  PT_end = ""
  for (i in 1:(nrow(tb_plot)-1)){
    if (tb_plot$bcol[i]==1 & tb_plot$bcol[i+1]==0){
      PT_start[cptS] <- tb_plot$PT_COD[i+1]
      SOC_COD[cptS] <- tb_plot$SOC_COD[i+1]
      cptS=cptS+1
    }
    if (tb_plot$bcol[i]==0 & tb_plot$bcol[i+1]==1){
      PT_end[cptE] <- tb_plot$PT_COD[i]
      cptE=cptE+1
    }
  }
  ##si nombre de SOC paire
  if(length(unique(tb_plot$SOC_COD))%%2==0)PT_end[cptE]<-tb_plot$PT_COD[nrow(tb_plot)]
  data_breaks2 <- as.data.frame(cbind(SOC_COD, PT_start, PT_end, col=0))
  
  data_breaks <- rbind(data_breaks1, data_breaks2)
  data_breaks$SOC_COD <- as.factor(data_breaks$SOC_COD)
  tb_plot$SOC_COD <- as.factor(tb_plot$SOC_COD)
  tb_plot <- left_join(tb_plot,data_breaks, by="SOC_COD")
  
  levels_SOC <- tb_plot %>% select(SOC_COD,rk) %>% arrange(rk)
  levels_SOC <- levels_SOC %>% distinct(SOC_COD)
  levels_SOC <- as.vector(levels_SOC$SOC_COD)
  
  ## vecteur de couleurs et de modalités pour la version avec et sans Overall
  lmod <- c("Single","Multiple")
  lcol <- c("#9A25E8","#179F75")
  
  
  #### plot
  DBplot <- function(data1){
    data1$SOC_COD <- factor(data1$SOC_COD, levels = c(levels_SOC))
    
    if (format=="dumbbell"){
      p1 <- ggplot(data1,aes(x=pctgrp,y=reorder(PT_COD,rk))) +
        geom_rect(aes(ymin = PT_start, ymax = PT_end),
                  xmin = -Inf,  xmax = Inf,
                  colour= ifelse(data1$col==1,"gray91","white"),
                  fill=ifelse(data1$col==1, "grey91","white"),
                  linewidth=5, show.legend = F) +
        geom_hline(aes(yintercept = PT_COD), color = "darkgray", linewidth = 0.05, lty=3) +
        geom_vline(xintercept = seq(0,1,by=0.2), color = "black", linewidth = 0.05, lty=1) +
        scale_x_continuous(breaks = seq(0,limpct,by=0.2), limits = c(0,limpct)) +
        geom_point(size=2.5,aes(colour = factor(subgroup), shape = factor(ARM))) +
        scale_color_manual(name="groupes percentages", values = lcol, breaks=lmod) +
        scale_shape_manual(name="groupes percentages", values = c(19,9)) +
        guides(colour = guide_legend(ncol = 1,nrow=2, byrow = TRUE),
               shape = guide_legend(ncol = 1,nrow=2, byrow = TRUE)) +
        labs(x="Percent of patient") +
        facet_grid(SOC_COD ~ ., scales = "free", space = "free", switch = "y") +
        theme(axis.line = element_line(color = "black", linetype = 1),
              axis.ticks.y = element_blank(),
              axis.text.y = element_text(size = rel(1.2)),
              axis.title.x = element_text(size=rel(1.5)),
              axis.title.y = element_blank(),
              axis.text.x = element_text(hjust = 1, size=rel(1.2)),
              panel.background = element_blank(),
              legend.position="bottom",
              legend.title = element_blank(),
              legend.text = element_text(size = rel(1.2)),
              panel.grid.major = element_line(linewidth = 0.5, color="gray90"),
              strip.text.y.left = element_text(angle = 0, hjust = 1, size = rel(1.2)),
              strip.placement = "outside",
              panel.spacing.y = unit(3,"pt"))
    } else if (format=="text") {
      p1 <- ggplot(data1,aes(x=factor(ARM),y=reorder(PT_COD,rk))) +
        geom_rect(aes(ymin = PT_start, ymax = PT_end),
                  xmin = -Inf,  xmax = Inf,
                  colour= ifelse(data1$col==1,"gray91","white"),
                  fill=ifelse(data1$col==1, "grey91","white"),
                  linewidth=5, show.legend = F) +
        geom_text(aes(label=frqgrp, color=factor(subgroup)), size=5) +
        scale_color_manual(name="groupes percentages", values = lcol, breaks=lmod) +
        facet_grid(SOC_COD ~ subgroup, scales = "free", space = "free", switch = "both") +
        theme(axis.line = element_line(color = "black", linetype = 1),
              axis.ticks.y = element_blank(),
              axis.text.y = element_text(size = rel(1.2)),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = element_text(hjust=1,size=rel(1.3),angle=75),
              panel.background = element_blank(),
              legend.position="none",
              legend.text = element_text(size = rel(1.2)),
              panel.grid.major = element_line(linewidth = 0.5, color="gray90"),
              strip.text.y.left = element_text(angle = 0, hjust = 1, size = rel(1.2)),
              strip.text.x.bottom = element_text(angle = 0, hjust = 0.5, size = rel(1.5)),
              strip.placement = "outside",
              panel.spacing.y = unit(3,"pt"))
    }
    
    q <- ggplotGrob(p1)
    lg <- linesGrob(x=unit(c(1,1),"npc"), y=unit(c(0,1),"npc"),
                    gp=gpar(col="gray20", lwd=1))
    for (k in grep("strip-l",q$layout$name)) {
      q$grobs[[k]]$grobs[[1]]$children[[1]] <- lg
    }
    
    valXdodge=-0.8
    
    if (format=="dumbbell") nr <- 2
    else if (format == "text") nr<-1
    
    p2 <- ggplot(data1, aes(y=RD, ymin=CI_1, ymax=CI_2, x=reorder(PT_COD,rk))) +
      geom_rect(aes(xmin = PT_start,xmax = PT_end),
                ymin = -Inf,ymax = Inf,
                colour=ifelse(data1$col==1, "gray91","white"),
                fill=ifelse(data1$col==1, "grey91","white"),
                linewidth=5, show.legend = F) +
      geom_vline(aes(xintercept = PT_COD), color = "darkgray", linewidth = 0.05, lty=3) +
      geom_hline(yintercept=0, lty=2, colour = "black", linewidth = 0.5) +
      geom_point(size=2, shape=19, aes(colour= subgroup, shape=subgroup),
                 position = position_dodge(width = valXdodge)) +
      geom_errorbar(width=0, linewidth=1, aes(colour=subgroup),
                    position = position_dodge(width = valXdodge)) +
      geom_text(aes(x=PT_COD, label=test,y=CI_2), col="red", size=6,  hjust=-1, vjust=0.6) +
      scale_color_manual(values = lcol, breaks=lmod) +
      facet_grid(SOC_COD ~ ., scales = "free", space = "free", switch = "y") +
      coord_flip() +
      guides(colour = guide_legend(nrow = nr, byrow = TRUE)) +
      theme(axis.line.x = element_line(color = "black", linetype = 1),
            axis.line.y = element_line(color = "gray80", linetype = 1),
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_text(size=rel(1.2)),
            axis.text.x = element_text(hjust = 1, size=rel(1.2)),
            panel.background = element_blank(),
            panel.grid.major = element_line(linewidth = 0.5, color="gray90"),
            legend.title = element_blank(),
            legend.text = element_text(size=rel(1.2)),
            legend.position="bottom",
            strip.text.y.left = element_blank(),
            strip.placement = "none",
            panel.spacing.y = unit(3,"pt"))
    
    plot_grid(q,  p2, labels = NULL,nrow = 1,rel_widths = c(0.70, 0.30))
  }
  
  #### séparation du graph en nbplot car trop de PT_ affichées
  #On fait des listes de SOC à peu près égales et on diviser les tables et les graphs selon ces deux listes
  # nb_SOC <- length(levels_SOC)
  # lim <- floor(nb_SOC/nbplot)
  # res <- list()
  # for (i in 1:nbplot){
  #   if (i==1) l_SOC <- levels_SOC[1:lim]
  #   else if (i==nbplot) {
  #     a <- (lim*(i-1))+(i-1)
  #     l_SOC <- levels_SOC[a:nb_SOC]
  #   }
  #   else {
  #     a <- (lim*(i-1))+(i-1)
  #     b <- (lim*i)+(i-1)
  #     l_SOC <- levels_SOC[a:b]
  #   }
  #   tb_ploti <- subset(tb_plot, SOC_COD %in% l_SOC)
  #
  #   res[[i]] <- DBplot(tb_ploti)
  # }
  
  plot <- DBplot(tb_plot)
  res <- list(plot = plot, height = 100+length(unique(tb_plot$PT_COD))*20)
  return(res)
}


# PanelLasagnaAlluvial <- function(data, PT, BPpdEI=TRUE)
PanelLasagnaAlluvialAE <- function(baseEI, baseTr, baseDates, CODvar = "PT_COD",choixEI, ARMe=NULL, unit, barplot=TRUE, BPType=1, suivi=FALSE, listcol , idpat=TRUE){
  #on récupère le nombre de modalité de la variable AE_GRADE pour les échelles de couleurs des graphiques
  vect_grade <- sort(unique((baseEI$AE_GRADE)))
  
  #### table EI
  #liste des groupes de traitement de la table baseTr
  list_ARM <- unique(baseTr$ARM)
  if (is.null(ARMe)) {
    #Liste des id patients dans le bras numéro 1
    list_pat1 <- unique(baseTr$USUBJID[baseTr$ARM == list_ARM[1]]) 
    #Liste des id patients dans le bras numéro 2
    list_pat2 <- unique(baseTr$USUBJID[baseTr$ARM == list_ARM[2]])
  } else if (!is.null(ARMe)){
    l2 <- unique(baseTr$ARM) 
    if (!(ARMe %in% l2)) return("Nom de bras de traitement non correct ou non présent dans la base.") 
    list_ARM[1] <- ARMe
    list_ARM[2] <- l2[l2 != ARMe]
    
    #Liste des id patients dans le bras numéro 1 : GC
    list_pat1 <- unique(baseTr$USUBJID[baseTr$ARM == list_ARM[1]]) 
    #Liste des id patients dans le bras numéro 2 : GC + Avelumab
    list_pat2 <- unique(baseTr$USUBJID[baseTr$ARM == list_ARM[2]])
  }
  #Ajouter une colonne ARM dans la table data en faisant correspondre les USUBJID selon la liste où ils sont présents
  baseEI$ARM <- ifelse(baseEI$USUBJID %in% list_pat1, "armG", "armD")
  baseTr$ARM <- ifelse(baseTr$ARM==list_ARM[1], "armG","armD")
  
  
  #### Ciblage sur l'EI #####
  if (CODvar == "PT_COD") {
    df_AE2 <- baseEI %>% select(USUBJID, PT_COD ,AE_GRADE, AE_DAT_START, AE_DAT_END, AE_ONGO,ARM)
    if (!(choixEI %in% df_AE2$PT_COD)) return("PT_COD choisit non présent dans la base de donnée")
    df_AE2 <- df_AE2[df_AE2$PT_COD == choixEI,] %>% select(-PT_COD)
  } else if (CODvar == "LLT_COD") {
    df_AE2 <- baseEI %>% select(USUBJID, LLT_COD ,AE_GRADE, AE_DAT_START, AE_DAT_END, AE_ONGO,ARM)
    if (!(choixEI %in% df_AE2$LLT_COD)) return("LLT_COD choisit non présent dans la base de donnée")
    df_AE2 <- df_AE2[df_AE2$LLT_COD == choixEI,] %>% select(-LLT_COD)
  } else return("Option non valide pour COD")
  
  #on merge pour ajouter la date de pré-inclusion,et la date de fin de traitement
  df_AE2 <- merge(df_AE2, df_Dates, by="USUBJID")
  df_AE2$SCREEN_DAT <- as.Date(df_AE2$SCREEN_DAT, format = "%d/%m/%Y")
  df_AE2$C1D1_DAT <- as.Date(df_AE2$C1D1_DAT, format = "%d/%m/%Y")
  df_AE2$EI_SUI_DAT <- as.Date(df_AE2$EI_SUI_DAT, format = "%d/%m/%Y")
  df_AE2$RANDO_DAT <- as.Date(df_AE2$RANDO_DAT, format = "%d/%m/%Y")
  
  # si date de début commence par ND ou NK on remplace par 01 du mois
  # ou si < rando_dat et même mois que rando_dat alors rando_dat
  # si < rando_dat mais pas même mois alors on regarde si < screening (ensuite idem que rando_dat)
  l1 <- grep(pattern = "ND/",df_AE2$AE_DAT_START,value=FALSE)
  l1 <- c(l1,grep(pattern = "NK/",df_AE2$AE_DAT_START,value=FALSE))
  # provisoirement on met le premier du mois en ensuite on fait le vérifications avec randodate et screendate
  df_AE2$AE_DAT_START <- str_replace(df_AE2$AE_DAT_START,"ND/","01/")
  df_AE2$AE_DAT_START <- str_replace(df_AE2$AE_DAT_START,"NK/","01/")
  df_AE2$AE_DAT_START <- as.Date(df_AE2$AE_DAT_START, format = "%d/%m/%Y")
  df_AE2$AE_DAT_END <- as.Date(df_AE2$AE_DAT_END, format = "%d/%m/%Y")
  #verification
  if (length(l1)>0){
    for (l in 1:length(l1)){
      if(df_AE2$AE_DAT_START[l1[l]]<df_AE2$C1D1_DAT[l1[l]] & 
         lubridate::month(df_AE2$AE_DAT_START[l1[l]])==lubridate::month(df_AE2$C1D1_DAT[l1[l]])) df_AE2$AE_DAT_START[l1[l]]<-df_AE2$RANDO_DAT[l1[l]]
      if(df_AE2$AE_DAT_START[l1[l]]<df_AE2$SCREEN_DAT[l1[l]] &
         lubridate::month(df_AE2$AE_DAT_START[l1[l]])==lubridate::month(df_AE2$SCREEN_DAT[l1[l]])) df_AE2$AE_DAT_START[l1[l]]<-df_AE2$SCREEN_DAT[l1[l]]
    }
  }
  # si pas de date de fin et AE_ONGO yes alors on met la date la date de suivie
  # autrement dit la plus lointaine dans le graph pour que cet EI soit affiché sur tous les cycles qui suivent sa date de début
  df_AE2 <- df_AE2[!(is.na(df_AE2$AE_DAT_END) & df_AE2$AE_ONGO!="Yes"),] # on retire les lignes qui n'ont pas de ongoing yes et pas de date de fin
  df_AE2$AE_DAT_END[is.na(df_AE2$AE_DAT_END)] <- df_AE2$EI_SUI_DAT[is.na(df_AE2$AE_DAT_END)]
  
  # retirer ceux terminés avant la date de début de traitement
  df_AE3 <- subset(df_AE2,!(df_AE2$AE_DAT_END < df_AE2$C1D1_DAT))
  # et ceux survenus après la date de fin de traitement seulement si suivi==FALSE et unit différent de cycle
  if(unit=="cycle" | (unit != "cycle" & suivi==FALSE)) df_AE3 <- subset(df_AE3,!(df_AE3$AE_DAT_START > df_AE3$EI_SUI_DAT))
  
  
  #################################################
  #création de la base avec les unités de temps
  ########################################################################################
  # Division de la variable VISITNUM pour avoir une variable avec seulement le numéro du cycle
  # for (i in 1:nrow(baseTr)) baseTr$Cycle_NUM[i] <- str_split_1(baseTr$VISITNUM[i], " ")[2]
  
  if (unit=="cycle"){
    #Récupération d'une date par cycle/période pour chaque individu (car peut y avoir plusieurs dates par cycle/période)
    df_unitdate <- baseTr %>% select(USUBJID, Cycle_NUM, TTT_DAT) %>% 
      distinct(USUBJID, Cycle_NUM, TTT_DAT) %>%
      group_by(USUBJID, Cycle_NUM,TTT_DAT) %>% dplyr::filter(TTT_DAT!="")
    df_unitdate$TTT_DAT <- as.Date(df_unitdate$TTT_DAT, format = "%d/%m/%Y")
    df_unitdate <- df_unitdate %>% group_by(USUBJID,Cycle_NUM) %>% filter(TTT_DAT==min(TTT_DAT))
    
    # Ajout d'une colonne avec la date de fin du cycle qui sera la jour précédent le TTT du cycle suivant
    df_unitdate$DAT_FIN_CYCLE = NA
    for (i in 1:(nrow(df_unitdate)-1)) df_unitdate$DAT_FIN_CYCLE[i] <- df_unitdate$TTT_DAT[i+1]
    df_unitdate$DAT_FIN_CYCLE <- as.Date(df_unitdate$DAT_FIN_CYCLE, origin = "1970-01-01")
    
    #correction cycle 6
    for(i in 1:nrow(df_unitdate)){
      if(df_unitdate$Cycle_NUM[i]==6){
        df_unitdate$DAT_FIN_CYCLE[i]<-as.Date(baseDates$EI_SUI_DAT[baseDates$USUBJID==df_unitdate$USUBJID[i]], format="%d/%m/%Y")
      }
    }
    names(df_unitdate) <- c("USUBJID","Cycle_NUM","DAT_DEB_CYCLE","DAT_FIN_CYCLE")
  } else if (unit %in% c("week","month","year")){
    if (unit=="week") {pas = 7
    } else if (unit=="month") {pas = 30
    } else if (unit=="year") {pas = 365}
    #création d'une table avec les dates pour chaque patient des limites pour les semaine, mois, ou années en fonction de unit
    df_AE_endmax <- df_AE3 %>% select(USUBJID, AE_DAT_END) %>%
      group_by(USUBJID) %>%
      filter(AE_DAT_END==max(AE_DAT_END))
    
    df_unitdate <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("USUBJID", "Cycle_NUM", "DAT_DEB_CYCLE","DAT_FIN_CYCLE"))
    for (p in df_AE_endmax$USUBJID){
      df_p <- df_AE_endmax[df_AE_endmax$USUBJID==p,]
      start <- unique(df_AE3$C1D1_DAT[df_AE3$USUBJID==p])
      
      c=1
      bsup <- ifelse(suivi==TRUE, unique(df_p$AE_DAT_END),df_AE3$EI_SUI_DAT[df_AE3$USUBJID==p])
      while(start < bsup){
        date <- start+pas
        new_line <- c(p,c,start,date)
        c=c+1
        start <- date
        
        df_unitdate <- rbind(df_unitdate,new_line)
      }
    } 
    names(df_unitdate) <- c("USUBJID","Cycle_NUM","DAT_DEB_CYCLE","DAT_FIN_CYCLE")
    df_unitdate$DAT_FIN_CYCLE <- as.Date(df_unitdate$DAT_FIN_CYCLE, origin = "1970-01-01")
    df_unitdate$DAT_DEB_CYCLE <- as.Date(df_unitdate$DAT_DEB_CYCLE, origin = "1970-01-01")
  } else return("Unité non valide pour l'option unit")
  
  
  ## Attribuer un (ou plusieurs) cycle à chaque EI de la base df_AE en fonction de la date d'occurence et du patient concerné
  ## si on fait full_join on aura aussi les individus qui n'ont pas eu l'EI anaemia et présents dans la table df_Tr3
  ## (comme dans le Alluvial)
  df_AE4 <- left_join(df_AE3, df_unitdate, by = "USUBJID", multiple = "all")
  
  #on garde les grades pour les lignes où la date de début et de fin de l'EI correspondent au cycle
  #sinon on remplace par 0 pour indiquer qu'a ce cycle le patient n'avait pas d'EI (grade 0)
  df_AE4$AE_GRADE[ ((df_AE4$AE_DAT_START > df_AE4$DAT_FIN_CYCLE) |
                      (df_AE4$AE_DAT_END < df_AE4$DAT_DEB_CYCLE))] =0
  df_AE4 <- df_AE4 %>% select(ARM, USUBJID, Cycle_NUM,AE_GRADE) %>% arrange(ARM, USUBJID, Cycle_NUM, desc(AE_GRADE))
  
  
  ## On ne garde que le grade max s'il y a plusieurs occurrence de cet EI dans un même cycle
  df_AE4 <- df_AE4 %>% distinct(USUBJID,ARM, Cycle_NUM, AE_GRADE) %>% 
    group_by(USUBJID,ARM,Cycle_NUM) %>% dplyr::filter(AE_GRADE==max(AE_GRADE))
  
  df_AE4$AE_GRADE <- as.factor(df_AE4$AE_GRADE)
  df_AE4$Cycle_NUM <- as.factor(df_AE4$Cycle_NUM)
  df_AE4$USUBJID <- as.factor(df_AE4$USUBJID)
  df_AE4$ARM <- as.factor(df_AE4$ARM)
  
  ## Séparation en deux sous-tables par bras pour en faire 2 graphiques à comparer
  df_AE4_1_Las <- df_AE4[df_AE4$ARM == "armG",]
  df_AE4_2_Las <- df_AE4[df_AE4$ARM == "armD",]
  
  ########### Ranking de chacune des tables pour les graphiques #######################
  ranking_las <- function(table_las){
    l_pat <- unique(table_las$USUBJID)
    table_rk_all <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("USUBJID", "AE_GRADE", "rank", "Cycle_NUM","ARM"))
    if (length(l_pat)!=0 & nrow(table_las)!=0){
      for (c in 1:max(as.numeric(table_las$Cycle_NUM))){
        table_rk <- table_las[(table_las$Cycle_NUM == c) & (table_las$USUBJID %in% l_pat) & (table_las$AE_GRADE != 0),]
        if (length(row.names(table_rk) !=0)){
          table_rk <- table_rk[order(table_rk$AE_GRADE, decreasing = TRUE),]
          table_rk$rank <- (nrow(table_rk_all)+1):(nrow(table_rk)+(nrow(table_rk_all)))
          table_rk_all <- rbind(table_rk_all,table_rk)
          l_pat <- l_pat[!(l_pat %in% table_rk_all$USUBJID)]
        }
      }
      table_rk_all <- table_rk_all %>% ungroup() %>% select(-AE_GRADE,-ARM,-Cycle_NUM)
      # fusion pour donner un rang à chaque individu de la table
      table_las <- merge(table_las, table_rk_all, by="USUBJID")
    } 
  }
  
  df_AE4_1_Las <- ranking_las(df_AE4_1_Las)
  df_AE4_2_Las <- ranking_las(df_AE4_2_Las)
  
  
  ################# Plot ##########################
  LasagnaAE <- function(data){
    ggplot(data, aes(x=Cycle_NUM, y=reorder(USUBJID, desc(rank)))) +
      geom_tile(aes(fill=AE_GRADE)) +
      scale_fill_manual(name=paste0("Grade max atteint \npour ",choixEI), breaks = c("0",vect_grade),
                        labels = c("Pas d'EI",vect_grade),
                        values = c("lightgray",listcol)) +
      labs(x = paste0(unit," of treatment"), y = NULL) +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = rel(1.5)),
            axis.title.x = element_blank(),
            axis.text.y = element_text(color = "black", size=rel(1.2)),
            axis.ticks = element_blank(),
            legend.position = "none",
            panel.background = element_blank(),
            panel.grid.major.y = element_line(color = "darkgray", linetype = 1),
            panel.spacing.y = element_blank()) +
      {if (idpat==FALSE) theme() %+replace% theme(axis.text.y = element_text(color = "white", size=rel(1.2)))}
  } 
  
  if(nrow(df_AE4_2_Las)!=0) p1 <- LasagnaAE(df_AE4_1_Las)
  if(nrow(df_AE4_1_Las)!=0) p2 <- LasagnaAE(df_AE4_2_Las)
  
  ########################### Alluvial #############
  #Comptage du nombre de patient dans chacun des bras
  df_Tr2  <- baseTr[baseTr$TTT_IND=="Yes",]
  USU_distinct  <- df_Tr2  %>% select(USUBJID, ARM, TTT_IND) %>% distinct(USUBJID, ARM)
  frq2 <- data.frame(xtabs(~ ARM, data=USU_distinct))
  
  if(nrow(df_AE4_1_Las)==0) df_AE5 <- df_AE4_2_Las %>% select(-rank)
  if(nrow(df_AE4_2_Las)==0) df_AE5 <- df_AE4_1_Las %>% select(-rank)
  if(nrow(df_AE4_2_Las)!=0 & nrow(df_AE4_1_Las)!=0) df_AE5 <- rbind(df_AE4_2_Las,df_AE4_1_Las) %>% select(-rank)
  
  if (unit=="cycle"){
    #Dans cette table on ne garde pas les individus qui n'ont pas eu le traitement
    # cf : pas de yes dans TTT_IND pour cet individu à chaque cycle
    list_NTTT <- list()
    l_all <- unique(baseTr$USUBJID)
    for (c in 1:max(as.numeric(baseTr$Cycle_NUM))){
      #liste des id des patients ayant reçu au moins une fois le traitement donc patients exposés à ce cycle c
      l_Y <- unique(baseTr$USUBJID[baseTr$TTT_IND=="Yes" & baseTr$Cycle_NUM == c]) 
      l_N <- l_all[!(l_all %in% l_Y)]
      list_NTTT[[c]] <- l_N 
    }
    # on ajoute à chaque cycle les USUBJID manquants (qui n'ont pas reçu le traitement à certains cycles)
    for (c in 1:max(as.numeric(df_unitdate$Cycle_NUM))){
      if(length(list_NTTT[[c]])!=0){
        df_add <- data.frame(USUBJID=as.factor(list_NTTT[[c]]),
                             ARM=as.factor(ifelse(list_NTTT[[c]] %in% list_pat1, "armG","armD")),
                             Cycle_NUM=as.factor(c),AE_GRADE="NA")
        df_AE5 <- rbind(df_AE5, df_add)
      }
    }
  } else if (unit!="cycle"){
    list_NTTT <- list()
    l_all <- unique(baseTr$USUBJID)
    #liste des id des patients ayant reçu au moins une fois le traitement donc patients exposés à ce cycle c
    l_Y <- unique(baseTr$USUBJID[baseTr$TTT_IND=="Yes"]) 
    list_NTTT <- l_all[!(l_all %in% l_Y)]
    
    # on ajoute à chaque cycle les USUBJID manquants (qui n'ont pas reçu le traitement du tout)
    for (c in 1:max(as.numeric(df_unitdate$Cycle_NUM))){
      if(length(list_NTTT)!=0){
        df_add <- data.frame(USUBJID=as.factor(list_NTTT),
                             ARM=as.factor(ifelse(list_NTTT %in% list_pat1, "armG","armD")),
                             Cycle_NUM=as.factor(c),AE_GRADE="NA")
        df_AE5 <- rbind(df_AE5, df_add)
      }
    }
  }
  
  #ajout des USUBJID qui n'ont pas eu l'EI
  for (c in 1:max(as.numeric(df_unitdate$Cycle_NUM))){
    l_add <- l_all[!(l_all %in% df_AE5$USUBJID[df_AE5$Cycle_NUM==c])]
    if(length(l_add)!=0){
      df_add <- data.frame(USUBJID=as.factor(l_add),
                           ARM=as.factor(ifelse(l_add %in% list_pat1, "armG","armD")),
                           Cycle_NUM=as.factor(c),AE_GRADE=as.factor(0))
      df_AE5 <- rbind(df_AE5, df_add)
    }
  }
  
  
  #si pas du tout de pat ayant l'EI dans un des deux groupes alors on supprime les lignes ajoutées pour ce groupe
  if(nrow(df_AE4_2_Las)==0) df_AE5_2_All <- subset(df_AE5, ARM=="armD")
  if(nrow(df_AE4_1_Las)==0) df_AE5_1_All <- subset(df_AE5, ARM=="armG")
  if(nrow(df_AE4_2_Las)!=0 & nrow(df_AE4_1_Las)!=0){
    ## Séparation en deux sous-tables par bras pour en faire 2 graphiques à comparer
    df_AE5_1_All <- df_AE5[df_AE5$ARM == "armG",]
    df_AE5_2_All <- df_AE5[df_AE5$ARM == "armD",]
  } 
  
  ### création de deux variables breaks pour afficher les % au deux graphique
  # on cherche pour quelles valeurs de chaque groupe on obtient 0, 25, 50, 75 et 100%
  AlluvialAE <- function(data,N){
    a = 0.25*N
    b = 0.5*N
    c = 0.75*N
    d = 1*N
    
    if ("NA" %in% data$AE_GRADE) data$AE_GRADE <- relevel(data$AE_GRADE, "NA")
    data <- data %>% select(USUBJID, ARM,Cycle_NUM,AE_GRADE) %>% arrange(USUBJID,Cycle_NUM,AE_GRADE)
    
    ggplot(data, 
           aes(x=Cycle_NUM, stratum = AE_GRADE, alluvium = USUBJID, fill = AE_GRADE)) +
      geom_flow() +
      geom_stratum() +
      scale_fill_manual(name=paste0("Grade max atteint \npour ",choixEI), breaks = c("0",vect_grade,"NA"),
                        labels = c("Pas d'EI",vect_grade,"NA"),
                        values = c("lightgray",listcol,"grey60")) +
      scale_y_continuous(breaks = c(0,a,b,c,d),
                         labels = c("0"="0","a"="25%","b"="50%","c"="75%","d"="100%")) +
      guides(fill=guide_legend(ncol=max(as.numeric(data$AE_GRADE))))+
      labs(x = paste0(unit," of treatement"), y = NULL) +
      theme(legend.position = "bottom",
            legend.text = element_text(size=rel(1.2)),
            legend.title = element_text(size=rel(1.2)),
            panel.background = element_blank(),
            panel.grid.major.y = element_line(color = "darkgray", linetype = 2),
            axis.text.y = element_text(color = 'black', size=rel(1.5)),
            axis.title.x = element_text(size=rel(1.5)),
            axis.text.x = element_text(color = 'black', size=rel(1.5)),
            axis.ticks.x = element_blank())
  }
  if(nrow(df_AE4_1_Las)!=0) p3 <- AlluvialAE(df_AE5_1_All, frq2$Freq[frq2$ARM=="armG"])
  if(nrow(df_AE4_2_Las)!=0) p4 <- AlluvialAE(df_AE5_2_All, frq2$Freq[frq2$ARM=="armD"])
  
  ################# diagrammes en barres à chaque cycle
  BarChart <- function(data, group, Type=BPType){
    df_bar <- data[data$AE_GRADE!="NA",] %>% select(Cycle_NUM, AE_GRADE) %>%
      group_by(Cycle_NUM, AE_GRADE) %>% summarize(count=n()) %>%
      mutate(pct=round((count/sum(count))*100,0))
    
    if(Type==2) df_bar <- subset(df_bar, df_bar$AE_GRADE!=0)
    
    ggplot(data = df_bar, aes(fill=AE_GRADE, x=Cycle_NUM, y = pct)) +
      geom_bar(position = "dodge", stat = 'identity') + 
      geom_text(aes(label=pct),
                size=rel(5),position = position_dodge(width=0.9),vjust = -0.5) +
      scale_x_discrete(limits=factor(seq(1,max(as.numeric(data$Cycle_NUM)), by=1))) +
      scale_y_continuous(limits=c(0,max(df_bar$pct)+10)) +
      scale_fill_manual(name=paste0("Grade max atteint \npour ",choixEI), breaks = c(vect_grade,"0"),
                        labels = c(vect_grade,"Pas d'EI"),
                        values = c(listcol,"lightgray")) +
      ggtitle(group) +
      labs(y="percent (%)")+
      theme(panel.background = element_blank(),
            plot.title = element_text(size=rel(1.5)),
            axis.ticks = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_text(color = "black", size=rel(1.5)),
            axis.text = element_blank(),
            legend.position = "none")
  }
  if (barplot==TRUE){
    if(nrow(df_AE4_2_Las)!=0) b1 <- BarChart(df_AE5_1_All,list_ARM[1])
    if(nrow(df_AE4_1_Las)!=0) b2 <- BarChart(df_AE5_2_All,list_ARM[2])
  }
  
  ### avec p1 et p2 du script Lasagna plot
  ### et b1 et b2 diagrammes en barres créé agalement dans le script LasagnaPlot (mais avec les tables de ce script)
  if(nrow(df_AE4_1_Las)==0){
    if (barplot==TRUE) top_row <- plot_grid(b1,labels = NULL,ncol = 2,rel_widths = c(0.5, 0.5))
    mid_row <- plot_grid(p1,labels = NULL,ncol = 2,rel_widths = c(0.5, 0.5))
    bottom_row <- plot_grid(p3,labels = NULL,ncol = 2,rel_widths = c(0.5, 0.5))
  }
  if(nrow(df_AE4_2_Las)==0){
    if (barplot==TRUE) top_row <- plot_grid(NULL,b2,labels = NULL,ncol = 2,rel_widths = c(0.5, 0.5))
    mid_row <- plot_grid(NULL,p2,labels = NULL,ncol = 2,rel_widths = c(0.5, 0.5))
    bottom_row <- plot_grid(NULL,p4,labels = NULL,ncol = 2,rel_widths = c(0.5, 0.5))
  }  
  if(nrow(df_AE4_2_Las)!=0 & nrow(df_AE4_1_Las)!=0){
    if (barplot==TRUE) top_row <- plot_grid(b1, b2,labels = NULL,ncol = 2,rel_widths = c(0.5, 0.5))
    mid_row <- plot_grid(p1, p2,labels = NULL,ncol = 2,rel_widths = c(0.5, 0.5))
    bottom_row <- plot_grid(p3, p4,labels = NULL,ncol = 2,rel_widths = c(0.5, 0.5))
  }
  if (barplot==TRUE) {plot_grid(top_row, mid_row ,bottom_row,labels = NULL,nrow = 3,rel_heights = c(0.2,0.4, 0.5))
  } else {plot_grid(mid_row ,bottom_row,labels = NULL,nrow = 2,rel_heights = c(0.45, 0.55))}
}


# ButterflyBarChartSup <- function(data, value=NA,SAE=FALSE)
ButterflyBarChartSup  <- function(baseEI, baseTr, varsup=3, ARMe=NULL, tri="RDAll",trivar=NULL){
  #### table EI 
  #liste des groupes de traitement de la table baseTr
  list_ARM <- unique(baseTr$ARM)
  if (is.null(ARMe)) {
    #Liste des id patients dans le bras numéro 1
    list_pat1 <- unique(baseTr$USUBJID[baseTr$ARM == list_ARM[1]]) 
    #Liste des id patients dans le bras numéro 2
    list_pat2 <- unique(baseTr$USUBJID[baseTr$ARM == list_ARM[2]])
  } else if (!is.null(ARMe)){
    l2 <- unique(baseTr$ARM) 
    if (!(ARMe %in% l2)) return("Nom de bras de traitement non correct ou non présent dans la base.") 
    list_ARM[1] <- ARMe
    list_ARM[2] <- l2[l2 != ARMe]
    
    #Liste des id patients dans le bras numéro 1 
    list_pat1 <- unique(baseTr$USUBJID[baseTr$ARM == list_ARM[1]]) 
    #Liste des id patients dans le bras numéro 2
    list_pat2 <- unique(baseTr$USUBJID[baseTr$ARM == list_ARM[2]])
  }
  #Ajouter une colonne ARM dans la table data en faisant correspondre les USUBJID selon la liste où ils sont présents
  baseEI$ARM <- ifelse(baseEI$USUBJID %in% list_pat1, "armG", "armD")
  baseTr$ARM <- ifelse(baseTr$ARM==list_ARM[1], "armG","armD")
  
  
  df_Tr2  <- baseTr[baseTr$TTT_IND=="Yes",]
  USU_distinct  <- df_Tr2  %>% select(USUBJID, ARM, TTT_IND) %>% distinct(USUBJID, ARM)
  frq2 <- data.frame(xtabs(~ ARM, data=USU_distinct))
  
  df <- baseEI %>% select(USUBJID, ARM, SOC_COD) %>% distinct(USUBJID, ARM, SOC_COD)
  Events <- df %>% group_by(ARM, SOC_COD) %>% summarise(Count=n())
  Events$pctAll <- round(ifelse(Events$ARM=="armG",Events$Count/frq2$Freq[frq2$ARM=="armG"], 
                                Events$Count/frq2$Freq[frq2$ARM=="armD"])*100,1)
  
  # récupérer la sous base avec les EIs de grade >= 3
  df <- baseEI %>% select(USUBJID, ARM, SOC_COD, AE_GRADE, SAE)
  if (varsup!="SAE") {
    df2 <- df %>% filter(AE_GRADE >= varsup)
  } else if (varsup=="SAE") {df2 <- df %>% filter(SAE=="Yes")}
  
  #on garde une occurence unique pour chaque USUBJID et SOC_COD
  df2 <- df2 %>% select(-AE_GRADE) %>% distinct(USUBJID,ARM,SOC_COD)
  
  Events2 <- df2 %>% group_by(ARM, SOC_COD) %>% summarise(CountS=n())
  Events2$pctS <- round(ifelse(Events2$ARM=="armG",Events2$CountS/frq2$Freq[frq2$ARM=="armG"],
                               Events2$CountS/frq2$Freq[frq2$ARM=="armD"])*100,1)
  
  df_ALL <- left_join(Events, Events2, by= c("ARM", "SOC_COD")) %>% arrange(desc(pctAll))
  
  df_long <- df_ALL %>%
    pivot_longer(c(pctAll, pctS),names_to = "grp", values_to = "pct")
  
  RDfunct <- function (a, b, N1, N0, CRC = FALSE, conf.level = 0.95) {
    .M <- a + b
    .T <- N1 + N0
    .R1 <- a/N1
    .R0 <- b/N0
    .RT <- .M/.T
    norm.pp <- qnorm(1 - (1 - conf.level)/2)
    if (CRC) {
      .RC1 <- norm.pp * sqrt(a * (N1 - a)/(N1^3))
      .RC0 <- norm.pp * sqrt(b * (N0 - b)/(N0^3))
      .RCT <- norm.pp * sqrt(.M * (.T - .M)/(.T^3))
      .MAT <- matrix(c(a, b, .M, N1, N0, .T, .R1, .R0, .RT, 
                       .R1 - .RC1, .R0 - .RC0, .RT - .RCT, .R1 + .RC1, 
                       .R0 + .RC0, .RT + .RCT), 3, 5)
      colnames(.MAT) <- c("Cases", "People at Risk", "Risk", 
                          "Lower CL", "Upper CL")
      rownames(.MAT) <- c("Exposed", "Unexposed", "Total")
    }  else {
      .MAT <- matrix(c(a, b, .M, N1, N0, .T, .R1, .R0, .RT), 
                     3, 3)
      colnames(.MAT) <- c("Cases", "People at risk", "Risk")
      rownames(.MAT) <- c("Exposed", "Unexposed", "Total")
    }
    class(.MAT) <- "table"
    ################ print(.MAT) ####################
    ESTIMATE <- .R1 - .R0
    .CHI <- ESTIMATE/sqrt(a * (N1 - a)/(N1^3) + b * (N0 - b)/(N0^3))
    p.v <- 2 * (1 - pnorm(abs(.CHI)))
    RDL <- ESTIMATE - norm.pp * sqrt(a * (N1 - a)/(N1^3) + b * 
                                       (N0 - b)/(N0^3))
    RDU <- ESTIMATE + norm.pp * sqrt(a * (N1 - a)/(N1^3) + b * 
                                       (N0 - b)/(N0^3))
    CINT <- c(RDL, RDU)
    attr(CINT, "conf.level") <- conf.level
    RVAL <- list(p.value = p.v, conf.int = CINT, estimate = ESTIMATE, 
                 method = "Risk difference and its significance probability (H0: The difference equals to zero)", 
                 data.name = paste(deparse(substitute(a)), deparse(substitute(b)), 
                                   deparse(substitute(N1)), deparse(substitute(N0))))
    class(RVAL) <- "htest"
    return(RVAL)
  }
  
  ############ Calcul du RD (ALL)
  df_RD <- df_ALL %>% select(-pctAll, -pctS,-CountS) %>% pivot_wider(names_from = c(ARM), values_from = Count)
  
  df_RD$armD[is.na(df_RD$armD)]<-0
  df_RD$armG[is.na(df_RD$armG)]<-0
  df_RD <- df_RD %>% pivot_longer(c("armG","armD"), names_to = "ARM", values_to = "yes")
  
  df_RD$no <- ifelse(df_RD$ARM=="armG",
                     frq2$Freq[frq2$ARM=="armG"]- df_RD$yes,
                     frq2$Freq[frq2$ARM=="armD"] - df_RD$yes)
  #Calcul du Risk Difference et de son intervalle de confiance
  dfx_all <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("Freq_Total", "SOC_COD","CI_1", "CI_2", "RD"))
  
  for (s in unique(df_RD$SOC_COD)){
    #pour chaque PT tableau permettant de calculer la p-value ainsi que le ratio
    df1 <- subset(df_RD, SOC_COD == s, select = (c(-SOC_COD)))
    
    dfx <- data.frame(SOC_COD = s,
                      frqTot = df_RD$yes[df_RD$SOC_COD == s][1] + df_RD$yes[df_RD$SOC_COD == s][2],
                      RD = RDfunct(df1$yes[df1$ARM=="armD"], df1$yes[df1$ARM=="armG"], 
                                   frq2$Freq[frq2$ARM=="armD"], frq2$Freq[frq2$ARM=="armG"], CRC=TRUE)$estimate,
                      CI_1 = RDfunct(df1$yes[df1$ARM=="armD"], df1$yes[df1$ARM=="armG"],
                                     frq2$Freq[frq2$ARM=="armD"], frq2$Freq[frq2$ARM=="armG"], CRC=TRUE)$conf.int[1],
                      CI_2 = RDfunct(df1$yes[df1$ARM=="armD"], df1$yes[df1$ARM=="armG"],
                                     frq2$Freq[frq2$ARM=="armD"], frq2$Freq[frq2$ARM=="armG"], CRC=TRUE)$conf.int[2],
                      row.names = NULL)
    # A chaque PT concatenation des tables
    dfx_all <- rbind(dfx_all,dfx)
  }
  
  df_plot_forest <- data.frame(SOC_COD = dfx_all$SOC_COD,
                               estimate1 =  dfx_all$RD,
                               lower1 = dfx_all$CI_1,
                               upper1 = dfx_all$CI_2, row.names=NULL)
  
  ############ autre Calcul du RD (grade>=value)
  df_RD2 <- df_ALL %>% select(-pctAll, -pctS, -Count) %>% pivot_wider(names_from = ARM, values_from = CountS)
  
  df_RD2$armG[is.na(df_RD2$armG)]<-0
  df_RD2$armD[is.na(df_RD2$armD)]<-0
  df_RD2 <- df_RD2 %>% pivot_longer(c("armG","armD"), names_to = "ARM", values_to = "yes")
  
  # ajout colonne avec le nombre de patients ayant eu cette SOC
  df_RD2 <- left_join(df_RD2, df_RD %>% select(SOC_COD, yes, ARM), by= c("ARM", "SOC_COD"))
  df_RD2$no <- df_RD2$yes.y-df_RD2$yes.x
  df_RD2 <- df_RD2 %>% select(-yes.y)
  colnames(df_RD2)<-c("SOC_COD","ARM","yes","no")
  #Calcul du Risque Relatif et de sont intervalle de confiance
  dfx_all <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("Freq_Total", "SOC_COD","CI_1", "CI_2", "RD"))
  for (s in unique(df_RD2$SOC_COD)){
    #pour chaque PT tableau permettant de calculer la p-value ainsi que le ratio
    df1 <- subset(df_RD2, SOC_COD == s, select = (c(-SOC_COD)))
    
    dfx <- data.frame(SOC_COD = s,
                      frqTot = df_RD2$yes[df_RD2$SOC_COD == s][1] + df_RD2$yes[df_RD2$SOC_COD == s][2],
                      RD = RDfunct(df1$yes[df1$ARM=="armD"], 
                                   df1$yes[df1$ARM=="armG"], 
                                   df1$no[df1$ARM=="armD"]+df1$yes[df1$ARM=="armD"],
                                   df1$no[df1$ARM=="armG"]+df1$yes[df1$ARM=="armG"], CRC=TRUE)$estimate,
                      CI_1 = RDfunct(df1$yes[df1$ARM=="armD"], 
                                     df1$yes[df1$ARM=="armG"], 
                                     df1$no[df1$ARM=="armD"]+df1$yes[df1$ARM=="armD"],
                                     df1$no[df1$ARM=="armG"]+df1$yes[df1$ARM=="armG"], CRC=TRUE)$conf.int[1],
                      CI_2 = RDfunct(df1$yes[df1$ARM=="armD"], 
                                     df1$yes[df1$ARM=="armG"], 
                                     df1$no[df1$ARM=="armD"]+df1$yes[df1$ARM=="armD"],
                                     df1$no[df1$ARM=="armG"]+df1$yes[df1$ARM=="armG"], CRC=TRUE)$conf.int[2],
                      row.names = NULL)
    # A chaque PT concatenation des tables
    dfx_all <- rbind(dfx_all,dfx)
  }
  
  dfx_all$RD[dfx_all$frqTot==0]<-NA
  dfx_all$CI_1[dfx_all$frqTot==0]<-NA
  dfx_all$CI_2[dfx_all$frqTot==0]<-NA
  
  df_plot_forest <- left_join(df_plot_forest, dfx_all %>% select(-frqTot),by="SOC_COD")
  colnames(df_plot_forest)<-c("SOC_COD","estimate1","lower1","upper1","estimate2","lower2","upper2")
  
  # mettre en évidence par des labels (geom_text) les EIs PT_COD significatifs
  df_plot_forest$test1 <- ""
  df_plot_forest$test1[df_plot_forest$lower1<0 & df_plot_forest$upper1<0]<-"*"
  df_plot_forest$test1[df_plot_forest$lower1>0 & df_plot_forest$upper1>0]<-"*"
  
  # mettre en évidence par des labels (geom_text) les EIs PT_COD significatifs
  df_plot_forest$test2 <- ""
  df_plot_forest$test2[df_plot_forest$lower2<0 & df_plot_forest$upper2<0]<-"*"
  df_plot_forest$test2[df_plot_forest$lower2>0 & df_plot_forest$upper2>0]<-"*"
  
  
  ######## ranking
  if (is.null(trivar)){
    df_long_rk <- df_long
  } else if (trivar==list_ARM[1]){
    df_long_rk <- df_long %>% filter(ARM=="armG")
  } else if (trivar==list_ARM[2]){
    df_long_rk <- df_long %>% filter(ARM=="armD")
  } else return("Valeur non valide pour l'option trivar")
  
  if(tri == "RDAll"){
    tb_rk <- df_plot_forest %>% arrange(desc(estimate1))
  } else if (tri == "RDVarsup"){
    tb_rk <- df_plot_forest %>% arrange(desc(estimate2))
  } else if (tri=="pctAll"){
    tb_rk <- df_long_rk %>% filter(grp=="pctAll") %>% arrange(desc(pct))
  } else if (tri=="pctVarsup"){
    tb_rk <- df_long_rk %>% filter(grp=="pctS") %>% arrange(desc(pct))
  }
  tb_rk$rank <- 1:nrow(tb_rk)
  tb_rk <- tb_rk %>% select(SOC_COD,rank) %>% ungroup() %>% distinct(SOC_COD,rank)
  
  
  df_long$dir <- ifelse(df_long$ARM=="armD",1,-1)
  df_long$alpha <- ifelse(df_long$grp=="pctS","pctS","pctAll")
  
  # pour avoir une unique colonne estimate, lower et upper avec un colonne groupe
  df1 <- df_plot_forest %>% select(-c(estimate2, upper2,lower2,test2))
  df2 <- df_plot_forest %>% select(-c(estimate1, upper1,lower1,test1))
  df1$grp <- "RDTot"
  df2$grp <- "RDtox"
  colnames(df1)<-c("SOC_COD","estimate","lower","upper","test","grp")
  colnames(df2)<-c("SOC_COD","estimate","lower","upper","test","grp")
  df_plot_forest <- rbind(df1,df2)
  
  ## ajout d'un des ranking (selon choix) à la table pour le graph
  df_plot_forest <- merge(df_plot_forest, tb_rk, by="SOC_COD")
  df_long <- merge(df_long, tb_rk, by="SOC_COD")
  
  ################## forest plot seul
  valXdodge=-0.7
  if (varsup!="SAE") {labForest <- paste0("Grade >=", varsup)
  } else if (varsup=="SAE") {labForest <- "Serieux"}
  
  p2 <- ggplot(data=df_plot_forest, aes(x=reorder(SOC_COD,rank), y=estimate,group=grp,colour=grp)) +
    geom_point(size=2, aes(shape=grp),position = position_dodge(width = valXdodge)) +
    geom_errorbar(aes(ymin=lower, ymax=upper),
                  width=0.5, linewidth=0.5, position = position_dodge(width = valXdodge)) +
    geom_hline(yintercept=0, lty=2, colour = "red", linewidth = 0.5) +
    geom_text(aes(x=reorder(SOC_COD,rank), label=test,y=upper),
              col="red", size=6,  hjust=-1, vjust=0.7,
              position = position_dodge(width = valXdodge)) +
    scale_color_manual(name="RD",values = c("gray60","black"), labels=c("All grade",labForest)) +
    scale_shape_manual(name="RD",values=c(19,17), labels=c("All grade",labForest))+
    scale_y_continuous(name = paste0("Risk Difference with 95% CI
                       \n",list_ARM[1],"              ",list_ARM[2])) +
    facet_grid(reorder(SOC_COD,rank) ~ ., scales = "free", space = "free", switch = "y") +
    scale_x_discrete(limits= rev(levels(df_plot_forest$SOC_COD))) +
    coord_flip() +
    theme(axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          panel.background = element_blank(),
          panel.grid.major.y = element_line(color="grey90", linetype=1),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_line(linewidth = 1, colour = "black"),
          axis.line.x = element_line(color = "black", linetype = 1),
          axis.title.x =element_text(size=rel(1.2)),
          axis.text.x = element_text(size=rel(1.3)),
          legend.position="bottom",
          legend.title = element_blank(),
          legend.text = element_text(size=rel(1.2)),
          strip.text.y.left = element_blank(),
          strip.placement = "none",
          panel.spacing.y = unit(3,"pt"))
  
  ################### Butterfly barplot superposé
  p1 <- ggplot() +
    geom_bar(
      data = filter(df_long, grp=="pctS"),
      aes(x = reorder(SOC_COD,desc(rank)), y = pct*dir, fill = interaction(as.factor(ARM),as.factor(grp)), group = grp),
      stat = "identity",
      width = 0.6
    ) +
    geom_bar(
      data = filter(df_long, grp=="pctAll"),
      aes(x = reorder(SOC_COD,desc(rank)), y = pct*dir, fill = interaction(as.factor(ARM),as.factor(grp)), group = grp),
      stat = "identity",
      width = 0.9
    ) +
    geom_hline(yintercept = 0) +
    coord_flip() +
    scale_y_continuous(labels = function(x){paste0(abs(x), "%")},
                       breaks = seq(-100,100,25)) +
    scale_fill_manual(name=paste0(list_ARM[2] ,"\n\n",list_ARM[1]),
                      values = c( "armG.pctS" = alpha("red", 1),
                                  "armG.pctAll" = alpha("red", 2/5),
                                  "armD.pctS" = alpha("deepskyblue2", 1),
                                  "armD.pctAll" = alpha("deepskyblue2", 2/5)),
                      labels = c("armG.pctS" = labForest,
                                 "armG.pctAll" = "All grades",
                                 "armD.pctS" = labForest,
                                 "armD.pctAll" = "All grade")) +
    labs(x = "System Organ Class", y = "Percent of patient") +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
    theme(
      panel.background = element_blank(),
      axis.line = element_line(color="black"),
      axis.ticks.y = element_blank(),
      panel.grid.major.x = element_line(color="grey80", linetype=2),
      panel.grid.major.y = element_line(color="grey90", linetype=1),
      legend.position = "bottom",
      axis.text = element_text(size=rel(1.2)),
      axis.title = element_text(size=rel(1.2)),
      legend.title = element_text("black", size=rel(1.2), hjust = 1),
      legend.text = element_text(size=rel(1.2))
    )
  
  plot_grid(p1,  p2, labels = NULL,nrow = 1, rel_widths = c(0.75, 0.25))
}


# LineErrorBars <- function(data,choixPT, grd=2)
LineErrorBars <- function(baseEI,baseTr,baseDates, CODvar="PT_COD" , grd=2, choixCOD, unit, suivi=TRUE){
  #liste des groupes de traitement de la table df_Tr
  list_ARM <- unique(baseTr$ARM)
  #Liste des id patients dans le bras numéro 1
  list_pat1 <- unique(baseTr$USUBJID[baseTr$ARM == list_ARM[1]]) 
  #Liste des id patients dans le bras numéro 2
  list_pat2 <- unique(baseTr$USUBJID[baseTr$ARM == list_ARM[2]])
  
  #Ajouter une colonne ARM dans la table data en faisant correspondre les USUBJID selon la liste où ils sont présents
  baseEI$ARM <- ifelse(baseEI$USUBJID %in% list_pat1, "arm1", "arm2")
  baseTr$ARM <- ifelse(baseTr$ARM==list_ARM[1], "arm1","arm2")
  
  df_Tr2  <- baseTr[baseTr$TTT_IND=="Yes",]
  USU_distinct  <- df_Tr2  %>% select(USUBJID, ARM, TTT_IND) %>% distinct(USUBJID, ARM)
  frq2 <- data.frame(xtabs(~ ARM, data=USU_distinct))
  
  #on merge pour ajouter la date de rando, la date de pré-inclusion,et la date de fin de traitement
  df_AE2 <- merge(baseEI, df_Dates, by="USUBJID")
  df_AE2$SCREEN_DAT <- as.Date(df_AE2$SCREEN_DAT, format = "%d/%m/%Y")
  df_AE2$C1D1_DAT <- as.Date(df_AE2$C1D1_DAT, format = "%d/%m/%Y")
  df_AE2$EI_SUI_DAT <- as.Date(df_AE2$EI_SUI_DAT, format = "%d/%m/%Y")
  df_AE2$RANDO_DAT <- as.Date(df_AE2$RANDO_DAT, format = "%d/%m/%Y")
  df_AE2$AE_DAT_END <- as.Date(df_AE2$AE_DAT_END, format = "%d/%m/%Y")
  df_AE2$AE_DAT_START <- as.Date(df_AE2$AE_DAT_START, format = "%d/%m/%Y")
  
  # si date de début commence par ND ou NK on remplace par 01 du mois
  # ou si < rando_dat et même mois que rando_dat alors rando_dat
  # si < rando_dat mais pas même mois alors on regarde si < screening (ensuite idem que rando_dat)
  l1 <- grep(pattern = "ND/",df_AE2$AE_DAT_START,value=FALSE)
  l1 <- c(l1,grep(pattern = "NK/",df_AE2$AE_DAT_START,value=FALSE))
  # provisoirement on met le premier du mois en ensuite on fait le vérifications avec randodate et screendate
  df_AE2$AE_DAT_START <- str_replace(df_AE2$AE_DAT_START,"ND/","01/")
  df_AE2$AE_DAT_START <- str_replace(df_AE2$AE_DAT_START,"NK/","01/")
  #verification
  if (length(l1)>0){
    for (l in 1:length(l1)){
      if(df_AE2$AE_DAT_START[l1[l]]<df_AE2$C1D1_DAT[l1[l]] & 
         lubridate::month(df_AE2$AE_DAT_START[l1[l]])==lubridate::month(df_AE2$C1D1_DAT[l1[l]])) df_AE2$AE_DAT_START[l1[l]]<-df_AE2$RANDO_DAT[l1[l]]
      if(df_AE2$AE_DAT_START[l1[l]]<df_AE2$SCREEN_DAT[l1[l]] &
         lubridate::month(df_AE2$AE_DAT_START[l1[l]])==lubridate::month(df_AE2$SCREEN_DAT[l1[l]])) df_AE2$AE_DAT_START[l1[l]]<-df_AE2$SCREEN_DAT[l1[l]]
    }
  }
  
  # si pas de date de fin et AE_ONGO yes alors on met la date la date de suivie
  # autrement dit la plus lointaine dans le graph pour que cet EI soit affiché sur tous les cycles qui suivent sa date de début
  df_AE2 <- df_AE2[!(is.na(df_AE2$AE_DAT_END) & df_AE2$AE_ONGO!="Yes"),] # on retire les lignes qui n'ont pas de ongoing yes et pas de date de fin
  df_AE2$AE_DAT_END[is.na(df_AE2$AE_DAT_END)] <- df_AE2$EI_SUI_DAT[is.na(df_AE2$AE_DAT_END)]
  
  # retirer ceux terminés avant la date de début de traitement
  df_AE3 <- subset(df_AE2,!(df_AE2$AE_DAT_END < df_AE2$C1D1_DAT))
  # et ceux survenus après la date de fin de traitement
  if(unit=="cycle" | (unit != "cycle" & suivi==FALSE)) df_AE3 <- subset(df_AE3,!(df_AE3$AE_DAT_START > df_AE3$EI_SUI_DAT))
  
  #################################################
  #création de la base avec les unités de temps
  #################################################
  if (unit=="cycle"){
    #Récupération d'une date par cycle pour chaque individu
    df_unitdate <- df_Tr2 %>% select(USUBJID, Cycle_NUM, TTT_DAT) %>% 
      distinct(USUBJID, Cycle_NUM, TTT_DAT) %>%
      group_by(USUBJID, Cycle_NUM,TTT_DAT) %>% dplyr::filter(TTT_DAT!="")
    df_unitdate$TTT_DAT <- as.Date(df_unitdate$TTT_DAT, format = "%d/%m/%Y")
    df_unitdate <- df_unitdate %>% group_by(USUBJID,Cycle_NUM) %>% filter(TTT_DAT==min(TTT_DAT))
    
    # Ajout d'une colonne avec la date de fin du cycle qui sera la jour précédent le TTT du cycle suivant
    df_unitdate$DAT_FIN_CYCLE = NA
    for (i in 1:(nrow(df_unitdate)-1)) df_unitdate$DAT_FIN_CYCLE[i] <- df_unitdate$TTT_DAT[i+1]
    df_unitdate$DAT_FIN_CYCLE <- as.Date(df_unitdate$DAT_FIN_CYCLE, origin = "1970-01-01")
    
    #correction cycle 6
    for(i in 1:nrow(df_unitdate)){
      if(df_unitdate$Cycle_NUM[i]==6){
        df_unitdate$DAT_FIN_CYCLE[i]<-as.Date(df_Dates$EI_SUI_DAT[df_Dates$USUBJID==df_Tr2$USUBJID[i]], format="%d/%m/%Y")
      }
    }
    names(df_unitdate) <- c("USUBJID","Cycle_NUM","DAT_DEB_CYCLE","DAT_FIN_CYCLE")
  } else if (unit %in% c("week","month","year")){
    if (unit=="week") {pas = 7
    } else if (unit=="month") {pas = 30
    } else if (unit=="year") {pas = 365}
    #création d'une table avec les dates pour chaque patient des limites pour les semaine, mois, ou années en fonction de unit
    df_AE_endmax <- df_AE3 %>% select(USUBJID, AE_DAT_END) %>%
      group_by(USUBJID) %>%
      filter(AE_DAT_END==max(AE_DAT_END))
    
    df_unitdate <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("USUBJID", "Cycle_NUM", "DAT_DEB_CYCLE","DAT_FIN_CYCLE"))
    for (p in df_AE_endmax$USUBJID){
      df_p <- df_AE_endmax[df_AE_endmax$USUBJID==p,]
      start <- unique(df_AE3$C1D1_DAT[df_AE3$USUBJID==p])
      
      c=1
      bsup <- ifelse(suivi==TRUE, unique(df_p$AE_DAT_END),df_AE3$EI_SUI_DAT[df_AE3$USUBJID==p])
      while(start < bsup){
        date <- start+pas
        new_line <- c(p,c,start,date)
        c=c+1
        start <- date
        
        df_unitdate <- rbind(df_unitdate,new_line)
      }
    } 
    names(df_unitdate) <- c("USUBJID","Cycle_NUM","DAT_DEB_CYCLE","DAT_FIN_CYCLE")
    df_unitdate$DAT_FIN_CYCLE <- as.Date(df_unitdate$DAT_FIN_CYCLE, origin = "1970-01-01")
    df_unitdate$DAT_DEB_CYCLE <- as.Date(df_unitdate$DAT_DEB_CYCLE, origin = "1970-01-01")
  } else return("Valeur non valide pour l'option unit")
  
  
  ## Attribuer un (ou plusieurs) cycle à chaque EI de la base df_AE en fonction de la date d'occurence et du patient concerné
  ## si on fait full_join on aura aussi les individus qui n'ont pas eu l'EI anaemia et présents dans la table df_Tr3
  ## (comme dans le Alluvial)
  df_AE4 <- left_join(df_AE3, df_unitdate, by = "USUBJID", multiple = "all")
  
  #on garde les grades pour les lignes où la date de début et de fin de l'EI correspondent au cycle
  #sinon on remplace par 0 pour indiquer qu'a ce cycle le patient n'avait pas d'EI (grade 0)
  df_AE4$AE_GRADE[ ((df_AE4$AE_DAT_START > df_AE4$DAT_FIN_CYCLE) |
                      (df_AE4$AE_DAT_END < df_AE4$DAT_DEB_CYCLE))] =0
  
  if (CODvar=='PT_COD'){
    df_AE4 <- df_AE4 %>% select(ARM, USUBJID,PT_COD, Cycle_NUM,AE_GRADE) %>% 
      arrange(ARM, USUBJID, Cycle_NUM, desc(AE_GRADE))
  } else if(CODvar == "LLT_COD"){
    df_AE4 <- df_AE4 %>% select(ARM, USUBJID,LLT_COD, Cycle_NUM,AE_GRADE) %>%
      arrange(ARM, USUBJID, Cycle_NUM, desc(AE_GRADE))
  } else return("Valeur non valide pour l'option CODvar")
  
  names(df_AE4) <- c("ARM","USUBJID","COD","Cycle_NUM","AE_GRADE")
  
  ###################################################
  ### Sous groupe Overall
  ###################################################
  #Selection des variables d'interêt
  # fréquence par PT et par cycle
  frq1 <- df_AE4 %>% select(USUBJID,ARM,COD, Cycle_NUM) %>% distinct(USUBJID, ARM, COD, Cycle_NUM) #liste des EIs distincts pour chaque patient en précisant son bras de traitement
  frq1 <- data.frame(xtabs(~ COD + ARM + Cycle_NUM, data = frq1)) #frequence de chaque type d'EI dans chacun des bras de traitement
  frq1 <- frq1 %>% pivot_wider(names_from = ARM, values_from = Freq)
  
  
  #######################################################
  ### Subgroup (<= grade x) pour chaque cycle
  #######################################################
  # table avec le compte d'EIs de grade >= x à chaque cycle
  if(grd > max(baseEI$AE_GRADE)) return("Grade non présent dans la base")
  if(grd == 1) return("Impossible de comparer les EIs de grade supérieur ou égale à 1 à tous les EIs, car il représentent toute la base de donnée")
  df_AE5 <- subset(df_AE4, df_AE4$AE_GRADE >=grd)
  #on garde une occurence unique pour chaque USUBJID et PT_COD
  df_AE5 <- df_AE5 %>% select(-AE_GRADE) %>% distinct(USUBJID,ARM,COD, Cycle_NUM)
  # fréquence par PT et par cycle pour les EIs de grade <=x
  frq2 <- data.frame(xtabs(~ COD + ARM + Cycle_NUM, data=df_AE5))
  frq2 <- frq2 %>% pivot_wider(names_from = ARM, values_from = Freq)
  
  #### on merge les deux tables créées frq1 et frq2 pour pouvoir calculer le RD
  frq <- left_join(frq1, frq2, by=c("COD","Cycle_NUM")) %>% mutate(across(where(is.numeric),~replace_na(.x,0)))
  names(frq) <- c("COD","Cycle_NUM","G1tot","G2tot","G1grd","G2grd")
  
  
  ########### fonction pour le calcul du RD (pris de fmsb::riskdifference) : modif -> pas de print
  RDfunct <- function (a, b, N1, N0, CRC = FALSE, conf.level = 0.95) {
    .M <- a + b
    .T <- N1 + N0
    .R1 <- a/N1
    .R0 <- b/N0
    .RT <- .M/.T
    norm.pp <- qnorm(1 - (1 - conf.level)/2)
    if (CRC) {
      .RC1 <- norm.pp * sqrt(a * (N1 - a)/(N1^3))
      .RC0 <- norm.pp * sqrt(b * (N0 - b)/(N0^3))
      .RCT <- norm.pp * sqrt(.M * (.T - .M)/(.T^3))
      .MAT <- matrix(c(a, b, .M, N1, N0, .T, .R1, .R0, .RT, 
                       .R1 - .RC1, .R0 - .RC0, .RT - .RCT, .R1 + .RC1, 
                       .R0 + .RC0, .RT + .RCT), 3, 5)
      colnames(.MAT) <- c("Cases", "People at Risk", "Risk", 
                          "Lower CL", "Upper CL")
      rownames(.MAT) <- c("Exposed", "Unexposed", "Total")
    } else {
      .MAT <- matrix(c(a, b, .M, N1, N0, .T, .R1, .R0, .RT), 
                     3, 3)
      colnames(.MAT) <- c("Cases", "People at risk", "Risk")
      rownames(.MAT) <- c("Exposed", "Unexposed", "Total")
    }
    class(.MAT) <- "table"
    ################ print(.MAT) ####################
    ESTIMATE <- .R1 - .R0
    .CHI <- ESTIMATE/sqrt(a * (N1 - a)/(N1^3) + b * (N0 - b)/(N0^3))
    p.v <- 2 * (1 - pnorm(abs(.CHI)))
    RDL <- ESTIMATE - norm.pp * sqrt(a * (N1 - a)/(N1^3) + b * 
                                       (N0 - b)/(N0^3))
    RDU <- ESTIMATE + norm.pp * sqrt(a * (N1 - a)/(N1^3) + b * 
                                       (N0 - b)/(N0^3))
    CINT <- c(RDL, RDU)
    attr(CINT, "conf.level") <- conf.level
    RVAL <- list(p.value = p.v, conf.int = CINT, estimate = ESTIMATE, 
                 method = "Risk difference and its significance probability (H0: The difference equals to zero)", 
                 data.name = paste(deparse(substitute(a)), deparse(substitute(b)), 
                                   deparse(substitute(N1)), deparse(substitute(N0))))
    class(RVAL) <- "htest"
    return(RVAL)
  }
  
  #### calcul du RD
  ## on cicle tout  d'abord l'EI choisi
  if (!(choixCOD %in% unique(frq$COD))) return("EI choisit non présent dans la base")
  frq <- frq[frq$COD == choixCOD,]
  
  #Creation d'une table pour accueilir toutes les données nécessaires
  dfx_all <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("Cycle_Num","CI_1", "CI_2", "RD"))
  
  for (c in unique(frq$Cycle_NUM)){
    #pour chaque PT tableau permettant de calculer la p-value ainsi que le ratio
    df1 <- subset(frq,Cycle_NUM==c, select = (c(-COD, -Cycle_NUM)))
    
    dfx <- data.frame(Cycle_NUM=c,
                      RD = RDfunct(df1$G2grd, df1$G1grd, df1$G2tot, df1$G1tot, CRC=TRUE)$estimate,
                      CI_1 = RDfunct(df1$G2grd, df1$G1grd, df1$G2tot, df1$G1tot, CRC=TRUE)$conf.int[1],
                      CI_2 = RDfunct(df1$G2grd, df1$G1grd, df1$G2tot, df1$G1tot, CRC=TRUE)$conf.int[2],
                      row.names = NULL)
    # A chaque PT concatenation des tables
    dfx_all <- rbind(dfx_all,dfx)
  }
  
  # on retire tous les NA
  dfx_all <- subset(dfx_all, !is.na(dfx_all$RD))
  
  p <- ggplot(dfx_all, aes(x=as.factor(Cycle_NUM),y=RD, group=1)) +
    geom_point() +
    geom_errorbar(aes(ymin=CI_2, ymax=CI_1),width=.1) +
    geom_hline(yintercept=0, lty=2, col="red",linewidth=0.5)+
    labs(y=paste0("RD\n(",list_ARM[2]," - ",list_ARM[1],")"),
         x=paste0(unit," of treatement"))+
    scale_y_continuous(position = "right")+
    ggtitle(paste("Graphique pour les grades >=", grd))+
    theme(panel.background = element_blank(),
          panel.grid.major.y = element_line(color = "darkgray", linewidth = 0.1, linetype = 3),
          axis.ticks.x = element_line(linewidth = 1, colour = "black"),
          axis.line = element_line(color = "black", linetype = 1),
          strip.placement = "none",
          axis.title = element_text(size=rel(1.2)),
          axis.text = element_text(size=rel(1.2)),
          strip.text.y.left = element_text(angle=0, hjust = 0.95, size=rel(1.8)),
          panel.spacing.y = unit(3,"pt"))
  
  p <- p + annotate(geom="rect", xmin=-Inf, xmax=Inf,ymin=-Inf,ymax=0, fill="deepskyblue2", alpha=0.2)+
    annotate(geom="rect",xmin=-Inf, xmax=Inf,ymin=0,ymax=Inf, fill="red", alpha=0.2)
  
  suppressWarnings(print(p))
}


# tendrilAE <- function(data)
TendrilAE <- function(baseEI, baseTr, baseDates, CODvar="PT_COD", coltype = NULL){
  #liste des groupes de traitement de la table df_Tr
  list_ARM <- unique(baseTr$ARM)
  #Liste des id patients dans le bras numéro 1
  list_pat1 <- unique(baseTr$USUBJID[baseTr$ARM == list_ARM[1]]) 
  #Liste des id patients dans le bras numéro 2
  list_pat2 <- unique(baseTr$USUBJID[baseTr$ARM == list_ARM[2]])
  #Ajouter une colonne ARM dans la table data en faisant correspondre les USUBJID selon la liste où ils sont présents
  baseEI$ARM <- ifelse(baseEI$USUBJID %in% list_pat1, "arm1", "arm2")
  
  #jointure pour récupérer la date de début de traitement de chaque individu
  if (CODvar=="PT_COD") {
    df_AE2 <- left_join(baseEI %>% select(USUBJID, ARM, PT_COD, AE_DAT_START),
                        baseDates %>% select(USUBJID,C1D1_DAT), 
                        by="USUBJID", multiple="all")
  } else if (CODvar == "LLT_COD") {
    df_AE2 <- left_join(baseEI %>% select(USUBJID, ARM, LLT_COD, AE_DAT_START),
                        baseDates %>% select(USUBJID,C1D1_DAT), 
                        by="USUBJID", multiple="all")
  } else return("Valeur non valide pour l'option CODvar")
  names(df_AE2) <- c("USUBJID","ARM","COD","AE_DAT_START", "C1D1_DAT")
  
  #transformation en format date
  df_AE2$AE_DAT_START <- as.Date(df_AE2$AE_DAT_START,format = "%d/%m/%Y")
  df_AE2$C1D1_DAT <- as.Date(df_AE2$C1D1_DAT, format = "%d/%m/%Y")
  # soustraction des deux dates pour obtenir la variable Days
  df_AE2$Days <- as.numeric(df_AE2$AE_DAT_START-df_AE2$C1D1_DAT)
  
  df_AE2 <- df_AE2[!is.na(df_AE2$Days),]
  #retrait des variable qui ne servent plus
  df_AE2 <- df_AE2 %>% select(-c(AE_DAT_START,C1D1_DAT)) 
  #transformation en facteur de deux variable de la base
  df_AE2$ARM <- as.factor(df_AE2$ARM)
  df_AE2$COD <- as.factor(df_AE2$COD)
  
  ##on remet mes valeurs d'origine pour les bras de traitement
  df_AE2$ARM <- ifelse(df_AE2$ARM=="arm1",list_ARM[1],list_ARM[2])
  
  # liste des patients avec leur bras de traitement
  SubjList <- baseEI %>% arrange(USUBJID,ARM)
  SubjList$ARM <- ifelse(SubjList$ARM=="arm1",list_ARM[1],list_ARM[2])
  
  data.Tendril <- Tendril(mydata = df_AE2,
                          rotations = 4, #set the degree to which each event pulls a tendril in a direction
                          AEfreqThreshold = 6, #Change the number of occurrences required to be plotted
                          Tag = "COD",
                          Treatments = c(list_ARM[1],list_ARM[2]),
                          Unique.Subject.Identifier = "USUBJID",
                          Terms = "COD",
                          Treat = "ARM",
                          StartDay = "Days",
                          SubjList = SubjList,
                          SubjList.subject = "USUBJID",
                          SubjList.treatment = "ARM")
  
  # vecteur pour les annotations
  # dans data.Tendril$data on prend les coordonnées x et y du dernier point de chaque Terms
  anno <- data.Tendril$data %>% group_by(Terms) %>% filter(StartDay==max(StartDay)) %>% arrange(Terms)
  anno <- anno %>% select(Terms,x,y)
  
  if(is.null(coltype)){
    plot(data.Tendril) + 
      geom_label(data=anno, aes(x=x,y=y, label=Terms), color="black",
                 min.segment.length = 0.1, force = 5,
                 max.overlaps = 10,
                 direction="y")
  } else if (coltype %in% c("p.adj","fish","p","rdiff","OR","RR","FDR.tot","TermsCount")){
    plot(data.Tendril, coloring=coltype) + 
      geom_label_repel(data=anno, aes(x=x,y=y, label=Terms), color="black",
                       min.segment.length = 0.1, force = 8,
                       max.overlaps = 30,
                       direction="y")
  } else return("Valeur non valide pour l'option coltype")
} 


# tendrilAETSeries <- function(data,PT=NULL)
TendrilAETimeSeries <- function(baseEI,baseTr, baseDates, CODlist=NULL, CODvar="PT_COD"){
  #liste des groupes de traitement de la table df_Tr
  list_ARM <- unique(baseTr$ARM)
  #Liste des id patients dans le bras numéro 1
  list_pat1 <- unique(baseTr$USUBJID[baseTr$ARM == list_ARM[1]]) 
  #Liste des id patients dans le bras numéro 2
  list_pat2 <- unique(baseTr$USUBJID[baseTr$ARM == list_ARM[2]])
  #Ajouter une colonne ARM dans la table data en faisant correspondre les USUBJID selon la liste où ils sont présents
  baseEI$ARM <- ifelse(baseEI$USUBJID %in% list_pat1, "arm1", "arm2")
  
  
  #jointure pour récupérer la date de début de traitement de chaque individu
  if (CODvar=="PT_COD") {df_AE2 <- left_join(baseEI %>% select(USUBJID, ARM, PT_COD, AE_DAT_START),
                                             baseDates %>% select(USUBJID,C1D1_DAT), by="USUBJID")
  } else if (CODvar=="LLT_COD") {
    df_AE2 <- left_join(baseEI %>% select(USUBJID, ARM, LLT_COD, AE_DAT_START),
                        baseDates %>% select(USUBJID,C1D1_DAT), by="USUBJID")
  } else return("Valeur non valide pour l'option CODvar")
  names(df_AE2) <- c("USUBJID","ARM","COD","AE_DAT_START","C1D1_DAT")
  
  #transformation en format date
  df_AE2$AE_DAT_START <- as.Date(df_AE2$AE_DAT_START,format = "%d/%m/%Y")
  df_AE2$C1D1_DAT <- as.Date(df_AE2$C1D1_DAT, format = "%d/%m/%Y")
  # soustraction des deux dates pour obtenir la variable Days
  df_AE2$Days <- as.numeric(df_AE2$AE_DAT_START-df_AE2$C1D1_DAT)
  
  df_AE2 <- df_AE2[!is.na(df_AE2$Days),]
  
  #retrait des variable qui ne servent plus
  df_AE2 <- df_AE2 %>% select(-c(AE_DAT_START,C1D1_DAT)) 
  #transformation en facteur de deux variable de la base
  df_AE2$ARM <- as.factor(df_AE2$ARM)
  df_AE2$COD <- as.factor(df_AE2$COD)
  
  ##on remet les valeurs d'origine pour les bras de traitement
  df_AE2$ARM <- ifelse(df_AE2$ARM=="arm1",list_ARM[1],list_ARM[2])
  
  # liste des patients avec leur bras de traitement
  SubjList <- baseEI %>% arrange(USUBJID,ARM)
  SubjList$ARM <- ifelse(SubjList$ARM=="arm1",list_ARM[1],list_ARM[2])
  
  data.Tendril <- Tendril(mydata = df_AE2,
                          rotations = 4, #set the degree to which each event pulls a tendril in a direction
                          AEfreqThreshold = 5, #Change the number of occurrences required to be plotted
                          Tag = "COD",
                          Treatments = c(list_ARM[1],list_ARM[2]),
                          Unique.Subject.Identifier = "USUBJID",
                          Terms = "COD",
                          Treat = "ARM",
                          StartDay = "Days",
                          SubjList = SubjList,
                          SubjList.subject = "USUBJID",
                          SubjList.treatment = "ARM")
  
  #table de données pour le TimeSeries
  t <- plot_timeseries(data.Tendril) 
  
  if (!is.null(CODlist)){
    p2 <- ggplot(t$data,aes(x=StartDay, y=Net, color=Terms)) +
      geom_line(linewidth=1) + 
      geom_point(size=2) +
      geom_hline(yintercept = 0, col="red",linetype=2, linewidth=1) +
      labs(x="Day since start of treatment", y=paste0("Net event - ",list_ARM[1]," over ",list_ARM[2])) +
      gghighlight::gghighlight(t$data$Terms %in% CODlist,
                               unhighlighted_params = list(linewidth = 1, colour = alpha("gray", 0.4))
      ) +
      theme(axis.text = element_text(size=rel(1.5)), #taille des labels des axes
            axis.title = element_text(size = rel(1.5)), #taille des titres des axes
            panel.background = element_blank(), #pas d'arrière plan
            panel.grid.major = element_line(color = "gray60", linewidth = 0.5, linetype = 1), #grille de lecture de couleur grise et d'épaisseur 0.5
            axis.line = element_line(color="gray30"), #couleur de la ligne de chacun des axes
            legend.position = "none" #ne pas afficher la légende
      )
    
  } else  if (is.null(CODlist)) {
    anno <- t$data %>% group_by(Terms) %>% filter(StartDay==max(StartDay)) %>% arrange(Terms)
    anno <- anno %>% select(Terms,StartDay,Net)
    
    p2 <- ggplot(t$data,aes(x=StartDay, y=Net, color=Terms)) +
      geom_line(linewidth=1) + 
      geom_point(size=2) +
      geom_hline(yintercept = 0, col="red",linetype=2, linewidth=1) +
      labs(x="Day since start of treatment", y=paste0("Net event - ",list_ARM[1]," over ",list_ARM[2])) + 
      geom_label_repel(data=anno, aes(x=StartDay, y=Net, label=Terms), 
                       color="black",
                       min.segment.length = 0.1, force = 8,
                       max.overlaps = 30,
                       direction = "x") +
      theme(axis.text = element_text(size=rel(1.5)), #taille des labels des axes
            axis.title = element_text(size = rel(1.5)), #taille des titres des axes
            panel.background = element_blank(), #pas d'arrière plan
            panel.grid.major = element_line(color = "gray60", linewidth = 0.5, linetype = 1), #grille de lecture de couleur grise et d'épaisseur 0.5
            axis.line = element_line(color="gray30"), #couleur de la ligne de chacun des axes
            legend.position = "none" #ne pas afficher la légende
      )
  }
  
  suppressWarnings(print(p2))
}


# TreemapAE <- function(data,ARM,SOC="ALL")
TreemapAE <- function(baseEI,baseTr, choixSOC=NULL, listcol, ARMe=NULL,CODvar="PT_COD"){
  #on récupère le nombre de modalité de la variable AE_GRADE pour les échelles de couleurs des graphiques
  vect_grade <- sort(unique((baseEI$AE_GRADE)))
  
  list_ARM <- unique(baseTr$ARM)
  if (is.null(ARMe)) {
    #Liste des id patients dans le bras numéro 1
    list_pat1 <- unique(baseTr$USUBJID[baseTr$ARM == list_ARM[1]]) 
    #Liste des id patients dans le bras numéro 2
    list_pat2 <- unique(baseTr$USUBJID[baseTr$ARM == list_ARM[2]])
  } else if (!is.null(ARMe)){
    l2 <- unique(baseTr$ARM) 
    if (!(ARMe %in% l2)) return("Nom de bras de traitement non correct ou non présent dans la base.") 
    list_ARM[1] <- ARMe
    list_ARM[2] <- l2[l2 != ARMe]
    
    #Liste des id patients dans le bras numéro 1
    list_pat1 <- unique(baseTr$USUBJID[baseTr$ARM == list_ARM[1]]) 
    #Liste des id patients dans le bras numéro 2
    list_pat2 <- unique(baseTr$USUBJID[baseTr$ARM == list_ARM[2]])
  }
  #Ajouter une colonne ARM dans la table data en faisant correspondre les USUBJID selon la liste où ils sont présents
  baseEI$ARM <- ifelse(baseEI$USUBJID %in% list_pat1, "armH", "armB")
  baseTr$ARM <- ifelse(baseTr$ARM==list_ARM[1], "armH","armB")
  
  df_Tr2  <- baseTr[baseTr$TTT_IND=="Yes",]
  USU_distinct  <- df_Tr2  %>% select(USUBJID, ARM, TTT_IND) %>% distinct(USUBJID, ARM)
  frq2 <- data.frame(xtabs(~ ARM, data=USU_distinct))
  
  if (CODvar == "PT_COD"){
    df_AE3 <- baseEI %>% 
      select(SOC_COD,PT_COD,ARM,AE_GRADE) %>%
      group_by(SOC_COD,PT_COD,ARM) %>% summarise(count=n()) 
    df_gmax2 <- baseEI %>% 
      select(SOC_COD,PT_COD,ARM,AE_GRADE) %>%
      group_by(SOC_COD,PT_COD,ARM) %>% dplyr::filter(AE_GRADE == max(AE_GRADE))
  } else if (CODvar=="LLT_COD"){
    df_AE3 <- baseEI %>% 
      select(SOC_COD,LLT_COD,ARM,AE_GRADE) %>%
      group_by(SOC_COD,LLT_COD,ARM) %>% summarise(count=n()) 
    df_gmax2 <- baseEI %>% 
      select(SOC_COD,LLT_COD,ARM,AE_GRADE) %>%
      group_by(SOC_COD,LLT_COD,ARM) %>% dplyr::filter(AE_GRADE == max(AE_GRADE))
  } else return("Valeur non valide pour l'option CODvar")
  names(df_AE3) <- c("SOC_COD","COD","ARM","count")
  names(df_gmax2) <- c("SOC_COD","COD","ARM","AE_GRADE")
  
  df_gmax2 <- df_gmax2 %>% distinct(SOC_COD, COD, ARM, AE_GRADE)
  data_plot <- merge(df_AE3,df_gmax2, by=c("SOC_COD","COD","ARM"))
  
  #pour afficher le pourcentage de patient atteint en plus dans la Treemap (notamment dans les version avec 1 SOC selctionnée)
  #on doit d'abord récupérer le nombre de patient atteint
  #en effet la variable count de df_AE3 correspont au nombre total d'EI on peut donc avoir plusieurs occurrence
  #pour calculer le pourcentage de patients on ne doit garder qu'une occurrence par patient
  if (CODvar == "PT_COD"){
    df_AE4 <- baseEI %>%
      select(PT_COD, ARM ,USUBJID) %>% distinct(PT_COD,USUBJID, ARM) %>% group_by(PT_COD, ARM) %>% summarise(count=n())
  } else if (CODvar=="LLT_COD"){
    df_AE4 <- baseEI %>%
      select(LLT_COD, ARM ,USUBJID) %>% distinct(LLT_COD,USUBJID, ARM) %>% group_by(LLT_COD, ARM) %>% summarise(count=n())
  } else return("Valeur non valide pour l'option CODvar")
  names(df_AE4) <- c("COD","ARM","count")
  
  df_AE4$pctPat <- round(ifelse(df_AE4$ARM=="armH", df_AE4$count/frq2$Freq[frq2$ARM=="armH"],
                                df_AE4$count/frq2$Freq[frq2$ARM=="armB"])*100,1)
  
  #on merge avec la table précédent pour attribuer les pourcentages
  data_plot <- merge(data_plot,df_AE4 %>% select(COD,ARM,pctPat), by=c("COD","ARM"))
  
  if (!is.null(choixSOC)){
    if (!(choixSOC %in% unique(data_plot$SOC_COD))) return("SOC choisie non présente dans la base")
    data_plot <- data_plot[data_plot$SOC_COD==choixSOC,]
    data_plot$pctEvents <- round((data_plot$count / sum(data_plot$count))*100,1)
    
    pH <- ggplot(data_plot %>% filter(ARM=="armH"), 
                 aes(area= count, fill=as.factor(AE_GRADE),
                     label= paste(COD,"\n",count,"(",pctEvents,"% of events)","\n",pctPat,"% of patient"),
                     subgroup=SOC_COD)) +
      geom_treemap() +
      geom_treemap_subgroup_border(color="gray20") +
      geom_treemap_subgroup_text(place="topleft", reflow = T, alpha=0.7, colour = "gray25",
                                 min.size=0, padding.y = grid::unit(-50,"mm")) +
      geom_treemap_text(colour="black", place="centre", size=18) +
      scale_fill_manual(name="Grade max atteint", breaks = vect_grade,
                        values = listcol) +
      ggtitle(paste0(choixSOC,"\n",list_ARM[1])) +
      theme(legend.position = "bottom",
            legend.text = element_text(size=rel(1.2)),
            legend.title = element_text(size = rel(1.2)))
    
    pB <- ggplot(data_plot %>% filter(ARM=="armB"), 
                 aes(area= count, fill=as.factor(AE_GRADE),
                     label= paste(COD,"\n",count,"(",pctEvents,"% of events)","\n",pctPat,"% of patient"),
                     subgroup=SOC_COD)) +
      geom_treemap() +
      geom_treemap_subgroup_border(color="gray20") +
      geom_treemap_subgroup_text(place="topleft", reflow = T, alpha=0.7, colour = "gray25",
                                 min.size=0, padding.y = grid::unit(-50,"mm")) +
      geom_treemap_text(colour="black", place="centre", size=18) +
      scale_fill_manual(name="Grade max atteint", breaks = vect_grade,
                        values = listcol) +
      ggtitle(list_ARM[2]) +
      theme(legend.position = "bottom",
            legend.text = element_text(size=rel(1.2)),
            legend.title = element_text(size = rel(1.2)))
  } else if (is.null(choixSOC)){
    pH <- ggplot(data_plot %>% filter(ARM=="armH"), 
                 aes(area= count, fill=as.factor(AE_GRADE), 
                     label= stringr::str_wrap(paste(COD,count,sep="\n"), 2),
                     subgroup=SOC_COD)) + 
      geom_treemap() + 
      geom_treemap_subgroup_border(color="gray20") +
      geom_treemap_subgroup_text(place="centre", reflow = T, alpha=0.7, colour = "gray25", 
                                 min.size=0) +
      geom_treemap_text(colour="white", place="centre", size=15) +
      scale_fill_manual(name="Grade max atteint", breaks = vect_grade,
                        values = listcol) +
      ggtitle(list_ARM[1]) +
      theme(legend.position = "bottom")
    
    pB <- ggplot(data_plot %>% filter(ARM=="armB"), 
                 aes(area= count, fill=as.factor(AE_GRADE), 
                     label= stringr::str_wrap(paste(COD,count,sep="\n"), 2),
                     subgroup=SOC_COD)) + 
      geom_treemap() + 
      geom_treemap_subgroup_border(color="gray20") +
      geom_treemap_subgroup_text(place="centre", reflow = T, alpha=0.7, colour = "gray25", 
                                 min.size=0) +
      geom_treemap_text(colour="white", place="centre", size=15) +
      scale_fill_manual(name="Grade max atteint", breaks = vect_grade,
                        values = listcol) +
      ggtitle(list_ARM[2]) +
      theme(legend.position = "bottom")
  }
  plot_grid(pH,pB,nrow = 2,rel_heights = c(0.5,0.5))
}

# PanelBio <- function(data,USU, suivi=TRUE, list_Bio=NULL)
PanelBioAE <- function(baseBio, USU, list_Bio=NULL, suivi=TRUE){
  Biopat <- subset(baseBio,baseBio$USUBJID==USU)
  Biopat$BIO_DAT <- as.Date(Biopat$BIO_DAT, format = "%d/%m/%Y")
  
  if (suivi==FALSE){
    lim <- Biopat$BIO_DAT[Biopat$VISITNUM=="End of treatment"]
    Biopat <- subset(Biopat, BIO_DAT<=lim)
  }
  if (!is.null(list_Bio)) {
    if (FALSE %in% (list_Bio %in% unique(Biopat$BIO_LIB))) return("Au moins une des biologie demandée n'est pas disponible dans la base")
    Biopat <- Biopat[Biopat$BIO_LIB %in% list_Bio,]
  }
  lab_biolib <- c(unique(paste(Biopat$BIO_LIB[Biopat$BIO_UNIT!=""],"\n (",Biopat$BIO_UNIT[Biopat$BIO_UNIT!=""],")")))
  names(lab_biolib) <- c(unique(Biopat$BIO_LIB))
  
  ggplot(Biopat %>% select(BIO_DAT,BIO_RES,BIO_LIB,VISITNUM), 
         aes(x=BIO_DAT,y=BIO_RES, label=VISITNUM)) +
    geom_line(linewidth=2, color="gray40") +
    geom_point(size=4, color="gray20") +
    geom_label_repel(direction = "y", nudge_y = 5, fill = alpha(c("white"),0.5),label.padding=.1) +
    labs(x="Date",y="") +
    facet_grid(BIO_LIB~., scales="free", space="fixed", switch="y",
               labeller=labeller(BIO_LIB = lab_biolib)) +
    scale_y_continuous(position = "right") +
    scale_x_date(date_labels = "%b%y") +
    theme(panel.background = element_blank(),
          panel.grid = element_line(color = "gray80"),
          axis.ticks.x = element_line(linewidth = 1, colour = "black"),
          axis.line = element_line(color = "black", linetype = 1),
          strip.placement = "left",
          axis.title = element_text(size=12),
          axis.text = element_text(size=10),
          strip.text.y.left = element_text(angle=0, hjust = 1, size=12))
}


# VolcanoAE <- function(data, caption=TRUE)
VolcanoAE <- function(baseEI, baseTr, CODvar="PT_COD", seuillab = 0.5, caption=TRUE){
  #liste des groupes de traitement de la table baseTr
  list_ARM <- unique(baseTr$ARM)
  #Liste des id patients dans le bras numéro 1
  list_pat1 <- unique(baseTr$USUBJID[baseTr$ARM == list_ARM[1]]) 
  #Liste des id patients dans le bras numéro 2
  list_pat2 <- unique(baseTr$USUBJID[baseTr$ARM == list_ARM[2]])
  #Ajouter une colonne ARM dans la table data en faisant correspondre les USUBJID selon la liste où ils sont présents
  baseEI$ARM <- ifelse(baseEI$USUBJID %in% list_pat1, "arm1", "arm2")
  baseTr$ARM <- ifelse(baseTr$ARM==list_ARM[1], "arm1","arm2")
  
  ############################################################################
  ######################## Construction table volcano ########################
  ###################
  # Calcul des freq
  ###################
  ## AE with events
  if (CODvar == "PT_COD") {
    data_distinct <- baseEI %>% select(USUBJID,ARM,PT_COD) %>% distinct(USUBJID, ARM, PT_COD)
    frq1 <- data.frame(xtabs(~ PT_COD + ARM, data = data_distinct))
  } else if (CODvar == "LLT_COD") {
    data_distinct <- baseEI %>% select(USUBJID,ARM,LLT_COD) %>% distinct(USUBJID, ARM, LLT_COD)
    frq1 <- data.frame(xtabs(~ LLT_COD + ARM, data = data_distinct))
  } else return("Option non valide pour CODvar")
  
  #pour ne pas avoir à ajouter un if pour l'option CODvar par la suite on change le nom de la var en COD (générique par rapport aux deux options)
  names(frq1) <- c("COD","ARM","Freq")
  
  ## Total subjects in each ARM
  df_Tr2  <- baseTr[baseTr$TTT_IND=="Yes",]
  USU_distinct  <- df_Tr2  %>% select(USUBJID, ARM, TTT_IND) %>% distinct(USUBJID, ARM)
  frq2 <- data.frame(xtabs(~ ARM, data=USU_distinct)) # compte le nombre de patient dans chacun des bras
  
  #Merged
  frq3 <- merge(frq1, frq2, by="ARM") 
  frq4 <- frq3 %>%
    mutate(no = Freq.y - Freq.x, # total - # with event
           yes = Freq.x) %>% # all with AE
    arrange(COD, ARM) %>%
    select(c(ARM, COD, yes, no))
  # frq4 : pour chaque type d'EI et par bras, nombre de patients ayant eu cet EI et nombre de patients ne l'ayant pas eu
  
  ########### fonction pour le calcul du RD (pris de fmsb::riskdifference) : modif -> pas de print
  RDfunct <- function (a, b, N1, N0, CRC = FALSE, conf.level = 0.95) {
    .M <- a + b
    .T <- N1 + N0
    .R1 <- a/N1
    .R0 <- b/N0
    .RT <- .M/.T
    norm.pp <- qnorm(1 - (1 - conf.level)/2)
    if (CRC) {
      .RC1 <- norm.pp * sqrt(a * (N1 - a)/(N1^3))
      .RC0 <- norm.pp * sqrt(b * (N0 - b)/(N0^3))
      .RCT <- norm.pp * sqrt(.M * (.T - .M)/(.T^3))
      .MAT <- matrix(c(a, b, .M, N1, N0, .T, .R1, .R0, .RT, 
                       .R1 - .RC1, .R0 - .RC0, .RT - .RCT, .R1 + .RC1, 
                       .R0 + .RC0, .RT + .RCT), 3, 5)
      colnames(.MAT) <- c("Cases", "People at Risk", "Risk", 
                          "Lower CL", "Upper CL")
      rownames(.MAT) <- c("Exposed", "Unexposed", "Total")
    } else {
      .MAT <- matrix(c(a, b, .M, N1, N0, .T, .R1, .R0, .RT), 
                     3, 3)
      colnames(.MAT) <- c("Cases", "People at risk", "Risk")
      rownames(.MAT) <- c("Exposed", "Unexposed", "Total")
    }
    class(.MAT) <- "table"
    ################ print(.MAT) ####################
    ESTIMATE <- .R1 - .R0
    .CHI <- ESTIMATE/sqrt(a * (N1 - a)/(N1^3) + b * (N0 - b)/(N0^3))
    p.v <- 2 * (1 - pnorm(abs(.CHI)))
    RDL <- ESTIMATE - norm.pp * sqrt(a * (N1 - a)/(N1^3) + b * 
                                       (N0 - b)/(N0^3))
    RDU <- ESTIMATE + norm.pp * sqrt(a * (N1 - a)/(N1^3) + b * 
                                       (N0 - b)/(N0^3))
    CINT <- c(RDL, RDU)
    attr(CINT, "conf.level") <- conf.level
    RVAL <- list(p.value = p.v, conf.int = CINT, estimate = ESTIMATE, 
                 method = "Risk difference and its significance probability (H0: The difference equals to zero)", 
                 data.name = paste(deparse(substitute(a)), deparse(substitute(b)), 
                                   deparse(substitute(N1)), deparse(substitute(N0))))
    class(RVAL) <- "htest"
    return(RVAL)
  }
  
  ########################################
  # Calcul Risk difference et P value
  ########################################
  #Creation d'une table pour accueilir toutes les données nécessaires
  dfx_all <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("Freq_Total", "COD", "pval","RD"))
  
  for (p in unique(frq4$COD)){
    #pour chaque PT tableau permettant de calculer la p-value ainsi que le ratio
    df1 <- subset(frq4, COD == p, select = (c(-COD, -ARM)))
    
    #Test de fisher sur le tableau précédent
    stat1 <- fisher.test(df1)
    
    dfx <- data.frame(COD = p,
                      pval = stat1[[1]],
                      frqTot = frq4$yes[frq4$COD == p][1] + frq4$yes[frq4$COD == p][2],
                      RD = RDfunct(df1[[1]][1], df1[[1]][2], frq2$Freq[frq2$ARM=="arm1"], frq2$Freq[frq2$ARM=="arm2"])$estimate,
                      row.names = NULL)
    # A chaque PT concaténation des tables
    dfx_all <- rbind(dfx_all,dfx)
  }
  
  ########################################
  vplot1 <- dfx_all %>%
    mutate(logpval = -log10(pval)) %>%
    dplyr::filter(is.finite(logpval))
  vplot1$harmful <- ifelse(vplot1$RD	<	0, paste("Increased risk in ", list_ARM[2]), paste("Increased risk in ", list_ARM[1]))
  
  ### Nombre de patients dans l'étude pour calculer la proportion
  ### Afin d'exclure les EIs avec une fréquence inférieure à moins de 2% des patients
  vplot1$pct <- (vplot1$frqTot / sum(frq2$Freq))*100
  
  
  ## limites
  lim1x <- floor(min(vplot1$RD)*10)/10
  lim2x <- ceiling(max(vplot1$RD)*10)/10
  
  limy <- ceiling(max(-log10(vplot1$pval)))
  
  ## text for caption
  if (caption==TRUE){
    labcap <- "
        Visual representation of AE data, Volcano plot for adverse events between two treatment arms. 
The x-axis represents the difference in proportions of participants experiencing each adverse event between the treatment arms.
The y-axis represents the p value from Fisher's exact test on the -log10 scale.
The centre of the bubble indicates the coordinates for each adverse event. The size of the bubble is proportional to the total number of events for both arms combined.
Colour saturation help to see if multiple AE are in the same coordinate with red indicating greater risk in the testing arm and blue in the control arm.
    Labels are added to events with value superior or equal to 0.5 on the y-axis."
  } else {labcap=""}
  
  #########################################
  # Volcano Plot
  #########################################
  p <- ggplot(vplot1,
              aes(x=RD, y=logpval, size=frqTot, 
                  label = ifelse(logpval > seuillab, COD, ""), 
                  color=harmful)) +
    geom_point(alpha = 0.40) +
    scale_size(range = c(1,22)) +
    labs(title = "Volcano Plot") + 
    scale_color_manual(breaks = c(paste("Increased risk in ", list_ARM[2]),
                                  paste("Increased risk in ", list_ARM[1])), 
                       values = c("red", "deepskyblue2")) +
    scale_y_continuous(name="-log10(p-value)", limits = c(0,limy)) +
    scale_x_continuous(name="Risk difference", breaks = seq(lim1x,lim2x,by=0.05), 
                       limits = c(lim1x,lim2x)) +
    geom_text_repel(size = 4,color="black",min.segment.length = 0.1, force = 5,
                    max.overlaps = 30,
                    nudge_y = ifelse(vplot1$RD<0,0.02,-0.02), 
                    nudge_x = ifelse(vplot1$RD<0,0.02,-0.02),
                    direction="both") +
    labs(shape="Treatment Group", caption=labcap) +
    guides(size = "none") +
    geom_hline(yintercept=-log10(0.05), col="red", linetype=2) +
    geom_text(x=lim2x,y=-log10(0.05),label = "p-value = 0.05", col="red", size=4, vjust=-0.5, hjust=0.5) +
    geom_vline(xintercept = 0, col="grey") +
    theme(panel.background = element_blank(),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(size = rel(1.2)),
          axis.text = element_text(size=rel(1.2)),
          axis.title = element_text(size=rel(1.5)),
          axis.line= element_line(linewidth =0.5, colour = "black"))
  
  plot(p)
}

