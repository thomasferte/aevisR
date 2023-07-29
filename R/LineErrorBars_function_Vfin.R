LineErrorBars <- function(baseEI,baseTr,baseDates, 
                          idvar, Termsvar, gradevar, EIdatestart_var, EIdateend_var,
                          TTTYN = NULL, ARMvar, tttdebdate_var, tttfindate_var,
                          visitedate_var=NULL, visitnum_var=NULL,
                          grd=2, choixEI, unit, suivi=TRUE, listcol=c("red","deepskyblue2"), caption=TRUE){
  #remplacement des noms de variables
  baseEI <- baseEI %>% rename("id_pat" = idvar,
                              "COD" = Termsvar,
                              "aedatestart" = EIdatestart_var,
                              "aedateend" = EIdateend_var,
                              "Grade" = gradevar)
  baseTr <- baseTr %>% rename("id_pat" = idvar,
                              "ARM" = ARMvar)
  if (!is.null(TTTYN)) baseTr <- baseTr %>% rename("TTTYN" = TTTYN)
  if (!is.null(visitedate_var)) baseTr <- baseTr %>% rename("visdate" = visitedate_var)
  if (!is.null(visitnum_var)) baseTr <- baseTr %>% rename("visnum" = visitnum_var)
  baseDates <- baseDates %>% rename("id_pat" = idvar,
                                    "tttdebdate" = tttdebdate_var,
                                    "tttfindate" = tttfindate_var)
  
  
  #liste des groupes de traitement de la table df_Tr
  list_ARM <- unique(baseTr$ARM)
  #Liste des id patients dans le bras numéro 1
  list_pat1 <- unique(baseTr$id_pat[baseTr$ARM == list_ARM[1]]) 
  #Liste des id patients dans le bras numéro 2
  list_pat2 <- unique(baseTr$id_pat[baseTr$ARM == list_ARM[2]])
  
  #Ajouter une colonne ARM dans la table data en faisant correspondre les id_pat selon la liste où ils sont présents
  baseEI$ARM <- ifelse(baseEI$id_pat %in% list_pat1, "arm1", "arm2")
  baseTr$ARM <- ifelse(baseTr$ARM==list_ARM[1], "arm1","arm2")
  
  if(is.null(TTTYN)){
    df_Tr2 <- baseTr
  } else {
    df_Tr2  <- baseTr[baseTr$TTTYN=="Yes",]
  }
  USU_distinct  <- df_Tr2  %>% select(id_pat, ARM) %>% distinct(id_pat, ARM)
  frq2 <- data.frame(xtabs(~ ARM, data=USU_distinct))
  
  #on merge pour ajouter la date de rando, la date de pré-inclusion,et la date de fin de traitement
  df_AE2 <- merge(baseEI, baseDates, by="id_pat")
  
  # si pas de date de fin ............
  # df_AE2$aedateend[is.na(df_AE2$aedateend)] <- df_AE2$tttfindate[is.na(df_AE2$aedateend)]
  
  # retirer ceux terminés avant la date de début de traitement
  df_AE3 <- subset(df_AE2,!(df_AE2$aedateend < df_AE2$tttdebdate))
  # et ceux survenus après la date de fin de traitement
  if(unit=="cycle" | (unit != "cycle" & suivi==FALSE)) df_AE3 <- subset(df_AE3,!(df_AE3$aedatestart > df_AE3$tttfindate))
  
  #################################################
  #création de la base avec les unités de temps
  #################################################
  if (unit=="cycle"){
    if (is.null(visitedate_var) | is.null(visitnum_var)) return("Si vous choisissez d'afficher des cycles alors les arguments visitdate_var et visitnum_var doivent tous les deux être renseignés")
    #Récupération d'une date par cycle pour chaque individu
    df_unitdate <- df_Tr2 %>% select(id_pat, visnum, visdate) %>% 
      distinct(id_pat, visnum, visdate) %>%
      group_by(id_pat, visnum,visdate)
    df_unitdate <- df_unitdate %>% filter(!is.na(visdate))
    df_unitdate <- df_unitdate %>% group_by(id_pat,visnum) %>% filter(visdate==min(visdate))
    
    # Ajout d'une colonne avec la date de fin du cycle qui sera la jour précédent le TTT du cycle suivant
    df_unitdate$DAT_FIN_CYCLE = NA
    for (i in 1:(nrow(df_unitdate)-1)) df_unitdate$DAT_FIN_CYCLE[i] <- df_unitdate$visdate[i+1]
    df_unitdate$DAT_FIN_CYCLE <- as.Date(df_unitdate$DAT_FIN_CYCLE, origin = "1970-01-01")
    
    #correction au dernier cycle
    for(i in 1:nrow(df_unitdate)){
      if(df_unitdate$visnum[i]==max(df_unitdate$visnum)){
        df_unitdate$DAT_FIN_CYCLE[i]<-baseDates$tttfindate[baseDates$id_pat==df_unitdate$id_pat[i]]
      }
    }
    names(df_unitdate) <- c("id_pat","visnum","DAT_DEB_CYCLE","DAT_FIN_CYCLE")
    df_unitdate$visnum <- as.numeric(df_unitdate$visnum)
  } else if (unit %in% c("week","month","quarter","halfyear","year")){
    if (unit=="week") {pas = 7
    } else if (unit=="month") {pas = 30
    } else if (unit=="quarter") {pas = 90
    } else if (unit=="halfyear") {pas= 180
    } else if (unit=="year") {pas = 365}
    #création d'une table avec les dates pour chaque patient des limites pour les semaine, mois, ou années en fonction de unit
    df_AE_endmax <- df_AE3 %>% select(id_pat, aedateend, tttfindate) %>%
      group_by(id_pat)
    if (nrow(df_AE_endmax==1) & (TRUE %in% is.na(df_AE_endmax$aedateend))) {
      df_AE_endmax <- df_AE_endmax %>% 
        filter(tttfindate==max(tttfindate)) %>%
        select(-aedateend) %>%
        rename("aedateend" = tttfindate)
    } else {
      df_AE_endmax <- df_AE_endmax %>% filter(aedateend==max(aedateend)) %>% select(-tttfindate)
    }
    
    df_unitdate <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("id_pat", "visnum", "DAT_DEB_CYCLE","DAT_FIN_CYCLE"))
    for (p in df_AE_endmax$id_pat){
      df_p <- df_AE_endmax[df_AE_endmax$id_pat==p,]
      start <- unique(df_AE3$tttdebdate[df_AE3$id_pat==p])
      
      c=1
      bsup <- ifelse(suivi==TRUE, unique(df_p$aedateend),df_AE3$tttfindate[df_AE3$id_pat==p])
      while(start < bsup){
        date <- start+pas
        new_line <- c(p,c,start,date)
        c=c+1
        start <- date
        
        df_unitdate <- rbind(df_unitdate,new_line)
      }
    } 
    names(df_unitdate) <- c("id_pat","visnum","DAT_DEB_CYCLE","DAT_FIN_CYCLE")
    df_unitdate$DAT_FIN_CYCLE <- as.Date(as.numeric(df_unitdate$DAT_FIN_CYCLE), origin = "1970-01-01")
    df_unitdate$DAT_DEB_CYCLE <- as.Date(as.numeric(df_unitdate$DAT_DEB_CYCLE), origin = "1970-01-01")
    df_unitdate$visnum <- as.numeric(df_unitdate$visnum)
  } else return("Valeur non valide pour l'option unit")
  
  
  ## Attribuer un (ou plusieurs) cycle à chaque EI de la base df_AE en fonction de la date d'occurence et du patient concerné
  ## si on fait full_join on aura aussi les individus qui n'ont pas eu l'EI anaemia et présents dans la table df_Tr3
  ## (comme dans le Alluvial)
  df_unitdate$id_pat <- as.character(df_unitdate$id_pat)
  df_AE4 <- left_join(df_AE3, df_unitdate, by = "id_pat", multiple = "all")
  
  #on garde les grades pour les lignes où la date de début et de fin de l'EI correspondent au cycle
  #sinon on remplace par 0 pour indiquer qu'a ce cycle le patient n'avait pas d'EI (grade 0)
  df_AE4$Grade[ ((df_AE4$aedatestart > df_AE4$DAT_FIN_CYCLE) |
                      (df_AE4$aedateend < df_AE4$DAT_DEB_CYCLE))] =0
  
  df_AE4 <- df_AE4 %>% select(ARM, id_pat,COD, visnum,Grade) %>% 
    arrange(ARM, id_pat, visnum, desc(Grade))
  
  ###################################################
  ### Sous groupe Overall
  ###################################################
  #Selection des variables d'interêt
  # fréquence par PT et par cycle
  frq1 <- df_AE4 %>% select(id_pat,ARM,COD, visnum) %>% distinct(id_pat, ARM, COD, visnum) #liste des EIs distincts pour chaque patient en précisant son bras de traitement
  frq1 <- data.frame(xtabs(~ COD + ARM + visnum, data = frq1)) #frequence de chaque type d'EI dans chacun des bras de traitement
  frq1 <- frq1 %>% pivot_wider(names_from = ARM, values_from = Freq)
  
  
  #######################################################
  ### Subgroup (<= grade x) pour chaque cycle
  #######################################################
  # table avec le compte d'EIs de grade >= x à chaque cycle
  if(grd > max(baseEI$Grade)) return("Grade non présent dans la base")
  if(grd == 1) return("Impossible de comparer les EIs de grade supérieur ou égale à 1 à tous les EIs, car il représentent toute la base de donnée")
  df_AE5 <- subset(df_AE4, df_AE4$Grade >=grd)
  #on garde une occurence unique pour chaque id_pat et COD
  df_AE5 <- df_AE5 %>% select(-Grade) %>% distinct(id_pat,ARM,COD, visnum)
  # fréquence par PT et par cycle pour les EIs de grade <=x
  frq2 <- data.frame(xtabs(~ COD + ARM + visnum, data=df_AE5))
  frq2 <- frq2 %>% pivot_wider(names_from = ARM, values_from = Freq)
  
  #### on merge les deux tables créées frq1 et frq2 pour pouvoir calculer le RD
  frq <- left_join(frq1, frq2, by=c("COD","visnum")) %>% mutate(across(where(is.numeric),~replace_na(.x,0)))
  names(frq) <- c("COD","visnum","G1tot","G2tot","G1grd","G2grd")
  
  
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
  ## on cible tout  d'abord l'EI choisi
  if (!(choixEI %in% unique(frq$COD))) return("EI choisit non présent dans la base")
  frq <- frq[frq$COD == choixEI,]
  
  #Creation d'une table pour accueilir toutes les données nécessaires
  dfx_all <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("visnum","CI_1", "CI_2", "RD"))
  
  for (c in unique(frq$visnum)){
    #pour chaque PT tableau permettant de calculer la p-value ainsi que le ratio
    df1 <- subset(frq,visnum==c, select = (c(-COD, -visnum)))
    
    dfx <- data.frame(visnum=c,
                      RD = RDfunct(df1$G2grd, df1$G1grd, df1$G2tot, df1$G1tot, CRC=TRUE)$estimate,
                      CI_1 = RDfunct(df1$G2grd, df1$G1grd, df1$G2tot, df1$G1tot, CRC=TRUE)$conf.int[1],
                      CI_2 = RDfunct(df1$G2grd, df1$G1grd, df1$G2tot, df1$G1tot, CRC=TRUE)$conf.int[2],
                      row.names = NULL)
    # A chaque PT concatenation des tables
    dfx_all <- rbind(dfx_all,dfx)
  }
  
  # on retire tous les NA
  dfx_all <- subset(dfx_all, !is.na(dfx_all$RD))
  lim <- max(as.numeric(dfx_all$visnum))
  
  p <- ggplot(dfx_all, aes(x=as.numeric(visnum),y=RD, group=1)) +
    geom_point() +
    geom_errorbar(aes(ymin=CI_2, ymax=CI_1),width=.1) +
    geom_hline(yintercept=0, lty=2, col="red",linewidth=1)+
    labs(y=paste0("RD\n(",list_ARM[2]," - ",list_ARM[1],")"),
         x=paste0(unit," of treatement"))+
    {if(unit=="week") scale_x_continuous(breaks = seq(0,lim,by=5))} +
    {if(unit=="month") scale_x_continuous(breaks = seq(0,lim,by=1))} +
    {if(unit=="cycle") scale_x_continuous(breaks = seq(0,lim,by=1))} +
    
    scale_y_continuous(position = "right") +
    ggtitle(paste("Graphique pour les grades >=", grd,"pour",choixEI))+
    theme(panel.background = element_blank(),
          panel.grid.major.y = element_line(color = "darkgray", linewidth = 0.1, linetype = 3),
          axis.ticks.x = element_line(linewidth = 1, colour = "black"),
          axis.line = element_line(color = "black", linetype = 1),
          strip.placement = "none",
          axis.title = element_text(size=10),
          axis.text = element_text(size=10),
          strip.text.y.left = element_text(angle=0, hjust = 0.95, size=15),
          panel.spacing.y = unit(3,"pt"))
  
  if(!is.null(listcol)){
    p <- p + annotate(geom="rect", xmin=-Inf, xmax=Inf,ymin=-Inf,ymax=0, fill=listcol[2], alpha=0.2)+
    annotate(geom="rect",xmin=-Inf, xmax=Inf,ymin=0,ymax=Inf, fill=listcol[1], alpha=0.2)
  }
  
  if(caption==TRUE){
    labcap <- paste("Ce graph représente le RD (avec IC 95%) d'avoir, pour l'EI choisit, 
    un grade >= x (ici", grd ,") par rapport à tout grade entre les deux groupes de traitement 
    pour une période données (cycle, semaine...).")
    p <- p + labs(caption = labcap)
  }
  
  
  suppressWarnings(print(p))
}
