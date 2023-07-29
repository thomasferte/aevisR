#fonction qui renvoie le plot ($plot) et sa hauteur ($height) qui est conditionnée au nombre de lignes affichées dans le plot
#la hauteur est utilisée dans l'application shiny pour l'affichage du graphique

PatientSpanAE <- function(baseEI, baseTr, baseDates, baseBio, 
                          idvar, Termsvar, SOCvar, EIdatestart_var, EIdateend_var, gradevar, ongovar, outvar, SAEvar, ttt1var, ttt2var,
                          ARMvar, tttlab_var, tttdate_var,visnum_var,
                          tttdebdate_var, tttfindate_var, randodate_var, screendate_var,
                          datebiovar, labbiovar, resbiovar, unitbiovar,
                          choixUSU, suivi=TRUE, listcol, bio=TRUE){
  #remplacement des noms de variables
  baseEI <- baseEI %>% rename("id_pat" = idvar,
                              "COD" = Termsvar,
                              "SOC" = SOCvar,
                              "aedatestart" = EIdatestart_var,
                              "aedateend" = EIdateend_var,
                              "grade" = gradevar,
                              "ongo" = ongovar,
                              "outcome" = outvar,
                              "serious" = SAEvar,
                              "ttt1" = ttt1var,
                              "ttt2" = ttt2var)
  baseTr <- baseTr %>% rename("id_pat" = idvar,
                              "ARM" = ARMvar,
                              "tttlab" = tttlab_var,
                              "visdate"=tttdate_var,
                              "visnum"=visnum_var)
  baseDates <- baseDates %>% rename("id_pat" = idvar,
                                    "randodate"=randodate_var,
                                    "screendate"=screendate_var,
                                    "tttdebdate" = tttdebdate_var,
                                    "tttfindate" = tttfindate_var)
  baseBio <- baseBio %>% rename("id_pat" = idvar,
                                "datebio" = datebiovar,
                                "labbio" = labbiovar,
                                "resbio" = resbiovar,
                                "unitbio" = unitbiovar
                                )
  
  #liste des groupes de traitement de la table baseTr
  list_ARM <- unique(baseTr$ARM)
  
  #on récupère le nombre de modalité de la variable grade pour les échelles de couleurs des graphiques
  vect_grade <- sort(unique((baseEI$grade)))
  # On stock son bras de traitement pour l'afficher dans le graphique
  ARMp <- unique(baseTr$ARM[baseTr$id_pat == choixUSU])
  
  ############ table patient ########## 
  df_pat <- subset(baseEI, baseEI$id_pat==choixUSU)
  
  randodate <- as.Date(baseDates$randodate[baseDates$id_pat==choixUSU],format="%d/%m/%Y")
  screendate <- as.Date(baseDates$screendate[baseDates$id_pat==choixUSU],format="%d/%m/%Y")
  suidate <- as.Date(baseDates$tttfindate[baseDates$id_pat==choixUSU],format="%d/%m/%Y")
  tttdebdate <- as.Date(baseDates$tttdebdate[baseDates$id_pat==choixUSU],format="%d/%m/%Y")

  # si pas de date de fin et ongo yes alors on met la date la plus lointaine pour ce patient
  # on indiquera par une flèche sur le graphique que cet EI est en cours
  # si pas de date de fin et ongo yes alors on met la date de début
  df_pat$aedateend[is.na(df_pat$aedateend) & df_pat$ongo=="Yes"] <- df_pat$aedatestart[is.na(df_pat$aedateend) & df_pat$ongo=="Yes"]
  
  # si on ne veut pas le suivi
  if (suivi==FALSE){
    #si date de fin sup mais pas date de début alors on change la date de fin avec la date de suivi
    for (i in 1:nrow(df_pat)){
      if(df_pat$aedatestart[i] < suidate & df_pat$aedateend[i] > suidate & !is.na(df_pat$aedateend[i])){
        df_pat$aedateend[i]<-suidate
        df_pat$ongo[i]<-"Yes"
        df_pat$outcome[i]<-NA
      } 
    }
    #on ne garde que ceux avec date de début inf à date de suivi et date de fin inf ou égale à la date de suivi
    df_pat <- subset(df_pat,(df_pat$aedateend <= suidate & df_pat$aedatestart <= suidate) | is.na(df_pat$aedateend))
  }
  
  # transformer les dates en jours depuis la date de début de traitement
  df_pat$aestartdate <- as.numeric(df_pat$aedatestart - tttdebdate)
  df_pat$aeenddate<- as.numeric(df_pat$aedateend - tttdebdate)
  df_pat <- df_pat %>% select(-c(aedatestart,aedateend, id_pat))
  
  # ranking pour le graph
  table_rk <- df_pat %>% arrange(SOC, grade, COD) %>% distinct(SOC, COD,grade)
  
  # liste des SOC et ajout à la table pat
  l_SOC <- unique(df_pat$SOC)
  l_SOC2 <- paste0(" ",l_SOC," ------")
  table_SOC <- cbind(l_SOC,l_SOC2,NA)
  colnames(table_SOC) <- c("SOC", "COD", "grade")  
  table_rk <- rbind(table_rk,table_SOC)
  table_rk$grade <- as.integer(table_rk$grade)
  table_rk <- table_rk %>% arrange(SOC, COD, grade)
  table_rk$rank <- 6:(nrow(table_rk)+5) #à partir du rang 6 car avant on a les traitements (x4) et la bio (traités par la suite)

  
  df_pat <- full_join(df_pat %>% select(-SOC), table_rk %>% select(-SOC), by=c("COD","grade"), multiple="all") 
  
  
  ############ table traitement ########## 
  # comme pour la table EI on selectionne les traitements associés au patients choisi
  df_Trpat <- subset(baseTr, baseTr$id_pat==choixUSU)
  # calcul en jour depuis la rando pour chaque traitement
  df_Trpat$visdate <- as.Date(df_Trpat$visdate, format = "%d/%m/%Y")
  df_Trpat$tttdat <- as.numeric(df_Trpat$visdate - tttdebdate)
  
  df_Trpat <- df_Trpat %>% select(visnum, tttlab, tttdat)
  df_Trpat <- df_Trpat[!is.na(df_Trpat$tttdat),] #on retire les lignes avec NA
  
  #rank de chacun des traitements
  tb_rank <- as.data.frame(unique(df_Trpat$tttlab))
  colnames(tb_rank)<-"tttlab"
  tb_rank$rank <- 1:nrow(tb_rank)
  df_Trpat <- merge(df_Trpat,tb_rank, by="tttlab")
  
  # vecteur de jours pour l'axe x
  vect2 <- unique(df_Trpat$tttdat)
  
  if (!is.null(baseBio)){
    ############ table biologie / Hemo ########## 
    df_Biopat <- subset(baseBio, baseBio$id_pat==choixUSU)
    df_Biopat$datebio <- as.Date(df_Biopat$datebio, format = "%d/%m/%Y")
    # if (suivi==FALSE)  df_Biopat <- subset(df_Biopat,df_Biopat$datebio <= suidate)
    df_Biopat$biodays <- as.numeric(df_Biopat$datebio - tttdebdate) 
    df_Biopat$rank<-5 #rang 5 car avant il y a les 4 traitements
    df_Biopat$col <- ifelse(df_Biopat$resbio <85, 2, ifelse(df_Biopat$resbio <100,1,0))
  }
    
  ######################################################################## 
  ## combinaison des trois tables avec renommage des colonnes
  #Creation d'une table pour accueilir toutes les données nécessaires
  dfxpat <- data.frame(aeterm=df_pat$COD,
                       grade=df_pat$grade,
                       ongo=df_pat$ongo,
                       outcome=df_pat$outcome,
                       serious=df_pat$serious,
                       aestartdate=df_pat$aestartdate,
                       aeenddate=df_pat$aeenddate,
                       tttdat=NA,biodat=NA,biores=NA,colbio=NA,
                       ttt1 = df_pat$ttt1,
                       ttt2 = df_pat$ttt2,
                       rank=df_pat$rank)
  dfxttt <- data.frame(aeterm=df_Trpat$tttlab,
                       grade=NA,ongo=NA,outcome=NA,serious=NA,
                       aestartdate=NA,aeenddate=NA,
                       tttdat=df_Trpat$tttdat,biodat=NA,biores=NA,
                       colbio=NA,ttt1 = NA,ttt2 = NA,
                       rank=df_Trpat$rank)
  if (!is.null(baseBio)){
    dfxbio <- data.frame(aeterm="Bio / Hemo",
                         grade=NA,ongo=NA,outcome=NA,serious=NA,
                         aestartdate=NA,aeenddate=NA,tttdat=NA,
                         biodat=df_Biopat$biodays,
                         biores=df_Biopat$resbio,
                         colbio=df_Biopat$col,
                         ttt1 = NA,ttt2 = NA,
                         rank=df_Biopat$rank)
    df_plot <- rbind(dfxpat, dfxttt,dfxbio)
  } else if (is.null(baseBio)){
    df_plot <- rbind(dfxpat, dfxttt)
  }
  
  ## ajout d'une colonne bcol 1 ou 0 selon si le fond doit être blanc ou gris
  df_num <- df_plot %>% select(aeterm,rank) %>% arrange(rank)
  cpt=0
  for (i in 1:nrow(df_num)){
    if(str_detect(df_num$aeterm[i],"----") == TRUE)  cpt=cpt+1
    df_num$num[i]=cpt
  }
  
  #A chaque numéro on récupère le min et le max qui seront donc les limites des rectangles pour l'axes des ordonnées (représenté par les COD)
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
  df_plot$grade <- as.factor(df_plot$grade)
  seqx <-seq(0,max,by=ifelse(max>200,40,20))
  seqdate <- as.character(as.Date(seqx,origin=tttdebdate))
  ltyp <- ifelse(df_plot$ttt1=="No" & (df_plot$ttt2=="No" | df_plot$ttt2=="Not applicable"),"black",
                 ifelse(df_plot$ttt1=="Yes" & (df_plot$ttt2 == "No" | df_plot$ttt2=="Not applicable"), "deepskyblue2",
                        ifelse(df_plot$ttt1=="No" & df_plot$ttt2 == "Yes","red","purple")))
  
  df_plot$ltype=ifelse(df_plot$ttt1=="No" & (df_plot$ttt2=="No" | df_plot$ttt2=="Not applicable"),"B",
                       ifelse(df_plot$ttt1=="Yes" & (df_plot$ttt2 == "No" | df_plot$ttt2=="Not applicable"), "D",
                              ifelse(df_plot$ttt1=="No" & df_plot$ttt2 == "Yes","R","P")))
  
  if (!is.null(baseBio)){
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
    geom_hline(yintercept = as.numeric(screendate - tttdebdate), color="gray50", lty=1, linewidth=0.5) +
    geom_text(x=-Inf,y=as.numeric(screendate - tttdebdate),label = "Screening", col="gray50", size=4, vjust=-0.3, hjust=-1, angle=90) +
    
    #ligne verticale pour la date de rando
    geom_hline(yintercept = as.numeric(randodate - tttdebdate), color="gray50", lty=1, linewidth=0.5) + 
    geom_text(x=-Inf,y=as.numeric(randodate - tttdebdate),label = "Rando", col="gray50", size=4, vjust=-0.3, hjust=-0.1, angle=90) +
    
    #ligne verticale pour la date de suivi (fin de traitement)
    geom_hline(yintercept = as.numeric(suidate - tttdebdate), color="red", lty=1, linewidth=0.5) +
    geom_text(x=Inf,y=as.numeric(suidate - tttdebdate),label = "Suivi", col="red", size=4, vjust=-0.3, hjust=1.5, angle=90) +
    
    #ligne verticale pleine noir pour le jours 0 : début du traitement
    geom_hline(yintercept = 0, color="gray20", lty=1, linewidth=0.5) +
    
    #segments représentants chaque EI (la couleur représentant le grade)
    geom_segment(aes(y=aestartdate, yend=aeenddate,
                     x=aeterm,xend=aeterm,
                     colour=grade),
                 linewidth=6) +
    scale_colour_manual(name="grade", breaks = vect_grade,labels = vect_grade,
                        values = listcol) +
    new_scale_color() +
    #ligne dans le segment principal s'il sagit d'un serious
    geom_segment(data=df_plot %>% filter(serious=="Yes"),
                 aes(y=aestartdate, yend=aeenddate,x=aeterm,xend=aeterm, colour=serious),linewidth=1) +
    scale_colour_manual(name="",breaks = "Yes",labels = "Serious",values = "black") +
    
    #points pour le début de l'EI (la couleur représantant le liens avec les traitements)
    geom_point(data=df_plot %>% filter(ltype!="B"),
               aes(y=aestartdate,x=reorder(aeterm,desc(rank)),fill=ltype),
               color="black",size=3,shape=21)+
    scale_fill_manual(name="Related", breaks = c("D","R","P"),
                      c(list_ARM[1],list_ARM[2],"Both"),
                      values = c('deepskyblue2', 'red', 'purple')) +
    
    #point pour la fin de l'EI (la forme représentant l'outcome de l'EI)
    geom_point(aes(y=aeenddate, x=reorder(aeterm,desc(rank)), shape = factor(outcome)),size=3, color="black")+
    scale_shape_manual(breaks = c("Recovered", "Recovered with sequelea", "Worsening", "Not recovered", "Death"),
                       values = c("Recovered" = 1, "Recovered with sequelea" = 10,"Worsening" = 0, "Not recovered" = 7, "Death"=8)) +
    
    #flèches pour representer les EIs qui sont ongoing
    geom_segment(data=df_plot %>% filter(ongo=="Yes"),
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
    {if (bio==TRUE) geom_text(aes(y=biodat, x=aeterm, label=biores),size=3, angle=45,fontface="bold",
              colour=ifelse(df_plot$colbio==2,"red",ifelse(df_plot$colbio==1,'red3','black')))} +
    
    #axes verticaux x2 : un pour les jours depuis la rando, un pour les dates brutes de ce patient
    scale_y_continuous(name="Days since rando",breaks = seqx,
                       sec.axis = dup_axis(trans=~.,name="", labels = seqdate)) +
    guides(colour = guide_legend(nrow = 2, byrow = TRUE)) +
    labs(x=NULL, y="Days since rando", shape = "Outcome") + 
    coord_flip() +
    ggtitle(paste0("Span chart for patient ",choixUSU, " (",ARMp,")")) + 
    theme(plot.title = element_text(size=18),
          panel.background = element_rect(fill="#ffffff", colour=NA),
          axis.title.x=element_text(vjust=-0.2), axis.title.y=element_text(vjust=1.5),
          panel.border = element_blank(), axis.line=element_blank(),
          panel.grid.minor=element_blank(),
          panel.grid.major.y = element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_text(size=12),
          strip.text=element_text(size=12),
          axis.text.y = element_text(size=12),
          axis.text.x.top = element_text(size=10, angle=30, vjust=0.5),
          axis.text.x.bottom = element_text(size=10),
          axis.line.x = element_line(color="gray70"),
          axis.ticks.x = element_line(color="gray70"),
          strip.background=element_rect(fill="#ffffff", colour=NA),
          panel.spacing.y=unit(1.5, "lines"),
          legend.position="bottom",
          legend.justification = "left",
          legend.box = "vertical",
          legend.box.just = "left",
          legend.key = element_blank(),
          legend.text = element_text(size=9),
          legend.title = element_text(size=9, family = "bold"),
          legend.spacing.y = unit(0.05,'cm'))
  #suppressWarnings(print(timeline))
  res <- list(plot = timeline, height = 170 +length(unique(df_plot$aeterm))*25)
  return(res)
}
