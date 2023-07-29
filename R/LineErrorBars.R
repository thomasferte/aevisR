#' LineErrorBars
#'
#' @description Ce graphique peut être combiné avec le panel Lasagna/Alluvial pour comparer avec un IC les pourcentage des patients atteints par des EIs de grade >= value.
#' Il est composé d’une barre d’erreur avec le RD et son IC à 95% pour chaque période (choix entre cycle, semaine, mois…) et permet par exemple de voir si la proportion de patient ayant eu une Anémie de grade >= 3 (parmis ceux ayant eu une Anémie) est significativement plus élevée dans un groupe de traitement ou non.
#' Ce graphique n’est adapté que pour deux groupes de traitements.
#'
#' @param baseEI : la base des EIs avec
#' \itemize{
#'    \item l’identifiant du patient (idvar) : au format character obligatoire et sans manquants
#'    \item le label des PT ou LLT (Termsvar) pour chaque EI : au format character obligatoire et sans manquants
#'    \item la date de début de l’EI (EIdatestart_var) : au format Date (important) obligatoire
#'    \item la date de fin de l’EI (EIdateend_var) : au format Date (important) obligatoire
#'    \item le grade de l’EI (gradevar) : au format numeric obligatoire en partant de 1 (et allant jusqu’au grade max par exemple 5) et sans manquants
#' }
#' @param baseTr : la base des traitements avec
#' \itemize{
#'    \item l’identifiant du patient (idvar) : au format character obligatoire et sans manquants
#'    \item le bras de traitement pour chaque individu (ARMvar) : au format character obligatoire et sans manquants
#'    \item [non obligatoire] une variable pour indiquer si le traitement à été pris (sert à détecter les patients “à risque”) TTTYN on recherchera les occurences en “Yes” donc doit être codée en “Yes”/“No” en character
#'    \item [non obligatoire] une variable pour le numéro de la visite (ou du cycle) (visitenum_var): non obligatoire, à indiquer seulement s’il y a un format de visites ou de cycles dans l’étude. Au format numeric et sans manquants.
#'    \item [non obligatoire] combinée à, la date de début de ce cycle (date de visite, de prise de traitement en début de période par exemple) (visitedate_var) : idem non obligatoire, à indiquer seulement s’il y a format de visites ou de cycles dans l’étude. Au format Date et peut avoir des manquantes si par exmple il n’y a pas eu de prise de traitement à ce cycle là (on aurait TTTYN=“No”).
#'    \item Note 2: il est possible d’avoir plusieurs dates par cycles/période si par exemple on a plusieurs injections ou plusieurs prises de traitement entre deux visites ou dans un même cycle. Pour choisir la date de début de cycle, nous prendrons la plus ancienne.
#' }
#' @param baseDates : la base qui contient les dates principales liées à chaque patient, celles qui nous sont utiles pour ce graphique sont :
#' \itemize{
#'    \item l’identifiant du patient (idvar) : au format character obligatoire et sans manquants
#'    \item la date de début de traitement tttdebdate_var : au format Date obligatoire et sans manquants
#'    \item la date de fin de traitement/fin de suivi tttfindate_var qui correspond à la fin de la période traitement par exemple, des EIs peuvent se situer après cette date. Si le format “cycle” est choisit on arrête le dernier cycle à cette date, si un autre format est choisit, la date de fin du graphique sera en fonction de l’option suivi. Si suivi=TRUE alors on prend tous les EIs enregistrés, sinon si suivi=False alors on termine l’axe du temps à la date de fin tttfindate_var. Au format Date et sans manqants (si on choisit l’option suivi=FALSE ou unit="cycle")
#' }
#' @param idvar column name
#' @param Termsvar column name
#' @param gradevar column name
#' @param EIdatestart_var column name
#' @param EIdateend_var column name
#' @param TTTYN column name
#' @param ARMvar column name
#' @param tttdebdate_var column name
#' @param tttfindate_var column name
#' @param visitedate_var column name
#' @param visitnum_var column name
#' @param grd : comparaison entre les deux groupe de la proportion de patients ayant eu au moins un EI de grade >= grd par rapport aux pct de patient ayant eu l’EI (tout grade confondu) (d’où ne peut pas être égal à 1) –> par défaut 2
#' @param choixEI : EI choisis à afficher sur le graphique –> ex “ANAEMIA”
#' @param unit : unité au choix entre “cycle”/“week”/“month”/“quarter”/“halfyear”/“year”. Comme précisé précédement le cycle peut correspondre à une période entre deux visites ou deux prise de traitement en fonction de l’étude… –> pas de valeur par défaut car dépend de chaque étude.
#' @param suivi : TRUE / FALSE afficher ou non les EIs survenus après la date de fin de traitement / début de suivi (tttfindate_var) –> par défaut TRUE
#' @param listcol : liste de couleurs pour chacun des groupes de traitements dans un vecteur soit avec les noms implémentés dans R (comme “red”, “blue”…) soit avec des codes (commençant par # par exemple “#52717F”) –> par défaut c(“red”,“deepskyblue2”)
#' @param caption : TRUE/FALSE afficher une note de base de graphique avec des informations supplémentaires sur le format et le continu du graphique –> par défaut TRUEtion
#'
#' @return A plot
#' @export
#'
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
  #Liste des id patients dans le bras num\u00e9ro 1
  list_pat1 <- unique(baseTr$id_pat[baseTr$ARM == list_ARM[1]])
  #Liste des id patients dans le bras num\u00e9ro 2
  list_pat2 <- unique(baseTr$id_pat[baseTr$ARM == list_ARM[2]])

  #Ajouter une colonne ARM dans la table data en faisant correspondre les id_pat selon la liste où ils sont pr\u00e9sents
  baseEI$ARM <- ifelse(baseEI$id_pat %in% list_pat1, "arm1", "arm2")
  baseTr$ARM <- ifelse(baseTr$ARM==list_ARM[1], "arm1","arm2")

  if(is.null(TTTYN)){
    df_Tr2 <- baseTr
  } else {
    df_Tr2  <- baseTr[baseTr$TTTYN=="Yes",]
  }
  USU_distinct  <- df_Tr2  %>% select(id_pat, ARM) %>% distinct(id_pat, ARM)
  frq2 <- data.frame(xtabs(~ ARM, data=USU_distinct))

  #on merge pour ajouter la date de rando, la date de pr\u00e9-inclusion,et la date de fin de traitement
  df_AE2 <- merge(baseEI, baseDates, by="id_pat")

  # si pas de date de fin ............
  # df_AE2$aedateend[is.na(df_AE2$aedateend)] <- df_AE2$tttfindate[is.na(df_AE2$aedateend)]

  # retirer ceux termin\u00e9s avant la date de d\u00e9but de traitement
  df_AE3 <- subset(df_AE2,!(df_AE2$aedateend < df_AE2$tttdebdate))
  # et ceux survenus apr\u00e8s la date de fin de traitement
  if(unit=="cycle" | (unit != "cycle" & suivi==FALSE)) df_AE3 <- subset(df_AE3,!(df_AE3$aedatestart > df_AE3$tttfindate))

  #################################################
  #cr\u00e9ation de la base avec les unit\u00e9s de temps
  #################################################
  if (unit=="cycle"){
    if (is.null(visitedate_var) | is.null(visitnum_var)) return("Si vous choisissez d'afficher des cycles alors les arguments visitdate_var et visitnum_var doivent tous les deux \u00eatre renseign\u00e9s")
    #R\u00e9cup\u00e9ration d'une date par cycle pour chaque individu
    df_unitdate <- df_Tr2 %>% select(id_pat, visnum, visdate) %>%
      distinct(id_pat, visnum, visdate) %>%
      group_by(id_pat, visnum,visdate)
    df_unitdate <- df_unitdate %>% filter(!is.na(visdate))
    df_unitdate <- df_unitdate %>% group_by(id_pat,visnum) %>% filter(visdate==min(visdate))

    # Ajout d'une colonne avec la date de fin du cycle qui sera la jour pr\u00e9c\u00e9dent le TTT du cycle suivant
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
    #cr\u00e9ation d'une table avec les dates pour chaque patient des limites pour les semaine, mois, ou ann\u00e9es en fonction de unit
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


  ## Attribuer un (ou plusieurs) cycle \u00e0 chaque EI de la base df_AE en fonction de la date d'occurence et du patient concern\u00e9
  ## si on fait full_join on aura aussi les individus qui n'ont pas eu l'EI anaemia et pr\u00e9sents dans la table df_Tr3
  ## (comme dans le Alluvial)
  df_unitdate$id_pat <- as.character(df_unitdate$id_pat)
  df_AE4 <- left_join(df_AE3, df_unitdate, by = "id_pat", multiple = "all")

  #on garde les grades pour les lignes où la date de d\u00e9but et de fin de l'EI correspondent au cycle
  #sinon on remplace par 0 pour indiquer qu'a ce cycle le patient n'avait pas d'EI (grade 0)
  df_AE4$Grade[ ((df_AE4$aedatestart > df_AE4$DAT_FIN_CYCLE) |
                      (df_AE4$aedateend < df_AE4$DAT_DEB_CYCLE))] =0

  df_AE4 <- df_AE4 %>% select(ARM, id_pat,COD, visnum,Grade) %>%
    arrange(ARM, id_pat, visnum, desc(Grade))

  ###################################################
  ### Sous groupe Overall
  ###################################################
  #Selection des variables d'inter\u00eat
  # fr\u00e9quence par PT et par cycle
  frq1 <- df_AE4 %>% select(id_pat,ARM,COD, visnum) %>% distinct(id_pat, ARM, COD, visnum) #liste des EIs distincts pour chaque patient en pr\u00e9cisant son bras de traitement
  frq1 <- data.frame(xtabs(~ COD + ARM + visnum, data = frq1)) #frequence de chaque type d'EI dans chacun des bras de traitement
  frq1 <- frq1 %>% pivot_wider(names_from = ARM, values_from = Freq)


  #######################################################
  ### Subgroup (<= grade x) pour chaque cycle
  #######################################################
  # table avec le compte d'EIs de grade >= x \u00e0 chaque cycle
  if(grd > max(baseEI$Grade)) return("Grade non pr\u00e9sent dans la base")
  if(grd == 1) return("Impossible de comparer les EIs de grade sup\u00e9rieur ou \u00e9gale \u00e0 1 \u00e0 tous les EIs, car il repr\u00e9sentent toute la base de donn\u00e9e")
  df_AE5 <- subset(df_AE4, df_AE4$Grade >=grd)
  #on garde une occurence unique pour chaque id_pat et COD
  df_AE5 <- df_AE5 %>% select(-Grade) %>% distinct(id_pat,ARM,COD, visnum)
  # fr\u00e9quence par PT et par cycle pour les EIs de grade <=x
  frq2 <- data.frame(xtabs(~ COD + ARM + visnum, data=df_AE5))
  frq2 <- frq2 %>% pivot_wider(names_from = ARM, values_from = Freq)

  #### on merge les deux tables cr\u00e9\u00e9es frq1 et frq2 pour pouvoir calculer le RD
  frq <- left_join(frq1, frq2, by=c("COD","visnum")) %>% mutate(across(where(is.numeric),~replace_na(.x,0)))
  names(frq) <- c("COD","visnum","G1tot","G2tot","G1grd","G2grd")

  #### calcul du RD
  ## on cible tout  d'abord l'EI choisi
  if (!(choixEI %in% unique(frq$COD))) return("EI choisit non pr\u00e9sent dans la base")
  frq <- frq[frq$COD == choixEI,]

  #Creation d'une table pour accueilir toutes les donn\u00e9es n\u00e9cessaires
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
    labcap <- paste("Ce graph repr\u00e9sente le RD (avec IC 95%) d'avoir, pour l'EI choisit,
    un grade >= x (ici", grd ,") par rapport \u00e0 tout grade entre les deux groupes de traitement
    pour une p\u00e9riode donn\u00e9es (cycle, semaine...).")
    p <- p + labs(caption = labcap)
  }


  suppressWarnings(print(p))
}
