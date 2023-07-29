#' TimeToSpanChart
#'
#' @description Ce graphique permet d’observer et surtout de comparer la temporalité de plusieurs aspects d’un groupe d’EI (une vingtaine) par rapport à la durée du traitement dans chaque bras. On y observe le temps médian d’appartition du premier EI sa durée et le temps médian d’appartition d’un grade supérieur à 3 et d’un grade supérieur à 4.
#'
#' @param baseEI la base des EIs avec
#' \itemize{
#'    \item l’identifiant du patient (idvar) : au format character obligatoire et sans manquants
#'    \item le label des PT ou LLT (Termsvar) pour chaque EI : au format character obligatoire et sans manquants
#'    \item le grade de l’EI (gradevar) : au format numeric obligatoire en partant de 1 (et allant jusqu’au grade max par exemple 5) et sans manquants
#'    \item la date de début de l’EI (EIdatestart_var) : au format Date (important) obligatoire
#'    \item la date de fin de l’EI (EIdateend_var) : au format Date (important) obligatoire
#'}
#' @param baseTr     la base des traitements avec
#' \itemize{
#'    \item l’identifiant du patient (idvar) : au format character obligatoire et sans manquants
#'    \item le bras de traitement pour chaque individu (ARMvar) : au format character obligatoire et sans manquants
#'    \item [non obligatoire] une variable pour indiquer si le traitement à été pris (sert à détecter les patients “à risque”) TTTYN on recherchera les occurences en “Yes” donc doit être codée en “Yes”/“No” en character
#'}
#' @param baseDates     la base qui contient les dates principales liées à chaque patient, celles qui nous sont utiles pour ce graphique sont :
#' \itemize{
#'    \item l’identifiant du patient (idvar) : au format character obligatoire et sans manquants
#'    \item la date de début de traitement tttdebdate_var : au format Date obligatoire et sans manquants
#'    \item la date de fin de traitement/fin de suivi tttfindate_var qui correspond à la fin de la période traitement par exemple, des EIs peuvent se situer après cette date. Si le format “cycle” est choisit on arrête le dernier cycle à cette date, si un autre format est choisit, la date de fin du graphique sera en fonction de l’option suivi. Si suivi=TRUE alors on prend tous les EIs enregistrés, sinon si suivi=False alors on termine l’axe du temps à la date de fin tttfindate_var. Au format Date et sans manqants (si on choisit l’option suivi=FALSE ou unit="cycle")
#'}
#' @param idvar column name
#' @param Termsvar column name
#' @param EIdatestart_var column name
#' @param EIdateend_var column name
#' @param gradevar column name
#' @param TTTYN column name
#' @param ARMvar column name
#' @param tttdebdate_var column name
#' @param tttfindate_var column name
#' @param listEI : liste des EIs choisis à afficher sur le graphique –> ex c(“ANAEMIA”,“ASTHENIA”,“NAUSEA”) –> par défaut NULL prendra les 20 premiers en nombre total d’évènements
#' @param unit : unité au choix entre “days”/“weeks”/“month” –> par défaut “weeks”
#' @param caption : TRUE/FALSE afficher une note de base de graphique avec des informations supplémentaires sur le format et le continu du graphique –> par défaut TRUE
#' @param listcol : liste de couleurs pour chacun des groupes de traitements dans un vecteur soit avec les noms implémentés dans R (comme “red”, “blue”…) soit avec des codes (commençant par # par exemple “#52717F”) –> par défaut c(“deepskyblue2”,“red”)
#'
#' @return A plot
#' @export
#'
TimeToSpanChart <- function(baseEI, baseTr, baseDates,
                            idvar, Termsvar, EIdatestart_var, EIdateend_var, gradevar, TTTYN = NULL, ARMvar, tttdebdate_var, tttfindate_var,
                            listEI=NULL, unit = "weeks", caption=TRUE, listcol=c("deepskyblue2","red")){

  #remplacement des noms de variables
  # names(baseEI)[names(baseEI) == id_pat] <- "id_pat"
  baseEI <- baseEI %>% rename("id_pat" = idvar,
                              "COD" = Termsvar,
                              "Grade" = gradevar,
                              "aedatestart"= EIdatestart_var,
                              "aedateend" = EIdateend_var)
  baseTr <- baseTr %>% rename("id_pat" = idvar,
                              "ARM" = ARMvar)
  if (!is.null(TTTYN)) baseTr <- baseTr %>% rename("TTTYN" = TTTYN)
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

  if (is.null(TTTYN)){
    df_Tr2 <- baseTr
  } else {
   df_Tr2  <- baseTr[baseTr$TTTYN=="Yes",]
  }
  USU_distinct  <- df_Tr2  %>% select(id_pat, ARM) %>% distinct(id_pat, ARM)
  frq2 <- data.frame(xtabs(~ ARM, data=USU_distinct))

  #on garde seulement les variable utiles
  df_AE2 <- baseEI %>% select(id_pat, COD, ARM, aedatestart, aedateend, Grade)

  #on merge pour ajouter la date de pr\u00e9-inclusion,et la date de fin de Treatment
  df_AE2 <- merge(df_AE2, baseDates %>% select(id_pat,tttdebdate,tttfindate), by="id_pat")

  #on ne garde que le premier \u00e9pisode de chaque EI par patients
  df_AE3 <- df_AE2 %>% group_by(id_pat, COD) %>% filter(aedatestart==min(aedatestart))

  if (unit=="month"){
    #on calcule une variable pour la dur\u00e9e de l'EI
    df_AE3$duration <- interval(df_AE3$aedatestart, df_AE3$aedateend) %/% months(1) +1
    #on calcule une variable pour le d\u00e9lai d'appartition
    df_AE3$onset <- interval(df_AE3$tttdebdate, df_AE3$aedatestart) %/% months(1) +1
  } else if (unit %in% c("weeks","days")){
    #on calcule une variable pour la dur\u00e9e de l'EI
    df_AE3$duration <- as.numeric(difftime(df_AE3$aedateend,df_AE3$aedatestart,units = unit))
    #on calcule une variable pour le d\u00e9lai d'appartition
    df_AE3$onset <- as.numeric(difftime(df_AE3$aedatestart,df_AE3$tttdebdate,units = unit))
  } else return("Valeur non valide pour l'option unit")

  # on choisit les EIs les plus fr\u00e9quents (si null pour listEI)
  tb_freq <- baseEI[baseEI$COD!="",] %>% group_by(COD) %>% summarise(count=n()) %>% arrange(desc(count))

  if (is.null(listEI)){
    l_COD <- tb_freq$COD[c(1:20)]
  } else if (!is.null(listEI)){
    if (FALSE %in% (listEI %in% tb_freq$COD)) return("Au moins un des EI demand\u00e9s n'est pas pr\u00e9sent dans la base.")
    l_COD <- listEI #sinon on prend la liste donn\u00e9e dans listEI
  }


  ### mediane time to first onset sur chaque EI de la liste d\u00e9finie avec les EIs les plus fr\u00e9quents
  df_plot <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("COD", "ARM", "med_onset", "med_duration"))
  for (p in l_COD){
    df <- subset(df_AE3, df_AE3$COD==p)

    df_1 <- subset(df, df$ARM == "arm1")
    dfx_1 <- data.frame(COD = p,
                        ARM = "arm1",
                        med_onset = median(df_1$onset),
                        med_duration = median(df_1$duration[!is.na(df_1$duration)]),
                        row.names = NULL)
    df_2 <- subset(df, df$ARM == "arm2")
    dfx_2 <- data.frame(COD = p,
                        ARM = "arm2",
                        med_onset = median(df_2$onset),
                        med_duration = median(df_2$duration[!is.na(df_2$duration)]),
                        row.names = NULL)

    dfx <- rbind(dfx_1, dfx_2)
    # A chaque PT concat\u00e9nation des tables
    df_plot <- rbind(df_plot,dfx)
  }

  ### Selectionner les lignes avec un grade >=3
  df_AE4 <- subset(df_AE2,df_AE2$Grade >= 3)
  #on calcule une variable pour le d\u00e9lai d'apparition du grade le plus elev\u00e9
  if (unit=="month"){
    df_AE4$G345 <- interval(df_AE4$tttdebdate,df_AE4$aedatestart) %/% months(1) +1
  } else if (unit %in% c("weeks","days")){
    df_AE4$G345 <- as.numeric(difftime(df_AE4$aedatestart,df_AE4$tttdebdate,units = unit))
  } else return("Valeur non valide pour l'option unit")

  df_AE4 <- df_AE4 %>% group_by(COD,ARM) %>% dplyr::summarise(med_G345 = median(G345))
  df_plot <- left_join(df_plot,df_AE4 %>% select(COD,ARM,med_G345), by=c("COD","ARM"))

  ### Selectionner les lignes avec un grade >=4
  df_AE5 <- subset(df_AE2,df_AE2$Grade >= 4)
  #on calcule une variable pour le d\u00e9lai d'apparition du grade le plus elev\u00e9
  if (unit=="month"){
    df_AE5$G45 <- interval(df_AE5$tttdebdate,df_AE5$aedatestart) %/% months(1) +1
  } else if (unit %in% c("weeks","days")){
    df_AE5$G45 <- as.numeric(difftime(df_AE5$aedatestart,df_AE5$tttdebdate,units = unit))
  } else return("Valeur non valide pour l'option unit")

  df_AE5 <- df_AE5 %>% group_by(COD,ARM) %>% dplyr::summarise(med_G45 = median(G45))
  df_plot <- left_join(df_plot,df_AE5 %>% select(COD,ARM,med_G45), by=c("COD","ARM"))

  ###calcul temps m\u00e9dian de Treatment dans chacun des groupe et ajout à la table df_plot
  #groupe1
  df_1 <- subset(df_AE2, df_AE2$ARM=="arm1")
  if (unit=="month"){
    df_1 <- df_1 %>% group_by(id_pat) %>%
      reframe(durttt=interval(tttdebdate,tttfindate) %/% months(1) +1) %>%
      distinct(id_pat,durttt)
  } else if (unit %in% c("weeks","days")){
    df_1 <- df_1 %>% group_by(id_pat) %>%
      reframe(durttt=difftime(tttfindate,tttdebdate, units = unit)) %>%
      distinct(id_pat,durttt)
  } else return("Valeur non valide pour l'option unit")
  medttt_1 <- median(as.numeric(df_1$durttt))

  #groupe2
  df_2 <- subset(df_AE2, df_AE2$ARM=="arm2")
  if (unit=="month"){
    df_2 <- df_2 %>% group_by(id_pat) %>%
      reframe(durttt=interval(tttdebdate,tttfindate) %/% months(1) +1) %>%
      distinct(id_pat,durttt)
  } else if (unit %in% c("weeks","days")){
    df_2 <- df_2 %>% group_by(id_pat) %>%
      reframe(durttt=difftime(tttfindate,tttdebdate, units = unit)) %>%
      distinct(id_pat,durttt)
  } else return("Valeur non valide pour l'option unit")
  medttt_2 <- median(as.numeric(df_2$durttt))

  newline_1 <- c("Treatment","arm1",0,medttt_1,NA,NA)
  newline_2 <- c("Treatment","arm2",0,medttt_2,NA,NA)
  df_plot <- rbind(df_plot,newline_1)
  df_plot <- rbind(df_plot,newline_2)
  df_plot$med_onset <- as.numeric(df_plot$med_onset)
  df_plot$med_duration <- as.numeric(df_plot$med_duration)
  df_plot$med_G345 <- as.numeric(df_plot$med_G345)
  df_plot$med_G45 <- as.numeric(df_plot$med_G45)


  ###calcul onset + duration pour avoir la position de la fin de la barre pour le plot
  df_plot$end <- df_plot$med_onset + df_plot$med_duration

  ##### autre version de la table pour le graph
  df_long <- df_plot %>% gather(type, time, -c(COD, ARM, med_duration, med_G345, med_G45))
  l_COD <- c("Treatment",tb_freq$COD[c(1:20)])

  ####
  df_plot$shape345 = "Grade>=3"
  df_plot$shape45 = "Grade>=4"
  df_plot$shapeF = "First onset"

  ## text for the caption
  if (caption==TRUE){
    labcap<-"
  The pattern of median time to onset and duration of the first episode for the 20 first AEs for each arm.
  The median time to onset of grade >=3 AE (all episode taking to account) is displayed by a cross, and a cross with square for grade >=4 AE.
  "
  } else {labcap=""}

  ##
  df_plot$ARM <- ifelse(df_plot$ARM == "arm1", list_ARM[1], list_ARM[2])
  df_long$ARM <- ifelse(df_long$ARM == "arm1", list_ARM[1], list_ARM[2])

  ### plot
  lim <- ceiling(max(df_plot$end[!is.na(df_plot$end)]))
  col=alpha(listcol,0.4)
  n = lim/100

  p <- ggplot(df_long, aes(x=ARM, y=time, colour=ARM)) +
    geom_line(linewidth=4) +
    geom_line(data=df_long %>% filter(COD=="Treatment"), linewidth=1.5, color=alpha("gray40",0.6)) +
    geom_point(data=df_plot,aes(y=med_onset,x=ARM, shape=shapeF),
               color= ifelse(df_plot$ARM==list_ARM[1], listcol[1], listcol[2]),
               size=3)+
    geom_point(data=df_plot,aes(y=med_G345,x=ARM, shape=shape345),color= "black", size=2)+
    geom_point(data=df_plot,aes(y=med_G45,x=ARM, shape=shape45),color= "black", size=2)+
    geom_text(data=df_plot %>% filter(COD!="Treatment"),aes(x=ARM, y=med_onset, label=round(med_onset,1)),
              col="black", nudge_y = -n, nudge_x=0.05, size=3) +
    geom_text(data=df_plot %>% filter(COD!="Treatment"),aes(x=ARM, y=end, label=round(med_duration,1)),
              col="black", nudge_y = n, nudge_x=0.05, size=3) +
    geom_text(data=df_plot %>% filter(COD=="Treatment"),aes(x=ARM, y=end, label=round(med_duration,1)),
              col="black", nudge_y = n, nudge_x=0.05, size=3) +
    scale_colour_manual(name="ARM", breaks = c(list_ARM[1],list_ARM[2]),
                        labels = c(list_ARM[1],list_ARM[2]),values = col) +
    scale_shape_manual(breaks = c("Grade>=4","Grade>=3","First onset"),
                       values = c("Grade>=4"= 7,"Grade>=3" = 4, "First onset" = 19)) +
    {if(unit=="weeks") scale_y_continuous(breaks = seq(0,lim,by=2))} +
    {if(unit=="days") scale_y_continuous(breaks = seq(0,lim,by=10))} +
    {if(unit=="month") scale_y_continuous(breaks = seq(0,lim,by=1))} +

    labs(y=paste0("Median time (",unit,")"),
         caption = labcap) +
    coord_flip() +
    facet_grid(factor(COD,levels = as.character(l_COD)) ~ ., scales = "free", space = "free", switch = "y") +
    theme(panel.background = element_blank(),
          panel.border = element_rect(color="gray80", fill=F),
          axis.title.y=element_blank(),
          axis.text.y = element_blank(),
          axis.line=element_line(color="black", linewidth = 1),
          panel.grid.minor=element_blank(),
          panel.grid.major.y = element_blank(),
          axis.ticks=element_blank(),
          legend.position="top",
          legend.justification =  "right",
          legend.key = element_blank(),
          legend.title = element_blank(),
          panel.grid.major.x = element_line(linewidth = 0.5, colour="grey80", linetype = 2),
          strip.text.y.left = element_text(angle = 0, hjust = 1, size = 9),
          strip.background = element_blank(),
          strip.placement = "outside")
  plot(p)
}
