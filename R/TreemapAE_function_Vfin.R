#' TreemapAE
#'
#' @description Pour ce graphique nous avons un rectangle par groupe de traitement divisé en plusieurs sous rectangles et colorés selon une échelle de couleur, le but est de comparer les proportions de chacun des sous-rectangles entre les groupes et leur couleurS.
#' Ici on divise chaque rectangle en sous rectangle selon les SOC puis selon chaque type d’EIs et on compare la taille par exemple du groupe de rectangle pour une SOC donnée entre les bras de traitement ou la taille d’un rectangle pour un EI entre les groupes de traitement ou alors la couleur globale des rectangles…
#' Les rectangles sont automatiquement ordonnés du coin inférieur gauche au coin supérieur droit dans l’ordre croissant selon leur taille.
#' Note : ce sont des comparaisons globales et non des comparaison précises avec des axes, il faudra creuser les hypothèses sorties de ce graphiques avec d’autres graphiques de ce documents ou en explirant les données
#' Ce graphique permet de comparer deux groupes de traitements car chacun à un rectangle qui lui est propre. Dans le cas où l’étude comporte plus de deux groupes de traitements, il est possible de faire un premier graphique avec les deux premiers bras puis de réitérer l’opération avec les autres bras de traitements.
#'
#' @param baseEI     la base des EIs avec
#' \itemize{
#'    \item l’identifiant du patient (idvar) : au format character obligatoire et sans manquants
#'    \item le label des PT ou LLT (Termsvar) pour chaque EI : au format character obligatoire et sans manquants
#'    \item la SOC correspondante (SOC_COD) : label (et non code) au format character obligatoire et sans manquants
#'    \item le grade de l’EI (gradevar) : au format numeric obligatoire en partant de 1 (et allant jusqu’au grade max par exemple 5) et sans manquants
#' }
#' @param baseTr la base des traitements avec
#' \itemize{
#'    \item l’identifiant du patient (idvar) : au format character obligatoire et sans manquants
#'    \item le bras de traitement pour chaque individu (ARMvar) : au format character obligatoire et sans manquants
#'    \item [non obligatoire] une variable pour indiquer si le traitement à été pris (sert à détecter les patients “à risque”) TTTYN on recherchera les occurences en “Yes” donc doit être codée en “Yes”/“No” en character
#' }
#' @param idvar column name
#' @param Termsvar column name
#' @param SOCvar column name
#' @param gradevar column name
#' @param TTTYN column name
#' @param ARMvar column name
#' @param choixSOC : permet de choisir de zoomer sur une SOC en particulier (ex : “BLOOD AND LYMPTAHIC SYSTEM DISORDERS”) –> par défaut NULL affiche toutes les SOC
#' @param listcol : vecteur de couleurs à indiquer pour chaque grade donc selon le nombre de modalités de la variable gradevar, doit impérativement être au moins de la même longeur que le nombre de grades présents dans l’étude (ok si 5 couleurs alors que grades 1 à 4) mais pas moins et contenir les valeurs des couleurs dans l’ordre des grades (du plus petit au plus grand) –> ex : c(‘#52717F’, ‘#00A991’, ‘#ffb703’, ‘#CF4B00’, ‘#9a031e’) –> n’a pas de valeur pas défaut
#' @param ARMe : indiquer le bras expérimental, il sera placé en premier cad à gauche dans le graphique pour comparer au bras contrôle à droite. Ecrire le label du bras expérimental comme il est dans la base de données (ex: ARMe=“armB”) –> par défaut si cette option n’est pas remplie (donc ARMe=NULL) alors les groupes seront placés dans l’ordre alphabétique des labels“
#' @param caption : TRUE/FALSE afficher une note de base de graphique avec des informations supplémentaires sur le format et le continu du graphique –> par défaut TRUE
#'
#' @return A plot
#' @export
#'
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
