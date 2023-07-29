#' TendrilAETimeSeries
#'
#' @description En prenant donc les mêmes informations que pour le Tendril plot, c’est a dire les EIs, le bras de traitement et le jours d’apparition de chaque EI on construit un Time Series plot avec en ordonné la différence d’occurence entre chaque groupe, et en abcisse le nombre de jours écoulés depuis le début du traitement.
#'
#' @param baseEI la base des EIs avec
#' \itemize{
#'    \item l’identifiant du patient (idvar) : au format character obligatoire et sans manquants
#'    \item le label des PT ou LLT (Termsvar) pour chaque EI : au format character obligatoire et sans manquants
#'    \item la SOC correspondante (SOC_COD) : label (et non code) au format character obligatoire et sans manquants
#'    \item la date de début de l’EI (EIdatestart_var) : au format Date (important) obligatoire
#' }
#' @param baseTr la base des traitements avec*
#' \itemize{
#'    \item l’identifiant du patient (idvar) : au format character obligatoire et sans manquants
#'    \item le bras de traitement pour chaque individu (ARMvar) : au format character obligatoire et sans manquants
#'    \item [non obligatoire] une variable pour indiquer si le traitement à été pris (sert à déte
#' }
#' @param baseDates la base qui contient les dates principales liées à chaque patient, celles qui nous sont utiles pour ce graphique sont :
#' \itemize{
#'    \item l’identifiant du patient (idvar) : au format character obligatoire et sans manquants
#'    \item la date de début de traitement tttdebdate_var : au format Date obligatoire et sans manquants
#'    \item la date de fin de traitement/fin de suivi tttfindate_var qui correspond à la fin de la période traitement par exemple, des EIs peuvent se situer après cette date. Si le format “cycle” est choisit on arrête le dernier cycle à cette date, si un autre format est choisit, la date de fin du graphique sera en fonction de l’option suivi. Si suivi=TRUE alors on prend tous les EIs enregistrés, sinon si suivi=False alors on termine l’axe du temps à la date de fin tttfindate_var. Au format Date et sans manqants (si on choisit l’option suivi=FALSE ou unit="cycle")
#' }
#' @param idvar column name
#' @param Termsvar column name
#' @param EIdatestart_var column name
#' @param ARMvar column name
#' @param tttdebdate_var column name
#' @param CODlist : liste de PT à mettre en valeur dans le graph (les autres seront tracés en plus clair en arrière plan si format=“All” ou non présentes si format=“list”), ex c(“ANAEMIA”,“NAUSEA”,“ASTHENIA”) –> par défaut NULL tous les EIs de la base de données sont affichés
#' @param listcol : vecteur/liste de couleurs à indiquer pour colorer la selection de Terms (dans CODlist), ex (lié à l’exemple pour CODlist) : c(“purple”,“orange”,“green”) –> par défaut NULL, si CODlist NON null alors tous les EIs selectionnés dans COD seront en noir (par rapport aux autres non selectionnés en gris)
#'
#' @return A plot
#' @export
#'
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
  #Liste des id patients dans le bras num\u00e9ro 1
  list_pat1 <- unique(baseTr$id_pat[baseTr$ARM == list_ARM[1]])
  #Liste des id patients dans le bras num\u00e9ro 2
  list_pat2 <- unique(baseTr$id_pat[baseTr$ARM == list_ARM[2]])
  #Ajouter une colonne ARM dans la table data en faisant correspondre les id_pat selon la liste où ils sont pr\u00e9sents
  baseEI$ARM <- ifelse(baseEI$id_pat %in% list_pat1, "arm1", "arm2")


  #jointure pour r\u00e9cup\u00e9rer la date de d\u00e9but de traitement de chaque individu
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

  #table de donn\u00e9es pour le TimeSeries
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
            panel.grid.major = element_line(color = "gray60", linewidth = 0.5, linetype = 1), #grille de lecture de couleur grise et d'\u00e9paisseur 0.5
            axis.line = element_line(color="gray30"), #couleur de la ligne de chacun des axes
            legend.position = "none" #ne pas afficher la l\u00e9gende
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
            panel.grid.major = element_line(color = "gray60", linewidth = 0.5, linetype = 1), #grille de lecture de couleur grise et d'\u00e9paisseur 0.5
            axis.line = element_line(color="gray30"), #couleur de la ligne de chacun des axes
            legend.position = "none" #ne pas afficher la l\u00e9gende
      )
  }

  suppressWarnings(print(p2))
}
