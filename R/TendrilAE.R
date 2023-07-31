#' TendrilAE
#'
#' @description Permet de montrer à la fois l’importance relative des risques et le pattern temporel des EIs durant l’étude. Visualisation qui résume le profil global des EIs d’une étude, montre les EIs d’importance majeure et illustre l’évolution temporelle des EIs reportés. Montre TOUS les EIs, même les récurrents (occurrence multiple pour chaque patient), et pas seulement la première occurrence comme c’est le cas pour d’autres graphiques.
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
#' @param coltype : “p.adj”,“fish”,“p”,“rdiff”,“OR”,“RR”,“FDR.tot”,“TermsCount” type de colorisation (options déjà présentes pour les fonction liées au package du Tendril) –> par défaut NULL qui donne une couleur à chaque branche color selon chaque Term
#' @param caption : TRUE/FALSE afficher une note de base de graphique avec des informations supplémentaires sur le format et le continu du graphique –> par défaut TRUE
#'
#' @return A plot
#' @export
#'
#' @examples
#' library(dplyr)
#' baseEI <- data.frame(idvar = paste0("Patients", round(runif(n = 100, min = 0, max = 100))),
#'                      Termsvar = round(runif(n = 100, min = 0, max = 2))) %>%
#'   dplyr::mutate(SOCvar = round(Termsvar/10)) %>%
#'   dplyr::mutate(across(everything(), .fns = as.character)) %>%
#'   mutate(EIdatestart_var = as.Date(runif(n = nrow(.), 1, 200), origin = "2021-01-01"),
#'          EIdateend_var = as.Date(runif(n = nrow(.), 201, 600), origin = "2021-01-01"),
#'          gradevar = round(runif(n = nrow(.), 1, 5)))
#'
#' baseTr  <- baseEI %>%
#'   dplyr::select(idvar) %>%
#'   dplyr::distinct() %>%
#'   dplyr::mutate(ARMvar = sample(x = c("Placebo", "Treatment"),
#'                                 size = nrow(.),
#'                                 replace = TRUE),
#'                 TTTYN = sample(x = c("Yes", "No"),
#'                                size = nrow(.),
#'                                replace = TRUE,
#'                                prob = c(0.9, 0.1)))
#'
#' baseDates <- baseEI %>%
#'   dplyr::select(idvar) %>%
#'   dplyr::distinct() %>%
#'   mutate(tttdebdate_var = as.Date(runif(n = nrow(.), -100, 0), origin = "2021-01-01"),
#'          tttfindate_var = as.Date(runif(n = nrow(.), 201, 600), origin = "2021-01-01"))
#'
#' TendrilAE(baseEI = baseEI, baseTr = baseTr, baseDates = baseDates,
#'           idvar = "idvar", Termsvar = "Termsvar", EIdatestart_var = "EIdatestart_var",
#'           ARMvar = "ARMvar", tttdebdate_var = "tttdebdate_var")
#'
TendrilAE <- function(baseEI, baseTr, baseDates,
                      idvar, Termsvar, EIdatestart_var, ARMvar, tttdebdate_var,
                      coltype = NULL, caption=TRUE){
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
  #Ajouter une colonne ARM dans la table data en faisant correspondre les id_pat selon la liste o\u00f9 ils sont pr\u00e9sents
  baseEI$ARM <- ifelse(baseEI$id_pat %in% list_pat1, "arm1", "arm2")

  #jointure pour r\u00e9cup\u00e9rer la date de d\u00e9but de traitement de chaque individu
  baseEI2 <- left_join(baseEI %>% select(id_pat, ARM, COD, aedatestart),
                      baseDates %>% select(id_pat,tttdebdate),
                      by="id_pat", multiple="all")

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

  ##on remet mes valeurs d\u0027origine pour les bras de traitement
  baseEI2$ARM <- ifelse(baseEI2$ARM=="arm1",as.character(list_ARM[1]),as.character(list_ARM[2]))

  # liste des patients avec leur bras de traitement
  SubjList <- baseEI %>% arrange(id_pat,ARM)
  SubjList$ARM <- ifelse(SubjList$ARM=="arm1",as.character(list_ARM[1]),as.character(list_ARM[2]))

  data.Tendril <- Tendril::Tendril(mydata = baseEI2,
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

  # vecteur pour les annotations
  # dans data.Tendril$data on prend les coordonn\u00e9es x et y du dernier point de chaque Terms
  anno <- data.Tendril$data %>% group_by(Terms) %>% filter(StartDay==max(StartDay)) %>% arrange(Terms)
  anno <- anno %>% select(Terms,x,y)
  labcap <- "Pas de diff\u00e9rence visuelle entre les bras de traitement (couleur ou autre), c\u0027est l\u0027interpr\u00e9tation du graphique qui permet de savoir si un EI est plus pr\u00e9sent dans un bras que dans l\u0027autre.
  La distance entre les points est proportionnelle \u00e0 l\u0027intervalle de temps entre les \u00e9v\u00e8nements.
  L\u0027angle est dict\u00e9 par un param\u00e8tre de la fonction Tendril(), fix\u00e9 \u00e0 rotations = 4. La branche se dirigera vers la droite si l\u0027\u00e9v\u00e8nement \u00e0 lieu dans le premier groupe et \u00e0 gauche s\u0027il \u00e0 lieu dans l\u0027autre groupe.
  L\u0027\u00e9volution temporelle est repr\u00e9sent\u00e9e le long de chaque branche, c\u0027est donc la forme qui est importante et qui porte l\u0027information.
  Les branches peuvent \u00eatre color\u00e9s selon plusieurs variable, par exemple selon la p-value du chi-square de Pearson
  ou selon le nombre d\u0027\u00e9v\u00e8nement pour chaque type d\u0027EI (branche) par un gradient de couleurs."

  if(is.null(coltype)){
    plot(data.Tendril) +
      {if (caption==TRUE) labs(caption = labcap)} +
      geom_label(data=anno, aes(x=x,y=y, label=Terms), color="black",
                 min.segment.length = 0.1, force = 5,
                 max.overlaps = 10,
                 direction="y")
  } else if (coltype %in% c("p.adj","fish","p","rdiff","OR","RR","FDR.tot","TermsCount")){
    plot(data.Tendril, coloring=coltype) +
      {if (caption==TRUE) labs(caption = labcap)} +
      geom_label_repel(data=anno, aes(x=x,y=y, label=Terms), color="black",
                       min.segment.length = 0.1, force = 8,
                       max.overlaps = 30,
                       direction="y")
  } else return("Valeur non valide pour l\u0027option coltype")
}
