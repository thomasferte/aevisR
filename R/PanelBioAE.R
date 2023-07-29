#' PanelBioAE
#'
#' @description Ce graphique représente de façon classique les résultats de biologies d’un patient au cours du temps (plusieurs unités possibles), il se combine bien avec le Patient Span Chart qui représente les EIs au cours du temps pour mettre en regards les biologies relevées et les évènements indésirables.
#' Pour ce type de graphique il peut y avoir n’importe quel nombre de bras de traitement car on ne représente qu’un patient à la fois pour lequel on indique le bras dans lequel il est.
#'
#' @param baseBio la base des biologies avec les résultats des différentes biologies pour chaque patient
#' \itemize{
#'    \item l’identifiant du patient (idvar) : au format character obligatoire et sans manquants
#'    \item la date du relevé biodate_var : au format Date obligatoire et sans manquants
#'    \item le label de la biologie réalisée biolib_var : au format character obligatoire et sans manquants peuvent être tous identique si une seule biologie dans l’étude
#'    \item l’unité pour chaque type de biologie unitbi_var : au format character obligatoire et sans manquants
#'    \item le résultats de biologie biores_var : en numéric obligatoire
#'    \item [non obligatoire] le label de la visite pendant laquelle a été faite la bio labvisite_var : au format character sans manquants
#' }
#' @param idvar column name
#' @param biodate_var column name
#' @param unitbio_var column name
#' @param biolib_var column name
#' @param labvisite_var column name
#' @param biores_var column name
#' @param USU : choix du patient pour lequel on souhaite voir les résultats de biologie –> comme présent dans la base ex : “1002”
#' @param list_Bio : liste avec les labels de biologies à afficher - ex : c(“ALAT”,“ASAT”) –> par défaut NULL ,pas de bio indiqué alors on les affiche tous
#' @param suivi : TRUE/FALSE afficher les résultats de bio mesurée après la fin du traitement/début du suivi –> par défaut TRUE
#'
#' @return A plot
#' @export
#'
PanelBioAE <- function(baseBio, idvar, biodate_var, unitbio_var,  biolib_var,
                       labvisite_var = NULL, biores_var,
                       USU, list_Bio=NULL, suivi=TRUE){
  #remplacement des noms de variables
  baseBio <- baseBio %>% rename("id_pat" = idvar,
                              "biodate"= biodate_var,
                              "unitbio" = unitbio_var,
                              "lib" = biolib_var,
                              "biores" = biores_var)
  if(!is.null(labvisite_var)) baseBio <- baseBio %>% rename("labvisite"=labvisite_var)

  Biopat <- subset(baseBio,baseBio$id_pat==USU)
  Biopat$biodate <- as.Date(Biopat$biodate, format = "%d/%m/%Y")

  if (suivi==FALSE & !is.null(labvisite_var)){
    lim <- Biopat$biodate[Biopat$labvisite=="End of treatment"]
    Biopat <- subset(Biopat, biodate<=lim)
  }
  if (!is.null(list_Bio)) {
    if (FALSE %in% (list_Bio %in% unique(Biopat$lib))) return("Au moins une des biologie demand\u00e9e n'est pas disponible dans la base")
    Biopat <- Biopat[Biopat$lib %in% list_Bio,]
  }
  lab_biolib <- c(unique(paste(Biopat$lib[Biopat$unitbio!=""],"\n (",Biopat$unitbio[Biopat$unitbio!=""],")")))
  names(lab_biolib) <- c(unique(Biopat$lib))

  if(!is.null(labvisite_var)){
     ggplot(Biopat %>% select(biodate,biores,lib,labvisite),
         aes(x=biodate,y=biores, label=labvisite)) +
    geom_line(linewidth=2, color="gray40") +
    geom_point(size=4, color="gray20") +
    geom_label_repel(direction = "y", nudge_y = 5, fill = alpha(c("white"),0.5),label.padding=.1) +
    labs(x="Date",y="") +
    facet_grid(lib~., scales="free", space="fixed", switch="y",
               labeller=labeller(lib = lab_biolib)) +
    scale_y_continuous(position = "right") +
    scale_x_date(date_labels = "%b%y") +
    ggtitle(paste("Patient:",USU)) +
    theme(panel.background = element_blank(),
          panel.grid = element_line(color = "gray80"),
          axis.ticks.x = element_line(linewidth = 1, colour = "black"),
          axis.line = element_line(color = "black", linetype = 1),
          strip.placement = "left",
          axis.title = element_text(size=12),
          axis.text = element_text(size=10),
          strip.text.y.left = element_text(angle=0, hjust = 1, size=12))
  } else {
    ggplot(Biopat %>% select(biodate,biores,lib), aes(x=biodate,y=biores)) +
      geom_line(linewidth=2, color="gray40") +
      geom_point(size=4, color="gray20") +
      labs(x="Date",y="") +
      facet_grid(lib~., scales="free", space="fixed", switch="y",
                 labeller=labeller(lib = lab_biolib)) +
      scale_y_continuous(position = "right") +
      scale_x_date(date_labels = "%b%y") +
      ggtitle(paste("Patient:",USU)) +
      theme(panel.background = element_blank(),
            panel.grid = element_line(color = "gray80"),
            axis.ticks.x = element_line(linewidth = 1, colour = "black"),
            axis.line = element_line(color = "black", linetype = 1),
            strip.placement = "left",
            axis.title = element_text(size=12),
            axis.text = element_text(size=10),
            strip.text.y.left = element_text(angle=0, hjust = 1, size=12))
  }
}
