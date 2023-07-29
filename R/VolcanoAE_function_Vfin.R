#' VolcanoAE
#'
#' @description chaque bulle = un EI, occurrence de l’EI pour la taille des
#' bulles (soit nombre d’EIs, soit nombre de patients ayant eu cet EI). Couleur
#' des bulles en fonction du groupe de traitement. P-value de Fisher en
#' ordonnées et pour la saturation des bulles. En abscisse une mesure de
#' différence entre les deux groupes soit l’absolute risk difference, le risque
#' ratio, odds ratio, ou incidence rate ratio…
#'
#' @param baseEI la base des EIs avec :
#' \itemize{
#'    \item idvar : l’identifiant du patient : au format character obligatoire
#'    et sans manquants
#'    \item Termsvar : le label des PT ou LLT pour chaque EI : au format
#'    character obligatoire et sans manquants
#' }
#' @param baseTr la base des traitements avec
#' \itemize{
#'    \item idvar : l’identifiant du patient : au format character obligatoire
#'    et sans manquants
#'    \item ARMvar : le bras de traitement pour chaque individu : au format
#'    character obligatoire et sans manquants
#'    \item TTTYN [optionnal] une variable pour indiquer si le traitement à été pris
#'    (sert à détecter les patients “à risque”) on recherchera les
#'    occurences en “Yes” donc doit être codée en “Yes”/“No” en character
#' }
#' @param idvar (character) column name of patient id column
#' @param Termsvar (character) column name of PT or LLT label for each AE
#' @param TTTYN (character, default = NULL) column name indicating if the
#' treatemnt was taken
#' @param ARMvar (character) column name for treatment group
#' @param caption (boolean, default = TRUE) TRUE/FALSE afficher une note de base
#' de graphique avec des informations supplémentaires sur le format et le
#' continu du graphique
#' @param listcol (vector of character, default = (“red”,“deepskyblue2”)) liste
#' de couleurs pour chacun des groupes de traitements dans un vecteur soit avec
#' les noms implémentés dans R (comme “red”, “blue”…) soit avec des codes
#' (commençant par # par exemple “#52717F”)
#' @param seuillab (numeric, default = 0.5) seuil vertical d’affichage des
#' label, permet de ne pas afficher tous les labels pour épurer le graphique
#' (entrer une valeur de p-value elle sera convertie en -log(10))
#'
#' @return A Volcano plot
#' @export
#'
VolcanoAE <- function(baseEI, baseTr,
                      idvar, Termsvar, TTTYN=NULL, ARMvar,
                      seuillab = 0.01, caption=TRUE, listcol = c("red", "deepskyblue2")){
  #remplacement des noms de variables
  # names(baseEI)[names(baseEI) == id_pat] <- "id_pat"
  baseEI <- baseEI %>% rename("id_pat" = idvar,
                              "COD" = Termsvar)
  baseTr <- baseTr %>% rename("id_pat" = idvar,
                              "ARM" = ARMvar)
  if (!is.null(TTTYN)) baseTr <- baseTr %>% rename("TTTYN" = TTTYN)

  #liste des groupes de traitement de la table baseTr
  list_ARM <- unique(baseTr$ARM)
  #Liste des id patients dans le bras numéro 1
  list_pat1 <- unique(baseTr$id_pat[baseTr$ARM == list_ARM[1]])
  #Liste des id patients dans le bras numéro 2
  list_pat2 <- unique(baseTr$id_pat[baseTr$ARM == list_ARM[2]])
  #Ajouter une colonne ARM dans la table data en faisant correspondre les id_pat selon la liste où ils sont présents
  baseEI$ARM <- ifelse(baseEI$id_pat %in% list_pat1, "arm1", "arm2")
  baseTr$ARM <- ifelse(baseTr$ARM==list_ARM[1], "arm1","arm2")

  ############################################################################
  ######################## Construction table volcano ########################
  ###################
  # Calcul des freq
  ###################
  data_distinct <- baseEI %>% select(id_pat,ARM,COD) %>% distinct(id_pat, ARM, COD)
  frq1 <- data.frame(xtabs(~ COD + ARM, data = data_distinct))

  ## Total subjects in each ARM
  if (is.null(TTTYN)) {
    df_Tr2 <- baseTr
  } else {
    df_Tr2  <- baseTr[baseTr$TTTYN=="Yes",]
  }
  USU_distinct  <- df_Tr2  %>% select(id_pat, ARM) %>% distinct(id_pat, ARM)
  frq2 <- data.frame(xtabs(~ ARM, data=USU_distinct)) # compte le nombre de patient dans chacun des bras

  #Merged
  frq3 <- merge(frq1, frq2, by="ARM")
  frq4 <- frq3 %>%
    mutate(no = Freq.y - Freq.x, # total - # with event
           yes = Freq.x) %>% # all with AE
    arrange(COD, ARM) %>%
    select(c(ARM, COD, yes, no))
  # frq4 : pour chaque type d'EI et par bras, nombre de patients ayant eu cet EI et nombre de patients ne l'ayant pas eu

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
                  label = ifelse(logpval > -log10(seuillab), COD, ""),
                  color=harmful)) +
    geom_point(alpha = 0.40) +
    scale_size(range = c(1,22)) +
    labs(title = "Volcano Plot") +
    scale_color_manual(breaks = c(paste("Increased risk in ", list_ARM[2]),
                                  paste("Increased risk in ", list_ARM[1])),
                       values = listcol) +
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
          legend.text = element_text(size = 10),
          axis.text = element_text(size=10),
          axis.title = element_text(size=12),
          axis.line= element_line(linewidth =0.5, colour = "black"))

  return(p)
}
