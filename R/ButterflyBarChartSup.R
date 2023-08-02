#' ButterflyBarChartSup
#'
#' @description Ce graphique est un graphique opposé par le centre donc en forme butterfly comme le graphique empilé précédent, mais celui-ci en est une autre version. Ici nous allons superposée une barre plus fine à la barre principale pour représenter le pourcentage de patients atteints par un EI de grade supérieur ou égale à 3 par rapport au pourcentage de patients atteints par cette EI (tout grade confondu).
#' La granularité de graphique sera au niveau des SOC, cad qu’une barre représentera le pourcentage de patient ayant eu au moins 1 EI classé dans chaque SOC.
#' On ajoute à droite de ce graphique un double forest plot pour permettre de quantifier statistiquement les différences de pourcentages entre les groupes présents dans le graphique de gauche.
#' Note : ici à l’inverse, mais de la même façon que pour le butterfly empilé nous pourrions créer une version pour multiplier ce graphique par SOC et ainsi avoir autant de graphique que de SOC dans l’étude avec cette fois ci cahque barre représentant un EI de la SOC correspondante.
#' Ce type de graphique n’est adapté que pour deux groupes de traitement.
#'
#' @param baseEI la base des EIs avec
#' \itemize{
#'    \item l’identifiant du patient (idvar) : au format character obligatoire et sans manquants
#'    \item le label des PT ou LLT (Termsvar) pour chaque EI : au format character obligatoire et sans manquants
#'    \item la SOC correspondante (SOC_COD) : label (et non code) au format character obligatoire et sans manquants
#'    \item le grade de l’EI (gradevar) : au format numeric obligatoire en partant de 1 (et allant jusqu’au grade max par exemple 5) et sans manquants
#'    \item si l’EI est de type sérieux (SAEvar) : codé en “Yes”/“No” obligatoire
#'}
#' @param baseTr la base des traitements avec
#' \itemize{
#'    \item l’identifiant du patient (idvar) : au format character obligatoire et sans manquants
#'    \item le bras de traitement pour chaque individu (ARMvar) : au format character obligatoire et sans manquants
#'    \item [non obligatoire] une variable pour indiquer si le traitement à été pris (sert à détecter les patients “à risque”) TTTYN on recherchera les occurences en “Yes” donc doit être codée en “Yes”/“No” en character
#'}
#' @param idvar column name
#' @param SOCvar column name
#' @param gradevar column name
#' @param SAEvar column name
#' @param TTTYN column name
#' @param ARMvar column name
#' @param listcol : liste de couleurs pour chacun des groupes de traitements dans un vecteur soit avec les noms implémentés dans R (comme “red”, “blue”…) soit avec des codes (commençant par # par exemple “#52717F”) –> par défaut c(“deepskyblue2”,“red”)
#' @param varsup : Nature du groupe d’EIs à superposer parmi 3,4 (pour les EIs de grade>= à 3 ou resp 4) ou “Serious” – Note : pas de guillemets pour les chiffres –> par défaut 3
#' @param dodge_width The dodge between risk difference (default = 0.1)
#'
#' @return A plot
#' @export
#'
#' @importFrom cowplot plot_grid
#'
#' @examples
#' library(dplyr)
#' baseEI <- data.frame(idvar = paste0("Patients", round(runif(n = 100, min = 0, max = 100))),
#'                      Termsvar = round(runif(n = 100, min = 0, max = 10))) %>%
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
#'                                prob = c(0.9, 0.1)),
#'                 ARMvar = factor(ARMvar, levels = c("Treatment", "Placebo")))
#'
#' ButterflyBarChartSup(baseEI = baseEI, baseTr = baseTr,
#'                      idvar = "idvar", SOCvar = "SOCvar", gradevar = "gradevar", TTTYN = "TTTYN")
#'
ButterflyBarChartSup  <- function(baseEI, baseTr,
                                  idvar, SOCvar, gradevar, SAEvar=NULL, TTTYN=NULL, ARMvar,
                                  varsup=3, listcol=c("red","deepskyblue2"),
                                  dodge_width = 0.1){
  #remplacement des noms de variables
  varsup_labels <- paste0(c("Grade >= ", "Grade < "), varsup)

  baseEI <- baseEI %>%
    rename("id_pat" = idvar,
           "COD" = Termsvar,
           "Grade" = gradevar) %>%
    mutate(Grade = case_when(Grade >= varsup ~ varsup_labels[1],
                             Grade < varsup ~ varsup_labels[2])) %>%
    distinct()
  baseTr <- baseTr %>% rename("id_pat" = idvar,
                              "ARM" = ARMvar)

  if (!is.null(TTTYN)){
    baseTr <- baseTr %>%
      rename("TTTYN" = TTTYN) %>%
      filter(TTTYN == "Yes")
  }

  if(!is.factor(baseTr$ARM)){
    message("baseTr$ARM is converted to factor. Please change factor levels before calling the plot function to change the arms order of plotting.")
    baseTr$ARM <- as.factor(baseTr$ARM)
  }

  if (!is.null(SAEvar)) baseEI <- baseEI %>%
    rename("Serious" = SAEvar)

  ### merge bases
  vecARM <- levels(baseTr$ARM)
  ### merge databases
  dfAllAE <- expand.grid(id_pat = unique(baseTr$id_pat),
                         SOCvar = unique(baseEI$SOCvar),
                         Grade = unique(baseEI$Grade)) %>%
    left_join(baseTr, by = "id_pat") %>%
    left_join(baseEI %>%
                select(id_pat, SOCvar, Grade) %>%
                mutate(AE = 1) %>%
                distinct(),
              by = c("id_pat", "SOCvar", "Grade")) %>%
    mutate(AE = if_else(is.na(AE), 0, AE))

  # compute database for all grades and worse grades
  dfAllAE_allGrade <- dfAllAE %>%
    group_by(id_pat, SOCvar, ARM, TTTYN) %>%
    summarise(AE = max(AE),
              type = "All grade",
              .groups = "drop")

  dfAllAE_sup3Grade <- dfAllAE %>%
    filter(Grade == varsup_labels[1]) %>%
    group_by(id_pat, SOCvar, ARM, TTTYN) %>%
    summarise(AE = max(AE),
              type = varsup_labels[1],
              .groups = "drop")

  # compute ci and all
  df_plot <- dfAllAE_allGrade %>%
    bind_rows(dfAllAE_sup3Grade) %>%
    group_by(SOCvar, ARM, type) %>%
    summarise(nb_event = sum(AE),
              nb_indiv = n(),
              percent_of_patient = nb_event/nb_indiv,
              .groups = "drop") %>%
    group_by(SOCvar, type) %>%
    group_split() %>%
    lapply(FUN = function(df_i){
      a = df_i %>% filter(ARM == vecARM[[1]]) %>% pull(nb_event)
      N1 = df_i %>% filter(ARM == vecARM[[1]]) %>% pull(nb_indiv)
      b = df_i %>% filter(ARM == vecARM[[2]]) %>% pull(nb_event)
      N0 = df_i %>% filter(ARM == vecARM[[2]]) %>% pull(nb_indiv)

      res <- RDfunct(a = a, b = b, N1 = N1, N0 = N0)

      df_i %>%
        mutate(p_value = res$p.value,
               estimate = res$estimate,
               ci_inf = res$conf.int[1],
               ci_sup = res$conf.int[2]) %>%
        return(.)
    }) %>%
    bind_rows() %>%
    mutate(test = if_else(p_value < 0.05, "*", ""))

  ### plot
  p2 <- df_plot %>%
    select(SOCvar, type, estimate, ci_inf, ci_sup, test) %>%
    distinct() %>%
    ggplot(mapping = aes(x = estimate,
                         y = SOCvar,
                         xmin = ci_inf,
                         xmax = ci_sup,
                         color = type,
                         shape = type,
                         label = test)) +
    geom_point(position = position_dodge(width = dodge_width)) +
    geom_errorbarh(height = 0, position = position_dodge(width = dodge_width)) +
    geom_vline(xintercept=0, lty=2, colour = "red", linewidth = 0.5) +
    geom_text(col="red", size=6,  hjust=-1, vjust=0.7,
              position = position_dodge(width = dodge_width)) +
    scale_color_manual(name="RD",values = c("gray60","#000000")) +
    scale_shape_manual(name="RD",values=c(19,17))+
    labs(y = "SOC",
         x = paste0("Risk ", vecARM[1]," -  risk ",vecARM[2],  "\n with 95% CI")) +
    guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
    theme(axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          panel.background = element_blank(),
          panel.grid.major.y = element_line(color="grey90", linetype=1),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_line(linewidth = 1, colour = "#000000"),
          axis.line.x = element_line(color = "#000000", linetype = 1),
          axis.title.x =element_text(size=10),
          axis.text.x = element_text(size=11),
          legend.position="bottom",
          legend.title = element_blank(),
          legend.text = element_text(size=10),
          strip.text.y.left = element_blank(),
          strip.placement = "none",
          panel.spacing.y = unit(3,"pt"))

  ################### Butterfly barplot superpos\u00e9
  df_plot1 <- df_plot %>%
    select(SOCvar, ARM, type, percent_of_patient) %>%
    mutate(percent_of_patient = case_when(ARM == vecARM[1] ~ percent_of_patient,
                                          ARM == vecARM[2] ~ -percent_of_patient))

  p1 <- ggplot() +
    geom_bar(
      data = df_plot1 %>% filter(type == "All grade"),
      aes(x = percent_of_patient, y = SOCvar, fill = interaction(ARM, type)),
      stat = "identity",
      width = 0.6
    ) +
    geom_bar(
      data = df_plot1 %>% filter(type == "Grade >= 3"),
      aes(x = percent_of_patient, y = SOCvar, fill = interaction(ARM, type)),
      stat = "identity",
      width = 0.3
    ) +
    geom_vline(xintercept = 0) +
    scale_x_continuous(labels = function(x){paste0(abs(x)*100, "%")},
                       breaks = seq(-1,1,0.25),
                       limits = c(-1, 1)) +
    scale_fill_manual(name=paste0(vecARM[1] ,"\n\n",vecARM[2]),
                      values = c(alpha(listcol[1], 2/5),
                                 alpha(listcol[1], 1),
                                 alpha(listcol[2], 2/5),
                                 alpha(listcol[2], 1)),
                      labels = rep(c("All grade", "Grade >= 3"), 2)) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
    theme(
      panel.background = element_blank(),
      axis.line = element_line(color="#000000"),
      axis.ticks.y = element_blank(),
      panel.grid.major.x = element_line(color="grey80", linetype=2),
      panel.grid.major.y = element_line(color="grey90", linetype=1),
      legend.position = "bottom",
      axis.text = element_text(size=10),
      axis.title = element_text(size=10),
      # legend.title = element_text("#000000", size=10, hjust = 1),
      legend.text = element_text(size=10)
    ) +
    labs(y = "System Organ Class", x = "Percent of patient\n")

  cowplot::plot_grid(p1,  p2, labels = NULL,nrow = 1, rel_widths = c(0.75, 0.25))
}
