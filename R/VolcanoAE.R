#' VolcanoAE
#'
#' VolcanoAE
#'
#' @description Each bubble represents an Adverse Event (AE), with the bubble's size representing the occurrence of the AE (either the number of AEs or the number of patients who experienced the AE). The color of the bubbles corresponds to the treatment group. Fisher's p-value is shown on the y-axis and determines the saturation of the bubbles. On the x-axis, a risk difference between the two groups is displayed.
#'
#' @param baseEI The base of AEs with:
#' \itemize{
#'    \item idvar: Patient identifier, in character format, mandatory and without missing values.
#'    \item Termsvar: PT or LLT label for each AE, in character format, mandatory and without missing values.
#' }
#' @param baseTr The base of treatments with:
#' \itemize{
#'    \item idvar: Patient identifier, in character format, mandatory and without missing values.
#'    \item ARMvar: Treatment arm for each individual, in character format, mandatory and without missing values.
#'    \item TTTYN : A variable indicating if the treatment was taken (used to detect "at-risk" patients). Occurrences will be searched for "Yes", so it should be coded as "Yes"/"No" in character format.
#' }
#' @param idvar (character) Column name of the patient identifier.
#' @param Termsvar (character) Column name of the label for each AE.
#' @param TTTYN (character, default = NULL) Column name indicating if the treatment was taken.
#' @param ARMvar (character) Column name for the treatment group.
#' @param caption (boolean, default = TRUE) TRUE/FALSE to display a basic graphic note with additional information about the format and content of the graph.
#' @param listcol (vector of character, default = (“red”,“deepskyblue2”)) A list of colors for each treatment group, provided as a vector with either R's implemented color names (e.g., “red”, “blue”...) or color codes (starting with #, e.g., “#52717F”).
#'
#' @return A Volcano plot
#' @export
#' @importFrom ggrepel geom_text_repel
#' @import ggplot2
#'
#' @examples
#' library(dplyr)
#' baseEI <- data.frame(idvar = paste0("Patients", round(runif(n = 100, min = 0, max = 100))),
#'                      Termsvar = round(runif(n = 100, min = 0, max = 20))) %>%
#'   dplyr::mutate(SOCvar = round(Termsvar/10)) %>%
#'   dplyr::mutate(across(everything(), .fns = as.character))
#'
#' baseTr  <- baseEI %>%
#'   dplyr::select(idvar) %>%
#'   dplyr::distinct() %>%
#'   dplyr::mutate(ARMvar = sample(x = c("Placebo", "Treatment"),
#'                                 size = nrow(.),
#'                                 replace = TRUE),
#'                 ARMvar = factor(ARMvar, levels = c("Treatment", "Placebo")),
#'                 TTTYN = sample(x = c("Yes", "No"),
#'                                size = nrow(.),
#'                                replace = TRUE,
#'                                prob = c(0.9, 0.1)))
#'
#' VolcanoAE(baseEI = baseEI,
#'           baseTr = baseTr,
#'           idvar = "idvar",
#'           Termsvar = "Termsvar",
#'           TTTYN = "TTTYN",
#'           ARMvar = "ARMvar")
#'
VolcanoAE <- function(baseEI, baseTr,
                      idvar, Termsvar, TTTYN=NULL, ARMvar,
                      caption=TRUE, listcol = c("red", "deepskyblue2")){
  #remplacement des noms de variables
  baseEI <- baseEI %>% rename("id_pat" = idvar,
                              "COD" = Termsvar)
  baseTr <- baseTr %>% rename("id_pat" = idvar,
                              "ARM" = ARMvar)
  if (!is.null(TTTYN)){
    baseTr <- baseTr %>%
      rename("TTTYN" = TTTYN) %>%
      filter(TTTYN == "Yes")
  }

  if(!is.factor(baseTr$ARM)){
    message("baseTr$ARM is converted to factor. Please change factor levels before calling VolcanoAE to change the arms order of plotting.")
    baseTr$ARM <- as.factor(baseTr$ARM)
  }

  vecARM <- levels(baseTr$ARM)
  ### merge databases
  dfAllAE <- expand.grid(id_pat = unique(baseTr$id_pat),
                         COD = unique(baseEI$COD)) %>%
    left_join(baseTr, by = "id_pat") %>%
    left_join(baseEI %>%
                select(id_pat, COD) %>%
                mutate(AE = 1) %>%
                distinct(),
              by = c("id_pat", "COD")) %>%
    mutate(AE = if_else(is.na(AE), 0, AE)) %>%
    # compute summary statistics
    group_by(COD) %>%
    mutate(nb_event = sum(AE)) %>%
    filter(nb_event != 0) %>%
    mutate(p_val_fisher = fisher.test(AE, ARM)$p.value) %>%
    group_by(COD, ARM, p_val_fisher, nb_event) %>%
    summarise(risk = sum(AE)/n(),
              .groups = "drop") %>%
    tidyr::pivot_wider(names_from = ARM, values_from = risk) %>%
    mutate(risk_difference = get(vecARM[1]) - get(vecARM[2]),
           sign_difference = factor(sign(risk_difference),
                                    levels = c(1, -1),
                                    labels = paste0("Increased risk in ", vecARM)),
           label = case_when(p_val_fisher < 0.05 ~ COD,
                             TRUE ~ ""))

  ## text for caption
  if (caption){
    labcap <- "
    Visual representation of AE data, Volcano plot for adverse events between two treatment arms.
    The x-axis represents the difference in proportions of participants experiencing each adverse event between the treatment arms.
    The y-axis represents the p value from Fisher's exact test.
    The centre of the bubble indicates the coordinates for each adverse event. The size of the bubble is proportional to the total number of events for both arms combined.
    Colour saturation help to see if multiple AE are in the same coordinate with red indicating greater risk in the testing arm and blue in the control arm.
    Labels are added to events with value inferior to 0.05 on the y-axis."
  } else {labcap=""}

  #########################################
  # Volcano Plot
  #########################################
  p <- ggplot(dfAllAE,
              aes(x=risk_difference, y=p_val_fisher, size=nb_event,
                  label = label,
                  color= sign_difference)) +
    geom_point(alpha = 0.40) +
    scale_size(range = c(1,10)) +
    scale_color_manual(values = listcol) +
    scale_y_log10() +
    annotation_logticks(sides = "l") +
    ggrepel::geom_text_repel(size = 4,
                             color="black",
                             min.segment.length = 0.1,
                             direction="both") +
    labs(shape="Treatment Group",
         caption=labcap,
         title = "Volcano Plot",
         x = "Risk difference",
         y = "P-value",
         color = "",
         size = "Nb of events") +
    geom_hline(yintercept= 0.05, col="red", linetype=2) +
    geom_text(x=min(dfAllAE$risk_difference),
              y= log10(0.05),
              label = "p-value = 0.05",
              col="red",
              size=4, vjust=-0.5, hjust="left") +
    geom_vline(xintercept = 0, col="grey") +
    theme_bw() +
    theme(legend.position = "bottom",
          plot.caption = element_text(hjust = 0))

  return(p)
}
