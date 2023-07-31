#' AlluvialAE
#'
#' @description helper for PanelLasagnaAlluvialAE
#'
#' @param data dataframe
#' @param N N
#' @param choixEI choixEI
#' @param vect_grade vect_grade
#' @param listcol listcol
#' @param unit unit
#'
#' @importFrom ggalluvial geom_flow geom_stratum
#'
#' @return A plot
#' @export
#'
AlluvialAE <- function(data,N,choixEI,vect_grade, listcol, unit){
  a = 0.25*N
  b = 0.5*N
  c = 0.75*N
  d = 1*N

  if ("NA" %in% data$grade) data$grade <- relevel(data$grade, "NA")
  data <- data %>% select(id_pat, ARM,visnum,grade) %>% arrange(id_pat,visnum,grade)

  ggplot(data,
         aes(x=visnum, stratum = grade, alluvium = id_pat, fill = grade)) +
    ggalluvial::geom_flow() +
    ggalluvial::geom_stratum() +
    scale_fill_manual(name=paste0("Grade max atteint \npour ",choixEI),
                      breaks = c("0",vect_grade,"NA"),
                      labels = c("Pas d'EI",vect_grade,"Non trait\u00e9s"),
                      values = c("lightgray",listcol,"grey60")) +
    scale_y_continuous(breaks = c(0,a,b,c,d),
                       labels = c("0"="0","a"="25%","b"="50%","c"="75%","d"="100%")) +
    guides(fill=guide_legend(ncol=max(as.numeric(data$grade))))+
    labs(x = paste0(unit," of treatement"), y = NULL) +
    theme(legend.position = "bottom",
          legend.text = element_text(size=10),
          legend.title = element_text(size=10),
          panel.background = element_blank(),
          panel.grid.major.y = element_line(color = "darkgray", linetype = 2),
          axis.text.y = element_text(color = 'black', size=12),
          axis.title.x = element_text(size=12),
          axis.text.x = element_text(color = 'black', size=12),
          axis.ticks.x = element_blank())
}
