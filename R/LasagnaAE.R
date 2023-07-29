#' LasagnaAE
#'
#' @description helper for PanelLasagnaAlluvialAE
#'
#' @param data dataframe
#'
#' @return A plot
#' @export
#'
LasagnaAE <- function(data){
  ggplot(data, aes(x=visnum, y=reorder(id_pat, desc(rank)))) +
    geom_tile(aes(fill=grade)) +
    scale_fill_manual(name=paste0("Grade max atteint \npour ",choixEI), breaks = c("0",vect_grade),
                      labels = c("Pas d'EI",vect_grade),
                      values = c("lightgray",listcol)) +
    labs(x = paste0(unit," of treatment"), y = NULL) +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size =12),
          axis.title.x = element_blank(),
          axis.text.y = element_text(color = "black", size=10),
          axis.ticks = element_blank(),
          legend.position = "none",
          panel.background = element_blank(),
          panel.grid.major.y = element_line(color = "darkgray", linetype = 1),
          panel.spacing.y = element_blank()) +
    {if (idpat==FALSE) theme() %+replace% theme(axis.text.y = element_text(color = "white", size=10))}
}
