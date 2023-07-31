#' BarChart
#'
#' @description helper for PanelLasagnaAlluvialAE
#'
#' @param data A dataframe
#' @param group A groupe
#' @param Type A type
#' @param choixEI choixEI
#' @param vect_grade vect_grade
#' @param listcol listcol
#'
#' @return A plot
#' @export
#'
BarChart <- function(data, group, Type, choixEI, vect_grade, listcol){
  df_bar <- data[data$grade!="NA",] %>% select(visnum, grade) %>%
    group_by(visnum, grade) %>% summarize(count=n()) %>%
    mutate(pct=round((count/sum(count))*100,0))

  if(Type==21 | Type==22) df_bar <- subset(df_bar, df_bar$grade!=0)

  plot <- ggplot(data = df_bar, aes(fill=grade, x=visnum, y = pct)) +
    geom_bar(position = "dodge", stat = 'identity') +
    geom_text(aes(label=pct),
              size=4,position = position_dodge(width=0.9),vjust = -0.5) +
    scale_x_discrete(limits=factor(seq(1,max(as.numeric(data$visnum)), by=1))) +
    scale_y_continuous(limits=c(0,max(df_bar$pct)+10)) +
    scale_fill_manual(name=paste0("Grade max atteint \npour ",choixEI), breaks = c(vect_grade,"0"),
                      labels = c(vect_grade,"Pas d'EI"),
                      values = c(listcol,"lightgray")) +
    ggtitle(group) +
    labs(y="percent (%)") +
    theme(panel.background = element_blank(),
          plot.title = element_text(size=12),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_text(color = "black", size=12),
          axis.text.x = element_blank(),
          legend.position = "none")

  if(Type==12 | Type==22){
    plot <- plot + theme(axis.text.x = element_text(size=10))
  } else {
    plot <- plot + theme(axis.text.x = element_blank())
  }
}
