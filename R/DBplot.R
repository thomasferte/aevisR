#' DBplot
#'
#' @description Plot the dumbell
#'
#' @param data1 a dataframe
#' @param data2 another
#' @param data3 another
#' @param SOCvar The SOC variable
#' @param levels_SOC The SOC levels
#' @param limpct The limpct object
#' @param listcol The color list
#' @param pas pas
#' @param nbEvents nbEvents
#' @param caption caption
#'
#' @return A plot
#' @export
#'
#' @import grid
#' @import cowplot
#'
DBplot <- function(data1,data2,data3, SOCvar, levels_SOC, limpct, listcol, pas, nbEvents, caption){
  if (!is.null(SOCvar)){
    data1$SOC <- factor(data1$SOC, levels = c(levels_SOC))
    data2$SOC <- factor(data2$SOC, levels = c(levels_SOC))
    data3$SOC <- factor(data3$SOC, levels = c(levels_SOC))
  }

  p1 <- ggplot(data1,aes(x=pct,y=reorder(COD,rk))) +
    {if (!is.null(SOCvar)) geom_rect(aes(ymin = COD_start,
                                         ymax = COD_end),
                                     xmin = -Inf,
                                     xmax = Inf,
                                     colour= ifelse(data1$grp==1,"gray91","white"),
                                     fill=ifelse(data1$grp==1, "grey91","white"),
                                     linewidth=5, show.legend = F)} +
    geom_hline(aes(yintercept = COD), color = "darkgray", linewidth = 0.05, lty=3) +
    geom_vline(xintercept = seq(0,limpct,by=0.25), color = "gray70", linewidth = 0.05, lty=1) +
    geom_point(size=2,aes(colour = factor(ARM), shape = factor(ARM))) +
    xlab('Percentage') + ylab('') +
    scale_colour_manual(values = listcol, name = "Treatment :") +
    scale_shape_manual(values = c(17,19), name = "Treatment :") +
    scale_x_continuous(name = "Percent of patient",breaks = seq(0,limpct,by=pas),
                       labels = scales::label_percent(), limits = c(0,limpct)) +
    {if(!is.null(SOCvar)) facet_grid(SOC ~ ., scales = "free", space = "free", switch = "y")} +
    theme(legend.position="bottom",
          legend.justification = c("right","top"),
          legend.box.just = "right",
          legend.margin = margin(6,6,6,6),
          legend.text = element_text(size=10),
          axis.line.x = element_line(color = "black", linetype = 1),
          axis.line.y = element_line(color = "black", linetype = 1),
          axis.ticks.y = element_blank(),
          axis.text.y = element_text(size = 10),
          axis.text.x = element_text(hjust = 1, size = 10),
          axis.title.x = element_text(size=12),
          panel.background = element_blank(),
          strip.text.y.left = element_text(angle = 0, hjust = 1, size = 10),
          strip.placement = "outside",
          panel.spacing.y = unit(3,"pt"))

  q <- ggplotGrob(p1)
  lg <- grid::linesGrob(x=unit(c(1,1),"npc"), y=unit(c(0,1),"npc"),
                        gp=grid::gpar(col="gray5", lwd=1))
  for (k in grep("strip-l",q$layout$name)) {
    q$grobs[[k]]$grobs[[1]]$children[[1]] <- lg
  }

  p2 <- ggplot(data=data2, aes(x=reorder(COD,rk), y=RD, ymin=CI_2, ymax=CI_1)) +
    {if (!is.null(SOCvar)) geom_rect(aes(xmin = COD_start,
                                         xmax = COD_end),
                                     ymin = -Inf,
                                     ymax = Inf,
                                     colour=ifelse(data2$grp==1, "gray91","white"),
                                     fill=ifelse(data2$grp==1, "grey91","white"),
                                     linewidth=5, show.legend = F)} +
    geom_vline(aes(xintercept = COD), color = "darkgray", linewidth = 0.05, lty=3) +
    geom_point(aes(shape="RD"), size=1.5) +
    scale_shape_manual(values = c("RD"=19)) +
    geom_errorbar(aes(color="IC"),width=0.5, linewidth=0.5) +
    scale_color_manual(values = c("IC"="black")) +
    geom_hline(yintercept=0, lty=2, colour = "red", linewidth = 1) +  # add a dotted line at x=1 after flip
    geom_text(aes(x=COD, label=test,y=CI_2), col="red", size=6,  hjust=-1, vjust=0.6) +
    scale_y_continuous(name = "Risk Difference with 95% CI")+
    coord_flip() +
    {if (!is.null(SOCvar)) facet_grid(SOC ~ ., scales = "free", space = "free", switch = "y")} +
    theme(axis.text.y = element_blank(),
          axis.text.x = element_text(size=10),
          axis.title.y = element_blank(),
          axis.title.x = element_text(size=12),
          panel.background = element_blank(),
          panel.grid.major.y = element_line(color = "darkgray", linewidth = 0.1, linetype = 3),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_line(linewidth = 1, colour = "black"),
          axis.line.x = element_line(color = "black", linetype = 1),
          legend.position="bottom",
          legend.title=element_blank(),
          strip.text.y.left = element_blank(),
          strip.placement = "none",
          panel.spacing.y = unit(3,"pt"))

  #deux col avec les chiffres brutes, ne seront ajout\u00e9s que si nbEvents est TRUE
  if (nbEvents==TRUE){
    p3 <-ggplot(data3,aes(x="",y=reorder(COD,rk))) +
      {if (!is.null(SOCvar)) geom_rect(aes(ymin = COD_start, ymax = COD_end),
                                       xmin = -Inf,  xmax = Inf,
                                       colour=ifelse(data3$grp==1, "gray91","white"),
                                       fill=ifelse(data3$grp==1, "grey91","white"),
                                       linewidth=5, show.legend = F)} +
      geom_text(aes(label=arm2, colour="grp1"), size=3.5, fontface="bold") +
      scale_colour_manual(values=c("grp1"= listcol[2])) +
      xlab("NbTot") +
      {if (!is.null(SOCvar)) facet_grid(SOC ~ ., scales = "free", space = "free", switch = "y")} +
      guides(colour = guide_legend(title=list_ARM[2],label=FALSE)) +
      theme(axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            panel.background = element_blank(),
            strip.text.y.left = element_blank(),
            strip.placement = "none",
            legend.position = "bottom",
            axis.line.x = element_line(color = "black", linetype = 1),
            axis.ticks.y = element_blank(),
            axis.ticks.x = element_blank())

    p4 <-ggplot(data3,aes(x="",y=reorder(COD,rk))) +
      {if (!is.null(SOCvar)) geom_rect(aes(ymin = COD_start,  ymax = COD_end),
                                       xmin = -Inf, xmax = Inf,
                                       colour=ifelse(data3$grp==1, "gray91","white"),
                                       fill=ifelse(data3$grp==1, "grey91","white"),
                                       linewidth=5, show.legend = F)} +
      geom_text(aes(label=arm1, colour="grp2"), size=3.5, fontface="bold") +
      scale_colour_manual(values=c("grp2"= listcol[1]))+
      xlab("NbTot") +
      {if (!is.null(SOCvar)) facet_grid(SOC ~ ., scales = "free", space = "free", switch = "y")} +
      guides(colour = guide_legend(title=list_ARM[1],label=FALSE)) +
      theme(axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            panel.background = element_blank(),
            strip.text.y.left = element_blank(),
            strip.placement = "none",
            legend.position = "bottom",
            axis.line.x = element_line(color = "black", linetype = 1),
            axis.ticks.y = element_blank(),
            axis.ticks.x = element_blank())

    plt <- cowplot::plot_grid(q,  p2, p3 , p4, labels = NULL,nrow = 1,rel_widths = c(0.7, 0.2,0.05,0.05))
    labcap <- "
      Visual representation of AE data, Dumbbell plot + forest plot for adverse events between two treatment arms.
      The left side of the figure displays the percentage of participants experiencing an adverse event (labelled on the y-axis) in the testing arm with a red circle and control arm with a blue triangle.
      The middle part of the figure displays the Risk Difference and corresponding 95% confidence interval.
      The left side displays two columns with the absolute number of AE in each groups (multiple episodes are counted).
      SOC are sorted by their RD and inside PT are sorted by their RD, each in increasing order."
  } else {
    plt <- cowplot::plot_grid(q,  p2, labels = NULL,nrow = 1,rel_widths = c(0.8, 0.3))
    labcap <- "
      Visual representation of AE data, Dumbbell plot + forest plot for adverse events between two treatment arms.
      The left side of the figure displays the percentage of participants experiencing an adverse event (labelled on the y-axis) in the testing arm with a red circle and control arm with a blue triangle.
      The right side of the figure displays the Risk Difference and corresponding 95% confidence interval.
      SOC are sorted by their RD and inside PT are sorted by their RD, each in increasing order."
  }

  if (caption==TRUE){
    plt <- cowplot::add_sub(plt,labcap,x=0, hjust=0, size=11)
  }
  cowplot::ggdraw(plt)
}
