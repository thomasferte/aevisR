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
    if (FALSE %in% (list_Bio %in% unique(Biopat$lib))) return("Au moins une des biologie demandée n'est pas disponible dans la base")
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