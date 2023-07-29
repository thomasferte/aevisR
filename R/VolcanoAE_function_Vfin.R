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
  
  ########### fonction pour le calcul du RD (pris de fmsb::riskdifference) : modif -> pas de print
  RDfunct <- function (a, b, N1, N0, CRC = FALSE, conf.level = 0.95) {
    .M <- a + b
    .T <- N1 + N0
    .R1 <- a/N1
    .R0 <- b/N0
    .RT <- .M/.T
    norm.pp <- qnorm(1 - (1 - conf.level)/2)
    if (CRC) {
      .RC1 <- norm.pp * sqrt(a * (N1 - a)/(N1^3))
      .RC0 <- norm.pp * sqrt(b * (N0 - b)/(N0^3))
      .RCT <- norm.pp * sqrt(.M * (.T - .M)/(.T^3))
      .MAT <- matrix(c(a, b, .M, N1, N0, .T, .R1, .R0, .RT, 
                       .R1 - .RC1, .R0 - .RC0, .RT - .RCT, .R1 + .RC1, 
                       .R0 + .RC0, .RT + .RCT), 3, 5)
      colnames(.MAT) <- c("Cases", "People at Risk", "Risk", 
                          "Lower CL", "Upper CL")
      rownames(.MAT) <- c("Exposed", "Unexposed", "Total")
    } else {
      .MAT <- matrix(c(a, b, .M, N1, N0, .T, .R1, .R0, .RT), 
                     3, 3)
      colnames(.MAT) <- c("Cases", "People at risk", "Risk")
      rownames(.MAT) <- c("Exposed", "Unexposed", "Total")
    }
    class(.MAT) <- "table"
    ################ print(.MAT) ####################
    ESTIMATE <- .R1 - .R0
    .CHI <- ESTIMATE/sqrt(a * (N1 - a)/(N1^3) + b * (N0 - b)/(N0^3))
    p.v <- 2 * (1 - pnorm(abs(.CHI)))
    RDL <- ESTIMATE - norm.pp * sqrt(a * (N1 - a)/(N1^3) + b * 
                                       (N0 - b)/(N0^3))
    RDU <- ESTIMATE + norm.pp * sqrt(a * (N1 - a)/(N1^3) + b * 
                                       (N0 - b)/(N0^3))
    CINT <- c(RDL, RDU)
    attr(CINT, "conf.level") <- conf.level
    RVAL <- list(p.value = p.v, conf.int = CINT, estimate = ESTIMATE, 
                 method = "Risk difference and its significance probability (H0: The difference equals to zero)", 
                 data.name = paste(deparse(substitute(a)), deparse(substitute(b)), 
                                   deparse(substitute(N1)), deparse(substitute(N0))))
    class(RVAL) <- "htest"
    return(RVAL)
  }
  
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
  
  plot(p)
}
