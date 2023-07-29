#' DumbbellAE
#'
#' @description Représente pour chaque EI :
#' \itemize{
#'    \item dans la partie gauche (Dumbbell plot) : représente soit le nombre
#'    brut de patients ayant eu au moins 1 fois l’EI soit la proportion de
#'    patients ayant eu cet EI - surtout lorsque les groupes ne sont pas
#'    égaux - avec 2 points par ligne/par EI (1 pour chaque groupe de traitement
#'    différenciés par des formes et couleurs). Note : Ne compte qu’une
#'    occurrence de chaque EI par patient, d’où l’utilité éventuelle d’ajouter
#'    deux colonnes au graphique avec le nombre total d’évènements par type EI
#'    et par groupe (en faisant toutefois attention à la taille des deux groupes
#'    de traitement pour comparer ces chiffres)
#'    \item dans la partie droite (Forest plot) : le Risk Difference (RD)
#'    calculé pour chaque type d’EI avec l’IC à 95%. Dans cette partie du
#'    graphique, il est aussi possible d’utiliser, sous la même forme, le risque
#'    relatif (RR) ou encore l’odd ratio (OR). Pour ce dernier on choisira une
#'    échelle logarithmique. –> Plus visuel que d’avoir seulement les chiffres
#'    et IC dans un tableau, ici on voit directement les IC qui se chevauchent
#'    ou non, ou ceux qui contiennent la valeur 0 (pour le RD) par exemple.
#' }
#' L’ajout du Forest plot à un Dumbbell plot classic est essentiel car il permet
#' d’interpréter au mieux l’écart entre les deux groupes et de quantifier
#' certains aspects du jeu de données. L’addition de ces deux graphiques permet
#' d’identifier facilement les différences significatives entre les bras de
#' traitement.
#' Ce graphique est adapté à seulement deux groupes de traitement.
#'
#' @param baseEI la base des EIs avec :
#' \itemize{
#'    \item idvar : l’identifiant du patient : au format character obligatoire
#'    et sans manquants
#'    \item Termsvar : le label des PT ou LLT pour chaque EI : au format
#'    character obligatoire et sans manquants
#'    \item SOCvar [optionnal]: le label de la SOC (SOCvar) associée à chaque
#'    EI : au format character et sans manquants.
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
#' @param SOCvar (character, default = NULL) column name of AE SOC
#' @param TTTYN (character, default = NULL) column name indicating if the
#' treatemnt was taken
#' @param ARMvar (character) column name for treatment group
#' @param nbplot (numeric, default = 1) nombre de graphiques à créer
#' (l’utilisateur doit choisir en fonction du nombre d’EI différents dans la
#' base de données) cad division du graphique global en plusieurs sous
#' graphiques lorsque le nombre d’EI est top grand.
#' @param nbEvents (boolean, default = FALSE) Choisir d’afficher le nombre
#' d’évènement brute par PT (ou LLT) sous forme de deux colonnes de chiffres
#' supplémentaires à la droite du graphique
#' @param TriSOC (character, default = "D) “C”/“D”/“alpha” pour faire un tri
#' croissant, décroissant ou selon le RD ou alphabétique des SOC
#' @param TriEI (character, default = "D) “C”/“D”/“alpha” idem pour PT (ou LLT)
#' @param caption (boolean, default = TRUE) TRUE/FALSE afficher une note de base
#' de graphique avec des informations supplémentaires sur le format et le
#' continu du graphique
#' @param listcol (vector of character, default = (“red”,“deepskyblue2”)) liste
#' de couleurs pour chacun des groupes de traitements dans un vecteur soit avec
#' les noms implémentés dans R (comme “red”, “blue”…) soit avec des codes
#' (commençant par # par exemple “#52717F”)
#'
#' @return A Dumbell + forest plots
#' @export
#'
#' @import dplyr
#' @import ggplot2
#' @import scales
#'
DumbbellAE <- function(baseEI, baseTr,
                       idvar, Termsvar, SOCvar=NULL, TTTYN=NULL, ARMvar,
                       nbplot=1, nbEvents=FALSE, TriSOC="D", TriEI="D", caption=TRUE, listcol= c("deepskyblue2", "red")){

  #remplacement des noms de variables
  baseEI <- baseEI %>% rename("id_pat" = idvar,
                              "COD" = Termsvar)
  if (!is.null(SOCvar)) baseEI <- baseEI %>% rename("SOC" = SOCvar)

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
  ######################## Construction table dumbbell #######################
  ###################
  # Calcul des freq
  ###################
  ## AE with events
  data_distinct <- baseEI %>% select(id_pat,ARM,COD) %>% distinct(id_pat, ARM, COD)
  frq1 <- data.frame(xtabs(~ COD + ARM, data = data_distinct))

  ## Total subjects in each ARM
  if (is.null(TTTYN)) {
    df_Tr2 <- baseTr
  } else {
    df_Tr2 <- baseTr[baseTr$TTTYN=="Yes",]
  }
  USU_distinct <- df_Tr2 %>% select(id_pat, ARM) %>% distinct(id_pat, ARM)
  frq2 <- data.frame(xtabs(~ ARM, data=USU_distinct))
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
    }
    else {
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

  ################### calcul de RD par PT #####################
  #Creation d'une table pour accueilir toutes les données nécessaires
  dfx_all <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("Freq_Total", "COD","CI_1", "CI_2", "RD"))

  for (p in unique(frq4$COD)){
    #pour chaque PT tableau permettant de calculer la p-value ainsi que le ratio
    df1 <- subset(frq4, COD == p, select = (c(-COD, -ARM)))

    dfx <- data.frame(COD = p,
                      frqTot = frq4$yes[frq4$COD == p][1] + frq4$yes[frq4$COD == p][2],
                      RD = RDfunct(df1[[1]][1], df1[[1]][2], frq2$Freq[frq2$ARM=="arm1"],
                                   frq2$Freq[frq2$ARM=="arm2"], CRC=TRUE)$estimate,
                      CI_1 = RDfunct(df1[[1]][1], df1[[1]][2], frq2$Freq[frq2$ARM=="arm1"],
                                     frq2$Freq[frq2$ARM=="arm2"], CRC=TRUE)$conf.int[1],
                      CI_2 = RDfunct(df1[[1]][1], df1[[1]][2], frq2$Freq[frq2$ARM=="arm1"],
                                     frq2$Freq[frq2$ARM=="arm2"], CRC=TRUE)$conf.int[2],
                      row.names = NULL)
    # A chaque PT concatenation des tables
    dfx_all <- rbind(dfx_all,dfx)
  }

  ## ranking selon l'option TriEI
  if (TriEI=="D") {dfx_all <- dfx_all %>% arrange(RD)
  } else if (TriEI=="C") {dfx_all <- dfx_all %>% arrange(desc(RD))
  } else if (TriEI=="alpha") {dfx_all <- dfx_all %>% arrange(desc(COD))
  } else return("Valeur non valide pour l'option TriEI")
  dfx_all$rkCOD <- 1:nrow(dfx_all)

  # on ajoute le RD  à la table utilisée pour le p1 pour le rank des SOC
  df_p1 <- merge(frq4,dfx_all %>% select(COD,rkCOD), by="COD")
  df_p1$pct <- ifelse(df_p1$ARM=="arm1",df_p1$yes/frq2$Freq[frq2$ARM=="arm1"],df_p1$yes/frq2$Freq[frq2$ARM=="arm2"])

  if (!is.null(SOCvar)){
    ################# calcul RD par SOC (pour le tri des SOC) ###################
    SOC_dis <- baseEI %>% select(id_pat,ARM,SOC) %>% distinct(id_pat, ARM, SOC)
    frq5 <- data.frame(xtabs(~ SOC + ARM, data = SOC_dis))
    #Merged
    frq6 <- merge(frq5, frq2, by="ARM")
    frq7 <- frq6 %>%
      mutate(no = Freq.y - Freq.x,
             yes = Freq.x) %>%
      arrange(SOC, ARM) %>%
      select(c(ARM, SOC, yes, no))

    dfx_SOC <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("SOC","RD_SOC"))
    for (s in unique(frq7$SOC)){
      df1 <- subset(frq7, SOC == s, select = (c(-SOC, -ARM)))
      dfx <- data.frame(SOC = s,
                        RD_SOC = RDfunct(df1[[1]][1], df1[[1]][2], frq2$Freq[frq2$ARM=="arm1"],
                                         frq2$Freq[frq2$ARM=="arm2"], CRC=TRUE)$estimate,
                        row.names = NULL)
      # A chaque PT concatenation des tables
      dfx_SOC <- rbind(dfx_SOC,dfx)
    }
    ## ranking selon l'option TriSOC
    if (TriSOC=="D") {dfx_SOC <- dfx_SOC %>% arrange(RD_SOC)
    } else if (TriSOC=="C") {dfx_SOC <- dfx_SOC %>% arrange(desc(RD_SOC))
    } else if (TriSOC=="alpha") {dfx_SOC <- dfx_SOC %>% arrange(desc(SOC))
    } else return("Valeur non valide pour l'option TriSOC")
    dfx_SOC$rkSOC <- 1:nrow(dfx_SOC)

    #######################################################
  }

  ### calcul des chiffres brutes pour les deux colonnes supp (option nbEvents)
  df_AE3 <- baseEI %>% select(id_pat,COD,ARM)

  frq8 <- data.frame(xtabs(~ COD + ARM, data = df_AE3))
  frq8 <- pivot_wider(frq8, names_from = ARM, values_from = Freq)

  if (!is.null(SOCvar)){
    #On ajoute les SOC correspondants aux COD avec une jointure
    tab_SOC <- baseEI %>% select(SOC, COD) %>% group_by(SOC,COD) %>% distinct(SOC,COD)

    df_p1 <-left_join(df_p1 %>% select(-no),tab_SOC,by="COD",multiple="all")
    # on ajoute le RD de chaque SOC à la table utilisée pour le p1 pour le rank des SOC
    df_p1 <- merge(df_p1,dfx_SOC, by="SOC")
    df_p2 <- left_join(dfx_all %>% select(-frqTot),tab_SOC, by="COD", multiple="all")
    df_p3 <- left_join(frq8,tab_SOC, by="COD", multiple="all")
  } else {
    df_p2 <- dfx_all %>% select(-frqTot)
    df_p3 <- merge(frq8, df_p1 %>% select(COD, rkCOD), by="COD")
  }

  # mettre en évidence par des labels (geom_text) les EIs COD significatifs
  df_p2$test <- ""
  df_p2$test[(df_p2$CI_2<0 & df_p2$CI_1<0) | (df_p2$CI_2>0 & df_p2$CI_1>0)]<-"*"

  #### variable rang pour ordonner les modalités
  if (is.null(SOCvar)){
    # names(df_p1) <- c("COD","ARM","yes","no","rk","pct")
    # names(df_p2) <- c("COD","RD","CI_1","CI_2","rk","test")
    # names(df_p3) <- c("COD", "arm1","arm2","rk")
    df_p1 <- df_p1 %>% rename("rk" = "rkCOD")
    df_p2 <- df_p2 %>% rename("rk" = "rkCOD")
    df_p3 <- df_p3 %>% rename("rk" = "rkCOD")
  } else if (!is.null(SOCvar)){
    tb_rank <- df_p1 %>% distinct(SOC, COD, rkCOD, rkSOC) %>% arrange(desc(rkSOC), desc(rkCOD), COD)
    tb_rank$rk <- 1:nrow(tb_rank)
    #on merge la table tb_rank avec chacune des trois table pour les 3 parties du graphique
    df_p2 <- merge(df_p2, tb_rank %>% select(-rkCOD, -rkSOC, -SOC), by="COD")
    df_p1 <- merge(df_p1, tb_rank %>% select(-rkCOD, -rkSOC, -SOC), by="COD")
    df_p3 <- merge(df_p3, tb_rank %>% select(-rkCOD, -rkSOC, -SOC), by="COD")

    # création d'une variable bcol (1 si doit être en gris et 0 sinon)
    df_num <- df_p2 %>% arrange(rk) %>% distinct(SOC)
    df_num$num <- 1:nrow(df_num)

    df_num <- merge(df_num, df_p1 %>% select(SOC,COD,rk), by="SOC")
    #A chaque numéro on récupère le min et le max qui seront donc les limites des rectangles pour l'axes des ordonnées (représenté par les COD)
    df_rect <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("grp", "SOC","COD_start", "COD_end"))
    for(i in unique(df_num$num)){
      dfx_num <- subset(df_num, df_num$num==i)

      dfx = data.frame(grp=ifelse(i%%2==0,0,1), #0 si pair, 1 si impair
                       SOC = unique(dfx_num$SOC),
                       COD_start = unique(dfx_num$COD[dfx_num$rk==min(dfx_num$rk)]),
                       COD_end = unique(dfx_num$COD[dfx_num$rk==max(dfx_num$rk)]))
      df_rect <- rbind(df_rect,dfx)
      #on merge avec les 3 tables qui servent à construire le graphique (df_p1,df_p2,df_p3)
    }
    df_p1 <- merge(df_p1,df_rect, by="SOC")
    df_p2 <- merge(df_p2,df_rect, by="SOC")
    df_p3 <- merge(df_p3,df_rect, by="SOC")
  }

  #on renomme la colonne ARM avec les noms d'origine (tromqué à 8 caractères)
  df_p1$ARM <- ifelse( df_p1$ARM=="arm1", substr(list_ARM[1],start=1, stop=8), substr(list_ARM[2],start=1, stop=8))

  ## limite pour le graph
  limpct <- ceiling(max(df_p1$pct)*10) #*10 car on veut prendre à la dizaine supérieure
  limpct <- limpct/10

  if (limpct < 0.10){
    pas = 0.01
  } else if (limpct < 0.50){
    pas = 0.05
  } else if (limpct < 0.75){
    pas = 0.10
  } else {
    pas = 0.25
  }

  ###################################################################################
  if (!is.null(SOCvar)){
    levels_SOC <- df_p1 %>% select(SOC,rk) %>% arrange(rk) %>% distinct(SOC)
    levels_SOC <- as.vector(levels_SOC$SOC)
  } else {
    levels_COD <- df_p1 %>% select(COD,rk) %>% arrange(rk) %>% distinct(COD)
    levels_COD <- as.vector(levels_COD$COD)
  }

  DBplot <- function(data1,data2,data3){
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
    lg <- linesGrob(x=unit(c(1,1),"npc"), y=unit(c(0,1),"npc"),
                    gp=gpar(col="gray5", lwd=1))
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

    #deux col avec les chiffres brutes, ne seront ajoutés que si nbEvents est TRUE
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

      plt <- plot_grid(q,  p2, p3 , p4, labels = NULL,nrow = 1,rel_widths = c(0.7, 0.2,0.05,0.05))
      labcap <- "
      Visual representation of AE data, Dumbbell plot + forest plot for adverse events between two treatment arms.
      The left side of the figure displays the percentage of participants experiencing an adverse event (labelled on the y-axis) in the testing arm with a red circle and control arm with a blue triangle.
      The middle part of the figure displays the Risk Difference and corresponding 95% confidence interval.
      The left side displays two columns with the absolute number of AE in each groups (multiple episodes are counted).
      SOC are sorted by their RD and inside PT are sorted by their RD, each in increasing order."
    } else {
      plt <- plot_grid(q,  p2, labels = NULL,nrow = 1,rel_widths = c(0.8, 0.3))
      labcap <- "
      Visual representation of AE data, Dumbbell plot + forest plot for adverse events between two treatment arms.
      The left side of the figure displays the percentage of participants experiencing an adverse event (labelled on the y-axis) in the testing arm with a red circle and control arm with a blue triangle.
      The right side of the figure displays the Risk Difference and corresponding 95% confidence interval.
      SOC are sorted by their RD and inside PT are sorted by their RD, each in increasing order."
    }

    if (caption==TRUE){
      plt <- add_sub(plt,labcap,x=0, hjust=0, size=11)
    }
    ggdraw(plt)
  }

  #### séparation du graph en nbplot car trop de PT_ affichées
  #On fait des listes de SOC à peu près égales et on diviser les tables et les graphs selon ces deux listes
  if (!is.null(SOCvar)) {
    nb_SOC <- length(levels_SOC)
    lim <- floor(nb_SOC/nbplot)
    res <- list()
    for (i in 1:nbplot){
      if (i==1) l_SOC <- levels_SOC[1:lim]
      else if (i==nbplot) {
        a <- (lim*(i-1))+(i-1)
        l_SOC <- levels_SOC[a:nb_SOC]
      } else {
        a <- (lim*(i-1))+(i-1)
        b <- (lim*i)+(i-1)
        l_SOC <- levels_SOC[a:b]
      }

      df_p1i <- subset(df_p1, SOC %in% l_SOC)
      df_p2i <- subset(df_p2, SOC %in% l_SOC)
      df_p3i <- subset(df_p3, SOC %in% l_SOC)

      res[[i]] <- DBplot(df_p1i,df_p2i,df_p3i)
    }
  } else if (is.null(SOCvar)){
    nb_COD <- length(levels_COD)
    lim <- floor(nb_COD/nbplot)
    res <- list()

    for (i in 1:nbplot){
      if (i==1) l_COD <- levels_COD[1:lim]
      else if (i==nbplot) {
        a <- (lim*(i-1))+(i-1)
        l_COD <- levels_COD[a:nb_COD]
      } else {
        a <- (lim*(i-1))+(i-1)
        b <- (lim*i)+(i-1)
        l_COD <- levels_COD[a:b]
      }

      df_p1i <- subset(df_p1, COD %in% l_COD)
      df_p2i <- subset(df_p2, COD %in% l_COD)
      df_p3i <- subset(df_p3, COD %in% l_COD)
      res[[i]] <- DBplot(df_p1i,df_p2i,df_p3i)
    }
  }

  return(res)
}
