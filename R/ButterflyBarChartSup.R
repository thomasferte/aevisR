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
#' @param ARMe : indiquer le bras expérimental, il sera placé en premier cad à gauche dans le graphique pour comparer au bras contrôle à droite. Ecrire le label du bras expérimental comme il est dans la base de données (ex: ARMe=“armB”) –> par défaut si cette option n’est pas remplie (donc ARMe=NULL) alors les groupes seront placés dans l’ordre alphabétique des labels
#' @param tri : tri selon “pctAll”,“pctVarsup”,“RDAll”,“RDVarsup” toujours en décroissant –> par défaut selon le RD du total “RDAll”
#' @param trivar : ex:“armA” label du groupe de traitement selon lequel effectuer le tri –> par défaut NULL tri selon les deux bras
#' @param listcol : liste de couleurs pour chacun des groupes de traitements dans un vecteur soit avec les noms implémentés dans R (comme “red”, “blue”…) soit avec des codes (commençant par # par exemple “#52717F”) –> par défaut c(“deepskyblue2”,“red”)
#' @param varsup : Nature du groupe d’EIs à superposer parmi 3,4 (pour les EIs de grade>= à 3 ou resp 4) ou “Serious” – Note : pas de guillemets pour les chiffres –> par défaut 3
#'
#' @return A plot
#' @export
#'
ButterflyBarChartSup  <- function(baseEI, baseTr,
                                  idvar, SOCvar, gradevar, SAEvar=NULL, TTTYN=NULL, ARMvar,
                                  varsup=3, ARMe=NULL, tri="RDAll",trivar=NULL, listcol=c("red","deepskyblue2")){
  #remplacement des noms de variables
  baseEI <- baseEI %>% rename("id_pat" = idvar,
                              "SOC" = SOCvar,
                              "Grade" = gradevar)
  if (!is.null(SAEvar)) baseEI <- baseEI %>% rename("Serious" = SAEvar)
  baseTr <- baseTr %>% rename("id_pat" = idvar,
                              "ARM" = ARMvar)
  if (!is.null(TTTYN)) baseTr <- baseTr %>% rename("TTTYN" = TTTYN)

  #### table EI
  #liste des groupes de traitement de la table baseTr
  list_ARM <- unique(baseTr$ARM)
  if (is.null(ARMe)) {
    #Liste des id patients dans le bras numéro 1
    list_pat1 <- unique(baseTr$id_pat[baseTr$ARM == list_ARM[1]])
    #Liste des id patients dans le bras numéro 2
    list_pat2 <- unique(baseTr$id_pat[baseTr$ARM == list_ARM[2]])
  } else if (!is.null(ARMe)){
    l2 <- unique(baseTr$ARM)
    if (!(ARMe %in% l2)) return("Nom de bras de traitement non correct ou non présent dans la base.")
    list_ARM[1] <- ARMe
    list_ARM[2] <- l2[l2 != ARMe]

    #Liste des id patients dans le bras numéro 1
    list_pat1 <- unique(baseTr$id_pat[baseTr$ARM == list_ARM[1]])
    #Liste des id patients dans le bras numéro 2
    list_pat2 <- unique(baseTr$id_pat[baseTr$ARM == list_ARM[2]])
  }
  #Ajouter une colonne ARM dans la table data en faisant correspondre les id_pat selon la liste où ils sont présents
  baseEI$ARM <- ifelse(baseEI$id_pat %in% list_pat1, "armG", "armD")
  baseTr$ARM <- ifelse(baseTr$ARM==list_ARM[1], "armG","armD")

  if (is.null(TTTYN)) {
    df_Tr2 <- baseTr
  } else {
    df_Tr2  <- baseTr[baseTr$TTTYN =="Yes",]
  }
  USU_distinct  <- df_Tr2  %>% select(id_pat, ARM) %>% distinct(id_pat, ARM)
  frq2 <- data.frame(xtabs(~ ARM, data=USU_distinct))

  df <- baseEI %>% select(id_pat, ARM, SOC) %>% distinct(id_pat, ARM, SOC)
  Events <- df %>% group_by(ARM, SOC) %>% summarise(Count=n())
  Events$pctAll <- round(ifelse(Events$ARM=="armG",Events$Count/frq2$Freq[frq2$ARM=="armG"],
                                Events$Count/frq2$Freq[frq2$ARM=="armD"])*100,1)

  # récupérer la sous base avec les EIs de grade >= 3
  if (varsup!="Serious") {
    df <- baseEI %>% select(id_pat, ARM, SOC, Grade)
    df2 <- df %>% filter(Grade >= varsup)
  } else if (varsup=="Serious") {
    df <- baseEI %>% select(id_pat, ARM, SOC, Grade, Serious)
    df2 <- df %>% filter(Serious=="Yes")
  }

  #on garde une occurence unique pour chaque id_pat et SOC
  df2 <- df2 %>% select(-Grade) %>% distinct(id_pat,ARM,SOC)

  Events2 <- df2 %>% group_by(ARM, SOC) %>% summarise(CountS=n())
  Events2$pctS <- round(ifelse(Events2$ARM=="armG",Events2$CountS/frq2$Freq[frq2$ARM=="armG"],
                               Events2$CountS/frq2$Freq[frq2$ARM=="armD"])*100,1)

  df_ALL <- left_join(Events, Events2, by= c("ARM", "SOC")) %>% arrange(desc(pctAll))

  df_long <- df_ALL %>%
    pivot_longer(c(pctAll, pctS),names_to = "grp", values_to = "pct")

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

  ############ Calcul du RD (ALL)
  df_RD <- df_ALL %>% select(-pctAll, -pctS,-CountS) %>% pivot_wider(names_from = c(ARM), values_from = Count)

  df_RD$armD[is.na(df_RD$armD)]<-0
  df_RD$armG[is.na(df_RD$armG)]<-0
  df_RD <- df_RD %>% pivot_longer(c("armG","armD"), names_to = "ARM", values_to = "yes")

  df_RD$no <- ifelse(df_RD$ARM=="armG",
                     frq2$Freq[frq2$ARM=="armG"]- df_RD$yes,
                     frq2$Freq[frq2$ARM=="armD"] - df_RD$yes)
  #Calcul du Risk Difference et de son intervalle de confiance
  dfx_all <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("Freq_Total", "SOC","CI_1", "CI_2", "RD"))

  for (s in unique(df_RD$SOC)){
    #pour chaque PT tableau permettant de calculer la p-value ainsi que le ratio
    df1 <- subset(df_RD, SOC == s, select = (c(-SOC)))

    dfx <- data.frame(SOC = s,
                      frqTot = df_RD$yes[df_RD$SOC == s][1] + df_RD$yes[df_RD$SOC == s][2],
                      RD = RDfunct(df1$yes[df1$ARM=="armD"], df1$yes[df1$ARM=="armG"],
                                   frq2$Freq[frq2$ARM=="armD"], frq2$Freq[frq2$ARM=="armG"], CRC=TRUE)$estimate,
                      CI_1 = RDfunct(df1$yes[df1$ARM=="armD"], df1$yes[df1$ARM=="armG"],
                                     frq2$Freq[frq2$ARM=="armD"], frq2$Freq[frq2$ARM=="armG"], CRC=TRUE)$conf.int[1],
                      CI_2 = RDfunct(df1$yes[df1$ARM=="armD"], df1$yes[df1$ARM=="armG"],
                                     frq2$Freq[frq2$ARM=="armD"], frq2$Freq[frq2$ARM=="armG"], CRC=TRUE)$conf.int[2],
                      row.names = NULL)
    # A chaque PT concatenation des tables
    dfx_all <- rbind(dfx_all,dfx)
  }

  df_plot_forest <- data.frame(SOC = dfx_all$SOC,
                               estimate1 =  dfx_all$RD,
                               lower1 = dfx_all$CI_1,
                               upper1 = dfx_all$CI_2, row.names=NULL)

  ############ autre Calcul du RD (grade>=value)
  df_RD2 <- df_ALL %>% select(-pctAll, -pctS, -Count) %>% pivot_wider(names_from = ARM, values_from = CountS)

  df_RD2$armG[is.na(df_RD2$armG)]<-0
  df_RD2$armD[is.na(df_RD2$armD)]<-0
  df_RD2 <- df_RD2 %>% pivot_longer(c("armG","armD"), names_to = "ARM", values_to = "yes")

  # ajout colonne avec le nombre de patients ayant eu cette SOC
  df_RD2 <- left_join(df_RD2, df_RD %>% select(SOC, yes, ARM), by= c("ARM", "SOC"))
  df_RD2$no <- df_RD2$yes.y-df_RD2$yes.x
  df_RD2 <- df_RD2 %>% select(-yes.y)
  colnames(df_RD2)<-c("SOC","ARM","yes","no")
  #Calcul du Risque Relatif et de sont intervalle de confiance
  dfx_all <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("Freq_Total", "SOC","CI_1", "CI_2", "RD"))
  for (s in unique(df_RD2$SOC)){
    #pour chaque PT tableau permettant de calculer la p-value ainsi que le ratio
    df1 <- subset(df_RD2, SOC == s, select = (c(-SOC)))

    dfx <- data.frame(SOC = s,
                      frqTot = df_RD2$yes[df_RD2$SOC == s][1] + df_RD2$yes[df_RD2$SOC == s][2],
                      RD = RDfunct(df1$yes[df1$ARM=="armD"],
                                   df1$yes[df1$ARM=="armG"],
                                   df1$no[df1$ARM=="armD"]+df1$yes[df1$ARM=="armD"],
                                   df1$no[df1$ARM=="armG"]+df1$yes[df1$ARM=="armG"], CRC=TRUE)$estimate,
                      CI_1 = RDfunct(df1$yes[df1$ARM=="armD"],
                                     df1$yes[df1$ARM=="armG"],
                                     df1$no[df1$ARM=="armD"]+df1$yes[df1$ARM=="armD"],
                                     df1$no[df1$ARM=="armG"]+df1$yes[df1$ARM=="armG"], CRC=TRUE)$conf.int[1],
                      CI_2 = RDfunct(df1$yes[df1$ARM=="armD"],
                                     df1$yes[df1$ARM=="armG"],
                                     df1$no[df1$ARM=="armD"]+df1$yes[df1$ARM=="armD"],
                                     df1$no[df1$ARM=="armG"]+df1$yes[df1$ARM=="armG"], CRC=TRUE)$conf.int[2],
                      row.names = NULL)
    # A chaque PT concatenation des tables
    dfx_all <- rbind(dfx_all,dfx)
  }

  dfx_all$RD[dfx_all$frqTot==0]<-NA
  dfx_all$CI_1[dfx_all$frqTot==0]<-NA
  dfx_all$CI_2[dfx_all$frqTot==0]<-NA

  df_plot_forest <- left_join(df_plot_forest, dfx_all %>% select(-frqTot),by="SOC")
  colnames(df_plot_forest)<-c("SOC","estimate1","lower1","upper1","estimate2","lower2","upper2")

  # mettre en évidence par des labels (geom_text) les EIs COD significatifs
  df_plot_forest$test1 <- ""
  df_plot_forest$test1[df_plot_forest$lower1<0 & df_plot_forest$upper1<0]<-"*"
  df_plot_forest$test1[df_plot_forest$lower1>0 & df_plot_forest$upper1>0]<-"*"

  # mettre en évidence par des labels (geom_text) les EIs COD significatifs
  df_plot_forest$test2 <- ""
  df_plot_forest$test2[df_plot_forest$lower2<0 & df_plot_forest$upper2<0]<-"*"
  df_plot_forest$test2[df_plot_forest$lower2>0 & df_plot_forest$upper2>0]<-"*"


  ######## ranking
  if (is.null(trivar)){
    df_long_rk <- df_long
  } else if (trivar==list_ARM[1]){
    df_long_rk <- df_long %>% filter(ARM=="armG")
  } else if (trivar==list_ARM[2]){
    df_long_rk <- df_long %>% filter(ARM=="armD")
  } else return("Valeur non valide pour l'option trivar")

  if(tri == "RDAll"){
    tb_rk <- df_plot_forest %>% arrange(desc(estimate1))
  } else if (tri == "RDVarsup"){
    tb_rk <- df_plot_forest %>% arrange(desc(estimate2))
  } else if (tri=="pctAll"){
    tb_rk <- df_long_rk %>% filter(grp=="pctAll") %>% arrange(desc(pct))
  } else if (tri=="pctVarsup"){
    tb_rk <- df_long_rk %>% filter(grp=="pctS") %>% arrange(desc(pct))
  }
  tb_rk$rank <- 1:nrow(tb_rk)
  tb_rk <- tb_rk %>% select(SOC,rank) %>% ungroup() %>% distinct(SOC,rank)


  df_long$dir <- ifelse(df_long$ARM=="armD",1,-1)
  df_long$alpha <- ifelse(df_long$grp=="pctS","pctS","pctAll")

  # pour avoir une unique colonne estimate, lower et upper avec un colonne groupe
  df1 <- df_plot_forest %>% select(-c(estimate2, upper2,lower2,test2))
  df2 <- df_plot_forest %>% select(-c(estimate1, upper1,lower1,test1))
  df1$grp <- "RDTot"
  df2$grp <- "RDtox"
  colnames(df1)<-c("SOC","estimate","lower","upper","test","grp")
  colnames(df2)<-c("SOC","estimate","lower","upper","test","grp")
  df_plot_forest <- rbind(df1,df2)

  ## ajout d'un des ranking (selon choix) à la table pour le graph
  df_plot_forest <- merge(df_plot_forest, tb_rk, by="SOC")
  df_long <- merge(df_long, tb_rk, by="SOC")

  ################## forest plot seul
  valXdodge=-0.7
  if (varsup!="Serious") {labForest <- paste0("Grade >=", varsup)
  } else if (varsup=="Serious") {labForest <- "Serieux"}

  p2 <- ggplot(data=df_plot_forest, aes(x=reorder(SOC,rank), y=estimate,group=grp,colour=grp)) +
    geom_point(size=2, aes(shape=grp),position = position_dodge(width = valXdodge)) +
    geom_errorbar(aes(ymin=lower, ymax=upper),
                  width=0.5, linewidth=0.5, position = position_dodge(width = valXdodge)) +
    geom_hline(yintercept=0, lty=2, colour = "red", linewidth = 0.5) +
    geom_text(aes(x=reorder(SOC,rank), label=test,y=upper),
              col="red", size=6,  hjust=-1, vjust=0.7,
              position = position_dodge(width = valXdodge)) +
    scale_color_manual(name="RD",values = c("gray60","black"), labels=c("All grade",labForest)) +
    scale_shape_manual(name="RD",values=c(19,17), labels=c("All grade",labForest))+
    scale_y_continuous(name = paste0("Risk Difference with 95% CI
                       \n",list_ARM[1],"              ",list_ARM[2])) +
    facet_grid(reorder(SOC,rank) ~ ., scales = "free", space = "free", switch = "y") +
    scale_x_discrete(limits= rev(levels(df_plot_forest$SOC))) +
    coord_flip() +
    theme(axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          panel.background = element_blank(),
          panel.grid.major.y = element_line(color="grey90", linetype=1),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_line(linewidth = 1, colour = "black"),
          axis.line.x = element_line(color = "black", linetype = 1),
          axis.title.x =element_text(size=10),
          axis.text.x = element_text(size=11),
          legend.position="bottom",
          legend.title = element_blank(),
          legend.text = element_text(size=10),
          strip.text.y.left = element_blank(),
          strip.placement = "none",
          panel.spacing.y = unit(3,"pt"))

  ################### Butterfly barplot superposé
  p1 <- ggplot() +
    geom_bar(
      data = filter(df_long, grp=="pctS"),
      aes(x = reorder(SOC,desc(rank)), y = pct*dir, fill = interaction(as.factor(ARM),as.factor(grp)), group = grp),
      stat = "identity",
      width = 0.6
    ) +
    geom_bar(
      data = filter(df_long, grp=="pctAll"),
      aes(x = reorder(SOC,desc(rank)), y = pct*dir, fill = interaction(as.factor(ARM),as.factor(grp)), group = grp),
      stat = "identity",
      width = 0.9
    ) +
    geom_hline(yintercept = 0) +
    coord_flip() +
    scale_y_continuous(labels = function(x){paste0(abs(x), "%")},
                       breaks = seq(-100,100,25)) +
    scale_fill_manual(name=paste0(list_ARM[2] ,"\n\n",list_ARM[1]),
                      values = c( "armG.pctS" = alpha(listcol[1], 1),
                                  "armG.pctAll" = alpha(listcol[1], 2/5),
                                  "armD.pctS" = alpha(listcol[2], 1),
                                  "armD.pctAll" = alpha(listcol[2], 2/5)),
                      labels = c("armG.pctS" = labForest,
                                 "armG.pctAll" = "All grades",
                                 "armD.pctS" = labForest,
                                 "armD.pctAll" = "All grade")) +
    labs(x = "System Organ Class", y = "Percent of patient") +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
    theme(
      panel.background = element_blank(),
      axis.line = element_line(color="black"),
      axis.ticks.y = element_blank(),
      panel.grid.major.x = element_line(color="grey80", linetype=2),
      panel.grid.major.y = element_line(color="grey90", linetype=1),
      legend.position = "bottom",
      axis.text = element_text(size=10),
      axis.title = element_text(size=10),
      legend.title = element_text("black", size=10, hjust = 1),
      legend.text = element_text(size=10)
    )

  plot_grid(p1,  p2, labels = NULL,nrow = 1, rel_widths = c(0.75, 0.25))
}
