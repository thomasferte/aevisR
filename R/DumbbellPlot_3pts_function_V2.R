## v2 : 4 points -> deux par groupe
## version avec aussi 4 colonnes de chiffres

## pour l'instant fonctions récupérées du shiny
# voir si changements avec la version fonction DumbbellPlot_3pts_function.R

# format : soit dumbbell (4 points forme et couleur selon les groupes) 
# soit text (4 colonnes de text)
Dumbbell3grp_rel <- function(data, Overall=TRUE, format="dumbbell"){
  #Liste des id patients dans le bras GC
  list_pat_GC <- unique(df_Tr$USUBJID[df_Tr$ARM == "GC"]) #23 patients
  #Liste des id patients dans le bras GC + Avelumab
  list_pat_AVE <- unique(df_Tr$USUBJID[df_Tr$ARM == "GC + Avelumab"]) #42 patients
  #Ajouter une colonne ARM dans la table data
  data$ARM <- ifelse(data$USUBJID %in% list_pat_GC, "GC", "GC + Avelumab")
  
  
  ###################################################
  ### Subgroup Related GC ou Avelumab
  ###################################################
  dfGCAVE <- data %>% select(USUBJID, ARM, PT_COD, AE_GC, AE_AVE) %>% distinct(USUBJID, ARM,PT_COD, AE_GC, AE_AVE)
  dfGCAVE <- subset(dfGCAVE, AE_AVE == "Yes")
  dfGCAVE <- dfGCAVE %>% select(-AE_GC, -AE_AVE)
  frq1 <- data.frame(xtabs(~ PT_COD + ARM, data = dfGCAVE)) #frequence de chaque type d'EI dans chacun des bras de traitement
  frq2 <- df_Tr[df_Tr$TTT_IND=="Yes",] #on récupère une sous table avec seulement les patients ayant eu au moins 1 fois le traitement
  frq2 <- frq2 %>% select(USUBJID, ARM, TTT_IND) %>% distinct(USUBJID, ARM) # liste des patients avec leur bras de traitement
  frq2 <- data.frame(xtabs(~ ARM, data=frq2)) # compte le nombre de patient dans chacun des bras
  #Merged
  frq3 <- merge(frq1, frq2, by="ARM") 
  frq4 <- frq3 %>%
    mutate(no = Freq.y - Freq.x, #total - # with event
           yes = Freq.x) %>% # all with AE
    arrange(PT_COD, ARM) %>%
    select(c(ARM, PT_COD, yes, no))
  
  ########### fontion pour le calcul du RD (pris de fmsb::riskdifference) : modif -> pas de print
  # RDfunct 
  
  ################### calcul de RD #####################
  #Creation d'une table pour accueilir toutes les données nécessaires
  dfx_GCAVE <- setNames(data.frame(matrix(ncol = 6, nrow = 0)), c("Freq_Total", "PT_COD","CI_1", "CI_2", "RD"))
  
  for (p in unique(frq4$PT_COD)){
    #pour chaque PT tableau permettant de calculer la p-value ainsi que le ratio
    df1 <- subset(frq4, PT_COD == p, select = (c(-PT_COD, -ARM)))
    
    dfx <- data.frame(PT_COD = p,
                      subgroup="RelatedAll",
                      frqTot = 0 + frq4$yes[frq4$PT_COD == p][1],
                      frqGC = 0,
                      frqAVE = frq4$yes[frq4$PT_COD == p & frq4$ARM=="GC + Avelumab"],
                      RD = RDfunct(0, df1[[1]][1], frq2$Freq[frq2$ARM=="GC"], frq2$Freq[frq2$ARM=="GC + Avelumab"], CRC=TRUE)$estimate,
                      CI_1 = RDfunct(0, df1[[1]][1], frq2$Freq[frq2$ARM=="GC"], frq2$Freq[frq2$ARM=="GC + Avelumab"], CRC=TRUE)$conf.int[1],
                      CI_2 = RDfunct(0, df1[[1]][1], frq2$Freq[frq2$ARM=="GC"], frq2$Freq[frq2$ARM=="GC + Avelumab"], CRC=TRUE)$conf.int[2],
                      row.names = NULL)
    # A chaque PT concatenation des tables
    dfx_GCAVE <- rbind(dfx_GCAVE,dfx)
  }
  dfx_GCAVE$pct = round(dfx_GCAVE$frqTot / sum(frq2$Freq),2)
  dfx_GCAVE$pctGC = round(dfx_GCAVE$frqGC / frq2$Freq[1],2)
  dfx_GCAVE$pctAVE = round(dfx_GCAVE$frqAVE / frq2$Freq[2],2)
  
  ###################################################
  ### Subgroup Related GC (group GC ou GC + Avelumab)
  ###################################################
  dfGC <- data %>% select(USUBJID, ARM, PT_COD, AE_GC) %>% distinct(USUBJID, ARM,PT_COD, AE_GC)
  dfGC <- subset(dfGC, AE_GC == "Yes")
  dfGC <- dfGC %>% select(-AE_GC)
  frq1 <- data.frame(xtabs(~ PT_COD + ARM, data = dfGC)) #frequence de chaque type d'EI dans chacun des bras de traitement
  #Merged
  frq3 <- merge(frq1, frq2, by="ARM") 
  frq4 <- frq3 %>%
    mutate(no = Freq.y - Freq.x, #total - # with event
           yes = Freq.x) %>% # all with AE
    arrange(PT_COD, ARM) %>%
    select(c(ARM, PT_COD, yes, no))
  
  ################### calcul de RD #####################
  #Creation d'une table pour accueilir toutes les données nécessaires
  dfx_GC <- setNames(data.frame(matrix(ncol = 6, nrow = 0)), c("Freq_Total", "PT_COD","CI_1", "CI_2", "RD"))
  
  for (p in unique(frq4$PT_COD)){
    #pour chaque PT tableau permettant de calculer la p-value ainsi que le ratio
    df1 <- subset(frq4, PT_COD == p, select = (c(-PT_COD, -ARM)))
    
    dfx <- data.frame(PT_COD = p,
                      subgroup="RelatedGC",
                      frqTot = frq4$yes[frq4$PT_COD == p][1] + frq4$yes[frq4$PT_COD == p][2],
                      frqGC = frq4$yes[frq4$PT_COD == p & frq4$ARM=="GC"],
                      frqAVE = frq4$yes[frq4$PT_COD == p & frq4$ARM=="GC + Avelumab"],
                      RD = RDfunct(df1[[1]][1], df1[[1]][2], frq2$Freq[frq2$ARM=="GC"], frq2$Freq[frq2$ARM=="GC + Avelumab"], CRC=TRUE)$estimate,
                      CI_1 = RDfunct(df1[[1]][1], df1[[1]][2], frq2$Freq[frq2$ARM=="GC"], frq2$Freq[frq2$ARM=="GC + Avelumab"], CRC=TRUE)$conf.int[1],
                      CI_2 = RDfunct(df1[[1]][1], df1[[1]][2], frq2$Freq[frq2$ARM=="GC"], frq2$Freq[frq2$ARM=="GC + Avelumab"], CRC=TRUE)$conf.int[2],
                      row.names = NULL)
    # A chaque PT concatenation des tables
    dfx_GC <- rbind(dfx_GC,dfx)
  }
  dfx_GC$pct = round(dfx_GC$frqTot / sum(frq2$Freq),2)
  dfx_GC$pctGC = round(dfx_GC$frqGC / frq2$Freq[1],2)
  dfx_GC$pctAVE = round(dfx_GC$frqAVE / frq2$Freq[2],2)
  
  
  ###################################################
  ### Group Overall
  ###################################################
  if(Overall==TRUE){
    df_all <- data %>% select(USUBJID, ARM, PT_COD) %>% distinct(USUBJID, ARM,PT_COD)
    frq1 <- data.frame(xtabs(~ PT_COD + ARM, data = df_all)) #frequence de chaque type d'EI dans chacun des bras de traitement
    #Merged
    frq3 <- merge(frq1, frq2, by="ARM") 
    frq4 <- frq3 %>%
      mutate(no = Freq.y - Freq.x, #total - # with event
             yes = Freq.x) %>% # all with AE
      arrange(PT_COD, ARM) %>%
      select(c(ARM, PT_COD, yes, no))
    
    ################### calcul de RD #####################
    #Creation d'une table pour accueilir toutes les données nécessaires
    dfx_all <- setNames(data.frame(matrix(ncol = 6, nrow = 0)), c("Freq_Total", "PT_COD","CI_1", "CI_2", "RD"))
    
    for (p in unique(frq4$PT_COD)){
      #pour chaque PT tableau permettant de calculer la p-value ainsi que le ratio
      df1 <- subset(frq4, PT_COD == p, select = (c(-PT_COD, -ARM)))
      
      dfx <- data.frame(PT_COD = p,
                        subgroup="Overall",
                        frqTot = frq4$yes[frq4$PT_COD == p][1] + frq4$yes[frq4$PT_COD == p][2],
                        RD = RDfunct(df1[[1]][1], df1[[1]][2], frq2$Freq[frq2$ARM=="GC"], frq2$Freq[frq2$ARM=="GC + Avelumab"], CRC=TRUE)$estimate,
                        CI_1 = RDfunct(df1[[1]][1], df1[[1]][2], frq2$Freq[frq2$ARM=="GC"], frq2$Freq[frq2$ARM=="GC + Avelumab"], CRC=TRUE)$conf.int[1],
                        CI_2 = RDfunct(df1[[1]][1], df1[[1]][2], frq2$Freq[frq2$ARM=="GC"], frq2$Freq[frq2$ARM=="GC + Avelumab"], CRC=TRUE)$conf.int[2],
                        row.names = NULL)
      # A chaque PT concatenation des tables
      dfx_all <- rbind(dfx_all,dfx)
    }
    dfx_all$pct = round(dfx_all$frqTot / sum(frq2$Freq),2)
    dfx_all$pctGC = round(dfx_all$frqGC / frq2$Freq[1],2)
    dfx_all$pctAVE = round(dfx_all$frqAVE / frq2$Freq[2],2)
  }
  
  #### table finale pour le plot
  tb_plot <- rbind(dfx_GCAVE, dfx_GC)
  if(Overall==TRUE) tb_plot <- rbind(tb_plot, dfx_all)
  
  ####
  # tb_plot <- tb_plot %>% pivot_longer(cols = c("pctGC","pctAVE"),names_to = "ARM", values_to = "pctgrp")
  tb_plot <- tb_plot %>% pivot_longer(cols = c("frqGC","frqAVE"),names_to = "ARM", values_to = "frqgrp")
  tb_plot$grptxt <- paste0(tb_plot$subgroup,"_",tb_plot$ARM)
  
  ################# calcul RD par SOC (pour le tri des SOC) ###################
  frq5 <- data %>% select(USUBJID,ARM,SOC_COD) %>% distinct(USUBJID, ARM, SOC_COD) 
  frq5 <- data.frame(xtabs(~ SOC_COD + ARM, data = frq5))
  #Merged
  frq6 <- merge(frq5, frq2, by="ARM") 
  frq7 <- frq6 %>%
    mutate(no = Freq.y - Freq.x, #total - # with event
           yes = Freq.x) %>% # all with AE
    arrange(SOC_COD, ARM) %>%
    select(c(ARM, SOC_COD, yes, no))
  
  dfx_SOC <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("SOC_COD","RD_SOC"))
  for (s in unique(frq7$SOC_COD)){
    #pour chaque PT tableau permettant de calculer la p-value ainsi que le ratio
    df1 <- subset(frq7, SOC_COD == s, select = (c(-SOC_COD, -ARM)))
    dfx <- data.frame(SOC_COD = s,
                      RD_SOC = RDfunct(df1[[1]][1], df1[[1]][2], frq2$Freq[frq2$ARM=="GC"], frq2$Freq[frq2$ARM=="GC + Avelumab"], CRC=TRUE)$estimate,
                      row.names = NULL)
    # A chaque PT concatenation des tables
    dfx_SOC <- rbind(dfx_SOC,dfx)
  }
  #######################################################
  
  ## limite pour le graph
  limpct <- ceiling(max(tb_plot$pct)*10) #*10 car on veut prendre à la dizaine supérieure
  limpct <- limpct/10
  
  ## On ajoute les SOC_COD correspondants aux PT_COD avec une jointure
  tab_SOC <- data %>% select(SOC_COD, PT_COD) %>% group_by(SOC_COD,PT_COD) %>% distinct(SOC_COD,PT_COD)
  tb_plot <- left_join(tb_plot,tab_SOC, by="PT_COD", multiple="all")
  # on ajoute le RD de chaque SOC à la table utilisée pour le p1 pour le rank des SOC
  tb_plot <- merge(tb_plot,dfx_SOC, by="SOC_COD")
  
  ## mettre en évidence par des labels (geom_text) les EIs PT_COD significatifs
  tb_plot$test <- ""
  tb_plot$test[tb_plot$CI_2<0 & tb_plot$CI_1<0]<-"*"
  tb_plot$test[tb_plot$CI_2>0 & tb_plot$CI_1>0]<-"*"
  
  ## variable rang pour ordonner les modalités
  if (Overall==TRUE){
    # on tri selon le RD de Overall donc on fait un pretri sur la base
    tb_plot_rk <- tb_plot[tb_plot$subgroup=="Overall",]
  } else { 
    #si PT présent en double alors on prend celui qui correspont à RelatedGC (automatique regarder comment choisir plutôt de garder RelatedAll)
    tb_plot_rk <- tb_plot %>% group_by(SOC_COD,PT_COD) %>% filter(!duplicated(PT_COD)) %>% ungroup()
  }
  tb_rank <- tb_plot_rk %>% distinct(SOC_COD, PT_COD, RD, RD_SOC) %>% arrange(desc(RD_SOC), desc(RD), PT_COD)
  tb_rank$rk <- 1:nrow(tb_rank)
  tb_plot <- merge(tb_plot, tb_rank %>% select(-c(SOC_COD,RD, RD_SOC)), by="PT_COD")
  
  ## création d'une variable bcol (1 si doit être en gris et 0 sinon)
  df_pcol <- tb_plot %>% arrange(rk) %>% distinct(SOC_COD)
  df_pcol$bcol <- 0
  for (i in 1:nrow(df_pcol)){
    if (i %% 2 != 0){
      df_pcol$bcol[i] <- 1
    }
  }
  tb_plot <- merge(tb_plot,df_pcol, by="SOC_COD")
  
  ### création d'un data frame pour les emplacement des couleurs
  # variable start/end : PT_COD du début de la zone et PT_COD de la fin de la zone
  # variable colors pour la couleur
  tb_plot <- tb_plot %>% arrange(rk)
  cptS=2
  cptE=1
  SOC_COD = ""
  PT_start = ""
  PT_end = ""
  PT_start[1] <- tb_plot$PT_COD[1]
  SOC_COD[1] <- tb_plot$SOC_COD[1]
  for (i in 1:(nrow(tb_plot)-1)){
    if (tb_plot$bcol[i]==0 & tb_plot$bcol[i+1]==1){
      PT_start[cptS] <- tb_plot$PT_COD[i+1]
      SOC_COD[cptS] <- tb_plot$SOC_COD[i+1]
      cptS=cptS+1
    }
    if (tb_plot$bcol[i]==1 & tb_plot$bcol[i+1]==0){
      PT_end[cptE] <- tb_plot$PT_COD[i] 
      cptE=cptE+1
    }
  }
  ##si nombre de SOC impaire
  if(length(unique(tb_plot$SOC_COD))%%2!=0)PT_end[cptE] <- tb_plot$PT_COD[nrow(tb_plot)]
  data_breaks1 <- as.data.frame(cbind(SOC_COD, PT_start, PT_end, col=1))
  cptS=1
  cptE=1
  SOC_COD = ""
  PT_start = ""
  PT_end = ""
  for (i in 1:(nrow(tb_plot)-1)){
    if (tb_plot$bcol[i]==1 & tb_plot$bcol[i+1]==0){
      PT_start[cptS] <- tb_plot$PT_COD[i+1] 
      SOC_COD[cptS] <- tb_plot$SOC_COD[i+1]
      cptS=cptS+1
    }
    if (tb_plot$bcol[i]==0 & tb_plot$bcol[i+1]==1){
      PT_end[cptE] <- tb_plot$PT_COD[i]
      cptE=cptE+1
    }
  }
  ##si nombre de SOC paire
  if(length(unique(tb_plot$SOC_COD))%%2==0)PT_end[cptE]<-tb_plot$PT_COD[nrow(tb_plot)]
  data_breaks2 <- as.data.frame(cbind(SOC_COD, PT_start, PT_end, col=0))
  
  data_breaks <- rbind(data_breaks1, data_breaks2)
  data_breaks$SOC_COD <- as.factor(data_breaks$SOC_COD)
  tb_plot$SOC_COD <- as.factor(tb_plot$SOC_COD)
  tb_plot <- left_join(tb_plot,data_breaks, by="SOC_COD", multiple="all")
  
  levels_SOC <- tb_plot %>% select(SOC_COD,rk) %>% arrange(rk)
  levels_SOC <- levels_SOC %>% distinct(SOC_COD)
  levels_SOC <- as.vector(levels_SOC$SOC_COD)
  
  ## liste modalités
  if(Overall==TRUE) {
    lmod <- c("Overall","RelatedAll","RelatedGC")
    lcol <- c("gray40","purple","orange")
  } else {
    lmod <- c("RelatedAll","RelatedGC")
    lcol <- c("purple","orange")
  }
  
  #### plot
  DBplot <- function(data1){
    data1$SOC_COD <- factor(data1$SOC_COD, levels = c(levels_SOC))
    
    p1 <- ggplot(data1,aes(x=pctgrp,y=reorder(PT_COD,rk))) +
      geom_rect(aes(ymin = PT_start, 
                    ymax = PT_end), 
                xmin = -Inf, 
                xmax = Inf,
                colour= ifelse(data1$col==1,"gray91","white"),
                fill=ifelse(data1$col==1, "grey91","white"),
                linewidth=5, show.legend = F) +
      geom_hline(aes(yintercept = PT_COD), color = "darkgray", linewidth = 0.05, lty=3) +
      geom_vline(xintercept = seq(0,1,by=0.2), color = "black", linewidth = 0.05, lty=1) +
      if (format=="dumbbell"){
        list(geom_point(size=2.5,aes(colour = factor(subgroup), shape = factor(ARM))),
             scale_color_manual(name="groupes percentages", values = lcol, breaks=lmod), 
             scale_shape_manual(name="groupes percentages", values = c(19,9))) 
      } +  
      scale_x_continuous(breaks = seq(0,1,by=0.2), limits = c(0,1)) +
      labs(x="Percent of patient") +
      facet_grid(SOC_COD ~ ., scales = "free", space = "free", switch = "y") +
      guides(colour = guide_legend(ncol = 1,nrow=2, byrow = TRUE),
             shape = guide_legend(ncol = 1,nrow=2, byrow = TRUE)) +
      theme(axis.line = element_line(color = "black", linetype = 1),
            axis.ticks.y = element_blank(),
            axis.text.y = element_text(size = 10),
            axis.title.x = element_text(size=12),
            axis.title.y = element_blank(),
            axis.text.x = element_text(hjust = 1, size=10),
            panel.background = element_blank(),
            legend.position="none",
            panel.grid.major = element_line(linewidth = 0.5, color="gray90"),
            strip.text.y.left = element_text(angle = 0, hjust = 1, size = 10),
            strip.placement = "outside",
            panel.spacing.y = unit(3,"pt"))
    q <- ggplotGrob(p1)
    lg <- linesGrob(x=unit(c(1,1),"npc"), y=unit(c(0,1),"npc"), 
                    gp=gpar(col="gray20", lwd=1))
    for (k in grep("strip-l",q$layout$name)) {
      q$grobs[[k]]$grobs[[1]]$children[[1]] <- lg
    }
    
    valXdodge=-0.8
    
    p2 <- ggplot(data1, aes(y=RD, ymin=CI_1, ymax=CI_2, x=reorder(PT_COD,rk))) +
      geom_rect(aes(xmin = PT_start,xmax = PT_end),
                ymin = -Inf,ymax = Inf,
                colour=ifelse(data1$col==1, "gray91","white"),
                fill=ifelse(data1$col==1, "grey91","white"),
                linewidth=5, show.legend = F) +
      geom_vline(aes(xintercept = PT_COD), color = "darkgray", linewidth = 0.05, lty=3) +
      geom_hline(yintercept=0, lty=2, colour = "black", linewidth = 0.5) +
      geom_point(size=2, shape=19, aes(colour= subgroup, shape=subgroup),
                 position = position_dodge(width = valXdodge)) +
      geom_errorbar(width=0, linewidth=1, aes(colour=subgroup),
                    position = position_dodge(width = valXdodge)) +
      geom_text(aes(x=PT_COD, label=test,y=CI_2), col="red", size=6,  hjust=-1, vjust=0.6) +
      scale_color_manual(values = lcol, breaks=lmod) + 
      facet_grid(SOC_COD ~ ., scales = "free", space = "free", switch = "y") +
      coord_flip() +
      theme(axis.line.x = element_line(color = "black", linetype = 1),
            axis.line.y = element_line(color = "gray80", linetype = 1),
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_text(size=12),
            axis.text.x = element_text(hjust = 1, size=10),
            panel.background = element_blank(),
            panel.grid.major = element_line(linewidth = 0.5, color="gray90"),
            legend.title = element_blank(),
            legend.text = element_text(size=10),
            strip.text.y.left = element_blank(),
            strip.placement = "none",
            panel.spacing.y = unit(3,"pt"))
    
    plot_grid(q,  p2, labels = NULL,nrow = 1,rel_widths = c(0.65, 0.35))
  }
  
  #### séparation du graph en nbplot car trop de PT_ affichées
  #On fait des listes de SOC à peu près égales et on diviser les tables et les graphs selon ces deux listes
  # nb_SOC <- length(levels_SOC) 
  # lim <- floor(nb_SOC/nbplot)
  # res <- list()
  # for (i in 1:nbplot){
  #   if (i==1) l_SOC <- levels_SOC[1:lim]
  #   else if (i==nbplot) {
  #     a <- (lim*(i-1))+(i-1)
  #     l_SOC <- levels_SOC[a:nb_SOC]
  #   }
  #   else {
  #     a <- (lim*(i-1))+(i-1)
  #     b <- (lim*i)+(i-1)
  #     l_SOC <- levels_SOC[a:b]
  #   }
  #   
  #   tb_ploti <- subset(tb_plot, SOC_COD %in% l_SOC)
  #   
  #   res[[i]] <- DBplot(tb_ploti)
  # }
  
  plot <- DBplot(tb_plot)
  res <- list(plot = plot, height = 100+length(unique(tb_plot$PT_COD))*20)
  return(res)
}

Dumbbell3grp_epi <- function(data, Overall=TRUE){
  #Liste des id patients dans le bras GC
  list_pat_GC <- unique(df_Tr$USUBJID[df_Tr$ARM == "GC"]) #23 patients
  #Liste des id patients dans le bras GC + Avelumab
  list_pat_AVE <- unique(df_Tr$USUBJID[df_Tr$ARM == "GC + Avelumab"]) #42 patients
  #Ajouter une colonne ARM dans la table data
  data$ARM <- ifelse(data$USUBJID %in% list_pat_GC, "GC", "GC + Avelumab")
  
  frq1 <- data.frame(xtabs(~ PT_COD + ARM, data = data)) #frequence de chaque type d'EI dans chacun des bras de traitement
  frq2 <- df_Tr[df_Tr$TTT_IND=="Yes",] #on récupère une sous table avec seulement les patients ayant eu au moins 1 fois le traitement
  frq2 <- frq2 %>% select(USUBJID, ARM, TTT_IND) %>% distinct(USUBJID, ARM) # liste des patients avec leur bras de traitement
  frq2 <- data.frame(xtabs(~ ARM, data=frq2)) # compte le nombre de patient dans chacun des bras
  
  #######
  df <- data %>% select(USUBJID, ARM, PT_COD) %>% arrange(USUBJID, ARM,PT_COD)
  df <- df %>% group_by(USUBJID, ARM, PT_COD) %>% summarise(Count=n())
  df$SINGLE <- ifelse(df$Count==1, 1,0)
  df$MULTIPLE <- ifelse(df$Count>1, 1, 0)
  
  
  ###################################################
  ### Subgroup single
  ###################################################
  dfS <- df %>% select(-Count,-MULTIPLE)
  dfS <- dfS %>% group_by(ARM, PT_COD) %>% summarise(Count=sum(SINGLE))
  dfS <- dfS %>% pivot_wider(values_from = Count, names_from = ARM, values_fill = 0)
  dfS <- dfS %>% pivot_longer(names_to = "ARM", values_to = "Count", cols = c("GC", "GC + Avelumab"))
  frq3 <- merge(dfS, frq2, by="ARM") 
  frq4 <- frq3 %>%
    mutate(no = Freq - Count, #total - # with event
           yes = Count) %>% # all with AE
    arrange(PT_COD, ARM) %>%
    select(c(ARM, PT_COD, yes, no))
  ########### fontion pour le calcul du RD (pris de fmsb::riskdifference) : modif -> pas de print
  # RDfunct 
  
  ################### calcul de RD #####################
  #Creation d'une table pour accueilir toutes les données nécessaires
  dfx_Single <- setNames(data.frame(matrix(ncol = 6, nrow = 0)), c("Freq_Total", "PT_COD","CI_1", "CI_2", "RD"))
  
  for (p in unique(frq4$PT_COD)){
    #pour chaque PT tableau permettant de calculer la p-value ainsi que le ratio
    df1 <- subset(frq4, PT_COD == p, select = (c(-PT_COD, -ARM)))
    
    dfx <- data.frame(PT_COD = p,
                      subgroup="Single",
                      frqTot = frq4$yes[frq4$PT_COD == p][1] + frq4$yes[frq4$PT_COD == p][2],
                      RD = RDfunct(df1[[1]][1], df1[[1]][2], frq2$Freq[frq2$ARM=="GC"], frq2$Freq[frq2$ARM=="GC + Avelumab"], CRC=TRUE)$estimate,
                      CI_1 = RDfunct(df1[[1]][1], df1[[1]][2], frq2$Freq[frq2$ARM=="GC"], frq2$Freq[frq2$ARM=="GC + Avelumab"], CRC=TRUE)$conf.int[1],
                      CI_2 = RDfunct(df1[[1]][1], df1[[1]][2], frq2$Freq[frq2$ARM=="GC"], frq2$Freq[frq2$ARM=="GC + Avelumab"], CRC=TRUE)$conf.int[2],
                      row.names = NULL)
    # A chaque PT concatenation des tables
    dfx_Single <- rbind(dfx_Single,dfx)
  }
  dfx_Single$pct = round(dfx_Single$frqTot / sum(frq2$Freq),2)
  
  ###################################################
  ### Subgroup Multiple
  ###################################################
  dfM <- df %>% select(-Count,-SINGLE)
  dfM <- dfM %>% group_by(ARM, PT_COD) %>% summarise(Count=sum(MULTIPLE))
  dfM <- dfM %>% pivot_wider(values_from = Count, names_from = ARM, values_fill = 0)
  dfM <- dfM %>% pivot_longer(names_to = "ARM", values_to = "Count", cols = c("GC", "GC + Avelumab"))
  frq3 <- merge(dfM, frq2, by="ARM") 
  frq4 <- frq3 %>%
    mutate(no = Freq - Count, #total - # with event
           yes = Count) %>% # all with AE
    arrange(PT_COD, ARM) %>%
    select(c(ARM, PT_COD, yes, no))
  
  ################### calcul de RD #####################
  #Creation d'une table pour accueilir toutes les données nécessaires
  dfx_Multiple <- setNames(data.frame(matrix(ncol = 6, nrow = 0)), c("Freq_Total", "PT_COD","CI_1", "CI_2", "RD"))
  
  for (p in unique(frq4$PT_COD)){
    #pour chaque PT tableau permettant de calculer la p-value ainsi que le ratio
    df1 <- subset(frq4, PT_COD == p, select = (c(-PT_COD, -ARM)))
    
    dfx <- data.frame(PT_COD = p,
                      subgroup="Multiple",
                      frqTot = frq4$yes[frq4$PT_COD == p][1] + frq4$yes[frq4$PT_COD == p][2],
                      RD = RDfunct(df1[[1]][1], df1[[1]][2], frq2$Freq[frq2$ARM=="GC"], frq2$Freq[frq2$ARM=="GC + Avelumab"], CRC=TRUE)$estimate,
                      CI_1 = RDfunct(df1[[1]][1], df1[[1]][2], frq2$Freq[frq2$ARM=="GC"], frq2$Freq[frq2$ARM=="GC + Avelumab"], CRC=TRUE)$conf.int[1],
                      CI_2 = RDfunct(df1[[1]][1], df1[[1]][2], frq2$Freq[frq2$ARM=="GC"], frq2$Freq[frq2$ARM=="GC + Avelumab"], CRC=TRUE)$conf.int[2],
                      row.names = NULL)
    # A chaque PT concatenation des tables
    dfx_Multiple <- rbind(dfx_Multiple,dfx)
  }
  dfx_Multiple$pct = round(dfx_Multiple$frqTot / sum(frq2$Freq),2)
  
  ###################################################
  ### Group Overall
  ###################################################
  if (Overall==TRUE){
    df_all <- data %>% select(USUBJID, ARM, PT_COD) %>% distinct(USUBJID, ARM,PT_COD)
    frq1 <- data.frame(xtabs(~ PT_COD + ARM, data = df_all)) #frequence de chaque type d'EI dans chacun des bras de traitement
    #Merged
    frq3 <- merge(frq1, frq2, by="ARM") 
    frq4 <- frq3 %>%
      mutate(no = Freq.y - Freq.x, #total - # with event
             yes = Freq.x) %>% # all with AE
      arrange(PT_COD, ARM) %>%
      select(c(ARM, PT_COD, yes, no))
    
    ################### calcul de RD #####################
    #Creation d'une table pour accueilir toutes les données nécessaires
    dfx_all <- setNames(data.frame(matrix(ncol = 6, nrow = 0)), c("Freq_Total", "PT_COD","CI_1", "CI_2", "RD"))
    
    for (p in unique(frq4$PT_COD)){
      #pour chaque PT tableau permettant de calculer la p-value ainsi que le ratio
      df1 <- subset(frq4, PT_COD == p, select = (c(-PT_COD, -ARM)))
      
      dfx <- data.frame(PT_COD = p,
                        subgroup="Overall",
                        frqTot = frq4$yes[frq4$PT_COD == p][1] + frq4$yes[frq4$PT_COD == p][2],
                        RD = RDfunct(df1[[1]][1], df1[[1]][2], frq2$Freq[frq2$ARM=="GC"], frq2$Freq[frq2$ARM=="GC + Avelumab"], CRC=TRUE)$estimate,
                        CI_1 = RDfunct(df1[[1]][1], df1[[1]][2], frq2$Freq[frq2$ARM=="GC"], frq2$Freq[frq2$ARM=="GC + Avelumab"], CRC=TRUE)$conf.int[1],
                        CI_2 = RDfunct(df1[[1]][1], df1[[1]][2], frq2$Freq[frq2$ARM=="GC"], frq2$Freq[frq2$ARM=="GC + Avelumab"], CRC=TRUE)$conf.int[2],
                        row.names = NULL)
      # A chaque PT concatenation des tables
      dfx_all <- rbind(dfx_all,dfx)
    }
    dfx_all$pct = round(dfx_all$frqTot / sum(frq2$Freq),2)
  }
  
  #### table finale pour le plot
  tb_plot <- rbind(dfx_Single, dfx_Multiple)
  if(Overall==TRUE) tb_plot <- rbind(tb_plot, dfx_all)
  
  ################# calcul RD par SOC (pour le tri des SOC) ###################
  frq5 <- data %>% select(USUBJID,ARM,SOC_COD) %>% distinct(USUBJID, ARM, SOC_COD) 
  frq5 <- data.frame(xtabs(~ SOC_COD + ARM, data = frq5))
  #Merged
  frq6 <- merge(frq5, frq2, by="ARM") 
  frq7 <- frq6 %>%
    mutate(no = Freq.y - Freq.x, #total - # with event
           yes = Freq.x) %>% # all with AE
    arrange(SOC_COD, ARM) %>%
    select(c(ARM, SOC_COD, yes, no))
  
  dfx_SOC <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("SOC_COD","RD_SOC"))
  for (s in unique(frq7$SOC_COD)){
    #pour chaque PT tableau permettant de calculer la p-value ainsi que le ratio
    df1 <- subset(frq7, SOC_COD == s, select = (c(-SOC_COD, -ARM)))
    dfx <- data.frame(SOC_COD = s,
                      RD_SOC = RDfunct(df1[[1]][1], df1[[1]][2], frq2$Freq[frq2$ARM=="GC"], frq2$Freq[frq2$ARM=="GC + Avelumab"], CRC=TRUE)$estimate,
                      row.names = NULL)
    # A chaque PT concatenation des tables
    dfx_SOC <- rbind(dfx_SOC,dfx)
  }
  #######################################################
  
  ## limite pour le graph
  # limpct <- ceiling(max(tb_plot$pct)*10) #*10 car on veut prendre à la dizaine supérieure
  # limpct <- limpct/10
  
  ## On ajoute les SOC_COD correspondants aux PT_COD avec une jointure
  tab_SOC <- data %>% select(SOC_COD, PT_COD) %>% group_by(SOC_COD,PT_COD) %>% distinct(SOC_COD,PT_COD)
  tb_plot <- left_join(tb_plot,tab_SOC, by="PT_COD", multiple="all")
  # on ajoute le RD de chaque SOC à la table utilisée pour le p1 pour le rank des SOC
  tb_plot <- merge(tb_plot,dfx_SOC, by="SOC_COD")
  
  ## mettre en évidence par des labels (geom_text) les EIs PT_COD significatifs
  tb_plot$test <- ""
  tb_plot$test[tb_plot$CI_2<0 & tb_plot$CI_1<0]<-"*"
  tb_plot$test[tb_plot$CI_2>0 & tb_plot$CI_1>0]<-"*"
  
  ## variable rang pour ordonner les modalités
  if (Overall==TRUE){
    # on tri selon le RD de Overall donc on fait un pretri sur la base
    tb_plot_rk <- tb_plot[tb_plot$subgroup=="Overall",]
  } else { tb_plot_rk <- tb_plot[tb_plot$subgroup=="Multiple",] }
  tb_rank <- tb_plot_rk %>% distinct(SOC_COD, PT_COD, RD, RD_SOC) %>% arrange(desc(RD_SOC), desc(RD), PT_COD)
  tb_rank$rk <- 1:nrow(tb_rank)
  tb_plot <- merge(tb_plot, tb_rank %>% select(-c(SOC_COD,RD, RD_SOC)), by="PT_COD")
  
  ## création d'une variable bcol (1 si doit être en gris et 0 sinon)
  df_pcol <- tb_plot %>% arrange(rk) %>% distinct(SOC_COD)
  df_pcol$bcol <- 0
  for (i in 1:nrow(df_pcol)){
    if (i %% 2 != 0){
      df_pcol$bcol[i] <- 1
    }
  }
  tb_plot <- merge(tb_plot,df_pcol, by="SOC_COD")
  
  ### création d'un data frame pour les emplacement des couleurs
  # variable start/end : PT_COD du début de la zone et PT_COD de la fin de la zone
  # variable colors pour la couleur
  tb_plot <- tb_plot %>% arrange(rk)
  cptS=2
  cptE=1
  SOC_COD = ""
  PT_start = ""
  PT_end = ""
  PT_start[1] <- tb_plot$PT_COD[1]
  SOC_COD[1] <- tb_plot$SOC_COD[1]
  for (i in 1:(nrow(tb_plot)-1)){
    if (tb_plot$bcol[i]==0 & tb_plot$bcol[i+1]==1){
      PT_start[cptS] <- tb_plot$PT_COD[i+1]
      SOC_COD[cptS] <- tb_plot$SOC_COD[i+1]
      cptS=cptS+1
    }
    if (tb_plot$bcol[i]==1 & tb_plot$bcol[i+1]==0){
      PT_end[cptE] <- tb_plot$PT_COD[i] 
      cptE=cptE+1
    }
  }
  ##si nombre de SOC impaire
  if(length(unique(tb_plot$SOC_COD))%%2!=0)PT_end[cptE] <- tb_plot$PT_COD[nrow(tb_plot)]
  data_breaks1 <- as.data.frame(cbind(SOC_COD, PT_start, PT_end, col=1))
  cptS=1
  cptE=1
  SOC_COD = ""
  PT_start = ""
  PT_end = ""
  for (i in 1:(nrow(tb_plot)-1)){
    if (tb_plot$bcol[i]==1 & tb_plot$bcol[i+1]==0){
      PT_start[cptS] <- tb_plot$PT_COD[i+1] 
      SOC_COD[cptS] <- tb_plot$SOC_COD[i+1]
      cptS=cptS+1
    }
    if (tb_plot$bcol[i]==0 & tb_plot$bcol[i+1]==1){
      PT_end[cptE] <- tb_plot$PT_COD[i]
      cptE=cptE+1
    }
  }
  ##si nombre de SOC paire
  if(length(unique(tb_plot$SOC_COD))%%2==0)PT_end[cptE]<-tb_plot$PT_COD[nrow(tb_plot)]
  data_breaks2 <- as.data.frame(cbind(SOC_COD, PT_start, PT_end, col=0))
  
  data_breaks <- rbind(data_breaks1, data_breaks2)
  data_breaks$SOC_COD <- as.factor(data_breaks$SOC_COD)
  tb_plot$SOC_COD <- as.factor(tb_plot$SOC_COD)
  tb_plot <- left_join(tb_plot,data_breaks, by="SOC_COD")
  
  levels_SOC <- tb_plot %>% select(SOC_COD,rk) %>% arrange(rk)
  levels_SOC <- levels_SOC %>% distinct(SOC_COD)
  levels_SOC <- as.vector(levels_SOC$SOC_COD)
  
  ## vecteur de couleurs et de modalités pour la version avec et sans Overall
  if(Overall==TRUE) {
    lmod <- c("Overall","Single","Multiple")
    lcol <- c("gray40","purple","orange")
  } else {
    lmod <- c("Single","Multiple")
    lcol <- c("purple","orange")
  }
  
  #### plot
  DBplot <- function(data1){
    data1$SOC_COD <- factor(data1$SOC_COD, levels = c(levels_SOC))
    
    p1 <- ggplot(data1,aes(x=pct,y=reorder(PT_COD,rk))) +
      geom_rect(aes(ymin = PT_start, 
                    ymax = PT_end), 
                xmin = -Inf, 
                xmax = Inf,
                colour= ifelse(data1$col==1,"gray91","white"),
                fill=ifelse(data1$col==1, "grey91","white"),
                linewidth=5, show.legend = F) +
      geom_hline(aes(yintercept = PT_COD), color = "darkgray", linewidth = 0.05, lty=3) +
      geom_vline(xintercept = seq(0,1,by=0.2), color = "black", linewidth = 0.05, lty=1) +
      geom_point(size=2.5,aes(colour = factor(subgroup), shape = factor(subgroup))) +
      scale_x_continuous(breaks = seq(0,1,by=0.2), limits = c(0,1)) +
      scale_color_manual(values = lcol, breaks=lmod) + 
      labs(x="Percent of patient") +
      facet_grid(SOC_COD ~ ., scales = "free", space = "free", switch = "y") +
      theme(axis.line = element_line(color = "black", linetype = 1),
            axis.ticks.y = element_blank(),
            axis.text.y = element_text(size = 10),
            axis.title.x = element_text(size=12),
            axis.title.y = element_blank(),
            axis.text.x = element_text(hjust = 1, size=10),
            panel.background = element_blank(),
            legend.position="none",
            panel.grid.major = element_line(linewidth = 0.5, color="gray90"),
            strip.text.y.left = element_text(angle = 0, hjust = 1, size = 10),
            strip.placement = "outside",
            panel.spacing.y = unit(3,"pt"))
    q <- ggplotGrob(p1)
    lg <- linesGrob(x=unit(c(1,1),"npc"), y=unit(c(0,1),"npc"), 
                    gp=gpar(col="gray20", lwd=1))
    for (k in grep("strip-l",q$layout$name)) {
      q$grobs[[k]]$grobs[[1]]$children[[1]] <- lg
    }
    
    valXdodge=-0.8
    
    p2 <- ggplot(data1, aes(y=RD, ymin=CI_1, ymax=CI_2, x=reorder(PT_COD,rk))) +
      geom_rect(aes(xmin = PT_start,xmax = PT_end),
                ymin = -Inf,ymax = Inf,
                colour=ifelse(data1$col==1, "gray91","white"),
                fill=ifelse(data1$col==1, "grey91","white"),
                linewidth=5, show.legend = F) +
      geom_vline(aes(xintercept = PT_COD), color = "darkgray", linewidth = 0.05, lty=3) +
      geom_hline(yintercept=0, lty=2, colour = "black", linewidth = 0.5) +
      geom_point(size=2, shape=19, aes(colour= subgroup, shape=subgroup),
                 position = position_dodge(width = valXdodge)) +
      geom_errorbar(width=0, linewidth=1, aes(colour=subgroup),
                    position = position_dodge(width = valXdodge)) +
      geom_text(aes(x=PT_COD, label=test,y=CI_2), col="red", size=6,  hjust=-1, vjust=0.6) +
      scale_color_manual(values = lcol, breaks=lmod) + 
      facet_grid(SOC_COD ~ ., scales = "free", space = "free", switch = "y") +
      coord_flip() +
      theme(axis.line.x = element_line(color = "black", linetype = 1),
            axis.line.y = element_line(color = "gray80", linetype = 1),
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_text(size=12),
            axis.text.x = element_text(hjust = 1, size=10),
            panel.background = element_blank(),
            panel.grid.major = element_line(linewidth = 0.5, color="gray90"),
            legend.title = element_blank(),
            legend.text = element_text(size=10),
            strip.text.y.left = element_blank(),
            strip.placement = "none",
            panel.spacing.y = unit(3,"pt"))
    
    plot_grid(q,  p2, labels = NULL,nrow = 1,rel_widths = c(0.65, 0.35))
  }
  
  #### séparation du graph en nbplot car trop de PT_ affichées
  #On fait des listes de SOC à peu près égales et on diviser les tables et les graphs selon ces deux listes
  # nb_SOC <- length(levels_SOC) 
  # lim <- floor(nb_SOC/nbplot)
  # res <- list()
  # for (i in 1:nbplot){
  #   if (i==1) l_SOC <- levels_SOC[1:lim]
  #   else if (i==nbplot) {
  #     a <- (lim*(i-1))+(i-1)
  #     l_SOC <- levels_SOC[a:nb_SOC]
  #   }
  #   else {
  #     a <- (lim*(i-1))+(i-1)
  #     b <- (lim*i)+(i-1)
  #     l_SOC <- levels_SOC[a:b]
  #   }
  #   tb_ploti <- subset(tb_plot, SOC_COD %in% l_SOC)
  #   
  #   res[[i]] <- DBplot(tb_ploti)
  # }
  
  plot <- DBplot(tb_plot)
  res <- list(plot = plot, height = 100+length(unique(tb_plot$PT_COD))*20)
  return(res)
}