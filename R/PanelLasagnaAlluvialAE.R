#' PanelLasagnaAlluvialAE
#'
#' @description Combinaison de plusieurs graphiques :
#' \itemize{
#'    \item Lasagna : en lignes les individus (idvar), en colonne l’échelle
#'    temporelle (jours, mois, cycles… -diffère en fonction de l’étude-),
#'    couleur selon le grade (ou une autre variable quali évoluant dans le
#'    temps).
#'    \item Alluvial : idem que pour lasagna mais cette fois chaque individu ne
#'    sera pas toujours sur la même ligne, il changera de ligne en fonction de
#'    son grade pour se placer avec les autres individus du même grade afin de
#'    pouvoir observer une proportion pour chaque grade et à chaque cycle.
#' }
#'
#' @param baseEI la base des EIs avec :
#' \itemize{
#'    \item idvar : l’identifiant du patient : au format character obligatoire
#'    et sans manquants
#'    \item Termsvar : le label des PT ou LLT pour chaque EI : au format
#'    character obligatoire et sans manquants
#'    \item EIdatestart_var : la date de début de l’EI  : au format Date
#'    (important) obligatoire
#'    \item EIdateend_var : la date de fin de l’EI : au format Date (important)
#'    obligatoire
#'    \item gradevar : le grade de l’EI : au format numeric obligatoire en partant de 1 (et allant jusqu’au grade max par exemple 5) et sans manquants
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
#'    \item visitenum_var [optionnal] une variable pour le numéro de la visite
#'    (ou du cycle) : non obligatoire, à indiquer seulement s’il y a un format
#'    de visites ou de cycles dans l’étude. Au format numeric et sans manquants.
#'    \item visitedate_var [optionnal] combinée à, la date de début de ce cycle
#'    (date de visite, de prise de traitement en début de période par exemple) :
#'    idem non obligatoire, à indiquer seulement s’il y a format de visites ou
#'    de cycles dans l’étude. Au format Date et peut avoir des manquantes si
#'    par exmple il n’y a pas eu de prise de traitement à ce cycle là (on aurait
#'     TTTYN=“No”).
#' }
#' @param baseDates
#' \itemize{
#'    \item idvar : l’identifiant du patient : au format character obligatoire
#'    et sans manquants
#'    \item tttdebdate_var : la date de début de traitement : au format Date
#'    obligatoire et sans manquants
#'    \item tttfindate_var : la date de fin de traitement/fin de suivi qui
#'    correspond à la fin de la période traitement par exemple, des EIs peuvent
#'    se situer après cette date. Si le format “cycle” est choisit on arrête le
#'    dernier cycle à cette date, si un autre format est choisit, la date de fin
#'     du graphique sera en fonction de l’option suivi. Si suivi=TRUE alors on
#'     prend tous les EIs enregistrés, sinon si suivi=False alors on termine
#'     l’axe du temps à la date de fin tttfindate_var. Au format Date et sans
#'     manqants (si on choisit l’option suivi=FALSE ou unit="cycle")
#' }
#' @param idvar patient column name
#' @param Termsvar PT or LLT AE label column name
#' @param EIdatestart_var AE start date column name
#' @param EIdateend_var AE end date column name
#' @param gradevar AE grade column name
#' @param TTTYN Treatment column name
#' @param ARMvar Treatment group column name
#' @param visitedate_var Visit date column name
#' @param visitnum_var Visit number column name
#' @param tttdebdate_var Treatment start column name
#' @param tttfindate_var Treatment end column name
#' @param choixEI label de l’EI qui sera représenté (à noter entre guillemets
#' ex : “ANAEMIA”)
#' @param ARMe ndiquer le bras expérimental, il sera placé en premier cad à
#' gauche dans le graphique pour comparer au bras contrôle à droite. Ecrire le
#' label du bras expérimental comme il est dans la base de données
#' (ex: ARMe=“armB”)
#' @param unit unité au choix entre “cycle”/“week”/“month”/“quarter”/“halfyear”/“year”.
#' Comme précisé précédement le cycle peut correspondre à une période entre deux
#' visites ou deux prise de traitement en fonction de l’étude… –> pas de valeur
#' par défaut car dépend de chaque étude.
#' @param barplot TRUE/FALSE choisir si on souhaite afficher la partie supérieure
#' avec les barplots –> par défaut TRUE car est utile pour avoir le pourcentage
#' exacte à chaque unité de temps mais si les périodes représentées sur le
#' graphiques sont trop nombreuses alors peut être diffilement lisible (penser
#' à peut être changer l’unité dans ce cas)
#' @param BPType 11/12 ou 21/22 (en numeric ex BPplot=11) type du bar plot, le
#' permier chiffre pour indiquer si on souhaite afficher la barre grise
#' représentant le pourcentage de patient n’ayant pas eu l’EI au cours du cycle
#' (1) ou non (2), le second chiffre pour indiquer si on souhaite afficher les
#' labels des périodes sous le bar plot (1 pour non et 2 pour oui) (pas
#' forcément nécessaire car déjà présents dans le lasagna et le alluvial et les
#' graphiques sont les uns au dessous des autres) –> par défaut BPType=11 barres
#' grises présentes et sans les labels des période (axe horizontal)
#' @param suivi TRUE/FALSE afficher ou non les EIs ayant eu lieu après la fin du
#' traitement/fin de suivi (tttfindate_var) (automatiquement FALSE si on choisit
#' l’unité “cycle”) –> par défaut FALSE
#' @param listcol vecteur de couleurs à indiquer pour chaque grade donc selon le
#' nombre de modalités de la variable gradevar, doit impérativement être au moins
#' de la même longeur que le nombre de grades présents dans l’étude (ok si 5
#' couleurs alors que grades 1 à 4) mais pas moins et contenir les valeurs des
#' couleurs dans l’ordre des grades (du plus petit au plus grand) –> ex :
#' c(‘#52717F’, ‘#00A991’, ‘#ffb703’, ‘#CF4B00’, ‘#9a031e’) –> n’a pas de valeur
#' pas défaut
#' @param idpat TRUE/FALSE affichage ou non des numéros des patients sur l’axe
#' vertical du lasagna plot (peut être utile de les retirer si les patients sont
#' trop nombreux pour afficher leur numéro) –> par défaut TRUE car graphique
#' utile pour suivre chaque patient
#'
#' @return A lasgna and alluvial plot combined
#' @export
#' @import lubridate
#'
#' @examples
#' library(dplyr)
#' baseEI <- data.frame(idvar = paste0("Patients", round(runif(n = 100, min = 0, max = 100))),
#'                      Termsvar = round(runif(n = 100, min = 0, max = 2))) %>%
#'   dplyr::mutate(SOCvar = round(Termsvar/10)) %>%
#'   dplyr::mutate(across(everything(), .fns = as.character)) %>%
#'   mutate(EIdatestart_var = as.Date(runif(n = nrow(.), 1, 200), origin = "2021-01-01"),
#'          EIdateend_var = as.Date(runif(n = nrow(.), 201, 600), origin = "2021-01-01"),
#'          gradevar = round(runif(n = nrow(.), 1, 5)))
#'
#' baseTr  <- baseEI %>%
#'   dplyr::select(idvar) %>%
#'   dplyr::distinct() %>%
#'   dplyr::mutate(ARMvar = sample(x = c("Placebo", "Treatment"),
#'                                 size = nrow(.),
#'                                 replace = TRUE),
#'                 TTTYN = sample(x = c("Yes", "No"),
#'                                size = nrow(.),
#'                                replace = TRUE,
#'                                prob = c(0.9, 0.1)))
#'
#' baseDates <- baseEI %>%
#'   dplyr::select(idvar) %>%
#'   dplyr::distinct() %>%
#'   mutate(tttdebdate_var = as.Date(runif(n = nrow(.), -100, 0), origin = "2021-01-01"),
#'          tttfindate_var = as.Date(runif(n = nrow(.), 201, 600), origin = "2021-01-01"))
#'
#' PanelLasagnaAlluvialAE(baseEI = baseEI,
#'                        baseTr = baseTr,
#'                        baseDates = baseDates,
#'                        idvar = "idvar",
#'                        Termsvar = "Termsvar",
#'                        EIdatestart_var = "EIdatestart_var",
#'                        EIdateend_var = "EIdateend_var",
#'                        gradevar = "gradevar",
#'                        ARMvar = "ARMvar",
#'                        tttdebdate_var = "tttdebdate_var",
#'                        tttfindate_var = "tttfindate_var",
#'                        choixEI = "2",
#'                        unit = "year")
#'
PanelLasagnaAlluvialAE <- function(baseEI, baseTr, baseDates,
                                   idvar,Termsvar, EIdatestart_var, EIdateend_var, gradevar, TTTYN=NULL,
                                   ARMvar,visitedate_var=NULL,visitnum_var=NULL,tttdebdate_var,tttfindate_var,
                                   choixEI, ARMe=NULL, unit, barplot=TRUE, BPType=11, suivi=FALSE, listcol = NULL, idpat=TRUE){
  #remplacement des noms de variables
  # names(baseEI)[names(baseEI) == id_pat] <- "id_pat"
  baseEI <- baseEI %>% rename("id_pat" = idvar,
                              "COD" = Termsvar,
                              "aedatestart" = EIdatestart_var,
                              "aedateend" = EIdateend_var,
                              "grade" = gradevar)

  # set color if not provided
  if(is.null(listcol)){
    nb_grade <- length(unique(baseEI$grade))
    listcol = viridis::viridis(n = nb_grade)
  }

  baseTr <- baseTr %>% rename("id_pat" = idvar,
                              "ARM" = ARMvar)
  if (!is.null(TTTYN)) baseTr <- baseTr %>% rename("TTTYN" = TTTYN)
  if (!is.null(visitedate_var)) baseTr <- baseTr %>% rename("visdate" = visitedate_var)
  if (!is.null(visitnum_var)) baseTr <- baseTr %>% rename("visnum" = visitnum_var)

  baseDates <- baseDates %>% rename("id_pat" = idvar,
                                    "tttdebdate" = tttdebdate_var,
                                    "tttfindate" = tttfindate_var)

  #on r\u00e9cupère le nombre de modalit\u00e9 de la variable grade pour les \u00e9chelles de couleurs des graphiques
  vect_grade <- sort(unique((baseEI$grade)))


  #### table EI
  #liste des groupes de traitement de la table baseTr
  list_ARM <- unique(baseTr$ARM)
  if (is.null(ARMe)) {
    #Liste des id patients dans le bras num\u00e9ro 1
    list_pat1 <- unique(baseTr$id_pat[baseTr$ARM == list_ARM[1]])
    #Liste des id patients dans le bras num\u00e9ro 2
    list_pat2 <- unique(baseTr$id_pat[baseTr$ARM == list_ARM[2]])
  } else if (!is.null(ARMe)){
    l2 <- unique(baseTr$ARM)
    if (!(ARMe %in% l2)) return("Nom de bras de traitement non correct ou non pr\u00e9sent dans la base.")
    list_ARM[1] <- ARMe
    list_ARM[2] <- l2[l2 != ARMe]

    #Liste des id patients dans le bras num\u00e9ro 1
    list_pat1 <- unique(baseTr$id_pat[baseTr$ARM == list_ARM[1]])
    #Liste des id patients dans le bras num\u00e9ro 2
    list_pat2 <- unique(baseTr$id_pat[baseTr$ARM == list_ARM[2]])
  }
  #Ajouter une colonne ARM dans la table data en faisant correspondre les id_pat selon la liste où ils sont pr\u00e9sents
  baseEI$ARM <- ifelse(baseEI$id_pat %in% list_pat1, "armG", "armD")
  baseTr$ARM <- ifelse(baseTr$ARM==list_ARM[1], "armG","armD")

  #### Ciblage sur l'EI #####
  baseEI2 <- baseEI %>% select(id_pat, COD ,grade, aedatestart, aedateend,ARM)
  if (!(choixEI %in% baseEI2$COD)) return("COD choisit non pr\u00e9sent dans la base de donn\u00e9e")
  baseEI2 <- baseEI2[baseEI2$COD == choixEI,] %>% select(-COD)


  #on merge pour ajouter les diff\u00e9rentes dates dans la table des EIs
  baseEI2 <- merge(baseEI2, baseDates, by="id_pat")

  #on v\u00e9rifie que les dates sont bien au format date sinon message d'erreur
  if (lubridate::is.Date(baseEI2$tttdebdate) == FALSE | lubridate::is.Date(baseEI2$tttfindate) == FALSE | lubridate::is.Date(baseEI2$aedateend) == FALSE | lubridate::is.Date(baseEI2$aedatestart) == FALSE){
    return("Au moins une des variable date n'est pas au format Date")
  }

  # si pas de date de fin et ongo yes alors on met la date la date de suivie
  # autrement dit la plus lointaine dans le graph pour que cet EI soit affich\u00e9 sur tous les cycles qui suivent sa date de d\u00e9but
  # baseEI2 <- baseEI2[!(is.na(baseEI2$aedateend) & baseEI2$ongo!="Yes"),] # on retire les lignes qui n'ont pas de ongoing yes et pas de date de fin
  # baseEI2$aedateend[is.na(baseEI2$aedateend)] <- baseEI2$tttfindate[is.na(baseEI2$aedateend)]

  # retirer ceux termin\u00e9s avant la date de d\u00e9but de traitement
  df_AE3 <- subset(baseEI2,!(baseEI2$aedateend < baseEI2$tttdebdate) | is.na(baseEI2$aedateend))
  # et ceux survenus après la date de fin de traitement seulement si suivi==FALSE et unit diff\u00e9rent de cycle
  if(unit=="cycle" | (unit != "cycle" & suivi==FALSE)) df_AE3 <- subset(df_AE3,!(df_AE3$aedatestart > df_AE3$tttfindate))


  #################################################
  #cr\u00e9ation de la base avec les unit\u00e9s de temps
  ########################################################################################
  if (unit=="cycle"){
    #R\u00e9cup\u00e9ration d'une date par cycle/p\u00e9riode pour chaque individu (car peut y avoir plusieurs dates par cycle/p\u00e9riode)
    df_unitdate <- baseTr %>% select(id_pat, visnum, visdate) %>%
      distinct(id_pat, visnum, visdate) %>%
      group_by(id_pat, visnum,visdate)
    df_unitdate <- df_unitdate %>% filter(!is.na(visdate))
    df_unitdate <- df_unitdate %>% group_by(id_pat,visnum) %>% filter(visdate==min(visdate))

    # Ajout d'une colonne avec la date de fin du cycle qui sera la jour pr\u00e9c\u00e9dent le TTT du cycle suivant
    df_unitdate$DAT_FIN_CYCLE = NA
    for (i in 1:(nrow(df_unitdate)-1)) df_unitdate$DAT_FIN_CYCLE[i] <- df_unitdate$visdate[i+1]
    df_unitdate$DAT_FIN_CYCLE <- as.Date(df_unitdate$DAT_FIN_CYCLE, origin = "1970-01-01")

    maxCycle <- max(df_unitdate$visnum)
    #correction dernier cycle ou on met la date de suivi donc la date de fin de traitement
    for(i in 1:nrow(df_unitdate)){
      if(df_unitdate$visnum[i]== maxCycle){
        df_unitdate$DAT_FIN_CYCLE[i]<-as.Date(baseDates$tttfindate[baseDates$id_pat==df_unitdate$id_pat[i]], format="%d/%m/%Y")
      }
    }
    names(df_unitdate) <- c("id_pat","visnum","DAT_DEB_CYCLE","DAT_FIN_CYCLE")
    df_unitdate$visnum <- as.numeric(df_unitdate$visnum)
  } else if (unit %in% c("week","month","quarter","halfyear","year")){
    if (unit=="week") {pas = 7
    } else if (unit=="month") {pas = 30
    } else if (unit=="quarter") {pas = 90
    } else if (unit=="halfyear") {pas= 180
    } else if (unit=="year") {pas = 365}
    #cr\u00e9ation d'une table avec les dates pour chaque patient des limites pour les semaine, mois, ou ann\u00e9es en fonction de unit
    df_AE_endmax <- df_AE3 %>% select(id_pat, aedateend, tttfindate) %>%
      group_by(id_pat)
    if (nrow(df_AE_endmax==1) & (TRUE %in% is.na(df_AE_endmax$aedateend))) {
      df_AE_endmax <- df_AE_endmax %>%
        filter(tttfindate==max(tttfindate)) %>%
        select(-aedateend) %>%
        rename("aedateend" = tttfindate)
    } else {
      df_AE_endmax <- df_AE_endmax %>% filter(aedateend==max(aedateend)) %>% select(-tttfindate)
    }

    df_unitdate <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("id_pat", "visnum", "DAT_DEB_CYCLE","DAT_FIN_CYCLE"))
    for (p in df_AE_endmax$id_pat){
      df_p <- df_AE_endmax[df_AE_endmax$id_pat==p,]
      start <- unique(df_AE3$tttdebdate[df_AE3$id_pat==p])

      c=1
      bsup <- ifelse(suivi==TRUE, unique(df_p$aedateend),df_AE3$tttfindate[df_AE3$id_pat==p])
      while(start < bsup){
        end <- start+pas
        new_line <- c(p,c,start,end)
        c=c+1
        start <- end

        df_unitdate <- rbind(df_unitdate,new_line)
      }
    }
    names(df_unitdate) <- c("id_pat","visnum","DAT_DEB_CYCLE","DAT_FIN_CYCLE")
    df_unitdate$DAT_FIN_CYCLE <- as.Date(as.numeric(df_unitdate$DAT_FIN_CYCLE), origin = "1970-01-01")
    df_unitdate$DAT_DEB_CYCLE <- as.Date(as.numeric(df_unitdate$DAT_DEB_CYCLE), origin = "1970-01-01")
    df_unitdate$visnum <- as.numeric(df_unitdate$visnum)
  } else return("Unit\u00e9 non valide pour l'option unit")


  ## Attribuer un (ou plusieurs) cycle à chaque EI de la base df_AE en fonction de la date d'occurence et du patient concern\u00e9
  ## si on fait full_join on aura aussi les individus qui n'ont pas eu l'EI anaemia et pr\u00e9sents dans la table df_Tr3
  ## (comme dans le Alluvial)
  df_unitdate$id_pat <- as.character(df_unitdate$id_pat)
  df_AE4 <- left_join(df_AE3, df_unitdate, by = "id_pat", relationship = "many-to-many")

  #on garde les grades pour les lignes où la date de d\u00e9but et de fin de l'EI correspondent au cycle
  #sinon on remplace par 0 pour indiquer qu'a ce cycle le patient n'avait pas d'EI (grade 0)
  df_AE4$grade[ ((df_AE4$aedatestart > df_AE4$DAT_FIN_CYCLE) |
                      (df_AE4$aedateend < df_AE4$DAT_DEB_CYCLE))] =0
  df_AE4 <- df_AE4 %>% select(ARM, id_pat, visnum,grade) %>% arrange(ARM, id_pat, visnum, desc(grade))


  ## On ne garde que le grade max s'il y a plusieurs occurrence de cet EI dans un même cycle
  df_AE4 <- df_AE4 %>% distinct(id_pat,ARM, visnum, grade) %>%
    group_by(id_pat,ARM,visnum) %>% dplyr::filter(grade==max(grade))

  df_AE4$grade <- as.factor(df_AE4$grade)
  df_AE4$visnum <- as.factor(df_AE4$visnum)
  df_AE4$id_pat <- as.factor(df_AE4$id_pat)
  df_AE4$ARM <- as.factor(df_AE4$ARM)

  ## S\u00e9paration en deux sous-tables par bras pour en faire 2 graphiques à comparer
  df_AE4_1_Las <- df_AE4[df_AE4$ARM == "armG",]
  df_AE4_2_Las <- df_AE4[df_AE4$ARM == "armD",]

  if(nrow(df_AE4_1_Las)!=0) df_AE4_1_Las <- ranking_las(df_AE4_1_Las)
  if(nrow(df_AE4_2_Las)!=0) df_AE4_2_Las <- ranking_las(df_AE4_2_Las)

  ################# Plot ##########################
  if(nrow(df_AE4_1_Las)!=0) p1 <- LasagnaAE(data = df_AE4_1_Las, idpat = idpat, unit = unit, choixEI = choixEI, vect_grade = vect_grade, listcol = listcol)
  if(nrow(df_AE4_2_Las)!=0) p2 <- LasagnaAE(df_AE4_2_Las, idpat = idpat, unit = unit, choixEI = choixEI, vect_grade = vect_grade, listcol = listcol)

  ########################### Alluvial #############
  #Comptage du nombre de patient dans chacun des bras
  if(is.null(TTTYN)){
    df_Tr2 <- baseTr
  } else {
   df_Tr2  <- baseTr[baseTr$TTTYN=="Yes",]
  }
  USU_distinct  <- df_Tr2  %>% select(id_pat, ARM) %>% distinct(id_pat, ARM)
  frq2 <- data.frame(xtabs(~ ARM, data=USU_distinct))

  if(nrow(df_AE4_1_Las)==0) df_AE5 <- df_AE4_2_Las %>% select(-rank)
  if(nrow(df_AE4_2_Las)==0) df_AE5 <- df_AE4_1_Las %>% select(-rank)
  if(nrow(df_AE4_2_Las)!=0 & nrow(df_AE4_1_Las)!=0) df_AE5 <- rbind(df_AE4_2_Las,df_AE4_1_Las) %>% select(-rank)

  l_all <- unique(baseTr$id_pat)
  if (!is.null(TTTYN)){
    if (unit=="cycle"){
      #Dans cette table on ne garde pas les individus qui n'ont pas eu le traitement
      # cf : pas de yes dans TTT_IND pour cet individu à chaque cycle
      list_NTTT <- list()
      for (c in 1:max(as.numeric(baseTr$visnum))){
        #liste des id des patients ayant reçu au moins une fois le traitement donc patients expos\u00e9s à ce cycle c
        l_Y <- unique(baseTr$id_pat[baseTr$TTTYN=="Yes" & baseTr$visnum == c])
        l_N <- l_all[!(l_all %in% l_Y)]
        list_NTTT[[c]] <- l_N
      }
      # on ajoute à chaque cycle les id_pat manquants (qui n'ont pas reçu le traitement à certains cycles)
      for (c in 1:max(as.numeric(df_unitdate$visnum))){
        if(length(list_NTTT[[c]])!=0){
          df_add <- data.frame(id_pat=as.factor(list_NTTT[[c]]),
                               ARM=as.factor(ifelse(list_NTTT[[c]] %in% list_pat1, "armG","armD")),
                               visnum=as.factor(c),grade="NA")
          df_AE5 <- rbind(df_AE5, df_add)
        }
      }
    } else if (unit!="cycle"){
      list_NTTT <- list()
      #liste des id des patients ayant reçu au moins une fois le traitement donc patients expos\u00e9s à ce cycle c
      l_Y <- unique(baseTr$id_pat[baseTr$TTTYN=="Yes"])
      list_NTTT <- l_all[!(l_all %in% l_Y)]

      # on ajoute à chaque cycle les id_pat manquants (qui n'ont pas reçu le traitement du tout)
      for (c in 1:max(as.numeric(df_unitdate$visnum))){
        if(length(list_NTTT)!=0){
          df_add <- data.frame(id_pat=as.factor(list_NTTT),
                               ARM=as.factor(ifelse(list_NTTT %in% list_pat1, "armG","armD")),
                               visnum=as.factor(c),grade="NA")
          df_AE5 <- rbind(df_AE5, df_add)
        }
      }
    }
  }

  #ajout des id_pat qui n'ont pas eu l'EI
  for (c in 1:max(as.numeric(df_unitdate$visnum))){
    l_add <- l_all[!(l_all %in% df_AE5$id_pat[df_AE5$visnum==c])]
    if(length(l_add)!=0){
      df_add <- data.frame(id_pat=as.factor(l_add),
                           ARM=as.factor(ifelse(l_add %in% list_pat1, "armG","armD")),
                           visnum=as.factor(c),grade=as.factor(0))
      df_AE5 <- rbind(df_AE5, df_add)
    }
  }



  #si pas du tout de pat ayant l'EI dans un des deux groupes alors on supprime les lignes ajout\u00e9es pour ce groupe
  if(nrow(df_AE4_1_Las)==0) {
    df_AE5_2_All <- subset(df_AE5, ARM=="armD")
    df_AE5_1_All <- as.data.frame(NULL)
  } else if(nrow(df_AE4_2_Las)==0) {
    df_AE5_1_All <- subset(df_AE5, ARM=="armG")
    df_AE5_2_All <- as.data.frame(NULL)
  } else if(nrow(df_AE4_2_Las)!=0 & nrow(df_AE4_1_Las)!=0){
    ## S\u00e9paration en deux sous-tables par bras pour en faire 2 graphiques à comparer
    df_AE5_1_All <- df_AE5[df_AE5$ARM == "armG",]
    df_AE5_2_All <- df_AE5[df_AE5$ARM == "armD",]
  }

  ### cr\u00e9ation de deux variables breaks pour afficher les % au deux graphique
  # on cherche pour quelles valeurs de chaque groupe on obtient 0, 25, 50, 75 et 100%
  print("Alluvial")
  if(nrow(df_AE4_1_Las)!=0) p3 <- AlluvialAE(data = df_AE5_1_All, N = frq2$Freq[frq2$ARM=="armG"],
                                             choixEI = choixEI, vect_grade = vect_grade, listcol = listcol, unit = unit)
  if(nrow(df_AE4_2_Las)!=0) p4 <- AlluvialAE(df_AE5_2_All, frq2$Freq[frq2$ARM=="armD"],
                                             choixEI = choixEI, vect_grade = vect_grade, listcol = listcol, unit = unit)

  ################# diagrammes en barres à chaque cycle
  print("barplot")
  if (barplot==TRUE){
    if(nrow(df_AE4_1_Las)!=0) b1 <- BarChart(df_AE5_1_All,list_ARM[1],
                                             Type = BPType,
                                             choixEI = choixEI, vect_grade = vect_grade, listcol = listcol)
    if(nrow(df_AE4_2_Las)!=0) b2 <- BarChart(df_AE5_2_All,list_ARM[2],
                                             Type = BPType,
                                             choixEI = choixEI, vect_grade = vect_grade, listcol = listcol)
  }

  ### avec p1 et p2 du script Lasagna plot
  ### et b1 et b2 diagrammes en barres cr\u00e9\u00e9 agalement dans le script LasagnaPlot (mais avec les tables de ce script)
  if(nrow(df_AE4_2_Las)==0){
    if (barplot==TRUE) top_row <- plot_grid(b1,labels = NULL,ncol = 2,rel_widths = c(0.5, 0.5))
    mid_row <- plot_grid(p1,labels = NULL,ncol = 2,rel_widths = c(0.5, 0.5))
    bottom_row <- plot_grid(p3,labels = NULL,ncol = 2,rel_widths = c(0.5, 0.5))
  }
  if(nrow(df_AE4_1_Las)==0){
    if (barplot==TRUE) top_row <- plot_grid(NULL,b2,labels = NULL,ncol = 2,rel_widths = c(0.5, 0.5))
    mid_row <- plot_grid(NULL,p2,labels = NULL,ncol = 2,rel_widths = c(0.5, 0.5))
    bottom_row <- plot_grid(NULL,p4,labels = NULL,ncol = 2,rel_widths = c(0.5, 0.5))
  }
  if(nrow(df_AE4_2_Las)!=0 & nrow(df_AE4_1_Las)!=0){
    if (barplot==TRUE) top_row <- plot_grid(b1, b2,labels = NULL,ncol = 2,rel_widths = c(0.5, 0.5))
    mid_row <- plot_grid(p1, p2,labels = NULL,ncol = 2,rel_widths = c(0.5, 0.5))
    bottom_row <- plot_grid(p3, p4,labels = NULL,ncol = 2,rel_widths = c(0.5, 0.5))
  }
  if (barplot==TRUE) {p <- plot_grid(top_row, mid_row ,bottom_row,labels = NULL,nrow = 3,rel_heights = c(0.2,0.4, 0.5))
  } else {p<- plot_grid(mid_row ,bottom_row,labels = NULL,nrow = 2,rel_heights = c(0.45, 0.55))}

  return(p)
}
