% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PanelLasagnaAlluvialAE.R
\name{PanelLasagnaAlluvialAE}
\alias{PanelLasagnaAlluvialAE}
\title{PanelLasagnaAlluvialAE}
\usage{
PanelLasagnaAlluvialAE(
  baseEI,
  baseTr,
  baseDates,
  idvar,
  Termsvar,
  EIdatestart_var,
  EIdateend_var,
  gradevar,
  TTTYN = NULL,
  ARMvar,
  visitedate_var = NULL,
  visitnum_var = NULL,
  tttdebdate_var,
  tttfindate_var,
  choixEI,
  ARMe = NULL,
  unit,
  barplot = TRUE,
  BPType = 11,
  suivi = FALSE,
  listcol = NULL,
  idpat = TRUE
)
}
\arguments{
\item{baseEI}{la base des EIs avec :
\itemize{
   \item idvar : l’identifiant du patient : au format character obligatoire
   et sans manquants
   \item Termsvar : le label des PT ou LLT pour chaque EI : au format
   character obligatoire et sans manquants
   \item EIdatestart_var : la date de début de l’EI  : au format Date
   (important) obligatoire
   \item EIdateend_var : la date de fin de l’EI : au format Date (important)
   obligatoire
   \item gradevar : le grade de l’EI : au format numeric obligatoire en partant de 1 (et allant jusqu’au grade max par exemple 5) et sans manquants
}}

\item{baseTr}{la base des traitements avec
\itemize{
   \item idvar : l’identifiant du patient : au format character obligatoire
   et sans manquants
   \item ARMvar : le bras de traitement pour chaque individu : au format
   character obligatoire et sans manquants
   \item TTTYN [optionnal] une variable pour indiquer si le traitement à été pris
   (sert à détecter les patients “à risque”) on recherchera les
   occurences en “Yes” donc doit être codée en “Yes”/“No” en character
   \item visitenum_var [optionnal] une variable pour le numéro de la visite
   (ou du cycle) : non obligatoire, à indiquer seulement s’il y a un format
   de visites ou de cycles dans l’étude. Au format numeric et sans manquants.
   \item visitedate_var [optionnal] combinée à, la date de début de ce cycle
   (date de visite, de prise de traitement en début de période par exemple) :
   idem non obligatoire, à indiquer seulement s’il y a format de visites ou
   de cycles dans l’étude. Au format Date et peut avoir des manquantes si
   par exmple il n’y a pas eu de prise de traitement à ce cycle là (on aurait
    TTTYN=“No”).
}}

\item{baseDates}{\itemize{
   \item idvar : l’identifiant du patient : au format character obligatoire
   et sans manquants
   \item tttdebdate_var : la date de début de traitement : au format Date
   obligatoire et sans manquants
   \item tttfindate_var : la date de fin de traitement/fin de suivi qui
   correspond à la fin de la période traitement par exemple, des EIs peuvent
   se situer après cette date. Si le format “cycle” est choisit on arrête le
   dernier cycle à cette date, si un autre format est choisit, la date de fin
    du graphique sera en fonction de l’option suivi. Si suivi=TRUE alors on
    prend tous les EIs enregistrés, sinon si suivi=False alors on termine
    l’axe du temps à la date de fin tttfindate_var. Au format Date et sans
    manqants (si on choisit l’option suivi=FALSE ou unit="cycle")
}}

\item{idvar}{patient column name}

\item{Termsvar}{PT or LLT AE label column name}

\item{EIdatestart_var}{AE start date column name}

\item{EIdateend_var}{AE end date column name}

\item{gradevar}{AE grade column name}

\item{TTTYN}{Treatment column name}

\item{ARMvar}{Treatment group column name}

\item{visitedate_var}{Visit date column name}

\item{visitnum_var}{Visit number column name}

\item{tttdebdate_var}{Treatment start column name}

\item{tttfindate_var}{Treatment end column name}

\item{choixEI}{label de l’EI qui sera représenté (à noter entre guillemets
ex : “ANAEMIA”)}

\item{ARMe}{ndiquer le bras expérimental, il sera placé en premier cad à
gauche dans le graphique pour comparer au bras contrôle à droite. Ecrire le
label du bras expérimental comme il est dans la base de données
(ex: ARMe=“armB”)}

\item{unit}{unité au choix entre “cycle”/“week”/“month”/“quarter”/“halfyear”/“year”.
Comme précisé précédement le cycle peut correspondre à une période entre deux
visites ou deux prise de traitement en fonction de l’étude… –> pas de valeur
par défaut car dépend de chaque étude.}

\item{barplot}{TRUE/FALSE choisir si on souhaite afficher la partie supérieure
avec les barplots –> par défaut TRUE car est utile pour avoir le pourcentage
exacte à chaque unité de temps mais si les périodes représentées sur le
graphiques sont trop nombreuses alors peut être diffilement lisible (penser
à peut être changer l’unité dans ce cas)}

\item{BPType}{11/12 ou 21/22 (en numeric ex BPplot=11) type du bar plot, le
permier chiffre pour indiquer si on souhaite afficher la barre grise
représentant le pourcentage de patient n’ayant pas eu l’EI au cours du cycle
(1) ou non (2), le second chiffre pour indiquer si on souhaite afficher les
labels des périodes sous le bar plot (1 pour non et 2 pour oui) (pas
forcément nécessaire car déjà présents dans le lasagna et le alluvial et les
graphiques sont les uns au dessous des autres) –> par défaut BPType=11 barres
grises présentes et sans les labels des période (axe horizontal)}

\item{suivi}{TRUE/FALSE afficher ou non les EIs ayant eu lieu après la fin du
traitement/fin de suivi (tttfindate_var) (automatiquement FALSE si on choisit
l’unité “cycle”) –> par défaut FALSE}

\item{listcol}{vecteur de couleurs à indiquer pour chaque grade donc selon le
nombre de modalités de la variable gradevar, doit impérativement être au moins
de la même longeur que le nombre de grades présents dans l’étude (ok si 5
couleurs alors que grades 1 à 4) mais pas moins et contenir les valeurs des
couleurs dans l’ordre des grades (du plus petit au plus grand) –> ex :
c(‘#52717F’, ‘#00A991’, ‘#ffb703’, ‘#CF4B00’, ‘#9a031e’) –> n’a pas de valeur
pas défaut}

\item{idpat}{TRUE/FALSE affichage ou non des numéros des patients sur l’axe
vertical du lasagna plot (peut être utile de les retirer si les patients sont
trop nombreux pour afficher leur numéro) –> par défaut TRUE car graphique
utile pour suivre chaque patient}
}
\value{
A lasgna and alluvial plot combined
}
\description{
Combinaison de plusieurs graphiques :
\itemize{
   \item Lasagna : en lignes les individus (idvar), en colonne l’échelle
   temporelle (jours, mois, cycles… -diffère en fonction de l’étude-),
   couleur selon le grade (ou une autre variable quali évoluant dans le
   temps).
   \item Alluvial : idem que pour lasagna mais cette fois chaque individu ne
   sera pas toujours sur la même ligne, il changera de ligne en fonction de
   son grade pour se placer avec les autres individus du même grade afin de
   pouvoir observer une proportion pour chaque grade et à chaque cycle.
}
}
\examples{
library(dplyr)
baseEI <- data.frame(idvar = paste0("Patients", round(runif(n = 100, min = 0, max = 100))),
                     Termsvar = round(runif(n = 100, min = 0, max = 2))) \%>\%
  dplyr::mutate(SOCvar = round(Termsvar/10)) \%>\%
  dplyr::mutate(across(everything(), .fns = as.character)) \%>\%
  mutate(EIdatestart_var = as.Date(runif(n = nrow(.), 1, 200), origin = "2021-01-01"),
         EIdateend_var = as.Date(runif(n = nrow(.), 201, 600), origin = "2021-01-01"),
         gradevar = round(runif(n = nrow(.), 1, 5)))

baseTr  <- baseEI \%>\%
  dplyr::select(idvar) \%>\%
  dplyr::distinct() \%>\%
  dplyr::mutate(ARMvar = sample(x = c("Placebo", "Treatment"),
                                size = nrow(.),
                                replace = TRUE),
                TTTYN = sample(x = c("Yes", "No"),
                               size = nrow(.),
                               replace = TRUE,
                               prob = c(0.9, 0.1)))

baseDates <- baseEI \%>\%
  dplyr::select(idvar) \%>\%
  dplyr::distinct() \%>\%
  mutate(tttdebdate_var = as.Date(runif(n = nrow(.), -100, 0), origin = "2021-01-01"),
         tttfindate_var = as.Date(runif(n = nrow(.), 201, 600), origin = "2021-01-01"))

PanelLasagnaAlluvialAE(baseEI = baseEI,
                       baseTr = baseTr,
                       baseDates = baseDates,
                       idvar = "idvar",
                       Termsvar = "Termsvar",
                       EIdatestart_var = "EIdatestart_var",
                       EIdateend_var = "EIdateend_var",
                       gradevar = "gradevar",
                       ARMvar = "ARMvar",
                       tttdebdate_var = "tttdebdate_var",
                       tttfindate_var = "tttfindate_var",
                       choixEI = "2",
                       unit = "year")

}
