#' ranking_las
#'
#' @description Ranking de chacune des tables pour les graphiques used by
#' PanelLasagnaAlluvialAE()
#'
#' @param table_las A table to rank
#'
#' @return A ranked table
#' @export
#'
ranking_las <- function(table_las){
  l_pat <- unique(table_las$id_pat)
  table_rk_all <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("id_pat", "grade", "rank", "visnum","ARM"))
  if (length(l_pat)!=0 & nrow(table_las)!=0){
    for (c in 1:max(as.numeric(table_las$visnum))){
      table_rk <- table_las[(table_las$visnum == c) & (table_las$id_pat %in% l_pat) & (table_las$grade != 0),]
      if (length(row.names(table_rk) !=0)){
        table_rk <- table_rk[order(table_rk$grade, decreasing = TRUE),]
        table_rk$rank <- (nrow(table_rk_all)+1):(nrow(table_rk)+(nrow(table_rk_all)))
        table_rk_all <- rbind(table_rk_all,table_rk)
        l_pat <- l_pat[!(l_pat %in% table_rk_all$id_pat)]
      }
    }
    table_rk_all <- table_rk_all %>% ungroup() %>% select(-grade,-ARM,-visnum)
    # fusion pour donner un rang à chaque individu de la table
    table_las <- merge(table_las, table_rk_all, by="id_pat")
  }
}
