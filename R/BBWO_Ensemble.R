BBWO_Ensemble <- function(Data,
                          mods = c("BC1", "TBC1", "TB1", "SG_wlr",
                                   "TP_wlr", "TB_wlr", "Maxent_3v", "Maxent_brn"),
                          thrhds = c(0.17,0.17,0.32,0.43,0.43,0.45,0.41,0.37),
                          RAVG = F) {
  if(RAVG == T) thrhds <- c(0.19,0.27,0.23,0.45,0.39,0.45,0.31,0.34)
  names(thrhds) <- mods
  cols<-c(mods, "Ensemble")
  tab <- matrix(NA, nrow = nrow(Data), ncol = length(cols))
  dimnames(tab)[[2]] <- cols
  for (m in 1:length(mods)) 
    tab[, m] <- as.numeric(Data[, mods[m]] >= thrhds[mods[m]])
  tab[, "Ensemble"] <- apply(tab[, -ncol(tab)], 1, sum)
  return(tab)
}
