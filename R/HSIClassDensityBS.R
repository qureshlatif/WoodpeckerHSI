HSIClassDensityBS <- function(dat.class, dat.sample, dat.background, units, thresholds, area, R, UnitID = "TID", HSI = "HSI") {
  dens.mat <- perc.mat <- matrix(NA, nrow = nrow(dat.class), ncol = R)
  dat.class <- dat.class %>% tbl_df
  dat.sample <- dat.sample %>% tbl_df
  dat.background <- dat.background %>% tbl_df
  for(r in 1:R) {
    tr <- units[sample(length(units), length(units), replace = T)]
    ind <- numeric()
    for(t in 1:length(tr)) ind <- c(ind, which((dat.sample[, UnitID] %>% as.matrix %>% as.character) == tr[t]))
    n <- dat.sample[ind, HSI] %>% as.matrix %>% as.numeric
    ind <- numeric()
    for(t in 1:length(tr)) ind <- c(ind, which((dat.background[, UnitID] %>% as.matrix %>% as.character) == tr[t]))
    g <- dat.background[ind, HSI] %>% as.matrix %>% as.numeric
    dc <- HSIClassDensities(n, g, thresholds, area)
    dens.mat[, r] <- dc$Density
    perc.mat[, r] <- (((dc$Density) / sum(dc$Density))*100) %>% round
  }
  dat.class <- cbind(dat.class,
                     Dens95lo =
                       apply(dens.mat, 1, function(x) quantile(x, prob = 0.025, type = 8)),
                     Dens95hi =
                       apply(dens.mat, 1, function(x) quantile(x, prob = 0.975, type = 8)),
                     Perc95lo =
                       apply(perc.mat, 1, function(x) quantile(x, prob = 0.025, type = 8)),
                     Perc95hi = 
                       apply(perc.mat, 1, function(x) quantile(x, prob = 0.975, type = 8))
  )
  return(dat.class)
}
