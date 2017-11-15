HSIClassDensityBS <- function(dat.class, dat.sample, dat.background, units, thresholds, area, R, UnitID = "TID", HSI = "HSI") {
  dens.mat <- perc.mat <- matrix(NA, nrow = nrow(dat.class), ncol = R)
  dat.class <- dat.class %>% tbl_df
  dat.sample <- dat.sample %>% tbl_df
  dat.background <- dat.background %>% tbl_df
  area.per.bkg <- area / nrow(dat.background)
  list.sample <- list.bkg <- list()
  for(t in 1:length(units)) {
    ind <- which(dat.sample[, UnitID] == units[t])
    list.sample[[length(list.sample) + 1]] <- dat.sample[ind, ]
    ind <- which(dat.background[, UnitID] == units[t])
    list.bkg[[length(list.bkg) + 1]] <- dat.background[ind, ]
  }
  names(list.sample) <- names(list.bkg) <- units
  for(r in 1:R) {
    tr <- units[sample(length(units), length(units), replace = T)]
    n <- do.call("rbind", list.sample[tr])
    n <- n[, HSI] %>% as.matrix %>% as.numeric
    g <- do.call("rbind", list.bkg[tr])
    g <- g[, HSI] %>% as.matrix %>% as.numeric
    a <- area.per.bkg * length(g)
    dc <- calcClassDensities(n, g, thresholds, a)
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
