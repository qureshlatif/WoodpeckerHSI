WLR_fit <-
function(dat, formula, Obs = "Nest") { # Fit weighted logistic regression
  w <- rep(1, nrow(dat))
  w[which(dat[, Obs] == 0)] <- sum(dat[, Obs] == 1) / sum(dat[, Obs] == 0)
  dat$w <- w
  mod <- glm(formula, family = binomial, weights = w, data = dat)
  return(mod)
}
