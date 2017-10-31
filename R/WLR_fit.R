WLR_fit <-
function(dat, mod, Obs = "Nest") { # Fit weighted logistic regression
  form <- paste0(Obs, "~", mod)
  w <- rep(1, nrow(dat))
  w[which(dat[, Obs] == 0)] <- sum(dat[, Obs] == 1) / sum(dat[, Obs] == 0)
  dat$w <- w
  mod <- glm(form, family = binomial, weights = w, data = dat)
  return(mod)
}
