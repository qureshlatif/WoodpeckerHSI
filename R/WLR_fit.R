WLR_fit <-
function(dat, mod, Obs = "Nest", w = NULL) { # Fit weighted logistic regression
  form <- paste0(Obs, "~", mod)
  if(is.null(w)) {
    w <- rep(1, nrow(dat))
    w[which(dat[, Obs] == 0)] <- sum(dat[, Obs] == 1) / sum(dat[, Obs] == 0)
  } else {
    w <- w
  }
  dat$w <- w
  mod <- glm(form, family = binomial, weights = w, data = dat)
  return(mod)
}
