WLR_AICtable <- function(data, mods, Obs = "Nest", AICc = T) {
  ifelse(AICc, IC <- "AICc", IC = "AIC")
  out <- data.frame()
  n <- sum(data[, Obs] == 1)
  mod <- WLR_fit(data, Nest~NULL, Obs = Obs) # Intercept-only model
  K <- length(coef(mod))
  ifelse(AICc == T, ic <- mod$deviance+2*K*(n/(n-K-1)), ic <- mod$deviance+2*K)
  add <- c("Null", ic, 1)
  out <- rbind(out, add, stringsAsFactors = F)
  names(out) <- c("Model", IC, "K")
  out$Model <- as.character(out$Model)
  out[, IC] <- as.numeric(out[, IC])
  out$K<-as.numeric(out$K)
  
  for (j in 1:length(mods)) {
    mod <- WLR_fit(data, as.formula(paste("Nest~", mods[[j]], sep="")), Obs = Obs) 
    K <- length(coef(mod))
    ifelse(AICc == T, ic <- mod$deviance+2*K*(n/(n-K-1)), ic <- mod$deviance+2*K)
    out$Model[j + 1] <- mods[[j]]
    out[j + 1, 2] <- ic
    out$K <- length(coef(mod))
  }
  out <- out[order(out[, IC]), ]
  out$dAIC <- out[, IC] - out[1, IC]
  wt.no <- exp(-0.5*out$dAIC)
  out$m_wt <- wt.no / (sum(wt.no))
  return(out)
}
