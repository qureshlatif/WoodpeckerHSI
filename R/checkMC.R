checkMC <- function(data, vars) {
  VIF <- numeric(length=length(vars))
  for(i in 1:length(vars)) {
    x <- paste(vars, collapse="+")
    m <- lm(as.formula(paste(vars[i],"~",x,sep="")), data = data)
    Rsqr <- summary(m)$r.squared
    VIF[i] <- 1/(1-Rsqr) #Variance Inflation Factor
  }
  names(VIF) <- vars
  cmat <- cor(data[, vars])
  out <- list(VIF = VIF, cmat = cmat)
  return(out)
}