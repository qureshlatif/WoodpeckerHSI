HSI_Mahal <- function (Cal, Lnd, vars = dimnames(Cal)[[2]]) {
  HSI <- numeric(length = nrow(Lnd))
  
  mns <- apply(Cal[, vars], 2, mean)
  sds <- apply(Cal[, vars], 2, sd)
  cal <- t(apply(Cal[, vars], 1, function(x) (x - mns)/sds))
  lnd <- t(apply(Lnd[, vars], 1, function(x) (x - mns)/sds))
  
  cnt <- apply(cal, 2, mean)
  cv <- cov(cal)
  dgf <- ncol(cal)          
  D.lnd <- mahalanobis(lnd, cnt, cv)
  p.lnd <- 1-pchisq(D.lnd, dgf)
  HSI <- p.lnd
  return(HSI)
}
