BBWO_Mxntbrn_ravg <- function(dnbr) {
  l.dnbr <- c(4.794391627055965, -822.0, 1186.0)
  dnbr[which(dnbr>l.dnbr[3])] <- l.dnbr[3]
  dnbr[which(dnbr<l.dnbr[2])] <- l.dnbr[2]
  linPN <- 4.794391627055965
  densNorm <- 1881.7073084751894
  entropy <- 8.961973486301357
  exponent <- l.dnbr[1]*((dnbr-l.dnbr[2])/(l.dnbr[3]-l.dnbr[2])) - linPN
  mx.raw  <- exp(exponent)/densNorm
  HSI <- (mx.raw*exp(entropy))/(1+mx.raw*exp(entropy))
  return(HSI)
}
