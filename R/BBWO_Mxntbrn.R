BBWO_Mxntbrn <- function(dnbr) {
  l.dnbr <- c(5.393373689029553, -590.47619629, 1024.12268066)
  dnbr[which(dnbr>l.dnbr[3])] <- l.dnbr[3]
  dnbr[which(dnbr<l.dnbr[2])] <- l.dnbr[2]
  linPN <- 5.393373689029553
  densNorm <- 1369.5883369712144
  entropy <- 8.899831912829168
  exponent <- l.dnbr[1]*((dnbr-l.dnbr[2])/(l.dnbr[3]-l.dnbr[2])) - linPN
  mx.raw  <- exp(exponent)/densNorm
  HSI <- (mx.raw*exp(entropy))/(1+mx.raw*exp(entropy))
  return(HSI)
}
