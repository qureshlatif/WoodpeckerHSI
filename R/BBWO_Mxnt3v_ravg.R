BBWO_Mxnt3v_ravg <- function(cosasp,dnbr,loccc40) {
  #Define features and parameters
  l.cosasp <- c(1.1380568510172837, -1.0, 1.0)
  
  l.dnbr <- c(16.542824885407935, -822.0, 1186.0)
  dnbr[which(dnbr>l.dnbr[3])]<-l.dnbr[3]
  dnbr[which(dnbr<l.dnbr[2])]<-l.dnbr[2]
  
  l.loccc40 <- c(1.3911901695916924, 0.0, 1.0)
  
  l.cosasp2 <- c(-0.1999789813675077, 0.0, 1.0)
  cosasp2 <- cosasp^2
  
  l.dnbr2 <- c(-9.423195511111064, 0.0, 1406596.0)
  dnbr2 <- dnbr^2
  dnbr2[which(dnbr2>l.dnbr2[3])]<-l.dnbr2[3]
  dnbr2[which(dnbr2<l.dnbr2[2])]<-l.dnbr2[2]
  
  l.loccc402 <- c(-1.8029770810743269, 0.0, 1.0)
  loccc402 <- loccc40^2
  
  l.ca_x_dnbr <- c(0.6234066025868835, -1135.38993288, 1072.0689954)
  ca_x_dnbr <- cosasp*dnbr
  ca_x_dnbr[which(ca_x_dnbr>l.ca_x_dnbr[3])]<-l.ca_x_dnbr[3]
  ca_x_dnbr[which(ca_x_dnbr<l.ca_x_dnbr[2])]<-l.ca_x_dnbr[2]
  
  l.ca_x_lcc40 <- c(-0.7642665313804556, -1.00000000000044, 1.000033333333)
  ca_x_lcc40 <- cosasp*loccc40
  ca_x_lcc40[which(ca_x_lcc40>l.ca_x_lcc40[3])]<-l.ca_x_lcc40[3]
  ca_x_lcc40[which(ca_x_lcc40<l.ca_x_lcc40[2])]<-l.ca_x_lcc40[2]
  
  l.dnbr_x_lcc40 <- c(3.0233898606701226, -363.23260000000005, 1186.0)
  dnbr_x_lcc40 <- dnbr*loccc40
  dnbr_x_lcc40[which(dnbr_x_lcc40>l.dnbr_x_lcc40[3])]<-l.dnbr_x_lcc40[3]
  dnbr_x_lcc40[which(dnbr_x_lcc40<l.dnbr_x_lcc40[2])]<-l.dnbr_x_lcc40[2]
  
  linPN <- 11.828167086326792
  densNorm <- 2569.3057793611993
  entropy <- 8.781253071131154
  
  exponent <- (l.cosasp[1]*((cosasp-l.cosasp[2])/(l.cosasp[3]-l.cosasp[2])) +
                 l.dnbr[1]*((dnbr-l.dnbr[2])/(l.dnbr[3]-l.dnbr[2])) +
                 l.loccc40[1]*((loccc40-l.loccc40[2])/(l.loccc40[3]-l.loccc40[2])) +
                 l.cosasp2[1]*((cosasp2-l.cosasp2[2])/(l.cosasp2[3]-l.cosasp2[2])) +
                 l.dnbr2[1]*((dnbr2-l.dnbr2[2])/(l.dnbr2[3]-l.dnbr2[2])) +
                 l.loccc402[1]*((loccc402-l.loccc402[2])/(l.loccc402[3]-l.loccc402[2])) +
                 l.ca_x_dnbr[1]*((ca_x_dnbr-l.ca_x_dnbr[2])/(l.ca_x_dnbr[3]-l.ca_x_dnbr[2])) +
                 l.ca_x_lcc40[1]*((ca_x_lcc40-l.ca_x_lcc40[2])/(l.ca_x_lcc40[3]-l.ca_x_lcc40[2])) +
                 l.dnbr_x_lcc40[1]*((dnbr_x_lcc40-l.dnbr_x_lcc40[2])/(l.dnbr_x_lcc40[3]-l.dnbr_x_lcc40[2]))
  ) - linPN
  mx.raw  <- exp(exponent)/densNorm
  HSI <- (mx.raw*exp(entropy))/(1+mx.raw*exp(entropy))
  return(HSI)
}
