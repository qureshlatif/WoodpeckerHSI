WHWO_green_simplified <-
function(LOCCC,LANDCC,CASP,PIPO,SLP) {
  #Define features and parameters
  L.loccc <- c(-3.1899511465315213, 0.0, 90.2546)
  L.landcc <- c(0.150519912201841, 0.291998, 74.3617)
  L.casp <- c(0.20293431644156784, -1.0, 1.0)
  L.pipo <- c(1.6313917199925456, 0.352009386916984, 99.7066588442358)
  L.slp <- c(2.9544345540039716, 0.0, 38.0)
  L.loccc2 <- c(-0.4731626863137912, 0.0, 8145.89282116)
  L.landcc2 <- c(3.1611917853347435, 0.08526283200399999, 5529.66242689)
  L.casp2 <- c(-0.6169494414158898, 0.0, 1.0)
  L.slp2 <- c(-5.24621625797648, 0.0, 1444.0)
  
  b.loccc <- L.loccc[1]
  loccc <- LOCCC
  loccc[which(loccc>L.loccc[3])] <- L.loccc[3]
  loccc[which(loccc<L.loccc[2])] <- L.loccc[2]
  
  b.landcc <- L.landcc[1]
  landcc <- LANDCC
  landcc[which(landcc>L.landcc[3])] <- L.landcc[3]
  landcc[which(landcc<L.landcc[2])] <- L.landcc[2]
  
  b.casp <- L.casp[1]
  casp <- CASP
  casp[which(casp>L.casp[3])] <- L.casp[3]
  casp[which(casp<L.casp[2])] <- L.casp[2]
  
  b.pipo <- L.pipo[1]
  pipo <- PIPO
  pipo[which(pipo>L.pipo[3])] <- L.pipo[3]
  pipo[which(pipo<L.pipo[2])] <- L.pipo[2]
  
  b.slp <- L.slp[1]
  slp <- SLP
  slp[which(slp>L.slp[3])] <- L.slp[3]
  slp[which(slp<L.slp[2])] <- L.slp[2]
  
  b.loccc2 <- L.loccc2[1]
  loccc2 <- LOCCC^2
  loccc2[which(loccc2>L.loccc2[3])] <- L.loccc2[3]
  loccc2[which(loccc2<L.loccc2[2])] <- L.loccc2[2]
  
  b.landcc2 <- L.landcc2[1]
  landcc2 <- LANDCC^2
  landcc2[which(landcc2>L.landcc2[3])] <- L.landcc2[3]
  landcc2[which(landcc2<L.landcc2[2])] <- L.landcc2[2]
  
  b.casp2 <- L.casp2[1]
  casp2 <- CASP^2
  casp2[which(casp2>L.casp2[3])] <- L.casp2[3]
  casp2[which(casp2<L.casp2[2])] <- L.casp2[2]
  
  b.slp2 <- L.slp2[1]
  slp2 <- SLP^2
  slp2[which(slp2>L.slp2[3])] <- L.slp2[3]
  slp2[which(slp2<L.slp2[2])] <- L.slp2[2]
  
  linPN <- 2.725653681930215
  densNorm <- 1802.5585412012722
  entropy <- 9.087756937905286
  
  exponent <- (b.loccc*((loccc-L.loccc[2])/(L.loccc[3]-L.loccc[2])) +
                 b.landcc*((landcc-L.landcc[2])/(L.landcc[3]-L.landcc[2])) +
                 b.casp*((casp-L.casp[2])/(L.casp[3]-L.casp[2])) +
                 b.pipo*((pipo-L.pipo[2])/(L.pipo[3]-L.pipo[2])) +
                 b.slp*((slp-L.slp[2])/(L.slp[3]-L.slp[2])) +
                 b.loccc2*((loccc2-L.loccc2[2])/(L.loccc2[3]-L.loccc2[2])) +
                 b.landcc2*((landcc2-L.landcc2[2])/(L.landcc2[3]-L.landcc2[2])) +
                 b.casp2*((casp2-L.casp2[2])/(L.casp2[3]-L.casp2[2])) +
                 b.slp2*((slp2-L.slp2[2])/(L.slp2[3]-L.slp2[2]))
  ) - linPN
  mx.raw  <- exp(exponent)/densNorm
  hsi <- (mx.raw*exp(entropy))/(1+mx.raw*exp(entropy))
  out <- list(exponent=exponent,mx.raw=mx.raw,hsi=hsi)
  return(out)
}
