WHWO_green_noTopo <-
function(LOCCC,LANDCC,PIPO) {
  #Define features and parameters
  L.loccc <- c(-3.029021674423218, 0.0, 90.2546)
  L.landcc <- c(0.8291463542808656, 0.291998, 74.3617)
  L.pipo <- c(1.5972202632379153, 0.352009386916984, 99.7066588442358)
  L.loccc2 <- c(-0.6321710815897361, 0.0, 8145.89282116)
  L.landcc2 <- c(2.4951578819906235, 0.08526283200399999, 5529.66242689)
  
  b.loccc <- L.loccc[1]
  loccc <- LOCCC
  loccc[which(loccc>L.loccc[3])] <- L.loccc[3]
  loccc[which(loccc<L.loccc[2])] <- L.loccc[2]
  
  b.landcc <- L.landcc[1]
  landcc <- LANDCC
  landcc[which(landcc>L.landcc[3])] <- L.landcc[3]
  landcc[which(landcc<L.landcc[2])] <- L.landcc[2]
  
  b.pipo <- L.pipo[1]
  pipo <- PIPO
  pipo[which(pipo>L.pipo[3])] <- L.pipo[3]
  pipo[which(pipo<L.pipo[2])] <- L.pipo[2]
  
  b.loccc2 <- L.loccc2[1]
  loccc2 <- LOCCC^2
  loccc2[which(loccc2>L.loccc2[3])] <- L.loccc2[3]
  loccc2[which(loccc2<L.loccc2[2])] <- L.loccc2[2]
  
  b.landcc2 <- L.landcc2[1]
  landcc2 <- LANDCC^2
  landcc2[which(landcc2>L.landcc2[3])] <- L.landcc2[3]
  landcc2[which(landcc2<L.landcc2[2])] <- L.landcc2[2]
  
  linPN <- 2.7604551431166064
  densNorm <- 1769.4222534332205
  entropy <- 9.104771402655066
  
  exponent <- (b.loccc*((loccc-L.loccc[2])/(L.loccc[3]-L.loccc[2])) +
                 b.landcc*((landcc-L.landcc[2])/(L.landcc[3]-L.landcc[2])) +
                 b.pipo*((pipo-L.pipo[2])/(L.pipo[3]-L.pipo[2])) +
                 b.loccc2*((loccc2-L.loccc2[2])/(L.loccc2[3]-L.loccc2[2])) +
                 b.landcc2*((landcc2-L.landcc2[2])/(L.landcc2[3]-L.landcc2[2]))
  ) - linPN
  mx.raw  <- exp(exponent)/densNorm
  hsi <- (mx.raw*exp(entropy))/(1+mx.raw*exp(entropy))
  out <- list(exponent=exponent,mx.raw=mx.raw,hsi=hsi)
  return(out)
}
