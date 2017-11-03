WHWO_burned <-
function(brn_1ha, brn_1km) {
  #Define features and parameters
  l.brn_1ha <- c(4.31894613511196, 0.0, 100.0)
  l.brn_1km <- c(0.0, 10.4136110296275, 96.6265767087122)
  l.brn_1ha2 <- c(0.29993693671872823, 0.0, 10000.0)
  l.brn_1km2 <- c(-0.2512532731941709, 108.44329467637952, 9336.695326444642)
  l.brn_1haX1km <- c(-3.753779600335546, 0.0, 9662.65767087122)
  linPN <- 4.13791960122693
  densNorm <- 1501.9205208100752
  entropy <- 9.57425566317889
  
  # Compile higher order features and clamping #
  brn_1ha[which(brn_1ha>l.brn_1ha[3])] <- l.brn_1ha[3]
  brn_1ha[which(brn_1ha<l.brn_1ha[2])] <- l.brn_1ha[2]
  
  brn_1km[which(brn_1km>l.brn_1km[3])] <- l.brn_1km[3]
  brn_1km[which(brn_1km<l.brn_1km[2])] <- l.brn_1km[2]
  
  brn_1ha2 <- brn_1ha^2
  brn_1ha2[which(brn_1ha2>l.brn_1ha2[3])] <- l.brn_1ha2[3]
  brn_1ha2[which(brn_1ha2<l.brn_1ha2[2])] <- l.brn_1ha2[2]
  
  brn_1km2 <- brn_1km^2
  brn_1km2[which(brn_1km2>l.brn_1km2[3])] <- l.brn_1km2[3]
  brn_1km2[which(brn_1km2<l.brn_1km2[2])] <- l.brn_1km2[2]
  
  brn_1haX1km <- brn_1ha*brn_1km
  brn_1haX1km[which(brn_1haX1km>l.brn_1haX1km[3])] <- l.brn_1haX1km[3]
  brn_1haX1km[which(brn_1haX1km<l.brn_1haX1km[2])] <- l.brn_1haX1km[2]
  
  # Apply model #
  exponent <- (l.brn_1ha[1]*((brn_1ha-l.brn_1ha[2])/(l.brn_1ha[3]-l.brn_1ha[2])) +
                 l.brn_1km[1]*((brn_1km-l.brn_1km[2])/(l.brn_1km[3]-l.brn_1km[2])) +
                 l.brn_1ha2[1]*((brn_1ha2 -l.brn_1ha2[2])/(l.brn_1ha2[3]-l.brn_1ha2[2])) +
                 l.brn_1km2[1]*((brn_1km2-l.brn_1km2[2])/(l.brn_1km2[3]-l.brn_1km2[2])) +
                 l.brn_1haX1km[1]*((brn_1haX1km-l.brn_1haX1km[2])/(l.brn_1haX1km[3]-l.brn_1haX1km[2]))
  ) - linPN
  mx.raw  <- exp(exponent)/densNorm
  hsi <- (mx.raw*exp(entropy))/(1+mx.raw*exp(entropy))
  out <- list(exponent=exponent,mx.raw=mx.raw,hsi=hsi)
  return(out)
}
