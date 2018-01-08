WHWO_burned_ravg <-
function(brn_1ha, brn_1km) {
  #Define features and parameters
  l.brn_1ha <- c(0.6000312347171055, 0.0, 100.0)
  l.brn_1km <- c(0.0, 0.0, 99.2079788794368)
  l.brn_1ha2 <- c(3.9814579752674257, 0.0, 10000.0)
  l.brn_1haX1km <- c(-4.148202826532182, 0.0, 9920.79788794368)
  linPN <- 3.9961044432811703
  densNorm <- 1252.4979392315925
  entropy <- 9.511352164677524
  
  # Compile higher order features and clamping #
  brn_1ha[which(brn_1ha>l.brn_1ha[3])] <- l.brn_1ha[3]
  brn_1ha[which(brn_1ha<l.brn_1ha[2])] <- l.brn_1ha[2]
  
  brn_1km[which(brn_1km>l.brn_1km[3])] <- l.brn_1km[3]
  brn_1km[which(brn_1km<l.brn_1km[2])] <- l.brn_1km[2]
  
  brn_1ha2 <- brn_1ha^2
  brn_1ha2[which(brn_1ha2>l.brn_1ha2[3])] <- l.brn_1ha2[3]
  brn_1ha2[which(brn_1ha2<l.brn_1ha2[2])] <- l.brn_1ha2[2]

  brn_1haX1km <- brn_1ha*brn_1km
  brn_1haX1km[which(brn_1haX1km>l.brn_1haX1km[3])] <- l.brn_1haX1km[3]
  brn_1haX1km[which(brn_1haX1km<l.brn_1haX1km[2])] <- l.brn_1haX1km[2]
  
  # Apply model #
  exponent <- (l.brn_1ha[1]*((brn_1ha-l.brn_1ha[2])/(l.brn_1ha[3]-l.brn_1ha[2])) +
                 l.brn_1km[1]*((brn_1km-l.brn_1km[2])/(l.brn_1km[3]-l.brn_1km[2])) +
                 l.brn_1ha2[1]*((brn_1ha2 -l.brn_1ha2[2])/(l.brn_1ha2[3]-l.brn_1ha2[2])) +
                 l.brn_1haX1km[1]*((brn_1haX1km-l.brn_1haX1km[2])/(l.brn_1haX1km[3]-l.brn_1haX1km[2]))
  ) - linPN
  mx.raw  <- exp(exponent)/densNorm
  hsi <- (mx.raw*exp(entropy))/(1+mx.raw*exp(entropy))
  out <- list(exponent=exponent,mx.raw=mx.raw,hsi=hsi)
  return(out)
}
