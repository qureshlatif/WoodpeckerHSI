calcClassDensities <- function(sampleHSI, bgroundHSI, thresholds, area) {
  if(nrow(dat.class) != (length(thresholds) - 1)) stop("Incorrect number of thresholds provided for the number of suitability classes.")
  HSI.md <- HSI.st <- HSI.end <- numeric(length = (length(thresholds) + 1))
  dat.class <- data.frame(HSI.md, HSI.st, HSI.end)
  for(r in 1:length(HSI.md)) {
    if(r == 1) {
      dat.class$HSI.md[r] <- mean(bgroundHSI[which(bgroundHSI < thresholds[1])])
      dat.class$HSI.st[r] <- 0
      dat.class$HSI.end[r] <- thresholds[1]
    }
    if(r > 1 & r < length(HSI.md)) {
      dat.class$HSI.md[r] <- mean(bgroundHSI[which(bgroundHSI >= thresholds[(r-1)] &
                                                     bgroundHSI < thresholds[r])])
      dat.class$HSI.st[r] <- thresholds[(r-1)]
      dat.class$HSI.end[r] <- thresholds[r]
    }
    if(r == length(HSI.md)) {
      dat.class$HSI.md[r] <- mean(bgroundHSI[which(bgroundHSI >= thresholds[(r-1)])])
      dat.class$HSI.st[r] <- thresholds[(r-1)]
      dat.class$HSI.end[r] <- 1
    }
  }
  dat.class$no_nests <- NA
  dat.class$Density <- NA
  for(r in 1:nrow(dat.class)) {
    dat.class$no_nests[r] <- sum(sampleHSI >= dat.class$HSI.st[r] & sampleHSI < dat.class$HSI.end[r])
    dat.class$Density[r] <-
      sum(sampleHSI >= dat.class$HSI.st[r] & sampleHSI < dat.class$HSI.end[r]) /
      (sum(bgroundHSI >= dat.class$HSI.st[r] & bgroundHSI < dat.class$HSI.end[r]) * (area / length(bgroundHSI)))
  }
  return(dat.class)
}
