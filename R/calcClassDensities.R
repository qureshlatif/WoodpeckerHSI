calcClassDensities <- function(sampleHSI, bgroundHSI, thresholds, area) {
  dat.class <- data.frame(HSI.md = c(mean(bgroundHSI[which(bgroundHSI < thresholds[1])]),
                                     mean(bgroundHSI[which(bgroundHSI >= thresholds[1] & bgroundHSI < thresholds[2])]),
                                     mean(bgroundHSI[which(bgroundHSI >= thresholds[2])])),
                          HSI.st = c(0, thresholds[1], thresholds[2]),
                          HSI.end = c(thresholds[1], thresholds[2], 1))
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
