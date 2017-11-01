calcBinDensities <- function(sampleHSI, bgroundHSI, bins, area) {
  cell.area <- area/length(bgroundHSI)
  HSI.sort <- sort(bgroundHSI)
  
  tab <- data.frame(HSI.md = numeric(length = length(bins$st)))
  tab$HSI.end <- tab$HSI.st <- 0
  tab$Density <- NA

  for(b in 1:length(bins$st)) {
    tab$HSI.md[b] <-
      mean(HSI.sort[which(bins$unitID %in% bins$st[b]:bins$end[b])])
    tab$HSI.st[b] <-
      min(HSI.sort[which(bins$unitID %in% bins$st[b]:bins$end[b])])
    tab$HSI.end[b] <-
      max(HSI.sort[which(bins$unitID %in% bins$st[b]:bins$end[b])])
    if(b == 1) tab$HSI.st[b] <- 0 # To make sure all nests with lowest HSIs are included
    if(b == nrow(tab)) tab$HSI.end[b] <- 1 # To make sure all nests with highest HSIs are included
    no.nests <- sum(sampleHSI >= tab$HSI.st[b] & sampleHSI <= tab$HSI.end[b])
    bin.area <- sum(bgroundHSI >= tab$HSI.st[b] & bgroundHSI <= tab$HSI.end[b])*cell.area
    tab$Density[b] <- no.nests / bin.area
  }
  return(tab)
}
