calcBins <- function(unit.size, n, bin.width) {
  unitID <- rep(1:ceiling(n/unit.size), each = unit.size)
  unitID <- unitID[1:n]
  bin.st <- 1:(max(unitID) - bin.width + 1)
  bin.end <- bin.width:max(unitID)
  return(list(unitID = unitID, st = bin.st, end = bin.end))
}
