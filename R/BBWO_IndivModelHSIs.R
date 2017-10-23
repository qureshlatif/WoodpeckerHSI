BBWO_IndivModelHSIs <- function(lndscp, BBWO_nest_samples = BBWO_nest_samples) {
  Data <- BBWO_MahalHSIs(lndscp)
  Data <- cbind(Data, BBWO_MxWLRHSIs(Data[, "COSASP"], Data[, "DNBR"], Data[, "LOCCC40"], Data[, "LANDCC40"]))
  return(Data)
}