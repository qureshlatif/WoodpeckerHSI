BBWO_IndivModelHSIs <- function(lndscp, BBWO_nest_samples = BBWO_nest_samples, RAVG = F) {
  if(RAVG == T) BBWO_nest_samples <- BBWO_nest_samples_ravg
  Data <- BBWO_MahalHSIs(lndscp)
  if(RAVG == F)
    Data <- cbind(Data, BBWO_MxWLRHSIs(Data[, "COSASP"], Data[, "DNBR"], Data[, "LOCCC40"], Data[, "LANDCC40"]))
  if(RAVG == T)
    Data <- cbind(Data, BBWO_MxWLRHSIs_ravg(Data[, "COSASP"], Data[, "DNBR"], Data[, "LOCCC40"], Data[, "LANDCC40"]))
  return(Data)
}