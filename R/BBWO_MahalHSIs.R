BBWO_MahalHSIs <- function(lndscp, nest.samples = BBWO_nest_samples) {
  BC1.HSI <- TBC1.HSI <- TB1.HSI <- numeric(length=nrow(lndscp)) 
  for (y in 1:length(nest.samples)) {
    Recs <- nest.samples[[y]]
    #BC1 model ################################################################################
    Lnd <- as.matrix(lndscp[,c("DNBR","LOCCC40","LANDCC40")])
    Loc <- as.matrix(Recs[,c("DNBR","LOCCC40","LANDCC40")])
    M <- HSI_Mahal(Loc,Lnd)
    BC1.HSI <- BC1.HSI + round(M,digits=4)
    #TBC1 model ################################################################################
    Lnd<-as.matrix(lndscp[,c("COSASP","DNBR","LOCCC40","LANDCC40")])
    Loc<-as.matrix(Recs[,c("COSASP","DNBR","LOCCC40","LANDCC40")])
    M <- HSI_Mahal(Loc,Lnd)
    TBC1.HSI <- TBC1.HSI + round(M,digits=4)
    #TB1 model ################################################################################
    Lnd<-as.matrix(lndscp[,c("COSASP","DNBR")])
    Loc<-as.matrix(Recs[,c("COSASP","DNBR")])
    M <- HSI_Mahal(Loc,Lnd)
    TB1.HSI <- TB1.HSI + round(M,digits=4)
  }
  #Calculate HSI means and s.e.'s across model replicates
  BC1.HSI <- BC1.HSI/100
  TBC1.HSI <- TBC1.HSI/100
  TB1.HSI <- TB1.HSI/100
  Data <- cbind(lndscp, BC1.HSI, TBC1.HSI, TB1.HSI)   #Bind cell IDs and spatial coordinates to matrix with HSI scores.
  dimnames(Data)[[2]][(ncol(Data) - 2):ncol(Data)] <- c("BC1","TBC1","TB1")
  return(Data)
}
