SnsPlsSpcMax <-
function(Obs, HSI) {        # Function for finding threshold that maximizes sensitivity + specificity
  HSI.nst <- HSI[which(Obs==1)]
  HSI.lnd <- HSI[which(Obs==0)]
  HSI.cnds <- seq(0.01,0.99,by=0.01)
  sens <- spcf <- Sum <- numeric(length=length(HSI.cnds))
  for (ii in 1:length(HSI.cnds)) {
    sens[ii] <- sum(HSI.nst>=HSI.cnds[ii])/length(HSI.nst)
    spcf[ii] <- sum(HSI.lnd<HSI.cnds[ii])/length(HSI.lnd)
    Sum[ii] <- sens[ii]+spcf[ii]
  } 
  SPS_th <- HSI.cnds[which(Sum==max(Sum))]
  SPS_sns <- sens[which(Sum==max(Sum))]
  SPS_spc <- spcf[which(Sum==max(Sum))]
  MSPS<-list(SPS_th,SPS_sns,SPS_spc)
  names(MSPS)<-c("HSI_thrshld","sensitivity","specificity")
  return(MSPS)
}
