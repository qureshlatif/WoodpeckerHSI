BBWO_MxWLRHSIs_ravg <- function(cosasp,dnbr,loccc40,lndcc40) {
  SG_wlr <- expit(-2.032777221 + 0.001542904*dnbr + 2.398416203*lndcc40)
  TP_wlr <- expit(-1.836478551 + 0.004457295*dnbr)
  TB_wlr <- expit(-0.513351301 + 0.001037312*dnbr)
  Maxent_3v <- BBWO_Mxnt3v_ravg(cosasp,dnbr,loccc40)
  Maxent_brn <- BBWO_Mxntbrn_ravg(dnbr)
  HSIs<-cbind(SG_wlr,TP_wlr,TB_wlr,Maxent_3v,Maxent_brn)
  return(HSIs)
}
