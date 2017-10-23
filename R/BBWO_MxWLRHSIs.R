BBWO_MxWLRHSIs <- function(cosasp,dnbr,loccc40,lndcc40) {
  SG_wlr <- expit(-3.130234119 + 0.003704316*dnbr + 3.154616050*lndcc40)
  TP_wlr <- expit(-2.485159177 + 0.006151492*dnbr)
  TB_wlr <- expit(-0.743163 + 0.001546*dnbr)
  Maxent_3v <- BBWO_Mxnt3v(cosasp,dnbr,loccc40)
  Maxent_brn <- BBWO_Mxntbrn(dnbr)
  HSIs<-cbind(SG_wlr,TP_wlr,TB_wlr,Maxent_3v,Maxent_brn)
  return(HSIs)
}
