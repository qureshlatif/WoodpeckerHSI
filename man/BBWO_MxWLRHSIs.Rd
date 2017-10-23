\name{BBWO_MxWLRHSIs}
\alias{BBWO_MxWLRHSIs}
\title{
Apply Maxent and WLR habitat suitability models for nesting Black-backed Woodpecker in dry conifer forest.
}
\description{
Applies all Maxent and weighted logistic regression (WLR) models representing 5 of 8 components of an ensemble for nesting Black-backed Woodpecker (BBWO; Latif et al. 2013) in burned dry conifer forest of the Inland Northwest and central Rocky Mountains.
}
\usage{
BBWO_MxWLRHSIs(cosasp,dnbr,loccc40,lndcc40)
}
\arguments{
  \item{cosasp}{Numeric vector whose values are cosine-transformed topographic aspect (i.e., north-south component) at the 30m pixel resolution.}
  \item{dnbr}{Numeric vector whose values are median delta-normalized burn ratio for 3x3 pixel neighborhood at 30m pixel resolution.}
  \item{loccc40}{Numeric vector whose values are percent of 3x3 pixel neighborhood (30m pixel resolution) that has >40 percent canopy cover before fire.}
  \item{landcc40}{Numeric vector whose values are percent of 1-km radius circular neighborhood that has >40 percent canopy cover before fire (at 30m pixel resolution).}
}
\details{This is a partial wrapper function. It employs sub-functions to apply 2 Maxent models, and adds 3 weighted logistic regression models. Resulting HSIs (range 0-1) are bound together and exported in matrix form. Models applied represent 5 of 8 models used to generate ensemble predictions for nesting Black-backed Woodpeckers (Latif et al. 2013).}
\value{
  \item{HSIs}{Numeric matrix with 5 columns correspinding each of the models applied. These are labeled 'SG_wlr', 'TP_wlr', 'TB_wlr', 'Maxent_3v', and 'Maxent_brn' corresponding, respectively, with 'Star Gulch logistic regression', 'Tripod logistic regression', 'Toolbox logistic regression', '3-variable Maxent', and 'dNBR Maxent' models described by Latif et al. (2013).}
}
\references{
Latif, Q. S., V. A. Saab, J. G. Dudley, and J. P. Hollenbeck. 2013. Ensemble modeling to predict habitat suitability for a large-scale disturbance specialist. Ecology and Evolution 3:4348-4364.
}
\author{
Quresh S. Latif, Rocky Mountain Research Station, U.S. Forest Service
}

\seealso{
Mahal_HSI
}
\examples{
## The function is currently defined as
function(cosasp,dnbr,loccc40,lndcc40) {
  SG_wlr <- expit(-3.130234119 + 0.003704316*dnbr + 3.154616050*lndcc40)
  TP_wlr <- expit(-2.485159177 + 0.006151492*dnbr)
  TB_wlr <- expit(-0.743163 + 0.001546*dnbr)
  Maxent_3v <- BBWO_Mxnt3v(cosasp,dnbr,loccc40)
  Maxent_brn <- BBWO_Mxntbrn(dnbr)
  HSIs<-cbind(SG_wlr,TP_wlr,TB_wlr,Maxent_3v,Maxent_brn)
  return(HSIs)
}
}
\keyword{models}% use one of  RShowDoc("KEYWORDS")
\keyword{Black-backed Woodpecker}
\keyword{habitat suitability}
\keyword{nesting}
\keyword{dry forest management}
\keyword{weighted logistic regression}
\keyword{Maxent}
\keyword{ensemble modeling}
