\name{BBWO_IndivModelHSIs}
\alias{BBWO_IndivModelHSIs}
\title{
Applies all habitat suitability models representing components of an ensemble for nesting Black-backed Woodpecker in dry conifer forest.
}
\description{
Wrapper function that generates HSIs for all models representing components of an ensemble for nesting Black-backed Woodpecker (BBWO; Latif et al. 2013) in burned dry conifer forest of the Inland Northwest and central Rocky Mountains.
}
\usage{
BBWO_IndivModelHSIs(lndscp, BBWO_nest_samples = BBWO_nest_samples)
}
\arguments{
  \item{lndscp}{matrix (or data frame coercible to matrix) containing columns representing variables needed to apply published models and rows representing pixels in a burned forest landscape.}
  \item{BBWO_nest_samples}{List whose elements are resampled BBWO nest location datasets used as reference data for calculating Mahalanobis HSIs. The default is the object 'BBWO_nest_samples', which is included in the 'WoodpeckerHSIs' package.}
}
\details{This wrapper function applies two sub-functions that together apply all 8 component models needed to generate ensemble predictions for nesting Black-backed Woodpeckers (Latif et al. 2013). Four variables are necessary to apply this group of models, and columns in 'lndscp' containing these variables must be named "COSASP", "DNBR", "LOCCC40", and "LANDCC40". Descriptions of these variables are provided by Latif et al. (2013).}
\value{
  \item{Data}{'lndscp' with output columns added. Output columns are labeled 'BC1', 'TBC1', 'TB1', 'SG_wlr', 'TP_wlr', 'TB_wlr', 'Maxent_3v', and 'Maxent_brn'. These correspond with '3-variable Mahalanobis', '4-variable Mahalanobis', '2-variable Mahalanobis', 'Star Gulch logistic regression', 'Tripod logistic regression', 'Toolbox logistic regression', '3-variable Maxent', and 'dNBR Maxent' models described by Latif et al. (2013).}
}
\references{
Latif, Q. S., V. A. Saab, J. G. Dudley, and J. P. Hollenbeck. 2013. Ensemble modeling to predict habitat suitability for a large-scale disturbance specialist. Ecology and Evolution 3:4348-4364.
}
\author{
Quresh S. Latif, Rocky Mountain Research Station, U.S. Forest Service
}

\seealso{
Mahal_HSI, BBWO_MxWLRHSIs, BBWO_Mxnt3v, BBWO_Mxntbrn
}
\keyword{models}% use one of  RShowDoc("KEYWORDS")
\keyword{Black-backed Woodpecker}
\keyword{habitat suitability}
\keyword{nesting}
\keyword{dry forest management}
\keyword{ensemble modeling}
