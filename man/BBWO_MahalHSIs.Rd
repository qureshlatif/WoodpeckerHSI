\name{BBWO_MahalHSIs}
\alias{BBWO_MahalHSIs}
\title{
Apply habitat suitability models for nesting Black-backed Woodpecker in dry conifer forest.
}
\description{
With required inputs, applies Mahalanobis distance models representing components of an ensemble for nesting Black-backed Woodpecker (BBWO; Latif et al. 2013) in burned dry conifer forest of the Inland Northwest and central Rocky Mountains.
}
\usage{
BBWO_MahalHSIs(lndscp, nest.samples = BBWO_nest_samples)
}
\arguments{
  \item{lndscp}{matrix (or data frame coercible to matrix) containing columns representing variables needed to apply published models and rows representing pixels in a burned forest landscape.}
  \item{nest.samples}{List whose elements are resampled BBWO nest location datasets used as reference data for calculating Mahalanobis HSIs. The default is the object 'BBWO_nest_samples', which is included in the 'WoodpeckerHSIs' package.}
}
\details{Applied here are models based on Mahalanobis distance applied to presence-only data. These are the 3 of 8 models used to generate ensemble predictions for nesting Black-backed Woodpeckers (Latif et al. 2013). Four variables are necessary to apply this group of models, and columns containing these variables must be named "COSASP", "DNBR", "LOCCC40", and "LANDCC40". Descriptions of these variables are provided by Latif et al. (2013).}
\value{
  \item{Data}{'lndscp' with output columns added. Output columns are labeled 'BC1', 'TBC1', and 'TB1', corresponding with the three HSI models applied. These models correspond with the '3-variable', '4-variable', and '2-variable' Mahalanobis models described by Latif et al. (2013).}
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
\keyword{models}% use one of  RShowDoc("KEYWORDS")
\keyword{Black-backed Woodpecker}
\keyword{habitat suitability}
\keyword{nesting}
\keyword{dry forest management}
\keyword{ensemble modeling}
