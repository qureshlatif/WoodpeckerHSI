\name{WHWO_green_simplified}
\alias{WHWO_green_simplified}
\title{
Apply HSI model for WHWO in unburned forest ignoring topography.
}
\description{
With required inputs, this model applies a version of the habitat suitability model developed by Latif et al. (2015) for nesting White-headed Woodpeckers in unburned (green) forests of the Inland Northwest.
}
\usage{
WHWO_green_simplified(LOCCC, LANDCC, CASP, PIPO, SLP)
}
\arguments{
  \item{LOCCC}{numeric vector with length = number of observations/pixels. Values are mean percent (0-100) canopy cover within a 3x3 moving window neighborhood.}
  \item{LANDCC}{numeric vector with length = number of observations/pixels. Values are mean percent (0-100) canopy cover within a 1-km radius (~314 ha) moving window neighborhood.}
  \item{CASP}{numeric vector with length = number of observations/pixels. Values are cosine-transformed topographic aspect (range: -1, 1) for 30-m pixels.}
  \item{PIPO}{numeric vector with length = number of observations/pixels. Values are mean percent (0-100) of 1-km radius (~314 ha) moving window neighborhood classified as ponderosa-dominated forest.}
  \item{SLP}{numeric vector with length = number of observations/pixels. Values are mean percent topographic slope (rise over run) for 30-m pixels.}
}
\details{The model applied here is a simplified version of the Maxent model evaluated by Latif et al. (2015). It was fitted once to all available data (rather than subsets designated for cross-validation). The model also excludes edge density of high- versus low-canopy cover patches, because this input contributed little to the original model and was computationally intensive to generate. Resulting HSIs from this model are virtually identical to the published model.}
\value{
  \item{exponent}{This value is a linear function of inputs subsequently re-scaled to get various other outputs.}
  \item{hsi}{Maxent output rescaled to range 0-1 using a logit transformation. This is the output evaluated by Latif et al. (2015) and provided by ArcGIS application tools to inform forest planning.}
  \item{mx.raw}{Raw Maxent output recommended by Merow et al. (2013) and most closely related to local abundance or density.}
}
\references{
Latif, Q. S., V. A. Saab, K. Mellen-Mclean, and J. G. Dudley. 2015. Evaluating habitat suitability models for nesting white-headed woodpeckers in unburned forest. The Journal of Wildlife Management 79:263-273.

Merow, C., M. J. Smith, and J. A. Silander. 2013. A practical guide to MaxEnt for modeling species' distributions: what it does, and why inputs and settings matter. Ecography 36:1058-1069.
}
\author{
Quresh S. Latif, Rocky Mountain Research Station, U.S. Forest Service
}

\seealso{
WHWO_green_noTopo
}
\keyword{models}% use one of  RShowDoc("KEYWORDS")
\keyword{White-headed Woodpecker}
\keyword{habitat suitability}
\keyword{nesting}
\keyword{dry forest management}
