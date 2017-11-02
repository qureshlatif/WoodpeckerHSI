\name{WHWO_green_noTopo}
\alias{WHWO_green_noTopo}
\title{
Apply HSI model for WHWO in unburned forest ignoring topography.
}
\description{
With required inputs, this model applies a version of the habitat suitability model developed by Latif et al. (2015) for nesting White-headed Woodpeckers in unburned (green) forests of the Inland Northwest that ignores topography.
}
\usage{
WHWO_green_noTopo(LOCCC, LANDCC, PIPO)
}
\arguments{
  \item{LOCCC}{numeric vector with length = number of observations/pixels. Values are mean percent (0-100) canopy cover within a 3x3 moving window neighborhood.}
  \item{LANDCC}{numeric vector with length = number of observations/pixels. Values are mean percent (0-100) canopy cover within a 1-km radius (~314 ha) moving window neighborhood.}
  \item{PIPO}{numeric vector with length = number of observations/pixels. Values are mean percent (0-100) of 1-km radius (~314 ha) moving window neighborhood classified as ponderosa-dominated forest.}
}
\details{The model applied here is a simplified version of the Maxent model evaluated by Latif et al. (2015). It was fitted once to all available data (rather than subsets designated for cross-validation) and excludes topography variables for better applicability in western Idaho where topographic relief is much greater than where the original model was developed. Evaluation with additional data is needed to determine how broadly this model can be applied.}
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
WHWO_green_simplified
}

\keyword{models}% use one of  RShowDoc("KEYWORDS")
\keyword{White-headed Woodpecker}
\keyword{habitat suitability}
\keyword{nesting}
\keyword{dry forest management}
