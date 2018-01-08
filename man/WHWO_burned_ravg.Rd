\name{WHWO_burned_ravg}
\alias{WHWO_burned_ravg}
\title{
Apply RAVG version of HSI model for WHWO in burned forest.
}
\description{
With required inputs, this model applies a habitat suitability model for nesting White-headed Woodpeckers in burned forests of the Inland Northwest (Latif et al. In Prep). The model is an alternate version of that provided by 'WHWO_burned' fitted to RAVG instead of MTBS burn severity data.
}
\usage{
WHWO_burned_ravg(brn_1ha, brn_1km)
}
\arguments{
  \item{brn_1ha}{numeric vector with length = number of observations/pixels. Values are mean percent (0-100) of 3x3 moving window neighborhood that is either severely burned (RAVG dNBR > 270) or open (pre-fire canopy cover < 10\%).}
  \item{brn_1km}{numeric vector with length = number of observations/pixels. Values are mean percent (0-100) of a 1-km radius (~314 ha) moving window neighborhood that is either severely burned (RAVG dNBR > 270) or open (pre-fire canopy cover < 10\%).}
}
\details{The model applied here is a RAVG-based version of a Maxent model developed at two wildfire locations (Toolbox, 2002; Canyon Creek, 2015; Latif et al. In Prep).}
\value{
  \item{exponent}{This value is a linear function of inputs subsequently re-scaled to get various other outputs.}
  \item{hsi}{Maxent output rescaled to range 0-1 using a logit transformation. This is the output evaluated by Latif et al. (2015) and provided by ArcGIS application tools to inform forest planning.}
  \item{mx.raw}{Raw Maxent output recommended by Merow et al. (2013) and most closely related to local abundance or density.}
}
\references{
Latif, Q. S., V. A. Saab, J. G. Dudley, and A. Markus. In Prep. Development and evaluation of habitat suitability models for nesting white-headed woodpecker in burned forest.

Merow, C., M. J. Smith, and J. A. Silander. 2013. A practical guide to MaxEnt for modeling species' distributions: what it does, and why inputs and settings matter. Ecography 36:1058-1069.
}
\author{
Quresh S. Latif, Rocky Mountain Research Station, U.S. Forest Service
}

\keyword{models}% use one of  RShowDoc("KEYWORDS")
\keyword{White-headed Woodpecker}
\keyword{wildfire}
\keyword{habitat suitability}
\keyword{nesting}
\keyword{dry forest management}
