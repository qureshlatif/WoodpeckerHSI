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

\examples{
## The function is currently defined as
function (LOCCC, LANDCC, PIPO) 
{
    L.loccc <- c(-3.02902167442322, 0, 90.2546)
    L.landcc <- c(0.829146354280866, 0.291998, 74.3617)
    L.pipo <- c(1.59722026323792, 0.352009386916984, 99.7066588442358)
    L.loccc2 <- c(-0.632171081589736, 0, 8145.89282116)
    L.landcc2 <- c(2.49515788199062, 0.085262832004, 5529.66242689)
    b.loccc <- L.loccc[1]
    loccc <- LOCCC
    loccc[which(loccc > L.loccc[3])] <- L.loccc[3]
    loccc[which(loccc < L.loccc[2])] <- L.loccc[2]
    b.landcc <- L.landcc[1]
    landcc <- LANDCC
    landcc[which(landcc > L.landcc[3])] <- L.landcc[3]
    landcc[which(landcc < L.landcc[2])] <- L.landcc[2]
    b.pipo <- L.pipo[1]
    pipo <- PIPO
    pipo[which(pipo > L.pipo[3])] <- L.pipo[3]
    pipo[which(pipo < L.pipo[2])] <- L.pipo[2]
    b.loccc2 <- L.loccc2[1]
    loccc2 <- LOCCC^2
    loccc2[which(loccc2 > L.loccc2[3])] <- L.loccc2[3]
    loccc2[which(loccc2 < L.loccc2[2])] <- L.loccc2[2]
    b.landcc2 <- L.landcc2[1]
    landcc2 <- LANDCC^2
    landcc2[which(landcc2 > L.landcc2[3])] <- L.landcc2[3]
    landcc2[which(landcc2 < L.landcc2[2])] <- L.landcc2[2]
    linPN <- 2.76045514311661
    densNorm <- 1769.42225343322
    entropy <- 9.10477140265507
    exponent <- (b.loccc * ((loccc - L.loccc[2])/(L.loccc[3] - 
        L.loccc[2])) + b.landcc * ((landcc - L.landcc[2])/(L.landcc[3] - 
        L.landcc[2])) + b.pipo * ((pipo - L.pipo[2])/(L.pipo[3] - 
        L.pipo[2])) + b.loccc2 * ((loccc2 - L.loccc2[2])/(L.loccc2[3] - 
        L.loccc2[2])) + b.landcc2 * ((landcc2 - L.landcc2[2])/(L.landcc2[3] - 
        L.landcc2[2]))) - linPN
    mx.raw <- exp(exponent)/densNorm
    hsi <- (mx.raw * exp(entropy))/(1 + mx.raw * exp(entropy))
    out <- list(exponent = exponent, mx.raw = mx.raw, hsi = hsi)
    return(out)
  }
}
\keyword{models}% use one of  RShowDoc("KEYWORDS")
\keyword{White-headed Woodpecker}
\keyword{habitat suitability}
\keyword{nesting}
\keyword{dry forest management}
