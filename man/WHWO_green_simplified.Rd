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
\examples{
## The function is currently defined as
function (LOCCC, LANDCC, CASP, PIPO, SLP) 
{
    L.loccc <- c(-3.18995114653152, 0, 90.2546)
    L.landcc <- c(0.150519912201841, 0.291998, 74.3617)
    L.casp <- c(0.202934316441568, -1, 1)
    L.pipo <- c(1.63139171999255, 0.352009386916984, 99.7066588442358)
    L.slp <- c(2.95443455400397, 0, 38)
    L.loccc2 <- c(-0.473162686313791, 0, 8145.89282116)
    L.landcc2 <- c(3.16119178533474, 0.085262832004, 5529.66242689)
    L.casp2 <- c(-0.61694944141589, 0, 1)
    L.slp2 <- c(-5.24621625797648, 0, 1444)
    b.loccc <- L.loccc[1]
    loccc <- LOCCC
    loccc[which(loccc > L.loccc[3])] <- L.loccc[3]
    loccc[which(loccc < L.loccc[2])] <- L.loccc[2]
    b.landcc <- L.landcc[1]
    landcc <- LANDCC
    landcc[which(landcc > L.landcc[3])] <- L.landcc[3]
    landcc[which(landcc < L.landcc[2])] <- L.landcc[2]
    b.casp <- L.casp[1]
    casp <- CASP
    casp[which(casp > L.casp[3])] <- L.casp[3]
    casp[which(casp < L.casp[2])] <- L.casp[2]
    b.pipo <- L.pipo[1]
    pipo <- PIPO
    pipo[which(pipo > L.pipo[3])] <- L.pipo[3]
    pipo[which(pipo < L.pipo[2])] <- L.pipo[2]
    b.slp <- L.slp[1]
    slp <- SLP
    slp[which(slp > L.slp[3])] <- L.slp[3]
    slp[which(slp < L.slp[2])] <- L.slp[2]
    b.loccc2 <- L.loccc2[1]
    loccc2 <- LOCCC^2
    loccc2[which(loccc2 > L.loccc2[3])] <- L.loccc2[3]
    loccc2[which(loccc2 < L.loccc2[2])] <- L.loccc2[2]
    b.landcc2 <- L.landcc2[1]
    landcc2 <- LANDCC^2
    landcc2[which(landcc2 > L.landcc2[3])] <- L.landcc2[3]
    landcc2[which(landcc2 < L.landcc2[2])] <- L.landcc2[2]
    b.casp2 <- L.casp2[1]
    casp2 <- CASP^2
    casp2[which(casp2 > L.casp2[3])] <- L.casp2[3]
    casp2[which(casp2 < L.casp2[2])] <- L.casp2[2]
    b.slp2 <- L.slp2[1]
    slp2 <- SLP^2
    slp2[which(slp2 > L.slp2[3])] <- L.slp2[3]
    slp2[which(slp2 < L.slp2[2])] <- L.slp2[2]
    linPN <- 2.72565368193021
    densNorm <- 1802.55854120127
    entropy <- 9.08775693790529
    exponent <- (b.loccc * ((loccc - L.loccc[2])/(L.loccc[3] - 
        L.loccc[2])) + b.landcc * ((landcc - L.landcc[2])/(L.landcc[3] - 
        L.landcc[2])) + b.casp * ((casp - L.casp[2])/(L.casp[3] - 
        L.casp[2])) + b.pipo * ((pipo - L.pipo[2])/(L.pipo[3] - 
        L.pipo[2])) + b.slp * ((slp - L.slp[2])/(L.slp[3] - L.slp[2])) + 
        b.loccc2 * ((loccc2 - L.loccc2[2])/(L.loccc2[3] - L.loccc2[2])) + 
        b.landcc2 * ((landcc2 - L.landcc2[2])/(L.landcc2[3] - 
            L.landcc2[2])) + b.casp2 * ((casp2 - L.casp2[2])/(L.casp2[3] - 
        L.casp2[2])) + b.slp2 * ((slp2 - L.slp2[2])/(L.slp2[3] - 
        L.slp2[2]))) - linPN
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
