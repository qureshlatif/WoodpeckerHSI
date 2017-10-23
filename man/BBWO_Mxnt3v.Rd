\name{BBWO_Mxnt3v}
\alias{BBWO_Mxnt3v}
\title{
Apply Maxent habitat suitability model with three variables for nesting Black-backed Woodpecker in dry conifer forest.
}
\description{
With required inputs, applies a Maxent model representing a component of an ensemble for nesting Black-backed Woodpecker (BBWO; Latif et al. 2013) in burned dry conifer forest of the Inland Northwest and central Rocky Mountains.
}
\usage{
BBWO_Mxnt3v(cosasp, dnbr, loccc40)
}
\arguments{
  \item{cosasp}{Numeric vector whose values are cosine-transformed topographic aspect (i.e., north-south component) at the 30m pixel resolution.}
  \item{dnbr}{Numeric vector whose values are median delta-normalized burn ratio for 3x3 pixel neighborhood at 30m pixel resolution.}
  \item{loccc40}{Numeric vector whose values are percent of 3x3 pixel neighborhood (30m pixel resolution) that has >40 percent canopy cover before fire.}
}
\details{A Maxent model with three variables is applied here. This model represents 1 of 8 models combined to generate ensemble predictions for nesting Black-backed Woodpeckers (Latif et al. 2013).}
\value{
  \item{HSI}{Numeric vector with range 0-1 representing the logistic Maxent output quantifying relative habitat suitability.}
}
\references{
Latif, Q. S., V. A. Saab, J. G. Dudley, and J. P. Hollenbeck. 2013. Ensemble modeling to predict habitat suitability for a large-scale disturbance specialist. Ecology and Evolution 3:4348-4364.

Merow, C., M. J. Smith, and J. A. Silander. 2013. A practical guide to MaxEnt for modeling species' distributions: what it does, and why inputs and settings matter. Ecography 36:1058-1069.
}
\author{
Quresh S. Latif, Rocky Mountain Research Station, U.S. Forest Service
}

\examples{
## The function is currently defined as
function(cosasp,dnbr,loccc40) {
  #Define features and parameters
  l.cosasp <- c(0.9381002574647688, -1, 1)
  
  l.dnbr <- c(10.786746325527512, -590.47619629, 1024.12268066)
  dnbr[which(dnbr>l.dnbr[3])]<-l.dnbr[3]
  dnbr[which(dnbr<l.dnbr[2])]<-l.dnbr[2]
  
  l.loccc40 <- c(1.1312580836869854, 0.0, 1.0)
  
  l.cosasp2 <- c(-0.3980392534351936, 0.0, 1.0)
  cosasp2 <- cosasp^2
  
  l.dnbr2 <- c(-6.415155660077871, 0.0, 1048827.2650422244)
  dnbr2 <- dnbr^2
  dnbr2[which(dnbr2>l.dnbr2[3])]<-l.dnbr2[3]
  dnbr2[which(dnbr2<l.dnbr2[2])]<-l.dnbr2[2]
  
  l.loccc402 <- c(-1.9494267010037454, 0.0, 1.0)
  loccc402 <- loccc40^2
  
  l.ca_x_lcc40 <- c(-0.08235355467277128, -1.0, 1.0)
  ca_x_lcc40 <- cosasp*loccc40
  ca_x_lcc40[which(ca_x_lcc40>l.ca_x_lcc40[3])]<-l.ca_x_lcc40[3]
  ca_x_lcc40[which(ca_x_lcc40<l.ca_x_lcc40[2])]<-l.ca_x_lcc40[2]
  
  l.dnbr_x_lcc40 <- c(3.6550086289032, -213.7978722982045, 1024.0771640968703)
  dnbr_x_lcc40 <- dnbr*loccc40
  dnbr_x_lcc40[which(dnbr_x_lcc40>l.dnbr_x_lcc40[3])]<-l.dnbr_x_lcc40[3]
  dnbr_x_lcc40[which(dnbr_x_lcc40<l.dnbr_x_lcc40[2])]<-l.dnbr_x_lcc40[2]
  
  linPN <- 8.12466828053413
  densNorm <- 2846.1535672273485
  entropy <- 8.807545807433492
  
  exponent <- (l.cosasp[1]*((cosasp-l.cosasp[2])/(l.cosasp[3]-l.cosasp[2])) +
                 l.dnbr[1]*((dnbr-l.dnbr[2])/(l.dnbr[3]-l.dnbr[2])) +
                 l.loccc40[1]*((loccc40-l.loccc40[2])/(l.loccc40[3]-l.loccc40[2])) +
                 l.cosasp2[1]*((cosasp2-l.cosasp2[2])/(l.cosasp2[3]-l.cosasp2[2])) +
                 l.dnbr2[1]*((dnbr2-l.dnbr2[2])/(l.dnbr2[3]-l.dnbr2[2])) +
                 l.loccc402[1]*((loccc402-l.loccc402[2])/(l.loccc402[3]-l.loccc402[2])) +
                 l.ca_x_lcc40[1]*((ca_x_lcc40-l.ca_x_lcc40[2])/(l.ca_x_lcc40[3]-l.ca_x_lcc40[2])) +
                 l.dnbr_x_lcc40[1]*((dnbr_x_lcc40-l.dnbr_x_lcc40[2])/(l.dnbr_x_lcc40[3]-l.dnbr_x_lcc40[2]))
  ) - linPN
  mx.raw  <- exp(exponent)/densNorm
  HSI <- (mx.raw*exp(entropy))/(1+mx.raw*exp(entropy))
  return(HSI)
}
}
\keyword{models}% use one of  RShowDoc("KEYWORDS")
\keyword{Black-backed Woodpecker}
\keyword{habitat suitability}
\keyword{nesting}
\keyword{dry forest management}
\keyword{Maxent}
