\name{calcBins}
\alias{calcBins}
\title{
  Generates indices for moving window bins
}
\description{
  Utility function that generates a set of indices for moving window bins targeting an implied dataset of sample size 'n' to support plotting HSI relationships with nest densities and calculating RSF plot index (RPI).
}
\usage{
  calcBins(unit.size, n, bin.width)
}
\arguments{
  \item{unit.size}{Numeric scalar - the number of observations to include in a unit}
  \item{n}{Numeric scalar - the total number of observations in the target sample}
  \item{bin.width}{Numeric scalar - the number of units to include in each moving window bin}
}
\details{This is a utility function that supports various other functions in the WoodpeckerHSI package that relate HSIs with observed nest densities and calculate associated metrics of predictive performance or GOF, i.e., RPI (Wiens et al. 2008).}
\value{
  \item{unitID}{Integer vector indicating the unit assignment of each observation; length = n}
  \item{st}{Integer vector indicating the lowest-value unitID to include in each bin; length = bin.width}
  \item{end}{Integer vector indicating the highest-value unitID to include in each bin; length = bin.width}
}
\references{
  Wiens, T. S., B. C. Dale, M. S. Boyce, and G. P. Kershaw. 2008. Three way k-fold cross-validation of resource selection functions. Ecological Modelling 212:244-255.
}
\author{
  Quresh S. Latif, Rocky Mountain Research Station, U.S. Forest Service
}

\keyword{models}% use one of  RShowDoc("KEYWORDS")
\keyword{HSI}
\keyword{Resource Selection Function}
\keyword{habitat}
\keyword{model evaluation}
