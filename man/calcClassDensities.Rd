\name{calcClassDensities}
\alias{calcClassDensities}
\title{
  Calculates density of observations for HSI suitability classes
}
\description{
  Calculates densities for fixed and distinct categories defined by habitat suitability index (HSI) breakpoints or thresholds. Used in the Woodpecker HSI package to calculate observed nest densities in suitability classes for plotting and mapping. The function could also be used to calculate density of any point observation within a study area, wherein values for a particular environmental variable or gradient are assessed at each observation and for each of a regularly spaced set of background pixels.
}
\usage{
  calcClassDensities(sampleHSI, bgroundHSI, thresholds, area)
}
\arguments{
  \item{sampleHSI}{Numeric vector of HSI values for observations (e.g., nest sites)}
  \item{bgroundHSI}{Numeric vector of HSI values for random background sample representing available sites}
  \item{thresholds}{Numeric vector of HSI breakpoints defining suitability classes}
  \item{area}{Numeric scalar representing the area surveyed}
}
\details{Calculates the density of observations within suitability classes defined by breakpoints in 'thresholds'. Output is a table with each row corresponding with a suitability class and number of rows equal to the number of classes.}
\value{Data frame with one row for each suitability class. Information provided are class parameters ("HSI.md" = median HSI, "HSI.st" = lower bound, "HSI.end" = upper bound) and the density of observations within each bin ("Density").}
\author{
  Quresh S. Latif, Rocky Mountain Research Station, U.S. Forest Service
}

\keyword{models}% use one of  RShowDoc("KEYWORDS")
\keyword{HSI}
\keyword{Resource Selection Function}
\keyword{habitat}
\keyword{model evaluation}
\keyword{density}
