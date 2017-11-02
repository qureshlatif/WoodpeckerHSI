\name{calcBinDensities}
\alias{calcBinDensities}
\title{
  Calculates density of observations for moving window bins along HSI gradient
}
\description{
  Calculates densities for moving window bins. Used in the Woodpecker HSI package to calculate observed nest densities in relation to habitat suitability values (HSI) values. The function could also be used to calculate density of any point observation within a study area, wherein values for a particular environmental variable or gradient are assessed at each observation and for each of a regularly spaced set of background pixels.
}
\usage{
  calcBinDensities(sampleHSI, bgroundHSI, bins, area)
}
\arguments{
  \item{sampleHSI}{Numeric vector of HSI values for observations (e.g., nest sites)}
  \item{bgroundHSI}{Numeric vector of HSI values for random background sample representing available sites}
  \item{bins}{List with three elements that assign background samples (values in bgroundHSI) to moving window bins within which densities are to be calculated (names(bins) = c("unitID", "st", "end"); see 'calcBins' function)}
  \item{area}{Numeric scalar representing the area surveyed}
}
\details{Calculates the density of observations within moving window bins indexed by the 'calcBins' function. Output is a table with each row corresponding with a bin and number of rows equal to the number of bins, i.e., length(bins$st).}
\value{Data frame (class = "tbl_df", see dplyr package) with rows corresponding to moving window bins (nrow = length(bins$st)). Information provided are bin parameters ("HSI.md" = median HSI, "HSI.st" = lower bound, "HSI.end" = upper bound) and the density of observations within each bin ("Density").}
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
\keyword{density}
