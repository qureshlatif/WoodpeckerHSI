\name{calcClassDensities}
\alias{calcClassDensities}
\title{
  Calculates density of observations for HSI suitability classes
}
\description{
  Calculates densities for fixed and distinct categories defined by habitat suitability index (HSI) breakpoints or thresholds. Used in the Woodpecker HSI package to calculate observed nest densities in suitability classes for plotting and mapping. The function could also be used to calculate density of any point observation within a study area, wherein values for a particular environmental variable or gradient are assessed at each observation and for each of a regularly spaced set of background pixels.
}
\usage{
  calcBins(sampleHSI, bgroundHSI, thresholds, area)
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

\examples{
  ## The function is currently defined as
  function(sampleHSI, bgroundHSI, thresholds, area) {
  dat.class <- data.frame(HSI.md = c(mean(bgroundHSI[which(bgroundHSI < thresholds[1])]),
                                     mean(bgroundHSI[which(bgroundHSI >= thresholds[1] & bgroundHSI < thresholds[2])]),
                                     mean(bgroundHSI[which(bgroundHSI >= thresholds[2])])),
                          HSI.st = c(0, thresholds[1], thresholds[2]),
                          HSI.end = c(thresholds[1], thresholds[2], 1))
  dat.class$no_nests <- NA
  dat.class$Density <- NA
  if(!is.null(bgDist)) dat.class$Density_adj <- dat.class$Distance <- 0
  for(r in 1:nrow(dat.class)) {
    dat.class$no_nests[r] <- sum(sampleHSI >= dat.class$HSI.st[r] & sampleHSI < dat.class$HSI.end[r])
    dat.class$Density[r] <-
      sum(sampleHSI >= dat.class$HSI.st[r] & sampleHSI < dat.class$HSI.end[r]) /
      (sum(bgroundHSI >= dat.class$HSI.st[r] & bgroundHSI < dat.class$HSI.end[r]) * (area / length(bgroundHSI)))
  }
  if(!is.null(bgDist)) for(r in 1:nrow(dat.class)) {
    dat.class$Distance[r] <- mean(bgDist[which(bgroundHSI >= dat.class$HSI.st[r] & bgroundHSI < dat.class$HSI.end[r])])
    p <- pdist(modDist, dat.class$Distance[r])
    dat.class$Density_adj[r] <- dat.class$Density[r] / p
  }
  return(dat.class)
}
}
\keyword{models}% use one of  RShowDoc("KEYWORDS")
\keyword{HSI}
\keyword{Resource Selection Function}
\keyword{habitat}
\keyword{model evaluation}
\keyword{density}
