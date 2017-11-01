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

\examples{
  ## The function is currently defined as
function(sampleHSI, bgroundHSI, bins, area) {
  cell.area <- area/length(bgroundHSI)
  HSI.sort <- sort(bgroundHSI)
  
  tab <- data.frame(HSI.md = numeric(length = length(bins$st)))
  tab$HSI.end <- tab$HSI.st <- 0
  tab$Density <- NA

  for(b in 1:length(bins$st)) {
    tab$HSI.md[b] <-
      mean(HSI.sort[which(bins$unitID \%in\% bins$st[b]:bins$end[b])])
    tab$HSI.st[b] <-
      min(HSI.sort[which(bins$unitID \%in\% bins$st[b]:bins$end[b])])
    tab$HSI.end[b] <-
      max(HSI.sort[which(bins$unitID \%in\% bins$st[b]:bins$end[b])])
    if(b == 1) tab$HSI.st[b] <- 0 # To make sure all nests with lowest HSIs are included
    if(b == nrow(tab)) tab$HSI.end[b] <- 1 # To make sure all nests with highest HSIs are included
    no.nests <- sum(sampleHSI >= tab$HSI.st[b] & sampleHSI <= tab$HSI.end[b])
    bin.area <- sum(bgroundHSI >= tab$HSI.st[b] & bgroundHSI <= tab$HSI.end[b])*cell.area
    tab$Density[b] <- no.nests / bin.area
  }
  }
  }
\keyword{models}% use one of  RShowDoc("KEYWORDS")
\keyword{HSI}
\keyword{Resource Selection Function}
\keyword{habitat}
\keyword{model evaluation}
\keyword{density}
