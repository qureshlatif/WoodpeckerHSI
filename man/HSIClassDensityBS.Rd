\name{HSIClassDensityBS}
\alias{HSIClassDensityBS}
\title{
  Add boot-strapped confidence intervals to HSI class densities
}
\description{
  Applies bootstrapping to calculate 95 percent CIs to densities in habitat suitability index (HSI) categories. Used in the Woodpecker HSI package to quantify uncertainty for observed nest densities for plotting and mapping.
}
\usage{
  HSIClassDensityBS(dat.class, dat.nest, dat.grid, transects, thresholds, area, R, TID = "TID", HSI = "HSI")
}
\arguments{
  \item{dat.class}{Data frame with one row for each suitability class. Ideally, use output from 'calcClassDensities' function.}
  \item{dat.sample}{Data frame with a record for each species observation (e.g., nest location) and at least two columns: 1. HSI values for species observations (e.g., nest locations) and 2. Sampling unit IDs indicating where observations were recorded}
  \item{dat.grid}{Data frame with a record for observation in a sample of random sites representing the area surveyed or available for site selection by the study species. This data frame should be large (at least 10000 observations) and contain at least two columns: 1. HSI values for each observations and 2. IDs representing the sampling unit represented by each observation.}
  \item{Units}{Character vector listing all unique sampling unit (e.g., transect) IDs}
  \item{thresholds}{Numeric vector of HSI breakpoints defining suitability classes}
  \item{area}{Numeric scalar representing the area surveyed}
  \item{R}{Numeric scalar representing the number of boot strapped replicates to draw}
  \item{UnitID}{Character string indicating the name of the field containing sampling unit IDs for observations in 'dat.sample' and 'dat.grid'.}
  \item{HSI}{Character string indicating the name of the field containing HSI values for observations in 'dat.sample' and 'dat.grid'.}
}
\details{Conducts boot-strapping of sampling units ('sample' with replace = T) to calculate 95 percent CIs based on R replicates with sample size equal to the number of sampling units. CIs are attached as new columns to 'dat.class'. CIs are calculated for observed densities and expected percent of species observations in each suitability class given equal sampling effort across classes.}
\value{Data frame - 'dat.class' with 95 percent CIs added as new columns: 'Dens95lo', 'Dens95hi', 'Perc95lo', and 'Perc95hi'}
\author{
  Quresh S. Latif, Rocky Mountain Research Station, U.S. Forest Service
}

\keyword{models}% use one of  RShowDoc("KEYWORDS")
\keyword{HSI}
\keyword{Resource Selection Function}
\keyword{habitat}
\keyword{model evaluation}
\keyword{density}
