\name{HSI_Mahal}
\alias{HSI_Mahal}
\title{
Calculate Mahalanobis distance based HSI scores.
}
\description{
Uses Mahalanobis distance model to calculate habitat suitability index (HSIs) values for a set of observations. The user must supply a reference dataset (Cal), target dataset (Lnd), and variables upon which the distance calculation is based. The variables must appear in both the reference and target datasets. The function will then calculate squared standardized distances from target observations to the reference data. The final values are distances rescaled using a Chi-squared distribution to range 0-1. Scaling is based upon the standardized variation in the reference data.
}
\usage{
HSI_Mahal(Cal, Lnd, vars = dimnames(Cal)[[2]])
}
\arguments{
  \item{Cal}{matrix (or data frame coercible to matrix) containing columns with continuously distributed variables}
  \item{Lnd}{matrix (or data frame coercible to matrix) containing columns with continuously distributed variables with names matching those in Cal}
  \item{vars}{character vector ot variable names appearing in 'Cal' and 'Val' to be used for distance-based HSI calculation}
}
\details{This is a somewhat generic function for calculating Mahalanobis HSI values. The function is used by others in the 'WoodpeckerHSIs' package to apply species-specific HSI models. The function carries implements a basic multivariate Mahalanobis distance model (i.e., unpartitioned model described by Rotenberry et al. 2006).}
\value{
  \item{HSI}{Numeric vector with range 0-1 representing rescaled distances from reference dataset to target observations.}
}
\references{
Rotenberry, J. T., K. L. Preston, and S. T. Knick. 2006. GIS-based niche modeling for mapping species' habitat. Ecology 87:1458-1464.
}
\author{
Quresh S. Latif, Rocky Mountain Research Station, U.S. Forest Service
}

\examples{
## The function is currently defined as
function (Cal, Lnd, vars = dimnames(Cal)[[2]]) {
  HSI <- numeric(length = nrow(Lnd))
  
  mns <- apply(Cal[, vars], 2, mean)
  sds <- apply(Cal[, vars], 2, sd)
  cal <- t(apply(Cal[, vars], 1, function(x) (x - mns)/sds))
  lnd <- t(apply(Lnd[, vars], 1, function(x) (x - mns)/sds))
  
  cnt <- apply(cal, 2, mean)
  cv <- cov(cal)
  dgf <- ncol(cal)          
  D.lnd <- mahalanobis(lnd, cnt, cv)
  p.lnd <- 1-pchisq(D.lnd, dgf)
  HSI <- p.lnd
  return(HSI)
}
}
\keyword{models}% use one of  RShowDoc("KEYWORDS")
\keyword{Mahalanobis distance}
\keyword{presence-only distribution}
\keyword{habitat}
