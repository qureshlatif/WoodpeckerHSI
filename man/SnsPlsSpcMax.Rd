\name{SnsPlsSpcMax}
\alias{SnsPlsSpcMax}
\title{maxSSS threshold}
\description{Gets HSI threshold that maximizes sum of sensitivity and specificity.}
\usage{
SnsPlsSpcMax(Obs, HSI)
}
\arguments{
  \item{Obs}{
A binary vector with 0 indicating absence and 1 indicating presence with length = length(HSI).}
  \item{HSI}{
A numeric vector with range 0 < HSI < 1 of length = length(Obs).
}
}
\value{
  \item{HSI_thrshld}{Threshold that maximizes sum of sensitivity and specificity.}
  \item{sensitivity}{Sensitivity at maxSSS threshold.}
  \item{specificity}{Specificity at maxSSS threshold.}
}
\references{
Liu, C., G. Newell, and M. White. 2016. On the selection of thresholds for predicting species occurrence with presence-only data. Ecology and Evolution 6:337-348.
}
\author{
Quresh Latif
}

