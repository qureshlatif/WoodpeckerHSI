\name{BBWO_Mxntbrn}
\alias{BBWO_Mxntbrn}
\title{
Apply Maxent habitat suitability model based solely on burn severity for nesting Black-backed Woodpecker in dry conifer forest.
}
\description{
With required inputs, applies a Maxent model representing a component of an ensemble for nesting Black-backed Woodpecker (BBWO; Latif et al. 2013) in burned dry conifer forest of the Inland Northwest and central Rocky Mountains.
}
\usage{
BBWO_Mxntbrn(dnbr)
}
\arguments{
  \item{dnbr}{Numeric vector whose values are median delta-normalized burn ratio for 3x3 pixel neighborhood at 30m pixel resolution.}
}
\details{A Maxent model with three variables is applied here. This model represents 1 of 8 models combined to generate ensemble predictions for nesting Black-backed Woodpeckers (Latif et al. 2013).}
\value{Numeric vector with range 0-1 representing the logistic Maxent output quantifying relative habitat suitability.}
\references{
Latif, Q. S., V. A. Saab, J. G. Dudley, and J. P. Hollenbeck. 2013. Ensemble modeling to predict habitat suitability for a large-scale disturbance specialist. Ecology and Evolution 3:4348-4364.

Merow, C., M. J. Smith, and J. A. Silander. 2013. A practical guide to MaxEnt for modeling species' distributions: what it does, and why inputs and settings matter. Ecography 36:1058-1069.
}
\author{
Quresh S. Latif, Rocky Mountain Research Station, U.S. Forest Service
}
\keyword{models}% use one of  RShowDoc("KEYWORDS")
\keyword{Black-backed Woodpecker}
\keyword{habitat suitability}
\keyword{nesting}
\keyword{dry forest management}
\keyword{Maxent}
