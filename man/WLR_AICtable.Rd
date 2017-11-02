\name{WLR_AICtable}
\alias{WLR_AICtable}
\title{
Generate AIC table for candidate weighted logistic regression (WLR) models.
}
\description{
With a list of candidate WLR models, AIC table is generated (see WLR_fit; Russell et al. 2007, Latif et al. 2016).
}
\usage{
WLR_AICtable(data, mods, Obs = "Nest", AICc = T, w = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Data frame with one column indicating which observations are use (1) versus non-use (0), and additional columns containing habitat covariates to be used as predictors in the model. All covariates listed in any elements of 'mods' must appear in this data frame.
}
  \item{mods}{
List of candidate models. Each element of this list should be a character string in the format "x1+x2+...". Best if generated from 'varCombosLinear' function.
}
  \item{Obs}{
The name of the column in 'data' indicating use-nonuse, i.e., y in the formula y~x.
}
  \item{AICc}{
If TRUE (default), the small-sample corrected version of AIC (AICc) is calculated. Otherwise, AIC is calculated.
}
  \item{w}{
Optional argument passed to WLR_fit.
}
}
\details{
Generates AICc (or AIC) table for candidate models. Also sorts by AICc, and generates delta-AIC and model weights.
}
\value{
Data frame with 5 columns: Model = string representing the model (element of 'mods'), AICc (or AIC) = the information criterion for ranking models, K = the number of parameters in each model, dAIC = the difference in AICc from the top-ranked model, and m_wt = AIC-based model weight.
}
\references{
Latif, Q. S., V. A. Saab, J. P. Hollenbeck, and J. G. Dudley. 2016. Transferability of habitat suitability models for nesting woodpeckers associated with wildfire. The Condor 118:766-790.

Russell, R. E., V. A. Saab, and J. G. Dudley. 2007. Habitat-suitability models for cavity-nesting birds in a postfire landscape. Journal of Wildlife Management 71:2600-2611.
}
\author{
Quresh S. Latif, Rocky Mountain Research Station, U.S. Forest Service
}

\seealso{
WLR_fit, varCombosLinear
}
\keyword{models}% use one of  RShowDoc("KEYWORDS")
\keyword{glm}
\keyword{woodpecker}
\keyword{weighted logistic regression}
\keyword{model selection}
