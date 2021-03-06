\name{BBWO_Ensemble}
\alias{BBWO_Ensemble}
\title{
Classify HSIs and generate ensemble predictions for nesting Black-backed Woodpecker in dry conifer forest.
}
\description{
Wrapper function that classifies model HSIs using pre-defined thresholds and sums classified values to generate ensemble predictions, which represent the number of models that characterize a given observation or pixel as suitable for nesting Black-backed Woodpecker (BBWO; Latif et al. 2013) in burned dry conifer forest of the Inland Northwest and central Rocky Mountains.
}
\usage{
BBWO_Ensemble(Data, mods = c("BC1", "TBC1", "TB1", "SG_wlr", "TP_wlr", "TB_wlr", "Maxent_3v", "Maxent_brn"),
  thrhds = c(0.17,0.17,0.32,0.43,0.43,0.45,0.41,0.37), RAVG = F)
}
\arguments{
  \item{Data}{matrix (or data frame coercible to matrix) containing columns representing HSIs (range 0-1) named in 'mods'. Most likely, Data = output from the 'BBWO_IndivModelHSIs' function.}
  \item{mods}{Character vector containing names of columns containing model HSIs to be combined into ensemble. Default are the names assigned by the 'BBWO_IndivModelHSIs' function.}
  \item{thrhds}{Numeric vector containing thresholds used to classify individual model HSI values into more suitable (1) vs less suitable (0). Length of 'thrhds' should equal length(mods). Default are thresholds identified by Latif et al. (2013).}
  \item{RAVG}{If false (default), thresholds for classifying individual model HSIs are as defined by the 'thrhds' argument. If true, thresholds are re-defined within the function as needed for RAVG-based alternate versions of individual models.}
}
\details{This the final wrapper function that classifies 8 component model HSIs and combines classified values into an ensemble prediction of habitat suitability for nesting Black-backed Woodpeckers (Latif et al. 2013). Ensemble predictions describe the number of models classifying a given observation or pixel as suitable. The 'Data' argument should contain all continuous HSIs from individual models, and will most likely be the output from the 'BBWO_IndivModelHSIs' function.}
\value{Matrix of classified HSIs (8 binary columns) and the final ensemble prediction (1 integer column with range 0-8). Columns are labeled 'BC1', 'TBC1', 'TB1', 'SG_wlr', 'TP_wlr', 'TB_wlr', 'Maxent_3v', 'Maxent_brn', and 'Ensemble'.}
\references{
Latif, Q. S., V. A. Saab, J. G. Dudley, and J. P. Hollenbeck. 2013. Ensemble modeling to predict habitat suitability for a large-scale disturbance specialist. Ecology and Evolution 3:4348-4364.
}
\author{
Quresh S. Latif, Rocky Mountain Research Station, U.S. Forest Service
}

\seealso{
Mahal_HSI, BBWO_MxWLRHSIs, BBWO_Mxnt3v, BBWO_Mxntbrn, BBWO_IndivModelHSIs
}
\examples{
Data <- BBWO_IndivModelHSIs(lndscp) # lndscp = matrix or data frame with required input variables.
EnsPred <- BBWO_Ensemble(Data)
}
\keyword{models}% use one of  RShowDoc("KEYWORDS")
\keyword{Black-backed Woodpecker}
\keyword{habitat suitability}
\keyword{nesting}
\keyword{dry forest management}
\keyword{ensemble modeling}
