\name{checkMC}
\alias{checkMC}
\title{
Calculate correlation matrix and variance inflation factors to check for multicollinearity.
}
\description{
Calculates variance inflation factors and correlation matrix for a list of variables and a given dataset.
}
\usage{
checkMC(data, vars)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Data frame with all variables of interest for which correlation matrix and variance inflation factors are desired (named in 'vars').
}
  \item{vars}{
Character vector listing column names in 'data' containing variables of interest.
}
}
\details{
Variance inflation factors (VIF = 1/[1 - R-squared]; Neter et al. 1996) and correlation matrix contains Pearson's coefficients. These can be used to assess the presence of multicollinearity for any multivariate linear modeling, after which variable combinations associated with multicollinearity can be dropped from consideration.
}
\value{
 \item{VIF} {Numeric vector (length = length(vars)) of variance inflation factors.}
 \item{cmat} {Matrix of Pearson's correlation coefficients (dim = c(length(vars), length(vars))).}
}
\references{
Neter, J., M. H. Kutner, C. J. Nachtsheim, and W. Wasserman. 1996. Applied linear statistical models. Times Mirror Higher Education Group, Inc.
}
\author{
Quresh S. Latif, Rocky Mountain Research Station, U.S. Forest Service
}

\examples{
## The function is currently defined as
function(data, vars) {
  VIF <- numeric(length=length(vars))
  for(i in 1:length(vars)) {
    x <- paste(vars, collapse="+")
    m <- lm(as.formula(paste(vars[i], "~", x, sep="")), data = data)
    Rsqr <- summary(m)$r.squared
    VIF[i] <- 1 / (1 - Rsqr) #Variance Inflation Factor
  }
  names(VIF) <- vars
  cmat <- cor(data[, vars])
  out <- list(VIF = VIF, cmat = cmat)
  return(out)
}
}
\keyword{models}% use one of  RShowDoc("KEYWORDS")
\keyword{glm}
\keyword{regression analysis}
\keyword{multicollinearity}
