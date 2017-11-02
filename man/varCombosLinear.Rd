\name{varCombosLinear}
\alias{varCombosLinear}
\title{
Generate all allowable linear covariate combinations for model selection.
}
\description{
Generates a list of candidate variable combinations (linear effects only) for model selection, e.g., with AIC.
}
\usage{
varCombosLinear(vars, K.max = length(vars), exclude = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{V}{
Character vector naming all variables from which to compile candidate combinations.
}
  \item{K.max}{
Numeric scalar setting the maximum number of variables in any given combination (default = length(vars)).
}
  \item{exclude}{
List whose elements are character vectors. Each element must be length > 1 and should represent a disallowed covariate combination, e.g., to avoid multi-collinearity. If set to the default (NULL), all combinations will be allowed within the 'K.max' limit.
}
}
\details{
Generates a list of covariate combinations in the form "x1+x2+...+xK" with K = to the kth covariate and max k = 'K.max'. Elements of this list can be used to build formulas for use in linear regression modeling functions, e.g., lm, glm, etc.
}
\value{
List with elements are strings representing candidate covariate combinations of form "x1+x2+...+xK".
}
\author{
Quresh S. Latif, Rocky Mountain Research Station, U.S. Forest Service
}

\keyword{models}% use one of  RShowDoc("KEYWORDS")
\keyword{glm}
\keyword{regression analysis}
\keyword{model selection}
