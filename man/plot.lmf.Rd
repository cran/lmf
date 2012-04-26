\name{plot.lmf}
\alias{plot.lmf}
\title{
Plot diagnostics for an lmf object
}
\description{
Plots four diagnostic plots for objects of class "lmf": a plot of residuals against
fitted values, a Normal Q-Q plot, a Scale-Location plot of the square root of standardized
residuals against fitted values and a plot of standardized residuals against leverage with
indications of cooks distance.
}
\usage{
  \method{plot}{lmf}(x, what = "total", ...)
}
\arguments{
  \item{x}{
  an object of class "lmf".
  }
  \item{what}{
  the part of the model for which diagnostic plots should be generated. Options are
  'total' (default) for the overall plots and 'age-year' for plots for each separate linear
  regression for each age within each year.
  }
  \item{\dots}{
  additional arguments to be passed through to plotting functions.
  }
}
\details{
The function is constructed partly from modifications of script
from the \code{plot.lm} function in package \pkg{stats}. See \code{?plot.lm}
for details on the plots provided.
}
\author{
Thomas Kvalnes
}
\seealso{
  \code{\link{lmf}}, \code{\link{plot.lm}}
}
\examples{
#Data set from Engen et al. 2012
data(sparrowdata)
#Fit model
lmf.1 <- lmf(formula = cbind(recruits, survival) ~ weight + tars,
               age = age, year = year, data = sparrowdata)
#Diagnostic plots across all age classes and years
par(mfrow = c(2, 2))
plot(lmf.1)
#Diagnostic plots for each age class within each year (the
#actual linear regressions)
par(mfrow = c(2, 2))
plot(lmf.1, what = "age-year")
}
