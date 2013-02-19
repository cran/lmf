\name{summary.lmf}
\alias{summary.lmf}
\alias{print.summary.lmf}
\title{
Summarizing lmf fits
}
\description{
\code{summary} method for class "lmf".
}
\usage{
  \method{summary}{lmf}(object, what.level = c("age", "year", "total"), ...)
  \method{print}{summary.lmf}(x, digits = max(3, getOption("digits") - 3),
  signif.stars = getOption("show.signif.stars"), ...)
}
\arguments{
  \item{object}{an object of class "lmf".
  }
  \item{what.level}{the lowest level of organisation at which parameters should
  be summarized. Options are "age" to summarize from the level of specific age
  classes, "year" to summarize from the level of years and "total" (default) to
  summarize the temporal parameters.
  }
  \item{x}{an object of class "summary.lmf".
  }
  \item{digits}{the number of significant digits to use when printing.
  }
  \item{signif.stars}{logical. If \code{TRUE}, 'significance stars' are printed
  for each coefficient.
  }
  \item{\dots}{further arguments passed to or from other methods.
  }
}
\details{
\code{print.summary.lmf} formats the coefficients, standard errors and
variance-covariance matrices into easily read tables. The tests of significance
of the coefficients of selection should be considered as suggestive and should
be interpreted with caution. For tests of signifcanse a robust approach is
provided through the function \code{boot.lmf}.
}
\value{
The function \code{summary.lmf} computes and returns a list of summary
statistics of the fitted \code{lmf} model given in \code{object}.

An object of class "summary.lmf" is a list containing at most the following
components:
  \item{call}{the matched call.}
  \item{what.level}{the lowest level of organisation at which parameters should
  be summarized.}
  \item{uage}{the unique age classes in the data set.}
  \item{nage}{the number of unique age classes in the data set.}
  \item{uyear}{the unique years in the data set.}
  \item{nyear}{the number of unique years in the data set.}
  \item{l}{the estimated projection matrix.}
  \item{lambda}{the deterministic multiplicative growth rate of the population.}
  \item{u}{the stable age distribution.}
  \item{v}{the vector of reproductive values for each age class.}
  \item{sigma2.e}{the environmental variance of the population.}
  \item{sigma2.dj}{a list containing the demographic variance for each age class.
  Sorted by age class.}
  \item{sigma2.dj.dof}{a list containing the degrees of freedom (dof) for the
  demographic variance for each age class. Sorted by age class.}
  \item{sigma2.dj.sd}{a list containing the standard deviation (sd) for the
  demographic variance for each age class. Sorted by age class.}
  \item{sigma2.d}{the total demographic variance of the population.}
  \item{sigma2.d.dof}{the degrees of freedom (dof) for the total demographic
  variance of the population.}
  \item{sigma2.d.sd}{the standard deviation (sd) for the total demographic
  variance of the population.}
  \item{coefficients.ajt}{the estimated selection coefficients for each age
  class within each year, with standard errors, t values and p-values.}
  \item{Ajt}{a list containing the named variance-covariance matrix for each
  age class within each year. Sorted by age class and year.}
  \item{coefficients.at}{the estimated selection coefficients within each year,
  with standard errors, t values and p-values.}
  \item{At}{a list containing the named variance-covariance matrix for each
  year. Sorted by year.}
  \item{coefficients.atC}{the estimated selection coefficients within each year
  corrected for sampling error, with standard errors, t values and p-values.}
  \item{coefficients.aM}{the estimated temporal mean selection coefficients,
  with standard errors, t values and p-values.}
  \item{M}{the estimated temporal covariance matrix (fluctuating selection).}
  \item{coefficients.anf}{the estimated temporal mean selection coefficients
  under the assumtion of no fluctuating selection, with standard errors,
  t values and p-values.}
  \item{Anf}{the estimated temporal covariance matrix assuming no
  fluctuating selection.}
}
\author{
Thomas Kvalnes
}
\seealso{
  \code{\link{lmf}}, \code{\link{summary}}, \code{\link{boot.lmf}}
}
\examples{
#Data set from Engen et al. 2012
data(sparrowdata)
#Fit model
lmf.1 <- lmf(formula = cbind(recruits, survival) ~ weight + tars,
               age = age, year = year, data = sparrowdata)
#Summary
summary(lmf.1)
#Summary from within years
summary(lmf.1, what.level = "year")
#Summary from within age classes
summary(lmf.1, what.level = "age")
}
