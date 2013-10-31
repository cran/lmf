\name{lmf}
\alias{lmf}
\alias{print.lmf}
\title{
Fitting age-structured selection model
}
\description{
\code{lmf} fit linear models within each combination of year and age class and
estimates coefficients of selection using maximum likelihood procedures.
\code{lmf} is compatible with populations without age-structure.
}
\usage{
lmf(formula, age, year, data, na.action = na.exclude,
method = c("BFGS"), control = list(maxit = 500,
reltol = sqrt(.Machine$double.eps)), ...)
}
\arguments{
  \item{formula}{
  an object of class "formula" (or one that can be coerced to that class): a
  symbolic description of the model to be fitted when estimating coefficients of
  selection. Format: response ~ terms. The detail of model
  specification are given under 'Details'.
  }
  \item{age}{
  used to define the name of the age column in the data set. Use NULL
  if no age data are available in the data set.
  }
  \item{year}{
  used to define the name of the year column in the data set.
  }
  \item{data}{
  data set with individual ids (optional), year of reproduction (year),
  maternal age (age; may be omitted if a model without age is desired), 
  number of female offspring (recruits), survival to the next reproductive
  event (survival) and phenotypic measurements. Age classes should have a
  natural order of increasing age. E.g. 1, 2, 3, ...
  }
  \item{na.action}{
  a function which indicate what should happend when the data contain NAs. The
  default is \code{na.exclude} (see \code{?na.fail}).
  }
  \item{method}{
  defines what optimalization algorithm to be used in the maximization of the
  loglikelihood. Alternatives are: "Nelder-Mead", "BFGS" (default), "CG",
  "L-BFGS-B" and "SANN". Not all are applicable here. See \code{?optim} for details.
  }
  \item{control}{
  a list of control parameters for the maximization of the likelihood.
  \code{maxit} sets the maximum number of iterations to use before convergence
  and \code{reltol} sets the relative threshold for improvement in the
  likelihood which desides whether to continue maximation or end.
  See \code{?optim} for details.
  }
  \item{\dots}{
  additional arguments to be passed to optim for the maximization of the
  loglikelihood. See \code{?optim} for options.
  }
}
\details{
\code{lmf} use formulas for model specification. These should be formatted as
decribed under arguments. Note however that your response should be specified
as a two-column matrix with the columns recruits and survival. The first column
should give the number of recruits that an individual produced a given year and
the second column should contain information on whether the individual survived
or not (1 or 0) to the next breeding season. These two columns will be used to
calculate the individual reproductive values (Wj) which the model will substitute
for the response in the age and year specific linear regressions
(i.e. \code{Wj ~ terms}).
}
\value{
\code{lmf} returns an object of class "lmf".

The function \code{summary} is used to obtain and
print a summary of the results. For construction of confidene intervals or
perform statistical inference on the parameters the function \code{boot.lmf} is used.

An object of class "lmf" is a list containing the following components:
  \item{running.time}{the total time used for computation.}
  \item{optim.time}{the time used for maximation of the loglikelihood.}
  \item{call}{the matched call.}
  \item{npar}{the number of parameters in the model.}
  \item{uage}{the unique age classes in the data set.}
  \item{nage}{the number of unique age classes in the data set.}
  \item{maxage}{the final age class.}
  \item{l}{the estimated projection matrix.}
  \item{lambda}{the deterministic multiplicative growth rate of the population.}
  \item{u}{the stable age distribution.}
  \item{v}{the vector of reproductive values for each age class.}
  \item{uyear}{the unique years in the data set.}
  \item{nyear}{the number of unique years in the data set.}
  \item{nobs}{the number of observations (counting individual-year).}
  \item{nobs.age}{the number of observations per age class.}
  \item{indnr}{assigned individual numbers (1:nobs).}
  \item{ajt}{a list containing the named vectors of the estimated selection
  coefficient for each age class within each year. Sorted by age class and year.}
  \item{Ajt.us}{a list containing the named unscaled variance-covariance matrix for
  each age class within each year. Sorted by age class and year.}
  \item{sigma.djt}{a list containing the vectors of residual standard errors from
  the linear regression for each age class within each year.
  Sorted by age class and year.}
  \item{dof}{a list containing the vectors of degrees of freedom (dof) from the
  linear regression for each age class within each year.
  Sorted by age class and year.}
  \item{res}{a list containing the vectors of residuals from the linear
  regression for each age class within each year. Sorted by age class and year.}
  \item{fit}{a list containing the vectors of fitted values from the linear
  regression for each age class within each year. Sorted by age class and year.}
  \item{leverage}{a list containing the vectors of estimated leverage for each
  data point from the linear regression for each age class within each year
  (see \code{lm.influence?}). Sorted by age class and year.}
  \item{cook}{a list containing the vectors of estimated Cook's distance for each
  data point from the linear regression for each age class within each year
  (see \code{cooks.distance?}). Sorted by age class and year.}
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
  \item{Ajt}{a list containing the named variance-covariance matrix (scaled by
  sigma2.dj) for each age class within each year. Sorted by age class and year.}
  \item{at}{a list containing the named vectors of the estimated selection
  coefficient for each year. Sorted by year.}
  \item{At}{a list containing the named variance-covariance matrix (scaled by
  sigma2.dj) for each year. Sorted by year.}
  \item{convergence}{"yes" indicates that the numerical maximation of the
  likelihood successfully converged before reaching the iteration limit \code{maxit}.}
  \item{iterations}{the number of iterations of the function in the numerical
  maximation of the likelihood.}
  \item{M}{the estimated temporal covariance matrix (fluctuating selection).}
  \item{aM}{the estimated temporal mean selection coefficients.}
  \item{atC}{the best linear predictor for the estimated yearly selection
  coefficients (i.e. corrected for sampling errors).}
  \item{Anf}{the estimated temporal covariance matrix assuming no
  fluctuating selection.}
  \item{anf}{the estimated temporal mean selection coefficients assuming
  no fluctuating selection.}
  \item{sigma2.e}{the environmental variance of the population.}
  \item{data.set}{the data set used in the analyses with a column of individual
  reproductive values addad}
}
\references{
Engen, S., Saether, B.-E., Kvalnes, T. and Jensen, H. 2012. Estimating fluctuating
selection in age-structured populations. Journal of Evolutionary Biology, 25, 1487-1499.
}
\author{
Thomas Kvalnes
}
\seealso{
  \code{\link{procomp}}, \code{\link{promat}}, \code{\link{eigenl}},
  \code{\link{lm.extract}}, \code{\link{fs}}, \code{\link{atCfn}},
  \code{\link{nfs}}, \code{\link{boot.lmf}}
}
\examples{
#Data set from Engen et al. 2012
data(sparrowdata)
#Fit model
lmf.1 <- lmf(formula = cbind(recruits, survival) ~ weight + tars,
               age = age, year = year, data = sparrowdata)
#View diagnostic plots
plot(lmf.1)
#View output
print(lmf.1)
#Print summary
summary(lmf.1)
}
