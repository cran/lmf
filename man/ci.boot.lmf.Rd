\name{ci.boot.lmf}
\alias{ci.boot.lmf}
\alias{print.ci.boot.lmf}
\title{
Confidence intervals for class "boot.lmf"
}
\description{
Constructs confidence intervals (CIs) for the bootstrapped parameters in
an object of class "boot.lmf".
}
\usage{
ci.boot.lmf(x, clevel = 0.05)
}
\arguments{
  \item{x}{an object of class "boot.lmf".
  }
  \item{clevel}{the confidence level required.
  }
}
\details{
\code{ci.boot.lmf} construct confidence intervals (CIs) from the quantiles of the
bootstrap replicates and uses the function \code{quantile}.

See Engen et al. 2012 for details on the method for estimating and bootstrapping
the parameters.
}
\value{
\code{ci.boot.lmf} returns a list containing the following components:
  \item{call}{the matched call.}
  \item{nboot}{the number of bootstrap replicates generated.}
  \item{what}{which set of parameters which has been to bootstrapped. See
  \code{?boot.lmf} for details.}
  \item{clevel}{the confidence level specified.}
  \item{uage}{the unique age classes in the data set.}
  \item{nage}{the number of unique age classes in the data set.}
  \item{l}{CI for the projection matrix.}
  \item{luv}{CI for \eqn{\lambda}, u and v.}
  \item{sigma2.dj}{CI for the demographic variance for each age class.}
  \item{sigma2.d}{CI for the total demographic variance}
  \item{M}{CI for the estimated temporal covariance matrix.}
  \item{aM}{CI for the estimated temporal mean coefficients of selection}
  \item{sigma2.e}{CI for the environmental variance}
  \item{Anf}{CI for the estimated temporal covariance matrix assuming no
  fluctuating selection.}
  \item{anf}{CI for the estimated temporal mean selection coefficients assuming
  no fluctuating selection.}
  
}
\references{
Engen, S., Saether, B.-E., Kvalnes, T. and Jensen, H. 2012. Estimating fluctuating
selection in age-structured populations. Journal of Evolutionary Biology, 25, 1487-1499.
}
\author{
Thomas Kvalnes
}
\seealso{
 \code{\link{lmf}}, \code{\link{boot.lmf}}, \code{\link{quantile}}
}
\examples{
#Data set from Engen et al. 2012
data(sparrowdata)
#Fit model
lmf.1 <- lmf(formula = cbind(recruits, survival) ~ weight + tars,
               age = age, year = year, data = sparrowdata)
#Bootstrap parameters
b.1 <- boot.lmf(object = lmf.1, nboot = 10, sig.dj = TRUE,
 what = "all", asim = "parametric")
#Generate CI
ci.boot.lmf(b.1)
}
