\name{fs}
\alias{fs}
\title{
Estimate temporal coefficients of selection
}
\description{
\code{fs} is a function which estimates the temporal covariance matrix (i.e.
fluctuating selection) and the temporal mean alpha coefficients (i.e. directional
selection) through a numerical maximization of a loglikelihood function.
}
\usage{
fs(At, at, npar, nyear, method, control, ...)
}
\arguments{
  \item{At}{a list containing the named yearly variance-covariance matrices.
  Sorted by year.
  }
  \item{at}{a list containing the named yearly vectors of the estimated selection
  coefficients. Sorted by year.
  }
  \item{npar}{the number of parameters in the model for the estimates selection
  coefficients.
  }
  \item{nyear}{the number of years with estimates of selection.
  }
  \item{method}{defines what optimalization algorithm to be used in the maximization of the
  loglikelihood. Alternatives are: "Nelder-Mead", "BFGS" (default), "CG",
  "L-BFGS-B" and "SANN". Not all are applicable here. See \code{?optim} for details.
  }
  \item{control}{a list of control parameters for the maximization of the likelihood.
  \code{maxit} sets the maximum number of iterations to use before convergence
  and \code{reltol} sets the relative threshold for improvement in the likelihood
  which desides whether to continue maximation or end. See \code{?optim}
  for details.
  }
  \item{\dots}{additional arguments to be passed to optim for the maximization of the
  loglikelihood. See \code{?optim} for options.
  }
}
\details{
\code{fs} is used internally in \code{lmf} when estimating temporal coefficients
of selection, but given the right data can be executed as a standalone. The input
to the arguments At and at are estimated within \code{lmf} by correctly combining
estimates of coefficients and variance-covariance within each age class and year
to a estimate for each year.

\code{fs} used \code{optim} for the numerical maximization of the
log likelihood function \code{lnL.M}. Problems of non-positive definite matrices
appearing as maximas, due to numerical rounding, are solved with \code{nearPD},
which implement the smallest possible numerical changes of some componets of the
matrices to achive positive definiteness. These changes does not affect the
results in any significant way.

Engen et al. 2012 describe the maximum likelihood method in details.
}
\value{
\code{fs} returns a list containing the following components:
  \item{convergence}{"yes" indicates that the numerical maximation of the
  likelihood successfully converged before reaching the iteration limit \code{maxit}.}
  \item{iterations}{the number of iterations of the function in the numerical
  maximation of the likelihood.}
  \item{M}{the estimated temporal covariance matrix (fluctuating selection).}
  \item{aM}{the estimated temporal mean selection coefficients.}
}
\references{
Engen, S., Saether, B.-E., Kvalnes, T. and Jensen, H. 2012. Estimating fluctuating
selection in age-structured populations. Journal of Evolutionary Biology, 25, 1487-1499.
}
\author{
Thomas Kvalnes
}
\seealso{
  \code{\link{lmf}}, \code{\link{lnL.M}}, \code{\link{nearPD}}
}
\examples{
#Data set from Engen et al. 2012
data(sparrowdata)
#Fit model
lmf.1 <- lmf(formula = cbind(recruits, survival) ~ weight + tars,
               age = age, year = year, data = sparrowdata)
#Extract At and at
At <- lmf.1$At
at <- lmf.1$at
#Estimate temporal selection coefficients
flusel <- fs(At = At, at = at, npar = 3, nyear = 7, method = "BFGS",
  control = list(maxit = 500, reltol = sqrt(.Machine$double.eps)))
#View output
str(flusel)
flusel
}
