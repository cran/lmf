\name{lnL.M}
\alias{lnL.M}
\title{
Maximum likelihood function for temporal coefficients of selection
}
\description{
\code{lnL.M} calculates the loglikelihood of the maximum likelihood function for
temporal coefficients of selection from Engen et al. 2012. Returns the estimate
of alpha for a given temporal variance-covariance matrix (M) if desired.
}
\usage{
lnL.M(D, At, at, npar, ret.alphas = FALSE)
}
\arguments{
  \item{D}{a vector with the non-zero elements of the upper triangular matrix
  of the Cholesky decomposition of a temporal variance-covariance matrix M.
  }
  \item{At}{a list containing the named yearly variance-covariance matrices.
  Sorted by year.
  }
  \item{at}{a list containing the named yearly vectors of the estimated selection
  coefficients. Sorted by year.
  }
  \item{npar}{the number of parameters in the model for the estimates selection
  coefficients.
  }
  \item{ret.alphas}{logical. If TRUE the function returns the vector with the
  estimates of the temporal mean selection coefficients (alpha) for the given
  temporal variance-covariance matrix M. FALSE (default) makes the function
  return the loglikelihood for the given M.
  }
}
\details{
The function was developed for internal use in \code{fs}, but can be applied as
a standalone.

The upper triangular matrix (D) of the Cholesky decomposition of M is defined
as \code{M = t(D)\%*\%D}.

Details of the method is provided in Engen et al. 2012.
}
\value{
\code{lnL.M} returns the loglikelihood estimate (for \code{ret.alpha = FALSE})
or the temporal mean selection coefficients (for \code{ret.alpha = TRUE}) for a
given temporal variance-covariance matrix M.
}
\references{
Engen, S., Saether, B.-E., Kvalnes, T. and Jensen, H. 2012. Estimating fluctuating
selection in age-structured populations. Journal of Evolutionary Biology, 00, 000-000.
}
\author{
Thomas Kvalnes
}
\seealso{
  \code{\link{lmf}}, \code{\link{fs}}, \code{\link{chol}}
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
#Estimate D
D <- as.vector(chol((Reduce('+', At) / length(At))))
D <- D[D != 0]
#Estimate loglikelihood
lnL.M(D = D, At = At, at = at, npar = 3, ret.alphas = FALSE)
#Estimate temporal mean selection coefficients
lnL.M(D = D, At = At, at = at, npar = 3, ret.alphas = TRUE)
}
