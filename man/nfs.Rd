\name{nfs}
\alias{nfs}
\title{
Estimate temporal coefficients of selection assuming no fluctuating selection
}
\description{
\code{nfs} is a function which estimates the temporal covariance matrix
and the temporal mean alpha coefficients (i.e. directional
selection) under the assumption of no fluctuating selection.
}
\usage{
nfs(At, at, npar, nyear)
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
}
\details{
\code{nfs} is used internally in \code{lmf} when estimating temporal mean
coefficients of selection under the assumption of no fluctuating selection, but
given the right data can be executed as a standalone. The input to the
arguments At and at are estimated within \code{lmf} by correctly combining
estimates of coefficients and variance-covariance within each age class and year
to a estimate for each year.

Engen et al. 2012 describe the method in details.
}
\value{
\code{nfs} returns a list containing the following components:
  \item{Anf}{the estimated temporal covariance matrix under the assumption of
  no flucutating selection.}
  \item{anf}{the estimated temporal mean selection coefficients under the
  assumption of no fluctuating selection.}
}
\references{
Engen, S., Saether, B.-E., Kvalnes, T. and Jensen, H. 2012. Estimating fluctuating
selection in age-structured populations. Journal of Evolutionary Biology, 00, 000-000.
}
\author{
Thomas Kvalnes
}
\seealso{
  \code{\link{lmf}}, \code{\link{fs}}
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
#Estimate temporal selection coefficients under the assumption of no
  #fluctuating selection
noflusel <- nfs(At = At, at = at, npar = 3, nyear = 7)
#View output
str(noflusel)
noflusel
}
