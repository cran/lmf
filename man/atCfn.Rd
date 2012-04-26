\name{atCfn}
\alias{atCfn}
\title{
Calculate corrected temporal mean coefficients of selection
}
\description{
Calculate the temporal mean coefficients of selection (alpha) corrected for
sampling error, i.e. the best linear predictor (BLP) of alpha.
}
\usage{
atCfn(aM, M, At, at)
}
\arguments{
  \item{aM}{the estimated temporal mean selection coefficients.
  }
  \item{M}{the estimated temporal covariance matrix (fluctuating selection).
  }
  \item{At}{a list containing the named variance-covariance matrix for
  each year. Sorted by year.
  }
  \item{at}{a list containing the named vectors of the estimated selection
  coefficient for each year. Sorted by year.
  }
}
\details{
Further details are found in Engen et al. 2012.
}
\value{
Returns a vector with the named best linear predictors for the temporal mean
coefficients of selection (alpha).
}
\references{
Engen, S., Saether, B.-E., Kvalnes, T. and Jensen, H. 2012. Estimating fluctuating
selection in age-structured populations. Journal of Evolutionary Biology, 00, 000-000.
}
\author{
Thomas Kvalnes
}
\seealso{
\code{\link{lmf}}
}
\examples{
#Data set from Engen et al. 2012
data(sparrowdata)
#Fit model
lmf.1 <- lmf(formula = cbind(recruits, survival) ~ weight + tars,
               age = age, year = year, data = sparrowdata)
#Extract aM, M, At and at
aM <- lmf.1$aM
M <- lmf.1$M
At <- lmf.1$At
at <- lmf.1$at
#Calculate Best Linear Predictor (BLP)
atCfn(aM = aM, M = M, At = At, at = at)
}
