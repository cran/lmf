\name{boot.lmf}
\alias{boot.lmf}
\alias{print.boot.lmf}
\title{
Bootstrap resampling for class "lmf"
}
\description{
Generates bootstrap replicates of the estimated parameters in a "lmf" model.
Ordinary bootstrap is performed for the projection matrix, while both
parametric and ordinary (non-parametric) resampling is available for the
remaining parameters in the model. In addition, bootstrapping under any choosen
null hypothesis is available for hypothesis testing.
}
\usage{
boot.lmf(object, nboot = 1000, what = c("projection", "alpha", "H0", "all"),
  asim = c("ordinary", "parametric"), sig.dj = TRUE,
  H0exp = list(alpha = NULL, M = NULL),
  H0con = c("fs", "nfs", "ds", "nds"), method = c("BFGS"),
  control = list(maxit = 500, reltol = sqrt(.Machine$double.eps)), ...)
}
\arguments{
  \item{object}{a fitted object of of class "lmf".
  }
  \item{nboot}{the number og bootstraps desired.
  }
  \item{what}{which set of parameters to bootstrap. Options are "projection" to
  only resample projection matrix, growth rate \eqn{(\lambda)}, stable age
  distribution (u) and reproductive values (v). "alpha" to resample demographic
  and environmental variances as well as all the estimates selection parameters.
  "H0" to resample temporal coefficients of selection under a given null
  hypothesis (This options requires specification of the additional
  arguments \code{H0exp} and \code{H0con}). "all" (default) to resample all the
  above mentioned parameters (also here \code{H0exp} and \code{H0con} must be
  specified for hypothesis testing or only "projection" and "alpha" will
  be resampled).
  }
  \item{asim}{the type of bootstrap for the parameters other than the
  projection matrix (which is always ordinary bootstrapped). Options are
  "parametric" (default) and "ordinary".
  }
  \item{sig.dj}{logical, \code{TRUE}(default) to include uncertainty in the
  estimation of the demographic variance when bootstrapping alpha estimates.
  }
  \item{H0exp}{a list with the first element a vector containing the expected
  temproal mean coefficients of selection (alpha) and the second element a
  matrix containing the elements of the expected temporal variance-covariance
  matrix (M) under the null hypothesis. This argument needs to be specified to
  perform hypothesis testing.
  }
  \item{H0con}{the conditions under which the null hypothesis should be tested.
  Options are "fs" to assume fluctuating selection, "nfs" to assume no
  fluctuating selection, "ds" to assume directional selection and "nds" to
  assume no directional selection. "nds" is not implemented due to increased
  risk of Type I error if the assumption is not correct, but is included
  here for completeness.
  }
  \item{method}{defines what optimalization algorithm to be used in the
  maximization of the loglikelihood. Alternatives are: "Nelder-Mead",
  "BFGS" (default), "CG", "L-BFGS-B" and "SANN". Not all are applicable here.
  See \code{?optim} for details.
  }
  \item{control}{a list of control parameters for the maximization of the
  likelihood. \code{maxit} sets the maximum number of iterations to use before
  convergence and \code{reltol} sets the relative threshold for improvement in
  the likelihood which desides whether to continue maximation or end.
  See \code{?optim} for details.
  }
  \item{\dots}{additional arguments to be passed to optim for the maximization
  of the loglikelihood. See \code{?optim} for options.
  }
}
\details{
The resampling procedures preserve the observed ratios of the different age
classes during resampling of the projection matrix.

Ordinary bootstrap will often be subject to bias due to few years of data in
most available data sets within biology (generally << 40), thus the parametric
bootstrap is recomended for most purposes.

The bootstrap procedure is closely associated with the method deployed in
\code{lmf} and further details can be found in Engen et al. 2012.
}
\value{
\code{boot.lmf} returns a object of class "boot.lmf".

The function \code{summary} is used to obtain and
print a summary of the bootstrap replicates and to print results from tests of
hypotheses. For construction of confidene intervals for the parameters the
function \code{ci.boot.lmf} is used.

An object of class "boot.lmf" is a list containing at most the following
components:
  \item{running.time}{the total time used for computation.}
  \item{optim.time}{the time used for maximation of the loglikelihood.}
  \item{call}{the matched call.}
  \item{asim}{the value specified of asim.}
  \item{nboot}{the number of bootstrap replicates generated.}
  \item{uage}{the unique age classes in the data set.}
  \item{nage}{the number of unique age classes in the data set.}
  \item{npar}{the number of parameters in the model.}
  \item{uyear}{the unique years in the data set.}
  \item{nyear}{the number of unique years in the data set.}
  \item{l}{the estimated projection matrix.}
  \item{lboot}{the bootstrap replicates of the projection matrix.}
  \item{lambda}{the deterministic multiplicative growth rate of the population.}
  \item{u}{the stable age distribution.}
  \item{v}{the vector of reproductive values for each age class.}
  \item{luvboot}{the bootstrap replicates of \eqn{\lambda}, u and v.}
  \item{sigma2.dj}{a list containing the demographic variance for each age class.
  Sorted by age class.}
  \item{djboot}{the bootstrap replicates of \code{sigma2.dj}.}
  \item{sigma2.d}{the total demographic variance of the population.}
  \item{dboot}{the bootstrap replicates of \code{sigma2.d}.}
  \item{Atboot}{the bootstrap replicates of the yearly variance-covariance
  matrices. The unscaled variance-covariance matrices are kept constant, but
  each set of yearly estimates are scaled by the bootstrapped \code{sigma2.dj}.}
  \item{atboot}{the bootstrap replicates of the yearly coefficients of
  selection. This can be performed "parametric"(default) or "ordinary".}
  \item{M}{the estimated temporal covariance matrix (fluctuating selection).}
  \item{aM}{the estimated temporal mean coefficients of selection.}
  \item{Mboot}{the bootstrap replicates of \code{M}.}
  \item{aMboot}{the bootstrap replicates of \code{aM}.}
  \item{atCboot}{the bootstrap replicates of the best linear predictor for
  the estimated yearly coefficients of selection (i.e. corrected for
  sampling errors).}
  \item{Anf}{the estimated temporal covariance matrix assuming no
  fluctuating selection.}
  \item{anf}{the estimated temporal mean selection coefficients assuming
  no fluctuating selection.}
  \item{Anfboot}{the bootstrap replicates of \code{Anf}.}
  \item{anfboot}{the bootstrap replicates of \code{anf}.}
  \item{sigma2.e}{the environmental variance of the population.}
  \item{eboot}{the bootstrap replicates of \code{sigma2.e}.}
  \item{eCboot}{the bootstrap replicates of \code{sigma2.eC}.}
  \item{H0aMboot}{the bootstrap replicates of \code{aM} under the
  specified null hypothesis \code{H0exp} and the assumption of fluctuating
  selection (\code{Hexp = "fs"}).}
  \item{H0anfboot}{the bootstrap replicates of \code{anf} under the
  specified null hypothesis \code{H0exp} and the assumption of no fluctuating
  selection (\code{Hexp = "nfs"}).}
  \item{H0atnfboot}{the bootstrap replicates of \code{at} under the
  specified null hypothesis \code{H0exp} and the assumption of directional
  selection (\code{Hexp = "ds"}). These bootstrap replicates are used to
  generate \code{H0Mnfboot}.}
  \item{H0Mnfboot}{the bootstrap replicates of \code{M} under the
  specified null hypothesis \code{H0exp} and the assumption of directional
  selection (\code{Hexp = "ds"}).}
}
\references{
Engen, S., Saether, B.-E., Kvalnes, T. and Jensen, H. 2012. Estimating fluctuating
selection in age-structured populations. Journal of Evolutionary Biology, 00, 000-000.
}
\author{
Thomas Kvalnes
}
\seealso{
  \code{\link{lmf}}, \code{\link{ci.boot.lmf}}
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
#Print
b.1
#Summary
summary(b.1)
#View density plots
plot(b.1)
#Test of hypoteses
b.2 <- boot.lmf(object = lmf.1, nboot = 10, sig.dj = TRUE,
 what = "H0", H0exp = list(rep(0, 3), matrix(0, ncol = 3, nrow = 3)),
 asim = "parametric")
#Summary
summary(b.2)
}
