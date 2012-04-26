\name{summary.boot.lmf}
\alias{summary.boot.lmf}
\alias{print.summary.boot.lmf}
\title{
Summarizing bootstraps of lmf fits
}
\description{
\code{summary} for class "boot.lmf".
}
\usage{
  \method{summary}{boot.lmf}(object, ret.bootstraps = FALSE, ...)
  \method{print}{summary.boot.lmf}(x, digits = max(3, getOption("digits") - 3),
  signif.stars = getOption("show.signif.stars"), ...)
}
\arguments{
  \item{object}{an object of class "boot.lmf".
  empty
  }
  \item{ret.bootstraps}{logical. If \code{TRUE} the bootstrap replicates are
  returned in the output.
  empty
  }
  \item{x}{an object of class "summary.boot.lmf".
  empty
  }
  \item{digits}{the number of significant digits to use when printing.
  empty
  }
  \item{signif.stars}{logical. If \code{TRUE}, 'significance stars' are printed
  for each coefficient.
  }
  \item{\dots}{further arguments passed to or from other methods.
  }
}
\details{
\code{summary.boot.lmf} formats bootstrap replicates in a user-friendly way,
and formats the temporal coefficients and variance-covariance matrix into
easily read tables for hypothesis tests.
}
\value{
The function \code{summary.boot.lmf} computes and returns a list of summary
statistics of the bootstrap replicates of a fitted \code{lmf} model given
in \code{object}.

An object of class "summary.boot.lmf" is a list containing at most the following
components:
  \item{call}{the matched call.}
  \item{nboot}{the number of bootstrap replicates generated.}
  \item{lest}{the estimated projection matrix.}
  \item{lboot.mean}{the bootstrap mean projection matrix.}
  \item{lbias}{the bootstrap bias of the components of the projection matrix.}
  \item{lboot.sd}{the boostrap standard deviation of the components of the
  projection matrix.}
   \item{luv}{the estimate, bootstrap mean, bias and standard deviation of the
  deterministic multiplicative growth rate of the population \eqn{(\lambda)},
  the stable age distribution (u) and the reproductive values (v).}
  \item{sigma2.e}{the estimate, bootstrap mean, bias and standard deviation of
  the environmental variance of the population.}
  \item{sigma2.dd}{the estimate, bootstrap mean, bias and standard deviation of
  the demographic variances (by age class and in total).}
  \item{aM}{the estimate, bootstrap mean, bias and standard deviation of the
  estimated temporal mean selection coefficients.}
  \item{Mest}{the estimated temporal variance-covariance matrix (M).}
  \item{Mboot.mean}{the bootstrap mean temporal variance-covariance matrix.}
  \item{Mbias}{the bootstrap bias of the components of the temporal
  variance-covariance matrix.}
  \item{Mboot.sd}{the boostrap standard deviation of the components of the
  temporal variance-covariance matrix.}
  \item{anf}{the estimate, bootstrap mean, bias and standard deviation of the
  estimated temporal mean selection coefficients under the assumption of no
  fluctuating selection.}
  \item{Anfest}{the estimated temporal variance-covariance matrix under the
  assumtion of no fluctuating selection.}
  \item{Anfboot.mean}{the bootstrap mean temporal variance-covariance matrix
  under the assumption of no fluctuating selection.}
  \item{Anfbias}{the bootstrap bias of the components of the temporal
  variance-covariance matrix under the assumption of no fluctuating selection.}
  \item{Anfboot.sd}{the boostrap standard deviation of the components of the
  temporal variance-covariance matrix under the assumption of no
  fluctuating selection.}
  \item{coefficients.aH0aMboot}{the estimated temporal mean selection
  coefficients, with bootstrapped standard errors, number of
  successes with regard to the null hypothesis and associated p-values. All
  under the specified null hypothesis \code{H0exp} and the assumption of fluctuating
  selection (\code{Hexp = "fs"}).}
  \item{coefficients.aH0anfboot}{the estimated temporal mean selection
  coefficients under the assumtion of no fluctuating selection, with
  bootstrapped standard errors, number of successes with regard to the null
  hypothesis and associated p-values. All under the specified null hypothesis
  \code{H0exp} and the assumption of no fluctuating selection
  (\code{Hexp = "nfs"}).}
  \item{coefficients.aH0Mnfboot}{the estimated temporal components of the
  variance-covariance matrix, with bootstrapped standard errors, number of
  successes with regard to the null hypothesis and associated p-values. All
  under the specified null hypothesis \code{H0exp} and the assumption of
  directional selection (\code{Hexp = "ds"}).}
  \item{lluvboot}{the bootstrap replicates of the projection matrix (columns
  f (fecundity) and s (survial)), lambda, the stable age distribution (u)
  and the reproductive values (v). Numbers in the column names
  indicate age class.}
  \item{deboot}{the bootstrap replicates of the demographic and environmental
  variances. Numbers in the column names indicate age class.}
  \item{atAboot}{the bootstrap replicates of the yearly coefficients of
  selection (at) and variance-covariance matrix (At). The first column indicate
  boostrap number and the second the boostrapped year. The subsequent columns
  contain coefficients (where (Intercept) (at) is the first coefficient), and components of the
  variance-covariance matrix (where (Intercept)-(Intercept) (At) is the first
  component (from the diagonal) of the matrix)}
  \item{aMMboot}{the bootstrap replicates of the temporal mean coefficients of
  selection (aM) and variance-covariance matrix (M). The first columns
  contain coefficients (where (Intercept) (a(M)) is the first coefficient), and
  subsequent columns contain the components of the variance-covariance
  matrix (where (Intercept)-(Intercept) (M) is the first
  component (from the diagonal) of the matrix)}
  \item{atCboot}{the bootstrap replicates of the yearly coefficients of
  selection (atC) corrected for sampling error. The first column indicate
  boostrap number, the second the boostrapped year and the subsequent columns
  contain the boostrapped coefficients.}
  \item{anfAboot}{the bootstrap replicates of the temporal mean coefficients of
  selection (anf) and variance-covariance matrix under the assumption of no
  fluctuating selection. The first columns contain coefficients
  (where (Intercept) (a(M=0)) is the first coefficient), and
  subsequent columns contain the components of the variance-covariance
  matrix (where (Intercept)-(Intercept) (At(M=0)) is the first
  component (from the diagonal) of the matrix)}
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
#Bootstrap parameters
b.1 <- boot.lmf(object = lmf.1, nboot = 10, sig.dj = TRUE,
 what = "all", asim = "parametric")
#Summary
summary(b.1)
}
