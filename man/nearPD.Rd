\name{nearPD}
\alias{nearPD}
\title{
Find nearest positive definite matrix
}
\description{
Compute the nearest positive definite matrix to an approximate one,
typically a correlation or variance-covariance matrix.
}
\usage{
nearPD(x, corr = FALSE, keepDiag = FALSE, do2eigen = TRUE, doSym = FALSE,
doDykstra = TRUE, only.values = FALSE, only.matrix = TRUE, eig.tol = 1e-06,
conv.tol = 1e-07, posd.tol = 1e-08, maxit = 100, trace = FALSE)
}
\arguments{
  \item{x}{
  numeric \emph{n * n} approximately positive definite matrix, typically an
  approximation to a correlation or covariance matrix.
  }
  \item{corr}{logical indicating if the matrix should be a
  \emph{correlation} matrix.
  }
  \item{keepDiag}{logical, generalizing \code{corr}: if \code{TRUE}, the
  resulting matrix should have the same diagonal (\code{\link{diag}(x)}) as
  the input matrix.
  }
  \item{do2eigen}{logical indicating if a \code{posdefify}
  eigen step should be applied to the result of the Higham algorithm.
  }
  \item{doSym}{logical indicating if \code{X <- (X + t(X))/2} should be done,
  after \code{X <- tcrossprod(Qd, Q)}. Some doubt if this is necessary.
  }
  \item{doDykstra}{logical indicating if Dykstra's correction should be used;
  true by default. If false, the algorithm is basically the direct fixpoint
  iteration \eqn{Y(k) = P_U(P_S(Y(k-1)))}.
  }
  \item{only.values}{logical; if \code{TRUE}, the result is just the vector
  of eigen values of the approximating matrix.
  }
  \item{only.matrix}{logical indicating if only the matrix should be returned.
  }
  \item{eig.tol}{defines relative positiveness of eigenvalues compared to
  largest one, \eqn{\lambda_1}. Eigen values \eqn{\lambda_k} are treated
  as if zero when \eqn{\lambda_k / \lambda_1 = eig.tol}.
  }
  \item{conv.tol}{convergence tolerance for Higham algorithm.
  }
  \item{posd.tol}{tolerance for enforcing positive definiteness (in the
  final \code{posdefify} step when \code{do2eigen} is \code{TRUE}).
  }
  \item{maxit}{maximum number of iterations allowed.
  }
  \item{trace}{logical or integer specifying if convergence monitoring
  should be traced.
  }
}
\details{
This function is identical to \code{nearPD} in package \pkg{Matrix} as
far as the algorithmic method is concerned, but has an addition of the argument
\code{only.matrix} to ease its application within the function \code{fs},
has lost the argument \code{ensureSymmetry} and have a small change in the
list returned when \code{only.matrix = FALSE}.

Please see \code{nearPD} in package \pkg{Matrix} for further details.
}
\value{
\code{nearPD} returns a numeric vector of eigen values of
the approximating matrix if \code{only.values = TRUE}, returns the computed
positive definite matrix if \code{only.matrix = TRUE} and else returns a list
with the following componets:
  \item{mat}{matrix of class "dpoMatrix", the computed
  positive-definite matrix.}
  \item{eigenvalues}{numeric vector of eigenvalues of \code{mat}.}
  \item{corr}{logical, just the argument \code{corr}.}
  \item{normF}{the Frobenius norm (\code{\link{norm}(x-X, "F")}) of the
  difference between the original and the resulting matrix.}
  \item{iterations}{number of iterations needed.}
  \item{converged}{logical indicating if iterations converged.}
}
\references{
Cheng, S.H. and Higham, N. 1998. A Modified Cholesky Algorithm Based
on a Symmetric Indefinite Factorization. SIAM Journal on Matrix Analysis and
Applications, 19, 1097-1110.

Knol, D.L. and ten Berge, J.M.F. 1989. Least-squares approximation of an
improper correlation matrix by a proper one. Psychometrika, 54, 53-61.

Higham, N. 2002. Computing the nearest correlation matrix - a problem
from finance. IMA Journal of Numerical Analysis, 22, 329-343.
}
\author{
Jens Oehlschlaegel donated a first version. Subsequent
changes by the Matrix package authors and present modifications
by Thomas Kvalnes.
}
\seealso{
  \code{\link{fs}}, \code{\link{lmf}}, \code{\link[Matrix]{nearPD}},
  \code{\link[sfsmisc]{posdefify}}
}
\examples{
#Simulated non-positive definite (PD) matrix
nonPD <- matrix(c(2.04e-03, 3.54e-05, 7.52e-03, 3.54e-05, 6.15e-07,
  1.30e-04, 7.52e-03, 1.30e-04, 2.76e-02), ncol = 3)
#View eigenvalues (PD = only positive eigenvalues)
eigen(nonPD)
#Calculate PD matrix
PD <- nearPD(nonPD, only.matrix = TRUE)
PD
#View eigenvalues
eigen(PD)
#More thorough examples are given in the help pages for nearPD
 #in the Matrix package.
}
