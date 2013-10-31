\name{rmnorm}
\alias{rmnorm}
\title{
Generate random numbers from the multivariate normal distribution
}
\description{
\code{rmnorm} generate random numbers from a multivariate normal distribution.
}
\usage{
rmnorm(n = 1, mean = rep(0, d), varcov)
}
\arguments{
  \item{n}{the number of random vectors to be generated.
  }
  \item{mean}{a vector with means of length \emph{d}.
  }
  \item{varcov}{a variance-covariance matrix with dimentions \emph{d * d}.
  }
}
\details{
This is a modification of the function \code{rmnorm} provided in \pkg{mnormt}.
The function works around problems of non-positive definite
variance-covariance matrices due to numerical rounding by use of the
function \code{nearPD}. Furthermore, when only a single random vector
is generated, the function now returns a named random vector with names
inherited from the colum names of the variance-covariance matrix.
}
\value{
For n > 1 \code{rmnorm} returns a matrix of \code{n} rows of random vectors,
while for n = 1 \code{rmnorm} returns a named random vector.
}
\references{
Genz, A. 1992. Numerical Computation of Multivariate Normal Probabilities.
Journal of Computational and Graphical Statiststics, 1, 141-149.

Genz, A. 1993. Comparison of methods for the computation of multivariate
normal probabilities. Computing Science and Statistics, 25, 400-405.

Genz, A.: Fortran code available at
http://www.math.wsu.edu/math/faculty/genz/software/fort77/mvn.f
}
\author{
Fortran code of SADMVN and most auxiliary functions by Alan Genz, some
additional auxiliary functions by people referred to within his program.
Porting to R and additional R code by Adelchi Azzalini, with current
modifications by Thomas Kvalnes.
}
\seealso{
  \code{\link{Normal}}, \code{\link{rnorm}}, \code{\link[mnormt]{rmnorm}},
  \code{\link{nearPD}}
}
\examples{
#Variance-covariance matrix
varcov <- matrix(c(2.047737e-03, 3.540039e-05, 0.0075178920, 3.540039e-05,
6.122832e-07, 0.0001299661, 7.517892e-03, 1.299661e-04, 0.0276005740), ncol = 3)
#Set names
nam <- c("a", "b", "c")
dimnames(varcov) <- list(nam, nam)
#Check positive definiteness (all positive eigenvalues = positive definite)
eigen(varcov) $values
#Mean
mean <- c(1, 0.3, 0.5)
#Generate n = 1 random vector
rmnorm(n = 1, mean = mean, varcov = varcov)
#Generate n = 10 random vectors
rmnorm(n = 10, mean = mean, varcov = varcov)
#Generate n = 1 random vectors when varcov is non-positive definite
#Non-positive definite varcov matrix
varcov2 <- matrix(c(2.04e-03, 3.54e-05, 7.52e-03, 3.54e-05, 6.15e-07,
  1.30e-04, 7.52e-03, 1.30e-04, 2.76e-02), ncol = 3)
  dimnames(varcov2) <- dimnames(varcov)
eigen(varcov2)
#Random vector
rmnorm(n = 1, mean = mean, varcov = varcov2)
}
