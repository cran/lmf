\name{inv}
\alias{inv}
\title{
Calculate the inverse of a matrix
}
\description{
Calculates the inverse of a symmetric positive definite matrix from its
Cholesky composition.
}
\usage{
inv(a)
}
\arguments{
  \item{a}{a symmetric positive definite matrix
  }
}
\details{
This is an interface to the LAPACK routine DPOTRI implemented in the function
\code{chol2inv}. The function calculates cholesky decomposition of the given
matrix and inputs this into \code{chol2inv}.
}
\value{
The inverse of the given matrix.
}
\references{
Anderson. E. et al. 1999. LAPACK Users' Guide. Third Edition.
SIAM [Available online at: http://www.netlib.org/lapack/lug/lapack_lug.html].
}
\author{
Thomas Kvalnes
}
\seealso{
  \code{\link{chol}}, \code{\link{chol2inv}}
}
\examples{
#Example matrix
mat <- matrix(c(1, 0.5, -3, 0.5, 3, 0.5, -3, 0.5, 12), ncol = 3)
#Show that it is positive definite
eigen(mat)$values
#Calculate inverse
inv(mat)
}
