\name{se}
\alias{se}
\title{
Standard error
}
\description{
Calculates the standard error of the values in \code{x}
}
\usage{
se(x, na.rm = FALSE)
}
\arguments{
  \item{x}{a numeric vector.
  }
  \item{na.rm}{a logical value indicating whether \code{NA} values
  should be stripped before the computation proceeds.
  }
}
\details{
The standard error of a length one vector is \code{NA} and if \code{x} is not
a vector an error will be printed.

The standard error is defined as:
\deqn{SE = \sqrt{\frac{var(x)}{length(x)}}}{SE = sqrt(var(x)/length(x))}
}
\value{
The standard error of \code{x} is returned.
}
\author{
Thomas Kvalnes
}
\seealso{
  \code{\link{var}}, \code{\link{length}}, \code{\link{sqrt}}
}
\examples{
#The standard error of samples with the same mean and standard deviation, but
  #of different sizes.
se(rnorm(n = 10, mean = 5, sd = 1))
se(rnorm(n = 100, mean = 5, sd = 1))
se(rnorm(n = 1000, mean = 5, sd = 1))
se(rnorm(n = 10000, mean = 5, sd = 1))
}
