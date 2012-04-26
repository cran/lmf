\name{lm.extract}
\alias{lm.extract}
\title{
Extract linear regression components
}
\description{
\code{lm.extract} fit a linear model and extract coefficients,
unscaled covariance matrix, residual variance, fitted values, residuals, degrees of freedom, and
leverage and cook's distance for each data point.
}
\usage{
lm.extract(formula, data, na.action = na.exclude)
}
\arguments{
  \item{formula}{
  an object of class "formula" (or one that can be coerced to that class): a
  symbolic description of the model to be fitted on the format \code{response ~ terms}.
  }
  \item{data}{
  a data set containing the variables in the model.
  }
  \item{na.action}{
  a function which indicate what should happend when the data contain NAs. The
  default is \code{na.exclude} (see \code{?na.fail}).
  }
}
\details{
\code{lm.extract} works through calls to \code{lm},  \code{residuals}, \code{predict},
\code{df.residuals}, \code{deviance}, \code{vcov}, \code{lm.influence} and \code{cooks.distance}.
Consult these functions for further details. The function was written for internal
use with \code{lmf}, but can be executed as a standalone.
}
\value{
\code{lm.extract} returns a list containing the following components:
  \item{ajt}{a named vector of coefficients}
  \item{res}{the residuals}
  \item{fit}{the fitted values}
  \item{dof}{the degrees of freedom}
  \item{sigma.djt}{the residual standard error}
  \item{Ajt.us}{a named unscaled variance-covariance matrix}
  \item{leverage}{the estimated leverage for each data point. I.e. a vector
  containing the diagonal of the 'hat' matrix (see \code{lm.influence?})}
  \item{cook}{the estimated Cook's distance for each data point (see \code{cooks.distance?})}
}
\author{
Thomas Kvalnes
}
\seealso{
  \code{\link{lm}}, \code{\link{summary.lm}}
}
\examples{
#Simulated data
xx <- rnorm(n = 100, mean = 10, sd = 2)
yy <- xx + 10 + rnorm(n = 100, 0, 2)
#Extract linear model components
extract <- lm.extract(formula = yy ~ xx, data = data.frame(xx = xx, yy = yy))
str(extract)
#Plot the xx-yy relation
plot(xx, yy)
abline(a = extract$ajt[1], b = extract$ajt[2])
}
