\name{plot.boot.lmf}
\alias{plot.boot.lmf}
\title{
Plotting function for class "boot.lmf"
}
\description{
Plots density plots for bootstrap replicates of parameters from objects
of class "lmf".
}
\usage{
  \method{plot}{boot.lmf}(x, what = c("all"), ...)
}
\arguments{
  \item{x}{an object of class "boot.lmf".
  }
  \item{what}{what parameters is to be plotted. Options are "projection",
  "alpha" and "all".
  }
  \item{\dots}{additional arguments to be passed to methods, such as
  graphical parameters (see \code{par}).
  }
}
\details{
Density of parameters is calculated using the function \code{density}.
}
\author{
Thomas Kvalnes
}
\seealso{
\code{\link{boot.lmf}}, \code{\link{density}}, \code{\link{plot.default}}
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
#Density plot
plot(b.1)
}
