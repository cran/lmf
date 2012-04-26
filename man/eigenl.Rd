\name{eigenl}
\alias{eigenl}
\title{
Calculating lambda, u and v
}
\description{
Calculates the deterministic multiplicative growth rate (lambda), the
stable age distribution (u) and the reproductive values (v) from a
given projection matrix.
}
\usage{
eigenl(pm)
}

\arguments{
  \item{pm}{
  a projection matrix. For instance, the output from the function \code{procomp}.
  }
}
\details{
Given a projection matrix (l), this function calculates the real dominant
eigenvalue (lambda), and the left (u) and right (v) eigenvectors, defined by
l*u = lambda*u and v*l = lambda*v.
}
\value{
\code{eigenl} returns a list containing the following components:
  \item{lambda}{the deterministic multiplicative growth rate of the population
  from which the projection matrix was estimated}
  \item{u}{the stable age distribution of the population from which the
  projection matrix was estimated}
  \item{u}{the reproductive values of the population from which the projection
  matrix was estimated}
}
\references{
Engen, S., Saether, B.-E., Kvalnes, T. and Jensen, H. 2012. Estimating fluctuating
selection in age-structured populations. Journal of Evolutionary Biology, 00, 000-000.
}
\author{
Thomas Kvalnes
}
\seealso{
  \code{\link{procomp}}, \code{\link{promat}}
}
\examples{
#Data set from Engen et al. 2012
data(sparrowdata)
#The uniue age classes
unique.age <- unique(sparrowdata$age)
#Estimate the projection matrix
projection.matrix <- promat(pc = procomp(a = sparrowdata, uage = unique.age), nage = length(unique.age))
#Estimate lambda, u and v
eigenl(pm = projection.matrix)
}
