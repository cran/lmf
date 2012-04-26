\name{projection}
\alias{projection}
\alias{procomp}
\alias{promat}
\title{
The projection matrix
}
\description{
Estimates the components of the projection matrix and constructs the matrix
given data on fecundity and survival in a age-structured population.
}
\usage{
procomp(a, uage)
promat(pc, nage)
}
\arguments{
  \item{a}{
  a data set with fecundity and survival of individuals with known age over
  several years. Each individual can be represented once a year.
  }
  \item{uage}{
  the unique age classes in the data set.
  }
  \item{pc}{
  a matrix with columns age, fecundity and survival, i.e. the output from
  running the function \code{procomp}.
  }
  \item{nage}{
  the number of unique age classes in the data set.
  }
}
\details{
This function was made for internal use in \code{lmf}, but can be applied as a
standalone.

The data set needs to contain the columns recruits (number of female offspring),
survival (1 if the individual survives to the next reproductive event and
0 if it dies) and age.

The contruction of the projection matrix is divided into two functions for ease
of storing and handling the estimates.
}
\value{
\code{procomp} returns a matrix with the components of the projection matrix having
columns age, fecundity and survival, while \code{promat} takes the output from
\code{procomp} and returns the projection matrix.
}
\references{
Caswell, H. 2000. Matrix Population Models, 2nd edn. Sinauer, Sunderland,
Massachusetts.
Engen, S., Saether, B.-E., Kvalnes, T. and Jensen, H. 2012. Estimating fluctuating
selection in age-structured populations. Journal of Evolutionary Biology, 00, 000-000.
}
\author{
Thomas Kvalnes
}
\seealso{
  \code{\link{eigenl}}
}
\examples{
#Data set from Engen et al. 2012
data(sparrowdata)
#The uniue age classes
unique.age <- unique(sparrowdata$age)
#Estimate the components of the projection matrix
projection.components <- procomp(a = sparrowdata, uage = unique.age)
#View components
projection.components
#Set up the projection matrix
promat(pc = projection.components, nage = length(unique.age))
}
