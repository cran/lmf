\name{sparrowdata}
\alias{sparrowdata}
\docType{data}
\title{
House Sparrow data set
}
\description{
A data set on a wild population of house sparrow \emph{Passer domesticus}.
}
\usage{data(sparrowdata)}
\format{
  A data frame with 116 observations on the following 7 variables.
  \describe{
    \item{\code{id}}{unique individual identifier}
    \item{\code{year}}{the year of breeding}
    \item{\code{age}}{the age of the individual}
    \item{\code{survival}}{equals 1 if the individual survives, otherwise 0}
    \item{\code{recruits}}{the number of recruits produced, i.e. offspring that
    survives to the next breeding season}
    \item{\code{weight}}{the body mass of the individuals at fledgling, i.e.
    when the bird was ready to leave the nest}
    \item{\code{tars}}{the tarsus length of the individuals at fledgling, i.e.
    when the bird was ready to leave the nest}
  }
}
\details{
Further details on the data set is provided by Engen et al. 2012.
}
\references{
Engen, S., Saether, B.-E., Kvalnes, T. and Jensen, H. 2012. Estimating fluctuating
selection in age-structured populations. Journal of Evolutionary Biology, 25, 1487-1499.
}
\examples{
#Data set from Engen et al. 2012
data(sparrowdata)
#View data
str(sparrowdata)
head(sparrowdata, 10)
}
