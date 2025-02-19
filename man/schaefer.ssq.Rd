% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/schaefer.ssq.R
\name{schaefer.ssq}
\alias{schaefer.ssq}
\title{Schaefer sum of squares}
\usage{
schaefer.ssq(par, B1, C, I)
}
\arguments{
\item{par}{A vector in the following order: \code{r}, \code{K} and
\code{q}}

\item{B1}{Initial biomass value.}

\item{C}{Observed catch data}

\item{I}{Observed CPUE data}
}
\value{
The squared deviations of observed and predicted CPUEs with
    a given set of parameters.
}
\description{
Function with sum of squares deviations
}
\details{
This functions is built to be passed to an optimizarion function
    (e.g. \code{optim} or \code{nlminb}) and it minimizes the sum of
    squares of CPUEs observed and predicted with a given set of
    parameters. This is equivalent to the observation error only
    model (assuming normally distributed errors).
}
\examples{
data(simple)

## Try some parameters
plot(simple$CPUE)
Bobs <- schaefer.gen(r = 1, K = 3000, C = simple$Catch, sd.proc = 0)
## Scale down biomass to be in same scale as CPUE
lines(Bobs*0.01)

## Get sum of square value at this point
schaefer.ssq(par = c(r = 1, K = 3000, q = 0.01),
             B1 = 2500, C = simple$Catch, I = simple$CPUE)

## Minimize with nlminb
mod <- nlminb(c(r = 1, K = 3000, q = 0.01), schaefer.ssq,
              B1 = 2500, C = simple$Catch, I = simple$CPUE,
              lower = 0, upper = Inf)
mod$par
format(mod$par, digits = 4, scientific = FALSE)
}
\author{
Fernando Mayer
}
