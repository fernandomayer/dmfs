##' Function with sum of squares deviations
##'
##' This functions is built to be passed to an optimizarion function
##'     (e.g. \code{optim} or \code{nlminb}) and it minimizes the sum of
##'     squares of CPUEs observed and predicted with a given set of
##'     parameters. This is equivalent to the observation error only
##'     model (assuming normally distributed errors).
##' @title Schaefer sum of squares
##' @param par A vector in the following order: \code{r}, \code{K} and
##'     \code{q}
##' @param B1 Initial biomass value.
##' @param C Observed catch data
##' @param I Observed CPUE data
##' @return The squared deviations of observed and predicted CPUEs with
##'     a given set of parameters.
##' @author Fernando Mayer
##' @example examples/schaefer.ssq_examples.R
##' @export
schaefer.ssq <- function(par, B1, C, I){
    r <- par[1]
    K <- par[2]
    q <- par[3]
    n <- length(C)
    Bpred <- numeric(n)
    Bpred[1] <- B1
    for(i in 2:n){
        Bpred[i] <- Bpred[i-1] + r * Bpred[i-1] *
                     (1 - (Bpred[i-1]/K)) - C[i-1]
    }
    Ipred <- q * Bpred
    ss <- sum((I - Ipred)^2)
    return(ss)
}
