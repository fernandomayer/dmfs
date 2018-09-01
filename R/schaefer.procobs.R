##' Negative log-likelihood function for the Schaefer model with both
##' process and observation errors.
##'
##' This function calculates the negative log-likelihood for the
##'     Schaefer model, considering multiplicative log-normal errors for
##'     the process and observation equation.
##'
##' The function is suited for use in \code{optim()}, \code{nlminb()},
##'     or \code{bbmle::mle2()} (with minor modifications). The
##'     minimization will be made in the sum of both likelihhods.
##' @title Negative log-likelihood function for the Schaefer model
##' @param par A (named) vector with starting values of parameters:
##'     \code{r}, \code{K}, \code{q}, \code{sigmaproc} (process standard
##'     deviation), \code{sigmaobs} (observation standard deviation).
##' @param B A vector of biomass estimates calculated based on the
##'     initial parameters.
##' @param I A vector of observed CPUE values.
##' @param C A vector of observed catch values.
##' @return The negative log-likelihood value for a given set of
##'     parameters and data.
##' @author Fernando Mayer
##' @example examples/schaefer.procobs_examples.R
##' @importFrom stats dnorm
##' @export
schaefer.procobs <- function(par, B, I, C){
    r <- par[1]
    K <- par[2]
    q <- par[3]
    sigmaproc <- par[4]
    sigmaobs <- par[5]
    n <- length(I)
    Bpred <- numeric(n)
    Bpred[1] <- B[1]
    for(i in 2:n){
        Bpred[i] <- B[i-1] + r*B[i-1]*(1 - B[i-1]/K) - C[i-1]
    }
    ansproc <- -sum(dnorm(log(B), log(Bpred), sigmaproc, log = TRUE))
    Ipred <- numeric(n)
    for(i in 1:n){
        Ipred[i] <- q * Bpred[1]
    }
    ansobs <- -sum(dnorm(log(I), log(Ipred), sigmaobs, log = TRUE))
    res <- ansproc + ansobs
    return(res)
}
