##' Negative log-likelihood function for the Schaefer model with both
##' process and observation errors.
##'
##' This function calculates the negative log-likelihood for the
##'     Schaefer model, considering multiplicative log-normal errors for
##'     the process and observation equation.
##'
##' This is an alternative approach (hopefully better) for
##' \code{schaefer.procobs2()}.
##'
##' The function is suited for use in \code{optim()}, \code{nlminb()},
##'     or \code{bbmle::mle2()} (with minor modifications). The
##'     minimization will be made in the sum of both likelihhods.
##' @title Negative log-likelihood function for the Schaefer model
##' @param par A (named) vector with starting values of parameters:
##'     \code{r}, \code{K}, \code{q}, \code{sigmaproc} (process standard
##'     deviation), \code{sigmaobs} (observation standard deviation).
##' @param B1 Initial biomass.
##' @param I A vector of observed CPUE values.
##' @param C A vector of observed catch values.
##' @return The negative log-likelihood value for a given set of
##'     parameters and data.
##' @author Fernando Mayer
##' @example examples/schaefer.procobs2_examples.R
##' @importFrom stats dnorm
##' @export
schaefer.procobs2 <- function(par, B1, I, C){
    r <- par[1]
    K <- par[2]
    q <- par[3]
    sigmaproc <- par[4]
    sigmaobs <- par[5]
    n <- length(I)
    Bpred <- numeric(n)
    Ipred <- numeric(n-1)
    ansproc <- numeric(n-1)
    ansobs <- numeric(n-1)
    Bpred[1] <- B1
    for(i in 1:(n-1)){
        Bpred[i+1] <- Bpred[i] + r*Bpred[i]*(1 - Bpred[i]/K) - C[i]
        ansproc[i] <- dnorm(log(Bpred[i+1]), log(Bpred[i]),
                            sigmaproc, log = TRUE)
        Ipred[i] <- q * Bpred[i]
        ansobs[i] <- dnorm(log(I[i]), log(Ipred[i]),
                           sigmaobs, log = TRUE)
    }
    res <- -sum(ansproc, ansobs)
    return(res)
}
