##' Generate biomass values based on Schaefer model
##'
##' Based in a set of parameters, generate a sequence of biomass values
##'     based on the Schaefer model.
##' @title Generate biomass values form Schaefer's model
##' @param r Intrinsic growth rate
##' @param K Population's carrying capacity. This value will also be
##'     used as starting value for biomass \eqn{B_1} (plus error).
##' @param C Observed catch data
##' @param sd.proc Process standard deviation.
##' @return A vector of biomass values with the same length as \code{C}
##' @author Fernando Mayer
##' @examples
##' set.seed(1)
##' B <- schaefer.gen(r = 0.8, K = 3000, C = runif(20, 70,
##'     900), sd.proc = 0.05)
##' plot(B, type = "l")
##' @export
schaefer.gen <- function(r, K, C, sd.proc){
    n <- length(C)
    e.proc <- rnorm(n, 0, sd.proc)
    B <- numeric(n)
    B[1] <- K * exp(e.proc[1])
    for(i in 2:n){
        B[i] <- (B[i-1] + r * B[i-1] * (1 - (B[i-1]/K))) - C[i-1]
    }
    B <- B * exp(e.proc)
    return(B)
}
