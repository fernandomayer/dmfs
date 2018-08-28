##' Generate biomass values based on Schaefer model
##'
##' Based in a set of parameters, generate a sequence of biomass values
##'     based on the Schaefer model.
##' @title Generate biomass values form Schaefer's model
##' @param Binit Initial biomass
##' @param r Intrinsic growth rate
##' @param K Population's carrying capacity
##' @param C Observed catch data
##' @return A vector of biomass values with the same length as \code{C}
##' @author Fernando Mayer
##' @examples
##' B <- schaefer.gen(Binit = 2500, r = 0.8, K = 3000, C = runif(20, 70,
##'     900))
##' plot(B, type = "l")
##' @export
schaefer.gen <- function(Binit, r, K, C){
    n <- length(C)
    B <- numeric(n)
    B[1] <- Binit
    for(i in 2:n){
        B[i] <- (B[i-1] + r * B[i-1] * (1 - (B[i-1]/K))) - C[i-1]
    }
    return(B)
}
