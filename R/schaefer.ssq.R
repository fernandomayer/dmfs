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
##' @param C Observed catch data
##' @param I Observed CPUE data
##' @return The squared deviations of observed and predicted CPUEs with
##'     a given set of parameters.
##' @author Fernando Mayer
##' @examples
##' data(simple)
##' schaefer.ssq(par = c(r = 1, K = 3000, q = 0.001), C =
##'     simple$Catch, I = simple$CPUE)
##' mod <- nlminb(c(r = 1, K = 3000, q = 0.001), schaefer.ssq,
##'               C = simple$Catch, I = simple$CPUE,
##'               lower = 0, upper = Inf)
##' mod$par
##' format(mod$par, digits = 4, scientific = FALSE)
##' @export
schaefer.ssq <- function(par, C, I){
    r <- par[1]
    K <- par[2]
    q <- par[3]
    Bpred <- schaefer.gen(r = r, K = K, C = C, sigmaproc = 0)
    Ipred <- q * Bpred
    ss <- sum((I - Ipred)^2)
    return(ss)
}
