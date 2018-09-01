##' Simulate a complete set of biomass dynamic data
##'
##' Simulate data needed for a biomass dynamic model. By default, data
##'     is simulated with both process and observation errors.
##'
##' Data are simulated from the Schaefer dynamic (process) model
##' \deqn{B_t = [B_{t-1} + r B_{t-1} (1 - B_{t-1}/K) - C_{t-1}]e^{u_t}}
##' for \eqn{t = 2, \ldots, n}, where \eqn{B_t} is the biomass at time
##' \eqn{t}, \eqn{r} is the growth rate, \eqn{K} is the carrying
##' capacity, \eqn{C_t} the observed catch, and \eqn{u_t \sim
##' N(0,\sigma^2_{proc})} is the process error (so that errors are
##' multiplicative log-normal). The initial biomass is defined as
##' \deqn{B_1 = K e^{u_1}}
##' i.e., it is assumed that the size of the population is at its
##' carrying capacity at the begginig of the series. The observation
##' equation is then
##' \deqn{I_t = [q B_t]e^{v_t}}
##' for \eqn{t = 1, \ldots, n}, where \eqn{q} is the catchability
##' coefficient and \eqn{v_t \sim N(0, \sigma^2_{obs})}, so the errors
##' are also multiplicative log-normal.
##'
##' To simulate process error only data, set \code{sd.obs} to
##' 0. For observation error data, set \code{sd.proc} to
##' 0. For deterministic data (no errors) set both to 0.
##'
##' When \code{eff.random} is \code{TRUE} (the default), the fishing
##' effort will be randomly sampled from a vector of values defined in
##' \code{eff}. If you want to simulate from specific values of effort,
##' then pass the values to \code{eff} and set \code{eff.random} to
##' \code{FALSE}. Note that in this case, the length of \code{eff} must
##' be the same as \code{n}. Argument \code{eff} doesn't have a specific
##' unit, it is generally set as "fishing units", such that a fishing
##' unit can be whatever is suitable (e.g. 1000 hooks, 10 fishing days,
##' etc).
##'
##' The simulation is made as follows. Given a set of parameters \eqn{r,
##' K, q, \sigma_obs, \sigma_proc}, the initial biomass is calculated.
##' Then we calculate the fishing mortality \eqn{F_t = qf_t}, which is
##' used to determine catches by \eqn{C_t = F_t B_t}. Then catches are
##' used to determine biomass, and biomass to determine CPUEs.
##'
##' @title Simulate data
##' @param n Number of time steps
##' @param r Intrinsic growth rate
##' @param K Population's carrying capacity
##' @param q Capturability coefficient
##' @param sd.obs Observation standard deviance
##' @param sd.proc Process standard deviance
##' @param eff Fishing effort (in units of fishing effort, see Details)
##' @param eff.random If \code{TRUE} (default), sample from a vector of
##'     possible effort values (see Details)
##' @return A data frame (also with class \code{dmfs}) with 7 columns:
##' \describe{
##' \item{f}{The fishing effort}
##' \item{C}{Simulated catch}
##' \item{I}{Simulated deterministic CPUE}
##' \item{B}{Simulated deterministic biomass}
##' \item{I.obs}{Simulated observation-error only CPUE}
##' \item{B.proc}{Simulated process-error only biomass}
##' \item{I.procobs}{Simulated CPUE with both process and observation
##'     errors}
##' }
##' The object also contains the parameter values used as attributes.
##' @examples
##' ## Using default arguments
##' set.seed(1)
##' sim <- simdata()
##' str(sim)
##' attr(sim, "pars")
##' plot(sim)
##' ## Using non-random effort
##' n <- 100
##' eff <- c(
##'     rep(1, 25),
##'     seq(1, 20, length = 25),
##'     rep(20, 25),
##'     seq(20, 1, length = 25)
##' )
##' sim <- simdata(n = n, eff = eff, eff.random = FALSE)
##' plot(sim)
##' @author Fernando Mayer
##' @importFrom stats rnorm
##' @export
simdata <- function(n = 30, r = 0.5, K = 1000, q = 0.01,
                    sd.obs = 0.1, sd.proc = 0.05,
                    eff = 1:20, eff.random = TRUE){
    ## Get the list of arguments to set as attributes
    args <- as.list(environment())[1:6]
    ## Error handling
    if(isFALSE(eff.random) & length(eff) != n){
        stop("n and eff must have the same length")
    }
    ## If eff.random is true, then sample from that vector, otherwise
    ## use it as is
    if(eff.random){
        f <- sample(eff, size = n, replace = TRUE)
    } else{
        f <- eff
    }
    ## Calculate fishing mortality
    F <- q * f
    ## Initiate vectors
    B <- C <- I <- numeric(n)
    ## Observation error
    e.obs <- rnorm(n, 0, sd.obs)
    ## Process error
    e.proc <- rnorm(n, 0, sd.proc)
    ## Biomass initial value
    B[1] <- K * exp(e.proc[1])
    ## Generate deterministic series
    for(i in 2:n){
        C[i-1] <- F[i-1] * B[i-1]
        B[i] <- B[i-1] + r * B[i-1] * (1 - B[i-1]/K) - C[i-1]
        I[i-1] <- q * B[i-1]
    }
    C[n] <- F[n] * B[n]
    I[n] <- q * B[n]
    ## Observation error only
    I.obs <- I * exp(e.obs)
    ## Process error only
    B.proc <- B * exp(e.proc)
    ## Observation and process error
    I.procobs <- q * B.proc * exp(e.obs)
    ## Data frame to return
    out <- data.frame(f, C, I, B, I.obs, B.proc, I.procobs)
    class(out) <- c("dmfs", "data.frame")
    attr(out, "pars") <- unlist(args)
    return(out)
}
