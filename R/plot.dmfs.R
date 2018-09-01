##' Plot simulated data for objects of class \code{dmfs}.
##'
##' Use this function to visualize data simulated with
##'     \code{simdata()}.
##' @title Plot simulated data
##' @param x An object of class \code{dmfs}.
##' @param qscale The parameter \eqn{q} (catchability coefficient), used
##'     to scale CPUE values so that they are at the same scale as
##'     biomass values (this is just for visualization purposes). If
##'     nothing is specified, then by default it will be used the same
##'     value for \code{q} used in the \code{simdata()} function
##'     (recommended).
##' @param ... Not used.
##' @return Plots of simulated dynamics.
##' @author Fernando Mayer
##' @examples
##' set.seed(1)
##' sim <- simdata()
##' plot(sim)
##' @import graphics
##' @export
plot.dmfs <- function(x, qscale, ...){
    op <- par(no.readonly = TRUE)
    ## Scaling factor for observations. If qscale is not specified, then
    ## use q from attributes of x
    q <- ifelse(missing(qscale),
                attr(x, "pars")["q"],
                qscale)
    ## Scale columns that start with I (observations)
    x[, grep("^I", names(x))] <- x[, grep("^I", names(x))]/q
    with(x,{
        rg <- range(I.procobs)
        ylim <- c(rg[1]*0.8, rg[2]*1.2)
        par(mfrow = c(4, 1))
        plot(B, type = "l", ylim = ylim, main = "No error",
             xlab = "Time")
        points(I)
        plot(B, type = "l", ylim = ylim, xlab = "Time",
             main = "Observation error only")
        lines(I.obs, type = "b", lty = 2)
        plot(B, type = "l", ylim = ylim, xlab = "Time",
             main = "Process error only")
        lines(B.proc, col = 2)
        plot(B, type = "l", ylim = ylim, xlab = "Time",
             main = "Observation and process error")
        lines(B.proc, col = 2)
        lines(I.procobs, type = "b", lty = 3)
    })
    on.exit(par(op), add = TRUE)
}
