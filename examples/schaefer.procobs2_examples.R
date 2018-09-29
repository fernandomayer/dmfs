## Simulate data
set.seed(123)
sim <- simdata()
plot(sim)

## See parameters used (same as default here)
pars <- attr(sim, "pars")[-1]
## Fix order
(pars <- c(pars[1:3],
           sigmaproc = unname(pars[5]),
           sigmaobs = unname(pars[4])))

## Generate initial biomass values with this set of parameters
Bobs <- schaefer.gen(r = pars["r"], K = pars["K"],
                     C = sim$C, sd.proc = pars["sigmaproc"])
plot(Bobs, type = "l")
points(sim$I.procobs/pars["q"])

## See the nll at this point
schaefer.procobs2(par = pars, B1 = pars["K"],
                  I = sim$I.procobs, C = sim$C)

## Minimize nll with nlminb
fit <- nlminb(
    start = pars,
    objective = schaefer.procobs2,
    B1 = pars["K"],
    I = sim$I.procobs, C = sim$C,
    control = list(eval.max = 10000, iter.max = 10000, trace = 1),
    lower = 0, upper = Inf
)
fit$par
format(fit$par, digits = 4, scientific = FALSE)

## Plot predicted CPUE
Bpred <- schaefer.gen(r = fit$par["r"], K = fit$par["K"],
                      C = sim$C, sd.proc = 0)
Ipred <- fit$par["q"] * Bpred
plot(sim$I.procobs, ylim = c(0, 12))
lines(Ipred)

## Using bbmle
\dontrun{
library(bbmle)
parnames(schaefer.procobs2) <- names(pars)
fit.mle <- mle2(
    schaefer.procobs2,
    start = pars,
    data = list(B1 = pars["K"], I = sim$I.procobs, C = sim$C),
    optimizer = "nlminb",
    control = list(eval.max = 10000, iter.max = 10000, trace = 1),
    lower = c(r = 0, K = 0, q = 0, sigmaproc = 0, sigmaobs = 0),
    upper = c(r = Inf, K = Inf, q = Inf,
              sigmaproc = Inf, sigmaobs = Inf)
)
fit.mle
summary(fit.mle)

## Comparing
format(fit$par, digits = 4, scientific = FALSE)
format(fit.mle@coef, digits = 4, scientific = FALSE)

## Use bbmle profile
prof <- profile(fit.mle)
confint(prof)
plot(prof)
}
