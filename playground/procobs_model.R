##======================================================================
## schaefer.procobs() test
##======================================================================

## Simulate data
set.seed(123)
sim <- simdata()
plot(sim)

## See parameters used (same as default here)
(pars <- attr(sim, "pars")[-1])

## Generate initial biomass values with this set of parameters
Bobs <- schaefer.gen(r = pars["r"], K = pars["K"],
                     C = sim$C, sigmaproc = pars["sd.proc"])
plot(Bobs, type = "l")
points(sim$I.procobs/pars["q"])

## See the nll at this point
schaefer.procobs(par = pars, B = Bobs,
                 I = sim$I.procobs, C = sim$C)

## Minimize nll with nlminb
fit <- nlminb(start = pars,
              objective = schaefer.procobs,
              B = Bobs,
              I = s1$I.procobs, C = s1$C,
              control = list(eval.max = 1000, iter.max = 1000),
              lower = 0, upper = Inf)
fit$par
format(fit$par, digits = 4, scientific = FALSE)

## Using bbmle
library(bbmle)
parnames(schaefer.procobs) <- names(pars)
fit.mle <- mle2(
    schaefer.procobs,
    start = pars,
    data = list(B = Bobs, I = s1$I.procobs, C = s1$C),
    optimizer = "nlminb",
    control = list(eval.max = 1000, iter.max = 1000, trace = 1),
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
