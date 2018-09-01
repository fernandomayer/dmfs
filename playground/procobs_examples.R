##----------------------------------------------------------------------
## Using simulated data

## Simulate data
set.seed(123)
sim <- simdata()
plot(sim)

## See parameters used (same as default here)
(pars <- attr(sim, "pars")[-1])

## Generate initial biomass values with this set of parameters
Bobs <- schaefer.gen(r = pars["r"], K = pars["K"],
                     C = sim$C, sd.proc = pars["sd.proc"])
plot(Bobs, type = "l")
points(sim$I.procobs/pars["q"])

## See the nll at this point
schaefer.procobs(par = pars, B = Bobs,
                 I = sim$I.procobs, C = sim$C)

## Minimize nll with nlminb
fit <- nlminb(start = pars,
              objective = schaefer.procobs,
              B = Bobs,
              I = sim$I.procobs, C = sim$C,
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
    data = list(B = Bobs, I = sim$I.procobs, C = sim$C),
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

##----------------------------------------------------------------------
## Using simple data
data(simple)

## Try some parameters
pars <- c(r = 1, K = 2800, q = 0.01, sd.obs = 0.1, sd.proc = 0.05)
plot(simple$CPUE)
Bobs <- schaefer.gen(r = pars["r"], K = pars["K"],
                     C = simple$Catch, sd.proc = pars["sd.proc"])
## Scale down biomass to be in same scale as CPUE
lines(Bobs * pars["q"])

## See the nll at this point
schaefer.procobs(par = pars, B = Bobs,
                 I = simple$CPUE, C = simple$Catch)

## Minimize nll with nlminb
fit <- nlminb(start = pars,
              objective = schaefer.procobs,
              B = Bobs,
              I = simple$CPUE, C = simple$Catch,
              control = list(eval.max = 1000, iter.max = 1000),
              lower = 0, upper = Inf)
fit$par
format(fit$par, digits = 4, scientific = FALSE)

## See predicted values
Bpred <- schaefer.gen(r = fit$par["r"], K = fit$par["K"],
                      C = simple$Catch, sd.proc = fit$par["sd.proc"])
range(simple$CPUE, Bpred * fit$par["q"])

## Fit model
plot(simple$CPUE, ylim = c(8, 44))
lines(Bpred * fit$par["q"])

## Generate CPUE predicted
e.pred <- rnorm(length(Bpred), 0, fit$par["sd.obs"])
Ipred <- fit$par["q"] * Bpred
points(Ipred, pch = 16, col = 2)
Ipred2 <- fit$par["q"] * Bpred * exp(e.pred)
points(Ipred2, pch = 16, col = 4)

##----------------------------------------------------------------------
## Using albacore data
data(albacore)

## Try some parameters
pars <- c(r = 0.3, K = 300, q = 0.2, sd.obs = 0.01, sd.proc = 0.005)
plot(albacore$I, ylim = c(0, 100))
Bobs <- schaefer.gen(r = pars["r"], K = pars["K"],
                     C = simple$Catch, sd.proc = pars["sd.proc"])
## Scale down biomass to be in same scale as CPUE
lines(Bobs * pars["q"])
lines(pars["K"] * albacore$P * pars["q"], col = 2)
Bobs <- pars["K"] * albacore$P

## See the nll at this point
schaefer.procobs(par = pars, B = Bobs,
                 I = albacore$I, C = albacore$C)

## Minimize nll with nlminb
fit <- nlminb(start = pars,
              objective = schaefer.procobs,
              B = Bobs,
              I = simple$CPUE, C = simple$Catch,
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
    data = list(B = Bobs, I = albacore$I, C = albacore$C),
    optimizer = "nlminb",
    control = list(eval.max = 1000, iter.max = 1000, trace = 1),
    lower = c(r = 0, K = 0, q = 0, sigmaproc = 0, sigmaobs = 0),
    upper = c(r = Inf, K = Inf, q = Inf,
              sigmaproc = Inf, sigmaobs = Inf)
)
fit.mle
summary(fit.mle)
## Looks good!

## Use bbmle profile
prof <- profile(fit.mle, maxsteps = 1000, std.err = 0.01)
confint(prof)
plot(prof)

## Dont go
fit.mle2 <- mle2(
    schaefer.procobs,
    start = pars,
    data = list(B = Bobs, I = albacore$I, C = albacore$C),
    optimizer = "optim", method = "L-BFGS-B",
    control = list(maxit = 1000, trace = 1),
    lower = c(r = 0, K = 0, q = 0, sigmaproc = 0, sigmaobs = 0),
    upper = c(r = Inf, K = Inf, q = Inf,
              sigmaproc = Inf, sigmaobs = Inf)
)
