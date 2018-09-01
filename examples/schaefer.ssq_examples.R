data(simple)

## Try some parameters
plot(simple$CPUE)
Bobs <- schaefer.gen(r = 1, K = 3000, C = simple$Catch, sd.proc = 0)
## Scale down biomass to be in same scale as CPUE
lines(Bobs*0.01)

## Get sum of square value at this point
schaefer.ssq(par = c(r = 1, K = 3000, q = 0.01),
             B1 = 2500, C = simple$Catch, I = simple$CPUE)

## Minimize with nlminb
mod <- nlminb(c(r = 1, K = 3000, q = 0.01), schaefer.ssq,
              B1 = 2500, C = simple$Catch, I = simple$CPUE,
              lower = 0, upper = Inf)
mod$par
format(mod$par, digits = 4, scientific = FALSE)
