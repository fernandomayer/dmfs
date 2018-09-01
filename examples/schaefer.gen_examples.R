set.seed(1)
B <- schaefer.gen(r = 0.8, K = 3000,
                  C = runif(20, 70, 900), sd.proc = 0.05)
plot(B, type = "l")
