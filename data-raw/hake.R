## This dataset is publicly available. I extracted it from the 'hake'
## dataset from the 'rcsurplus' package
oldwd <- getwd()
setwd("data-raw")
hake <- read.csv("hake.csv")
str(hake)
hake$cpue <- exp(hake$log_cpue)
all.equal(hake$catch/hake$effort, hake$cpue)
## Keep only CPUE
hake <- hake[, -4]
str(hake)
save(hake, file = "../data/hake.rda")
setwd(oldwd)
