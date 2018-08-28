oldwd <- getwd()
setwd("data-raw")
simple <- read.csv2("simple_data.csv")
str(simple)
## Calculate effort in fisherman/day
simple$Eff <- simple$Fisherman/simple$Days
## Calculate observed CPUE
simple$CPUE <- simple$Catch/simple$Eff
str(simple)
select <- c("Year", "Catch", "Eff", "CPUE")
simple <- simple[, select]
save(simple, file = "../data/simple.rda")
setwd(oldwd)
