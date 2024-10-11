# Test d18O model
source("code/models/Omodel.R")

## Get modern river and lake water data 
library(isoWater)
gsl.l = wiDB_data(projects = "00118")
gsl.r = wiDB_data(projects = "00021")
d18O = c(gsl.l$data$d18O, gsl.r$data$d18O)

## Test EI spectrum
O.tst = Omod(seq(0, 1, by = 0.05))

## Plot response
plot(O.tst$EI, O.tst$d18Oc)

## Compare with modern lake and river water
plot(O.tst$EI, O.tst$d18Ow)
d = density(d18O)
lines(d$y * 8, d$x)

# Test d13C model
source("code/models/Cmodel.R")

## Mixing spectrum
C.tst = Cmod(seq(0, 1, by = 0.05))
plot(C.tst)
