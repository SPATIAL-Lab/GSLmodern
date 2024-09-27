# Prep calibration data

## Intcal, https://intcal.org/curves/intcal20.14c 
ic = read.csv("data/intcal20.14c", skip = 10)
names(ic) = c("CAL_BP", "Age_14C", "Sigma", "Delta_14C", "Sigma")

lambda = -1 / 8267

## Adjust to 2007
ic$age = ic$CAL_BP + 57
ic$age_14C = ic$Age_14C + 57

## Calculate F14C
ic$F14C = exp(-lambda * (ic$age - ic$age_14C))
ic$F14C.sd = exp(-lambda * ic$Sigma) - 1

## NH zone 1 bomb curve from https://doi.org/10.1017/RDC.2021.95 
bc = read.csv("data/bombCurve.csv")
bc$age = 2007 - bc$Year

plot(bc$age, bc$F14C, xlim = c(100, 0))
points(ic$age, ic$F14C)

cal = rbind(bc[c("age", "F14C", "F14C.sd")], ic[c("age", "F14C", "F14C.sd")])

save(cal, file = "out/cal14C.rda")
