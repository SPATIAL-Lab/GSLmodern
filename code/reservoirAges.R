
# Load data
sa = read.csv("data/shortAll.csv")
load("out/adDDQ.rda")
load("out/adDDL.rda")
load("bigout/ageModDDQ.rda")
load("bigout/ageModDDL.rda")
load("out/cal14C.rda")

# Parse core data
sa.DDQ = sa[sa$Core == "DDQ",]
sa.DDL = sa[sa$Core == "DDL",]

# Reservoir ages
rcd.DDQ = sa.DDQ[!is.na(sa.DDQ$Fmc), c(1, 2, 5, 12, 13)]
## DDQ_29 210Pb age uncertainty too high to usefully interpret reservoir age
## Also has unusually low d13C
rcd.DDQ = rcd.DDQ[rcd.DDQ$ID != "DDQ_29",]
rcd.DDL = sa.DDL[!is.na(sa.DDL$Fmc), c(1, 2, 5, 12, 13)]

di.DDQ = match(rcd.DDQ$Depth.mean, ad.DDQ$Depth)
di.DDL = match(rcd.DDL$Depth.mean, ad.DDL$Depth)

ages.DDQ = post.DDQ$BUGSoutput$sims.list$age[, di.DDQ]
ages.DDL = post.DDL$BUGSoutput$sims.list$age[, di.DDL]

rcd = rbind(rcd.DDQ, rcd.DDL)
ages = cbind(ages.DDQ, ages.DDL)

## Start of iterative analysis
niter = 1000

## Space for R values
R = matrix(nrow = nrow(rcd), ncol = niter)

## Iterate over samples
for(i in 1:nrow(rcd)){
  ## Draw 210Pb ages
  a = sample(ages[, i], niter)
  
  ## Draw Fmc values from samples
  Fmc = rnorm(niter, rcd$Fmc[i], rcd$Fmc.err[i])
  
  ## Convert to original Fmc
  ## Account for difference between 210Pb age model zero (2007) and analysis year (2011) 
  FmT = Fmc * exp((a + 4) / 8267)
  
  ## Space to index sample age against calibration curve
  aind = integer(niter)
  for(j in 1:niter){
    ## Find calibration curve age closest to sample age, return index
    aoff = abs(a[j] - cal$age)
    aind[j] = match(min(aoff), aoff)
  }
  
  ## Draw value of atmospheric Fmc at sample age
  FmT_atm = rnorm(niter, cal$F14C[aind], cal$F14C.sd[aind])
  
  ## Reservoir age
  R[i, ] = -8033 * log(FmT / FmT_atm)
}
row.names(R) = rcd$ID

save(R, file = "out/Rages.rda")

R.sum = t(apply(R, 1, quantile, c(0.025, 0.5, 0.975)))
Age.sum = 2007 - t(apply(ages, 2, quantile, c(0.025, 0.5, 0.975)))
R.sum = cbind(rcd, Age.sum, R.sum)
names(R.sum)[6:11] = c("Age.025", "Age.med", "Age.975", "R.025", "R.med", "R.975")

save(R.sum, file = "out/Rsummary.rda")

