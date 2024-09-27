
# Read data
sa = read.csv("data/shortAll.csv")

# Parse core data
holes = unique(sa$Core)
ci = match(sa$Core, holes)
sa.DDQ = sa[sa$Core == "DDQ",]
sa.DDL = sa[sa$Core == "DDL",]

# Assign ages
load("out/adDDQ.rda")
load("out/adDDL.rda")

di.DDQ = match(sa.DDQ$Depth.mean, ad.DDQ$Depth)
di.DDL = match(sa.DDL$Depth.mean, ad.DDL$Depth)

sa.DDQ = cbind(sa.DDQ, 2007 - ad.DDQ[di.DDQ, 2:4])
sa.DDL = cbind(sa.DDL, 2007 - ad.DDL[di.DDL, 2:4])

# Plot colors
alpha = 100
qbg = col2rgb("cadetblue")
qbg = rgb(qbg[1], qbg[2], qbg[3], alpha, maxColorValue = 255)
lbg = col2rgb("coral2")
lbg = rgb(lbg[1], lbg[2], lbg[3], alpha, maxColorValue = 255)

# Reservoir ages
rcd.DDQ = sa.DDQ[!is.na(sa.DDQ$Fmc), c(1, 2, 5, 12, 13)]
rcd.DDL = sa.DDL[!is.na(sa.DDL$Fmc), c(1, 2, 5, 12, 13)]

di.DDQ = match(rcd.DDQ$Depth.mean, ad.DDQ$Depth)
di.DDL = match(rcd.DDL$Depth.mean, ad.DDL$Depth)

load("bigout/ageModDDQ.rda")
load("bigout/ageModDDL.rda")
load("out/cal14C.rda")

ages.DDQ = post.DDQ$BUGSoutput$sims.list$age[, di.DDQ]
ages.DDL = post.DDL$BUGSoutput$sims.list$age[, di.DDL]

rcd = rbind(rcd.DDQ, rcd.DDL)
ages = cbind(ages.DDQ, ages.DDL)

## Start of iterative analysis
niter = 1000
R = matrix(nrow = nrow(rcd), ncol = niter)
for(i in 1:nrow(rcd)){
  ## Sample age
  a = sample(ages[, i], niter)
  Fmc = rnorm(niter, rcd$Fmc[i], rcd$Fmc.err[i])
  FmT = Fmc * exp(a / 8267)
  
  aind = integer(niter)
  for(j in 1:niter){
    aoff = abs(a[j] - cal$age)
    aind = match(min(aoff), aoff)
  }
  FmT_atm = rnorm(niter, cal$F14C[aind], cal$F14C.sd[aind])
  
  R[i, ] = -8033 * log(FmT / FmT_atm)
}
row.names(R) = rcd$ID

save(R, file = "out/Rages.rda")



# Plotting

par(mar = c(5, 5, 1, 1))
plot(sa.DDQ$Age.med, sa.DDQ$d13C.carb, pch = 21, bg = "cadetblue3", 
     xlim = rev(range(sa.DDQ$Age.med)), ylim = c(-1, 3.5),
     xlab = expression(""^{210}*"Pb age (CE)"),
     ylab = expression(delta^{13}*"C"["carbonate"]*" (VPDB)"))
arrows(sa.DDQ$Age.025, sa.DDQ$d13C.carb, sa.DDQ$Age.975, sa.DDQ$d13C.carb,
       length = 0, lwd = 4, col = qbg)
arrows(sa.DDQ$Age.med, sa.DDQ$d13C.carb + 2 * sa.DDQ$d13C.carb.sd, 
       sa.DDQ$Age.med, sa.DDQ$d13C.carb - 2 * sa.DDQ$d13C.carb.sd,
       length = 0, lwd = 4, col = qbg)

arrows(sa.DDL$Age.025, sa.DDL$d13C.carb, sa.DDL$Age.975, sa.DDL$d13C.carb,
       length = 0, lwd = 4, col = lbg)
arrows(sa.DDL$Age.med, sa.DDL$d13C.carb + 2 * sa.DDL$d13C.carb.sd, 
       sa.DDL$Age.med, sa.DDL$d13C.carb - 2 * sa.DDL$d13C.carb.sd,
       length = 0, lwd = 4, col = lbg)

lines(sa.DDQ$Age.med, sa.DDQ$d13C.carb)
lines(sa.DDL$Age.med, sa.DDL$d13C.carb)

points(sa.DDQ$Age.med, sa.DDQ$d13C.carb, pch = 21, bg = "cadetblue3", cex = 1.5)
points(sa.DDL$Age.med, sa.DDL$d13C.carb, pch = 21, bg = "coral2", cex = 1.5)

abline(v = 1847, lty = 3)
abline(v = 1959, lty = 3)
box()

par(mar = c(5, 5, 1, 1))
plot(sa.DDQ$Age.med, sa.DDQ$d18O.carb, pch = 21, bg = "cadetblue3", 
     xlim = rev(range(sa.DDQ$Age.med)), ylim = c(-8, -4),
     xlab = expression(""^{210}*"Pb age (CE)"),
     ylab = expression(delta^{18}*"O"["carbonate"]*" (VPDB)"))
arrows(sa.DDQ$Age.025, sa.DDQ$d18O.carb, sa.DDQ$Age.975, sa.DDQ$d18O.carb,
       length = 0, lwd = 4, col = qbg)
arrows(sa.DDQ$Age.med, sa.DDQ$d18O.carb + 2 * sa.DDQ$d18O.carb.sd, 
       sa.DDQ$Age.med, sa.DDQ$d18O.carb - 2 * sa.DDQ$d18O.carb.sd,
       length = 0, lwd = 4, col = qbg)

arrows(sa.DDL$Age.025, sa.DDL$d18O.carb, sa.DDL$Age.975, sa.DDL$d18O.carb,
       length = 0, lwd = 4, col = lbg)
arrows(sa.DDL$Age.med, sa.DDL$d18O.carb + 2 * sa.DDL$d18O.carb.sd, 
       sa.DDL$Age.med, sa.DDL$d18O.carb - 2 * sa.DDL$d18O.carb.sd,
       length = 0, lwd = 4, col = lbg)

lines(sa.DDQ$Age.med, sa.DDQ$d18O.carb)
lines(sa.DDL$Age.med, sa.DDL$d18O.carb)

points(sa.DDQ$Age.med, sa.DDQ$d18O.carb, pch = 21, bg = "cadetblue3", cex = 1.5)
points(sa.DDL$Age.med, sa.DDL$d18O.carb, pch = 21, bg = "coral2", cex = 1.5)
abline(v = 1847, lty = 3)
abline(v = 1959, lty = 3)
box()






lc = read.csv("data/longCarb.csv")
lr = read.csv("data/longReservoir.csv")

#points(sa.3510$Age.med, sa.3510$d13C.carb, pch = 21, bg = "dark red")


plot(lc$d13C.carb, lc$d18O.carb, pch = 16, col = "light grey", 
     xlim = c(-3, 4))
points(sa$d13C.carb[sa$Depth.min != 0], sa$d18O.carb[sa$Depth.min != 0],
       pch = 21, bg = "light blue")
points(sa$d13C.carb[sa$Depth.min == 0], sa$d18O.carb[sa$Depth.min == 0],
       pch = 21, bg = "dark blue")
