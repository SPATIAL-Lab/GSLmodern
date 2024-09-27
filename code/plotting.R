
# Read data
sa = read.csv("data/shortAll.csv")

# Parse core data
sa.DDQ = sa[sa$Core == "DDQ",]
sa.DDL = sa[sa$Core == "DDL",]

# Assign ages
load("out/adDDQ.rda")
load("out/adDDL.rda")

di.DDQ = match(sa.DDQ$Depth.mean, ad.DDQ$Depth)
di.DDL = match(sa.DDL$Depth.mean, ad.DDL$Depth)

sa.DDQ = cbind(sa.DDQ, 2007 - ad.DDQ[di.DDQ, 2:4])
sa.DDL = cbind(sa.DDL, 2007 - ad.DDL[di.DDL, 2:4])

# Read data
load("out/Rsummary.rda")

# Plot colors
alpha = 100
qbg = col2rgb("cadetblue")
qbg = rgb(qbg[1], qbg[2], qbg[3], alpha, maxColorValue = 255)
lbg = col2rgb("coral2")
lbg = rgb(lbg[1], lbg[2], lbg[3], alpha, maxColorValue = 255)


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


par(mar = c(5, 5, 1, 1))
plot(R.sum$Age.med, R.sum$R.med, pch = 21, bg = "cadetblue3", 
     xlim = rev(range(R.sum$Age.med)), #ylim = c(-1, 3.5),
     xlab = expression(""^{210}*"Pb age (CE)"),
     ylab = "Radiocarbon reservoir age")
arrows(R.sum$Age.025, R.sum$R.med, R.sum$Age.975, R.sum$R.med,
       length = 0, lwd = 4, col = qbg)
arrows(R.sum$Age.med, R.sum$R.025, 
       R.sum$Age.med, R.sum$R.975,
       length = 0, lwd = 4, col = qbg)

arrows(sa.DDL$Age.025, sa.DDL$d13C.carb, sa.DDL$Age.975, sa.DDL$d13C.carb,
       length = 0, lwd = 4, col = lbg)
arrows(sa.DDL$Age.med, sa.DDL$d13C.carb + 2 * sa.DDL$d13C.carb.sd, 
       sa.DDL$Age.med, sa.DDL$d13C.carb - 2 * sa.DDL$d13C.carb.sd,
       length = 0, lwd = 4, col = lbg)

lines(sa.DDQ$Age.med, sa.DDQ$d13C.carb)
lines(sa.DDL$Age.med, sa.DDL$d13C.carb)

points(R.sum$Age.med, R.sum$R.med, pch = 21, bg = "cadetblue3", cex = 1.5)
points(sa.DDL$Age.med, sa.DDL$d13C.carb, pch = 21, bg = "coral2", cex = 1.5)

abline(v = 1847, lty = 3)
abline(v = 1959, lty = 3)
box()

