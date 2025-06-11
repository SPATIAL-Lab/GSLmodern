# Load data
## Requires that 01_ageModel210.R has been run locally
source("code/X1_prep210.R")
source("code/X3_prepFigs.R")
load("out/data210_DDL.rda")
load("out/data210_DDQ.rda")
load("bigout/ageModDDL.rda")
load("bigout/ageModDDQ.rda")

# Depths vector
depths = dvec(32, 0.1)

# Plot space
png("out/FigS1.png", width = 6, height = 9, units = "in", res = 600)
layout(matrix(1:6, nrow = 3, byrow = TRUE))
par(mar = c(5, 5, 1, 1))

# 210Pb vs depth
plot(post.DDL$BUGSoutput$median$pb, depths, type = "l",
     xlim = range(c(post.DDL$BUGSoutput$median$pb, post.DDQ$BUGSoutput$median$pb)),
     ylim = rev(range(depths)), 
     xlab = expression("Unsupported "^{210}*"Pb (dpm/g)"),
     ylab = "Depth (cm)", lwd = 2, col = lbg)
points(data210.DDL$pb210$pb210, depths[data210.DDL$pb210$depth.ind], pch = 21,
       bg = lbg, cex = 2)

lines(post.DDQ$BUGSoutput$median$pb, depths, lwd = 2, col = qbg)
points(data210.DDQ$pb210$pb210, depths[data210.DDQ$pb210$depth.ind], pch = 21,
       bg = qbg, cex = 2)

text(par("usr")[1] + 0.05 * diff(par("usr")[1:2]),
     par("usr")[3] + 0.95 * diff(par("usr")[3:4]), "A")

# Cumulative density vs depth
plot(post.DDL$BUGSoutput$median$cdm, depths, type = "l", 
     xlim = range(c(post.DDL$BUGSoutput$median$cdm, post.DDQ$BUGSoutput$median$cdm)), 
     ylim = rev(range(depths)), 
     xlab = expression("Cumulative dry mass (g/cm"^2*")"),
     ylab = "Depth (cm)", lwd = 2, col = lbg)
points(data210.DDL$mass$cdm, depths[data210.DDL$mass$depth.ind], pch = 21,
       bg = lbg, cex = 2)

lines(post.DDQ$BUGSoutput$median$cdm, depths, lwd = 2, col = qbg)
points(data210.DDQ$mass$cdm, depths[data210.DDQ$mass$depth.ind], pch = 21,
       bg = qbg, cex = 2)

text(par("usr")[1] + 0.95 * diff(par("usr")[1:2]),
     par("usr")[3] + 0.95 * diff(par("usr")[3:4]), "B")

# Accumulation rate vs depth
ar.DDL = apply(post.DDL$BUGSoutput$sims.list$ar, 2, quantile, c(0.025, 0.5, 0.975))
ar.DDQ = apply(post.DDQ$BUGSoutput$sims.list$ar, 2, quantile, c(0.025, 0.5, 0.975))

plot(ar.DDL[2, ], depths, type = "l", xlim = range(rbind(ar.DDL, ar.DDQ)), 
     ylim = rev(range(depths)), 
     xlab = expression("Accumulation rate (g/cm"^2*"/year)"),
     ylab = "Depth (cm)", lwd = 2, col = lbg)
lines(ar.DDL[1,], depths, col = lbg)
lines(ar.DDL[3,], depths, col = lbg)

lines(ar.DDQ[2, ], depths, lwd = 2, col = qbg)
lines(ar.DDQ[1,], depths, col = qbg)
lines(ar.DDQ[3,], depths, col = qbg)

text(par("usr")[1] + 0.05 * diff(par("usr")[1:2]),
     par("usr")[3] + 0.95 * diff(par("usr")[3:4]), "C")

# Density vs depth
rho.DDL = apply(post.DDL$BUGSoutput$sims.list$rho, 2, quantile, c(0.025, 0.5, 0.975))
rho.DDQ = apply(post.DDQ$BUGSoutput$sims.list$rho, 2, quantile, c(0.025, 0.5, 0.975))

plot(rho.DDL[2, ], depths, type = "l", xlim = range(rbind(rho.DDL, rho.DDQ)), 
     ylim = rev(range(depths)), 
     xlab = expression("Bulk density (g/cm"^3*")"),
     ylab = "Depth (cm)", lwd = 2, col = lbg)
lines(rho.DDL[1,], depths, col = lbg)
lines(rho.DDL[3,], depths, col = lbg)

lines(rho.DDQ[2, ], depths, lwd = 2, col = qbg)
lines(rho.DDQ[1,], depths, col = qbg)
lines(rho.DDQ[3,], depths, col = qbg)

text(par("usr")[1] + 0.95 * diff(par("usr")[1:2]),
     par("usr")[3] + 0.95 * diff(par("usr")[3:4]), "D")

# Age vs depth
ages.DDL = 2007 - apply(post.DDL$BUGSoutput$sims.list$age, 2, quantile, c(0.025, 0.5, 0.975))
ages.DDQ = 2007 - apply(post.DDQ$BUGSoutput$sims.list$age, 2, quantile, c(0.025, 0.5, 0.975))

plot(ages.DDL[2, ], depths, type = "l", xlim = range(rbind(ages.DDL, ages.DDQ)), 
     ylim = rev(range(depths)), 
     xlab = "Age (calendar years)",
     ylab = "Depth (cm)", lwd = 2, col = lbg)
lines(ages.DDL[1,], depths, col = lbg)
lines(ages.DDL[3,], depths, col = lbg)

lines(ages.DDQ[2, ], depths, lwd = 2, col = qbg)
lines(ages.DDQ[1,], depths, col = qbg)
lines(ages.DDQ[3,], depths, col = qbg)

text(par("usr")[1] + 0.05 * diff(par("usr")[1:2]),
     par("usr")[3] + 0.95 * diff(par("usr")[3:4]), "E")

dev.off()
