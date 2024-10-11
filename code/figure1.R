source("code/loadData.R")

# Plot colors
alpha = 100
qbg = col2rgb("cadetblue")
qbg = rgb(qbg[1], qbg[2], qbg[3], alpha, maxColorValue = 255)
lbg = col2rgb("coral2")
lbg = rgb(lbg[1], lbg[2], lbg[3], alpha, maxColorValue = 255)

# Plotting
png("out/Fig1.png", 6, 8.3, "in", res = 600)
layout(matrix(c(1, 2, 3), nrow = 3), 
       heights = c(lcm(2.6 * 2.54), lcm(2.5 * 2.54), lcm(3.2 * 2.54)))

## Carbon isotopes ----
par(mai = c(0, 0.7, 0.1, 0.7))
plot(sa.DDQ$Age.med, sa.DDQ$d13C.carb, pch = 21, bg = "cadetblue3", 
     xlim = rev(range(sa.DDQ$Age.med)), ylim = c(-1, 3.5), axes = FALSE,
     xlab = "", ylab = "")
mtext(expression(delta^{13}*"C"["carbonate"]*" (VPDB)"), 2, 3)
axis(2)
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

## Reservoir age ----
par(mai = c(0, 0.7, 0, 0.7))
plot(R.sum$Age.med, R.sum$R.med, pch = 21, bg = "cadetblue3", 
     xlim = rev(range(sa.DDQ$Age.med)), ylim = range(R.sum[, 9:11]),
     axes = FALSE, xlab = "", ylab = "")
mtext("Radiocarbon reservoir age", 4, 3)
axis(4)
arrows(R.DDQ$Age.025, R.DDQ$R.med, R.DDQ$Age.975, R.DDQ$R.med,
       length = 0, lwd = 4, col = qbg)
arrows(R.DDQ$Age.med, R.DDQ$R.025, R.DDQ$Age.med, R.DDQ$R.975,
       length = 0, lwd = 4, col = qbg)

arrows(R.DDL$Age.025, R.DDL$R.med, R.DDL$Age.975, R.DDL$R.med,
       length = 0, lwd = 4, col = lbg)
arrows(R.DDL$Age.med, R.DDL$R.025, R.DDL$Age.med, R.DDL$R.975,
       length = 0, lwd = 4, col = lbg)

points(R.DDQ$Age.med, R.DDQ$R.med, pch = 21, bg = "cadetblue3", cex = 1.5)
points(R.DDL$Age.med, R.DDL$R.med, pch = 21, bg = "coral2", cex = 1.5)

abline(v = 1847, lty = 3)
abline(v = 1959, lty = 3)

## Oxygen isotopes ----
par(mai = c(0.7, 0.7, 0, 0.7))
plot(sa.DDQ$Age.med, sa.DDQ$d18O.carb, pch = 21, bg = "cadetblue3", 
     xlim = rev(range(sa.DDQ$Age.med)), ylim = c(-8, -4), axes = FALSE,
     xlab = "", ylab = "")
mtext(expression(""^{210}*"Pb age (CE)"), 1, 3)
mtext(expression(delta^{18}*"O"["carbonate"]*" (VPDB)"), 2, 3)
axis(1)
axis(2)
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

dev.off()
