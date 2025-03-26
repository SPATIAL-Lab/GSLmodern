source("code/loadData.R")
source("code/X1_helpers.R")

# Plot space
png("out/Fig1.png", 6, 8.3, "in", res = 600)
layout(matrix(c(1, 2, 3), nrow = 3), 
       heights = c(lcm(2.6 * 2.54), lcm(2.5 * 2.54), lcm(3.2 * 2.54)))

## Carbon isotopes ----
par(mai = c(0, 0.7, 0.1, 0.7))
plot(sqrt(2007 - sa.DDQ$Age.med), 
     sa.DDQ$d13C.carb, pch = 21, bg = qbg, 
     xlim = reage(rev(range(sa.DDQ$Age.med))), ylim = c(-1, 3.5), axes = FALSE,
     xlab = "", ylab = "")
mtext(expression(delta^{13}*"C"["carbonate"]*" (VPDB)"), 2, 3)
axis(2)

abline(v = reage(1847), lty = 3, col = "grey20")
abline(v = reage(1959), lty = 3, col = "grey20")

arrows(reage(sa.DDQ$Age.025), sa.DDQ$d13C.carb, 
       reage(sa.DDQ$Age.975), sa.DDQ$d13C.carb,
       length = 0, lwd = 4, col = qbg.t)
arrows(reage(sa.DDQ$Age.med), sa.DDQ$d13C.carb + 2 * sa.DDQ$d13C.carb.sd, 
       reage(sa.DDQ$Age.med), sa.DDQ$d13C.carb - 2 * sa.DDQ$d13C.carb.sd,
       length = 0, lwd = 4, col = qbg.t)

arrows(reage(sa.DDL$Age.025), sa.DDL$d13C.carb, 
       reage(sa.DDL$Age.975), sa.DDL$d13C.carb,
       length = 0, lwd = 4, col = lbg.t)
arrows(reage(sa.DDL$Age.med), sa.DDL$d13C.carb + 2 * sa.DDL$d13C.carb.sd, 
       reage(sa.DDL$Age.med), sa.DDL$d13C.carb - 2 * sa.DDL$d13C.carb.sd,
       length = 0, lwd = 4, col = lbg.t)

lines(reage(sa.DDQ$Age.med), sa.DDQ$d13C.carb, col = qbg)
lines(reage(sa.DDL$Age.med), sa.DDL$d13C.carb, col = lbg)

points(reage(sa.DDQ$Age.med), sa.DDQ$d13C.carb, pch = 21, 
       bg = qbg, cex = 2)
points(reage(sa.DDL$Age.med), sa.DDL$d13C.carb, pch = 21, 
       bg = lbg, cex = 2)

text(par("usr")[1] + 0.05 * diff(par("usr")[1:2]),
     par("usr")[3] + 0.90 * diff(par("usr")[3:4]), "A", cex = 1.5)

## Reservoir age ----
par(mai = c(0, 0.7, 0, 0.7))
plot(reage(R.sum$Age.med), R.sum$R.med, pch = 21, bg = qbg, 
     xlim = reage(rev(range(sa.DDQ$Age.med))), ylim = range(R.sum[, 9:11]),
     axes = FALSE, xlab = "", ylab = "")
mtext(expression(""^{14}*"C reservoir age (years)"), 4, 3)
axis(4)

abline(v = reage(1847), lty = 3, col = "grey20")
abline(v = reage(1959), lty = 3, col = "grey20")

arrows(reage(R.DDQ$Age.025), R.DDQ$R.med, 
       reage(R.DDQ$Age.975), R.DDQ$R.med,
       length = 0, lwd = 4, col = qbg.t)
arrows(reage(R.DDQ$Age.med), R.DDQ$R.025, 
       reage(R.DDQ$Age.med), R.DDQ$R.975,
       length = 0, lwd = 4, col = qbg.t)

arrows(reage(R.DDL$Age.025), R.DDL$R.med, 
       reage(R.DDL$Age.975), R.DDL$R.med,
       length = 0, lwd = 4, col = lbg.t)
arrows(reage(R.DDL$Age.med), R.DDL$R.025, 
       reage(R.DDL$Age.med), R.DDL$R.975,
       length = 0, lwd = 4, col = lbg.t)

points(reage(R.DDQ$Age.med), R.DDQ$R.med, pch = 21, 
       bg = qbg, cex = 2)
points(reage(R.DDL$Age.med), R.DDL$R.med, pch = 21, 
       bg = lbg, cex = 2)

text(par("usr")[1] + 0.05 * diff(par("usr")[1:2]),
     par("usr")[3] + 0.90 * diff(par("usr")[3:4]), "B", cex = 1.5)

## Oxygen isotopes ----
par(mai = c(0.7, 0.7, 0, 0.7))
plot(reage(sa.DDQ$Age.med), sa.DDQ$d18O.carb, pch = 21, bg = qbg, 
     xlim = reage(rev(range(sa.DDQ$Age.med))), ylim = c(-8, -4), 
     axes = FALSE, xlab = "", ylab = "")
mtext(expression(""^{210}*"Pb age (CE)"), 1, 3)
mtext(expression(delta^{18}*"O"["carbonate"]*" (VPDB)"), 2, 3)
axis(1, at = reage(c(2000, 1800, 1500, 1000, 0)), 
     labels = c(2000, 1800, 1500, 1000, 0))
axis(2)

abline(v = reage(1847), lty = 3, col = "grey20")
abline(v = reage(1959), lty = 3, col = "grey20")

arrows(reage(sa.DDQ$Age.025), sa.DDQ$d18O.carb, 
       reage(sa.DDQ$Age.975), sa.DDQ$d18O.carb,
       length = 0, lwd = 4, col = qbg.t)
arrows(reage(sa.DDQ$Age.med), sa.DDQ$d18O.carb + 2 * sa.DDQ$d18O.carb.sd, 
       reage(sa.DDQ$Age.med), sa.DDQ$d18O.carb - 2 * sa.DDQ$d18O.carb.sd,
       length = 0, lwd = 4, col = qbg.t)

arrows(reage(sa.DDL$Age.025), sa.DDL$d18O.carb, 
       reage(sa.DDL$Age.975), sa.DDL$d18O.carb,
       length = 0, lwd = 4, col = lbg.t)
arrows(reage(sa.DDL$Age.med), sa.DDL$d18O.carb + 2 * sa.DDL$d18O.carb.sd, 
       reage(sa.DDL$Age.med), sa.DDL$d18O.carb - 2 * sa.DDL$d18O.carb.sd,
       length = 0, lwd = 4, col = lbg.t)

lines(reage(sa.DDQ$Age.med), sa.DDQ$d18O.carb)
lines(reage(sa.DDL$Age.med), sa.DDL$d18O.carb)

points(reage(sa.DDQ$Age.med), sa.DDQ$d18O.carb, pch = 21, 
       bg = qbg, cex = 2)
points(reage(sa.DDL$Age.med), sa.DDL$d18O.carb, pch = 21, 
       bg = lbg, cex = 2)

text(par("usr")[1] + 0.05 * diff(par("usr")[1:2]),
     par("usr")[3] + 0.90 * diff(par("usr")[3:4]), "C", cex = 1.5)

dev.off()
