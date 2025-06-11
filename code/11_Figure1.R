source("code/X3_prepFigs.R")

# Plot space
png("out/Fig1.png", 8.3, 5.5, "in", res = 600)
layout(matrix(c(1, 2, 3), ncol = 3), 
       widths = c(lcm(3.2 * 2.54), lcm(2.5 * 2.54), lcm(2.6 * 2.54)))

## Carbon isotopes ----
par(mai = c(0.1, 0.7, 0.7, 0))
plot(sa.DDQ$d13C.carb, reage(sa.DDQ$Age.med),  pch = 21, bg = qbg, 
     xlim = c(-1, 3.5), axes = FALSE, ylim = reage(range(sa.DDQ$Age.med)),
     xlab = "", ylab = "")
mtext(expression(delta^{13}*"C"["carbonate"]*" (VPDB)"), 3, 3)
axis(3)
mtext(expression(""^{210}*"Pb age (CE)"), 2, 3)
axis(2, at = reage(seq(0, 2000, by = 100)), tcl = 0.2, labels = FALSE)
axis(2, at = reage(c(1959, 1847, 1500, 1000, 0)), 
     labels = c(1959, 1847, 1500, 1000, 0))

abline(h = reage(1847), lty = 3, col = "grey20")
abline(h = reage(1959), lty = 3, col = "grey20")

arrows(sa.DDQ$d13C.carb, reage(sa.DDQ$Age.025),  
       sa.DDQ$d13C.carb, reage(sa.DDQ$Age.975), 
       length = 0, lwd = 4, col = qbg.t)
arrows(sa.DDQ$d13C.carb + 2 * sa.DDQ$d13C.carb.sd, reage(sa.DDQ$Age.med), 
       sa.DDQ$d13C.carb - 2 * sa.DDQ$d13C.carb.sd, reage(sa.DDQ$Age.med), 
       length = 0, lwd = 4, col = qbg.t)

arrows(sa.DDL$d13C.carb, reage(sa.DDL$Age.025), 
       sa.DDL$d13C.carb, reage(sa.DDL$Age.975), 
       length = 0, lwd = 4, col = lbg.t)
arrows(sa.DDL$d13C.carb + 2 * sa.DDL$d13C.carb.sd, reage(sa.DDL$Age.med), 
       sa.DDL$d13C.carb - 2 * sa.DDL$d13C.carb.sd, reage(sa.DDL$Age.med), 
       length = 0, lwd = 4, col = lbg.t)

lines(sa.DDQ$d13C.carb, reage(sa.DDQ$Age.med), col = qbg)
lines(sa.DDL$d13C.carb, reage(sa.DDL$Age.med), col = lbg)

points(sa.DDQ$d13C.carb, reage(sa.DDQ$Age.med), pch = 21, 
       bg = qbg, cex = 2)
points(sa.DDL$d13C.carb, reage(sa.DDL$Age.med), pch = 21, 
       bg = lbg, cex = 2)

text(par("usr")[1] + 0.1 * diff(par("usr")[1:2]),
     par("usr")[3] + 0.05 * diff(par("usr")[3:4]), 
     "A", cex = 1.5)

## Reservoir age ----
par(mai = c(0.1, 0, 0.7, 0))
plot(R.sum$R.med, reage(R.sum$Age.med), pch = 21, bg = qbg, 
     xlim = range(R.sum[, 9:11]), ylim = reage(range(sa.DDQ$Age.med)),
     axes = FALSE, xlab = "", ylab = "")
mtext(expression(""^{14}*"C reservoir age (years)"), 3, 3)
axis(3)

abline(h = reage(1847), lty = 3, col = "grey20")
abline(h = reage(1959), lty = 3, col = "grey20")

arrows(R.DDQ$R.med, reage(R.DDQ$Age.025), 
       R.DDQ$R.med, reage(R.DDQ$Age.975), 
       length = 0, lwd = 4, col = qbg.t)
arrows(R.DDQ$R.025, reage(R.DDQ$Age.med), 
       R.DDQ$R.975, reage(R.DDQ$Age.med), 
       length = 0, lwd = 4, col = qbg.t)

arrows(R.DDL$R.med, reage(R.DDL$Age.025), 
       R.DDL$R.med, reage(R.DDL$Age.975), 
       length = 0, lwd = 4, col = lbg.t)
arrows(R.DDL$R.025, reage(R.DDL$Age.med), 
       R.DDL$R.975, reage(R.DDL$Age.med), 
       length = 0, lwd = 4, col = lbg.t)

points(R.DDQ$R.med, reage(R.DDQ$Age.med), pch = 21, 
       bg = qbg, cex = 2)
points(R.DDL$R.med, reage(R.DDL$Age.med), pch = 21, 
       bg = lbg, cex = 2)

text(par("usr")[1] + 0.1 * diff(par("usr")[1:2]),
     par("usr")[3] + 0.05 * diff(par("usr")[3:4]), 
     "B", cex = 1.5)

## Oxygen isotopes ----
par(mai = c(0.1, 0, 0.7, 0.1))
plot(sa.DDQ$d18O.carb, reage(sa.DDQ$Age.med), pch = 21, bg = qbg, 
     xlim = c(-8, -4), ylim = reage(range(sa.DDQ$Age.med)), 
     axes = FALSE, xlab = "", ylab = "")
mtext(expression(delta^{18}*"O"["carbonate"]*" (VPDB)"), 3, 3)
axis(3)

abline(h = reage(1847), lty = 3, col = "grey20")
abline(h = reage(1959), lty = 3, col = "grey20")

arrows(sa.DDQ$d18O.carb, reage(sa.DDQ$Age.025), 
       sa.DDQ$d18O.carb, reage(sa.DDQ$Age.975), 
       length = 0, lwd = 4, col = qbg.t)
arrows(sa.DDQ$d18O.carb + 2 * sa.DDQ$d18O.carb.sd, reage(sa.DDQ$Age.med), 
       sa.DDQ$d18O.carb - 2 * sa.DDQ$d18O.carb.sd, reage(sa.DDQ$Age.med), 
       length = 0, lwd = 4, col = qbg.t)

arrows(sa.DDL$d18O.carb, reage(sa.DDL$Age.025), 
       sa.DDL$d18O.carb, reage(sa.DDL$Age.975), 
       length = 0, lwd = 4, col = lbg.t)
arrows(sa.DDL$d18O.carb + 2 * sa.DDL$d18O.carb.sd, reage(sa.DDL$Age.med), 
       sa.DDL$d18O.carb - 2 * sa.DDL$d18O.carb.sd, reage(sa.DDL$Age.med), 
       length = 0, lwd = 4, col = lbg.t)

lines(sa.DDQ$d18O.carb, reage(sa.DDQ$Age.med), col = qbg)
lines(sa.DDL$d18O.carb, reage(sa.DDL$Age.med), col = lbg)

points(sa.DDQ$d18O.carb, reage(sa.DDQ$Age.med), pch = 21, 
       bg = qbg, cex = 2)
points(sa.DDL$d18O.carb, reage(sa.DDL$Age.med), pch = 21, 
       bg = lbg, cex = 2)

text(par("usr")[1] + 0.1 * diff(par("usr")[1:2]),
     par("usr")[3] + 0.05 * diff(par("usr")[3:4]), 
     "C", cex = 1.5)

dev.off()

