source("code/loadData.R")
source("code/helpers.R")

# Load models
source("code/models/Omodel.R")
source("code/models/Cmodel.R")

# Calcite precipitation temperature
tc = 18

# Bivariate kernel density for Holocene
library(ks)
kd = kde(as.matrix(lc[c("d13C.carb", "d18O.carb")]))

# Initialize plot
png("out/Fig2.png", width = 5.2, height = 9.2, units = "in", res = 600)
layout(matrix(c(1, 2), nrow = 2), heights = c(lcm(4.2 * 2.54), lcm(5 * 2.54)))
par(mai = c(0, 1, 0.2, 0.2))

# Holocene data
plot(lc$d13C.carb, lc$d18O.carb, pch = 16, col = "gray70", 
     xlim = c(-5, 5), ylim = c(-9, -3), cex = 0.75, axes = FALSE,
     xlab = expression(delta^{13}*"C"[carb]),
     ylab = expression(delta^{18}*"O"[carb]))
axis(2)
box()
plot(kd, cont = 95, drawlabels = FALSE, add = TRUE, lwd = 3, col = "gray70")

# Model scenario 1
## Range of Fatm, terminal lake
Fatm = seq(0.40, 1, by = 0.01)
EI = rep(1, length(Fatm))

## Model
O = Omod(EI, tc)
C = Cmod(Fatm, tc)
s1 = cbind(O, C)

# Scenario 2
## Range of Fatm correlated with EI 
Fatm = seq(0.40, 1, by = 0.01)
EI = 1 - (1 - Fatm) * 1.8

## Model
O = Omod(EI, tc)
C = Cmod(Fatm, tc)
s2 = cbind(O, C)

# Add each scenario to plot
lines(s1$d13Cc, s1$d18Oc, lwd = 2)
lines(s2$d13Cc, s2$d18Oc, lwd = 2)

# Add CE data
points(sa.DDL$d13C.carb, sa.DDL$d18O.carb,
       pch = 21, bg = "palegreen3", cex = 1.5, lwd = 2)
points(sa.DDQ$d13C.carb, sa.DDQ$d18O.carb,
       pch = 21, bg = "palegreen3", cex = 1.5, lwd = 2)

points(sa.DDL$d13C.carb[sa.DDL$Age.med > 1790], 
       sa.DDL$d18O.carb[sa.DDL$Age.med > 1790],
       pch = 21, bg = "darkgoldenrod2", cex = 1.5, lwd = 2)
points(sa.DDQ$d13C.carb[sa.DDQ$Age.med > 1790], 
       sa.DDQ$d18O.carb[sa.DDQ$Age.med > 1790],
       pch = 21, bg = "darkgoldenrod2", cex = 1.5, lwd = 2)

points(sa.DDL$d13C.carb[sa.DDL$Age.med > 1957], 
       sa.DDL$d18O.carb[sa.DDL$Age.med > 1957],
       pch = 21, bg = "red3", cex = 1.5, lwd = 2)
points(sa.DDQ$d13C.carb[sa.DDQ$Age.med > 1957], 
       sa.DDQ$d18O.carb[sa.DDQ$Age.med > 1957],
       pch = 21, bg = "red3", cex = 1.5, lwd = 2)

arrows(4, -8, 4, -6, length = 0.1)
text(4, -6.8, "+ E/I", pos = 2, srt = 90)
arrows(4, -3.7, 2, -3.7, length = 0.1)
text(3, -3.7, "+ Organic C", pos = 3)

text(par("usr")[1] + diff(par("usr")[1:2]) * 0.05,
     par("usr")[4] - diff(par("usr")[3:4]) * 0.05,
     "A", adj = c(0, 1))

# Carbon panel
## Add carbonate d13C at radiocarbon levels, convert to R to F14R
lr = add.d13C(lr, lc)
lr$F14R = exp(lr$R.v1 / -8033)

sa.sub = sa.DDL[!is.na(sa.DDL$d13C.carb), ]
R.DDL = add.d13C(R.DDL, sa.sub)
R.DDL$F14R = exp(R.DDL$R.med / -8033)

sa.sub = sa.DDQ[!is.na(sa.DDQ$d13C.carb), ]
R.DDQ = add.d13C(R.DDQ, sa.sub)
R.DDQ$F14R = exp(R.DDQ$R.med / -8033)

## Plot
par(mai = c(1, 1, 0, 0.2))
plot(lr$d13Cc, lr$F14R, xlim = c(-5, 5), ylim = c(0.65, 1.03), 
     pch = 16, col = "gray70", cex = 0.75,
     xlab = expression(delta^{13}*"C"[carb]),
     ylab = expression("F"^{14}*"R"))

kd = kde(as.matrix(lr[c("d13Cc", "F14R")]))
plot(kd, cont = 95, drawlabels = FALSE, add = TRUE, lwd = 3, col = "gray70")

points(R.DDL$d13Cc, R.DDL$F14R, 
       pch = 21, bg = "palegreen3", cex = 1.5, lwd = 2)
points(R.DDQ$d13Cc, R.DDQ$F14R, 
       pch = 21, bg = "palegreen3", cex = 1.5, lwd = 2)

points(R.DDL$d13Cc[R.DDL$Age.med > 1790],
       R.DDL$F14R[R.DDL$Age.med > 1790], 
       pch = 21, bg = "darkgoldenrod2", cex = 1.5, lwd = 2)
points(R.DDQ$d13Cc[R.DDQ$Age.med > 1790],
       R.DDQ$F14R[R.DDQ$Age.med > 1790], 
       pch = 21, bg = "darkgoldenrod2", cex = 1.5, lwd = 2)

points(R.DDL$d13Cc[R.DDL$Age.med > 1959],
       R.DDL$F14R[R.DDL$Age.med > 1959], 
       pch = 21, bg = "red3", cex = 1.5, lwd = 2)
points(R.DDQ$d13Cc[R.DDQ$Age.med > 1959],
       R.DDQ$F14R[R.DDQ$Age.med > 1959], 
       pch = 21, bg = "red3", cex = 1.5, lwd = 2)

d13Cc.atm = Cmod(1, tc)
d13Cc.org = Cmod(0, tc)
lines(c(0, d13Cc.atm$d13Cc), c(0, 1), lwd = 2)
Forg = 0.5
lines(c(0, d13Cc.org$d13Cc * Forg + d13Cc.atm$d13Cc * (1 - Forg)),
      c(0, 1), lwd = 2)
lines(c(d13Cc.atm$d13Cc, d13Cc.org$d13Cc * Forg + d13Cc.atm$d13Cc * (1 - Forg)), 
      c(1, 1), lwd = 2)
points(d13Cc.atm$d13Cc, 1, pch = 16)
points(d13Cc.org$d13Cc * Forg + d13Cc.atm$d13Cc * (1 - Forg), 1, pch = 16)

text(d13Cc.atm$d13Cc, 1, "100% atm", adj = c(1.1, 2.2))
text(d13Cc.org$d13Cc * Forg + d13Cc.atm$d13Cc * (1 - Forg), 1, 
     "50:50 atm:org", adj = c(-0.1, 2.2))
text(0, 0.68, "100% rock", adj = c(0.5, 1.5), srt = 90)
arrows(0, 0.72, 0, 0.64, length = 0.1)

text(par("usr")[1] + diff(par("usr")[1:2]) * 0.05,
     par("usr")[4] - diff(par("usr")[3:4]) * 0.05,
     "B", adj = c(0, 1))

dev.off()
