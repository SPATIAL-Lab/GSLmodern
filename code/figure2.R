source("code/loadData.R")
library(ks)

# Bivariate kernal density for Holocene
kd = kde(as.matrix(lc[c("d13C.carb", "d18O.carb")]))

# Initialize plot
png("out/Fig2.png", 6, 6, "in", res = 600)
par(mar = c(5, 5, 1, 1))

# Holocene data
plot(lc$d13C.carb, lc$d18O.carb, pch = 16, col = "gray50", 
     xlim = c(-3, 4), ylim = c(-9, -3), cex = 0.75, 
     xlab = expression(delta^{13}*"C"[carb]),
     ylab = expression(delta^{18}*"O"[carb]))
plot(kd, cont = 95, drawlabels = FALSE, add = TRUE, lwd = 3, col = "gray50")


# Load models
source("code/models/Omodel.R")
source("code/models/Cmodel.R")
# Calcite precipitation temperature
tc = 18

# Model scenario 1
## Range of Fatm, terminal lake
Fatm = seq(0.50, 1, by = 0.01)
EI = rep(1, length(Fatm))

## Model
O = Omod(EI, tc)
C = Cmod(Fatm, tc)
s1 = cbind(O, C)

# Scenario 2
## Range of Fatm correlated with EI 
Fatm = seq(0.50, 1, by = 0.01)
EI = 1 - (1 - Fatm) * 1.8

## Model
O = Omod(EI, tc)
C = Cmod(Fatm, tc)
s2 = cbind(O, C)

# Add cross-scenario connectors
## Index for 10% Fatm intervals
ri = match(seq(0.6, 0.9, by = 0.1), s1$Fatm) 

## Plot them
for(i in seq_along(ri)){
  lines(c(s1$d13Cc[ri[i]], s2$d13Cc[ri[i]]),
        c(s1$d18Oc[ri[i]], s2$d18Oc[ri[i]]), lty = 2)
  text(s1$d13Cc[ri[i]], s1$d18Oc[ri[i]], paste("Fatm =", s1$Fatm[ri[i]]),
       pos = 3, offset = 1.5)
  text(s2$d13Cc[ri[i]], s2$d18Oc[ri[i]], paste("E/I =", s2$EI[ri[i]]),
       adj = c(0, 2))
}

# Add each scenario to plot
lines(s1$d13Cc, s1$d18Oc, lwd = 2, lty = 2)
lines(s2$d13Cc, s2$d18Oc, lwd = 2, lty = 2)

# Add CE data
points(sa.DDL$d13C.carb, sa.DDL$d18O.carb,
       pch = 21, bg = "palegreen3", cex = 1.5)
points(sa.DDQ$d13C.carb, sa.DDQ$d18O.carb,
       pch = 21, bg = "palegreen3", cex = 1.5)

points(sa.DDL$d13C.carb[sa.DDL$Age.med > 1790], 
       sa.DDL$d18O.carb[sa.DDL$Age.med > 1790],
       pch = 21, bg = "darkgoldenrod2", cex = 1.5)
points(sa.DDQ$d13C.carb[sa.DDQ$Age.med > 1790], 
       sa.DDQ$d18O.carb[sa.DDQ$Age.med > 1790],
       pch = 21, bg = "darkgoldenrod2", cex = 1.5)

points(sa.DDL$d13C.carb[sa.DDL$Age.med > 1957], 
       sa.DDL$d18O.carb[sa.DDL$Age.med > 1957],
       pch = 21, bg = "red3", cex = 1.5)
points(sa.DDQ$d13C.carb[sa.DDQ$Age.med > 1957], 
       sa.DDQ$d18O.carb[sa.DDQ$Age.med > 1957],
       pch = 21, bg = "red3", cex = 1.5)

dev.off()

