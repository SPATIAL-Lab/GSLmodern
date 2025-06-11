# Map figure
source("code/X3_prepFigs.R")
source("code/Z1_prepMap.R")

png("out/Fig1.png", width = 6.5, height = 6.8, units = "in", res = 600)
plot(gsl, axes = FALSE)
bds = par("usr")
rect(bds[1], bds[3], bds[2], bds[4], xpd = TRUE, col = "grey70", lwd = 2)

plot(bath, add = TRUE, legend = FALSE)

polys(gsl, lwd = 2)

# Faults
lines(faults, col = "azure4")

# Sites and labels
pch = c(21, 20)[sites$Type]
bg = rep("black", length(pch))
bg[sites$ID == "DDL"] = lbg
bg[sites$ID == "DDQ"] = qbg
bg[sites$ID == "GSL00-1"] = "grey70"

points(sites, pch = pch, bg = bg, cex = 1.5, lwd = 2)

text(crds(sites)[sites$ID != "GSL00-1", 1], 
     crds(sites)[sites$ID != "GSL00-1", 2], 
     sites$ID[sites$ID != "GSL00-1"], adj = c(1.2, 0.5))
text(crds(sites)[sites$ID == "GSL00-1", 1], 
     crds(sites)[sites$ID == "GSL00-1", 2], 
     sites$ID[sites$ID == "GSL00-1"], adj = c(1.1, 1.2))

# Bay names
text(bds[1] + 0.23 * diff(bds[1:2]),
     bds[3] + 0.3 * diff(bds[3:4]),
     "Gilbert Bay", adj = c(1, 0.5))
text(bds[1] + 0.35 * diff(bds[1:2]),
     bds[3] + 0.85 * diff(bds[3:4]),
     "Gunnison Bay", adj = c(0, 0.5))

lines(c(bds[1] + 0.24 * diff(bds[1:2]),
        bds[1] + 0.33 * diff(bds[1:2])),
      c(bds[3] + 0.3 * diff(bds[3:4]),
        bds[3] + 0.33 * diff(bds[3:4])), lwd = 3, col = "grey20")

lines(c(bds[1] + 0.34 * diff(bds[1:2]),
        bds[1] + 0.26 * diff(bds[1:2])),
      c(bds[3] + 0.85 * diff(bds[3:4]),
        bds[3] + 0.82 * diff(bds[3:4])), lwd = 3, col = "grey20")

bds = par("usr")
bds = c(grconvertX(bds[1:2], "user", "ndc"),
        grconvertY(bds[3:4], "user", "ndc"))
bds = c(bds[1] + 0.77 * diff(bds[1:2]), bds[2],
        bds[3] + 0.77 * diff(bds[3:4]), bds[4])

# Inset
par(fig = bds, new = TRUE)
plot(ut, axes = FALSE, lwd = 2, mar = rep(1, 4))
polys(gsl, col = "black")

dev.off()