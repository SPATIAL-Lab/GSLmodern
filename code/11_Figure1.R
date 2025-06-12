# Map figure
source("code/X3_prepFigs.R")
source("code/Z1_prepMap.R")
library(viridisLite)
z = range(values(bath), na.rm = TRUE)

png("out/Fig1.png", width = 6.5, height = 6.8, units = "in", res = 600)
plot(gsl, axes = FALSE)
bds = par("usr")
rect(bds[1], bds[3], bds[2], bds[4], xpd = TRUE, col = "grey70", lwd = 2)

cols = viridis(11, alpha = 0.75)
plot(bath, add = TRUE, legend = FALSE, col = cols)
polys(gsl, lwd = 2)

# Faults
lines(faults, col = "indianred", lwd = 1.5)

# Legend
xmin = 0.02
xmax = 0.08
ymin = 0.02
ymax = 0.25

rect(bds[1], bds[3], bds[1] + (xmax + 0.085) * diff(bds[1:2]),
     bds[3] + (ymax + 0.02) * diff(bds[3:4]), col = rgb(1, 1, 1, 0.75),
     border = 1, xpd = TRUE)

rasterImage(rev(cols), bds[1] + xmin * diff(bds[1:2]), 
            bds[3] + ymin * diff(bds[3:4]), bds[1] + xmax * diff(bds[1:2]), 
            bds[3] + ymax * diff(bds[3:4]), interpolate = FALSE, xpd = TRUE)
rect(bds[1] + xmin * diff(bds[1:2]), bds[3] + ymin * diff(bds[3:4]),
     bds[1] + xmax * diff(bds[1:2]), bds[3] + ymax * diff(bds[3:4]), xpd = TRUE)

labs = seq(1272, 1278, by = 2)
ldpt = 1280 - labs
lx = bds[1] + (xmax + 0.02) * diff(bds[1:2])
ly = (labs - z[1]) / diff(z)
ly = bds[3] + (ymin + (ymax - ymin) * ly) * diff(bds[3:4])
for(i in seq_along(ly)){
  text(lx, ly[i], ldpt[i], xpd = TRUE)
}
text(bds[1] + (xmax + 0.05) * diff(bds[1:2]), 
     bds[3] + mean(c(ymin, ymax)) * diff(bds[3:4]),
     "Water depth (m)", srt = 90, xpd = TRUE)

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

# Geographical features
text(bds[1] + 0.23 * diff(bds[1:2]),
     bds[3] + 0.35 * diff(bds[3:4]),
     "Gilbert Bay", adj = c(1, 0.5))
text(bds[1] + 0.35 * diff(bds[1:2]),
     bds[3] + 0.85 * diff(bds[3:4]),
     "Gunnison Bay", adj = c(0, 0.5))
text(bds[1] + 0.21 * diff(bds[1:2]),
     bds[3] + 0.48 * diff(bds[3:4]),
     "Rail causeway", adj = c(1, 0.5), xpd = TRUE)
text(bds[1] + 0.80 * diff(bds[1:2]),
     bds[3] + 0.12 * diff(bds[3:4]),
     "East Lake\nFault", adj = c(0, 0.5), xpd = TRUE)
text(bds[1] + 0.32 * diff(bds[1:2]),
     bds[3] + 0.21 * diff(bds[3:4]),
     "Carrington\nFault", adj = c(1, 0.5), xpd = TRUE)
text(bds[1] + 0.80 * diff(bds[1:2]),
     bds[3] + 0.45 * diff(bds[3:4]),
     "Ogden Bay", adj = c(0, 0.5))

lines(c(bds[1] + 0.24 * diff(bds[1:2]),
        bds[1] + 0.33 * diff(bds[1:2])),
      c(bds[3] + 0.35 * diff(bds[3:4]),
        bds[3] + 0.35 * diff(bds[3:4])), lwd = 3, col = "grey20")
lines(c(bds[1] + 0.34 * diff(bds[1:2]),
        bds[1] + 0.26 * diff(bds[1:2])),
      c(bds[3] + 0.85 * diff(bds[3:4]),
        bds[3] + 0.82 * diff(bds[3:4])), lwd = 3, col = "grey20")
lines(c(bds[1] + 0.22 * diff(bds[1:2]),
        bds[1] + 0.30 * diff(bds[1:2])),
      c(bds[3] + 0.48 * diff(bds[3:4]),
        bds[3] + 0.525 * diff(bds[3:4])), lwd = 3, col = "grey20")
lines(c(bds[1] + 0.79 * diff(bds[1:2]),
        bds[1] + 0.73 * diff(bds[1:2])),
      c(bds[3] + 0.12 * diff(bds[3:4]),
        bds[3] + 0.16 * diff(bds[3:4])), lwd = 3, col = "grey20")
lines(c(bds[1] + 0.32 * diff(bds[1:2]),
        bds[1] + 0.356 * diff(bds[1:2])),
      c(bds[3] + 0.24 * diff(bds[3:4]),
        bds[3] + 0.30 * diff(bds[3:4])), lwd = 3, col = "grey20")
lines(c(bds[1] + 0.79 * diff(bds[1:2]),
        bds[1] + 0.70 * diff(bds[1:2])),
      c(bds[3] + 0.45 * diff(bds[3:4]),
        bds[3] + 0.45 * diff(bds[3:4])), lwd = 3, col = "grey20")

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