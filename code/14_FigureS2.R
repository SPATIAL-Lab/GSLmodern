source("code/X3_prepFigs.R")

d = sa[!(sa$Core %in% c("DDL", "DDQ")), ]

sites = unique(d$Core)

png("out/FigS2.png", width = 6.2, height = 6, units = "in", res = 600)
layout(matrix(c(1, 2), nrow = 1), widths = c(lcm(3.5 * 2.54), lcm(2.7 * 2.54)))

# Carbon
par(mai = c(1, 1, 0.2, 0))
plot(0, 0, xlim = range(d$d13C.carb, na.rm = TRUE), 
     ylim = rev(range(d$Depth.mean)), type = "n",
     xlab = expression(delta^{13}*"C"["carbonate"]*" (VPDB)"),
     ylab = "Depth (cm)")

for(i in seq_along(sites)){
  d.sub = d[d$Core == sites[i], ]
  lines(d.sub$d13C.carb, d.sub$Depth.mean, lwd = 2, col = i)
  points(d.sub$d13C.carb, d.sub$Depth.mean, pch = 21, cex = 1.5, bg = i)
}

legend("bottomleft", sites, lwd = 2, col = seq(1, length(sites)), bty = "n")

# Oxygen
par(mai = c(1, 0, 0.2, 0.2))
plot(0, 0, xlim = range(d$d18O.carb, na.rm = TRUE), 
     ylim = rev(range(d$Depth.mean)), type = "n", axes = FALSE,
     xlab = expression(delta^{18}*"O"["carbonate"]*" (VPDB)"),
     ylab = "")
axis(1)
box()

for(i in seq_along(sites)){
  d.sub = d[d$Core == sites[i], ]
  lines(d.sub$d18O.carb, d.sub$Depth.mean, lwd = 2, col = i)
  points(d.sub$d18O.carb, d.sub$Depth.mean, pch = 21, cex = 1.5, bg = i)
}

dev.off()
