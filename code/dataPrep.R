
sa = read.csv("data/shortAll.csv")
lc = read.csv("data/longCarb.csv")
lr = read.csv("data/longReservoir.csv")

# US Pb210 data from Oliver et al., 2009



holes = unique(sa$Core)
ci = match(sa$Core, holes)

plot(lc$d13C.carb, lc$d18O.carb, pch = 16, col = "light grey", 
     xlim = c(-3, 4))
points(sa$d13C.carb[sa$Depth.min != 0], sa$d18O.carb[sa$Depth.min != 0],
       pch = 21, bg = "light blue")
points(sa$d13C.carb[sa$Depth.min == 0], sa$d18O.carb[sa$Depth.min == 0],
       pch = 21, bg = "dark blue")
