
# Read data
sa = read.csv("data/shortAll.csv")
lc = read.csv("data/longCarb.csv")
lr = read.csv("data/longReservoir.csv")

# Parse short core data
holes = unique(sa$Core)
ci = match(sa$Core, holes)
sa.3510 = sa[sa$Core == "3510-1",]
sa.DDQ = sa[sa$Core == "DDQ",]
sa.DDL = sa[sa$Core == "DDL",]

# Assign ages
load("out/ad3510.rda")
load("out/adDDQ.rda")
load("out/adDDL.rda")

di.3510 = match(sa.3510$Depth.mean, ad.3510$Depth)
di.DDQ = match(sa.DDQ$Depth.mean, ad.DDQ$Depth)
di.DDL = match(sa.DDL$Depth.mean, ad.DDL$Depth)

sa.3510 = cbind(sa.3510, ad.3510[di.3510, 2:4])
sa.DDQ = cbind(sa.DDQ, ad.DDQ[di.DDQ, 2:4])
sa.DDL = cbind(sa.DDL, ad.DDL[di.DDL, 2:4])


plot(sa.DDQ$Age.med, sa.DDQ$d13C.carb, pch = 21, bg = "blue", ylim = c(0, 3.5))
arrows(sa.DDQ$Age.025, sa.DDQ$d13C.carb, sa.DDQ$Age.975, sa.DDQ$d13C.carb,
       length = 0, col = "blue")

points(sa.DDL$Age.med, sa.DDL$d13C.carb, pch = 21, bg = "dark green")
arrows(sa.DDL$Age.025, sa.DDL$d13C.carb, sa.DDL$Age.975, sa.DDL$d13C.carb,
       length = 0, col = "dark green")

#points(sa.3510$Age.med, sa.3510$d13C.carb, pch = 21, bg = "dark red")


plot(lc$d13C.carb, lc$d18O.carb, pch = 16, col = "light grey", 
     xlim = c(-3, 4))
points(sa$d13C.carb[sa$Depth.min != 0], sa$d18O.carb[sa$Depth.min != 0],
       pch = 21, bg = "light blue")
points(sa$d13C.carb[sa$Depth.min == 0], sa$d18O.carb[sa$Depth.min == 0],
       pch = 21, bg = "dark blue")
