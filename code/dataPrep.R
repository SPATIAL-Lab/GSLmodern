


lc = read.csv("data/longCarb.csv")
lr = read.csv("data/longReservoir.csv")

#points(sa.3510$Age.med, sa.3510$d13C.carb, pch = 21, bg = "dark red")


plot(lc$d13C.carb, lc$d18O.carb, pch = 16, col = "light grey", 
     xlim = c(-3, 4))
points(sa$d13C.carb[sa$Depth.min != 0], sa$d18O.carb[sa$Depth.min != 0],
       pch = 21, bg = "light blue")
points(sa$d13C.carb[sa$Depth.min == 0], sa$d18O.carb[sa$Depth.min == 0],
       pch = 21, bg = "dark blue")
