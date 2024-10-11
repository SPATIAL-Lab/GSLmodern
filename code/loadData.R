
# Read stable isotope data
sa = read.csv("data/shortAll.csv")
lc = read.csv("data/longCarb.csv")
lr = read.csv("data/longReservoir.csv")

# Parse core data
sa.DDQ = sa[sa$Core == "DDQ",]
sa.DDL = sa[sa$Core == "DDL",]

# Assign ages
load("out/adDDQ.rda")
load("out/adDDL.rda")

di.DDQ = match(sa.DDQ$Depth.mean, ad.DDQ$Depth)
di.DDL = match(sa.DDL$Depth.mean, ad.DDL$Depth)

sa.DDQ = cbind(sa.DDQ, 2007 - ad.DDQ[di.DDQ, 2:4])
sa.DDL = cbind(sa.DDL, 2007 - ad.DDL[di.DDL, 2:4])

# Read reservoir age data
load("out/Rsummary.rda")
R.DDQ = R.sum[R.sum$Core == "DDQ",]
R.DDL = R.sum[R.sum$Core == "DDL",]

