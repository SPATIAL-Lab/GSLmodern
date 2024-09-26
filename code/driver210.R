library(R2jags)

# Read and parse data from Olivers et al., 2009
d = read.csv("data/pb210.csv")
d.3510 = d[grep("3510", d$ID),]
d.DDQ = d[grep("DDQ", d$ID),]
d.DDL = d[grep("DDL", d$ID),]

# 3510
dd = d.3510

# Step size and number, cumulative mass units
ds = 0.1
bd = 10
nd = bd / ds
depths = seq(ds, bd, by = ds)

# Data for JAGS; convert to precision, generate cms index values
pb210 = dd[c("Depth", "pb210", "pb210.sd")]
pb210$pb210.sd = 1 / pb210$pb210.sd ^ 2
pb210$Depth = match(pb210$Depth, round(depths, 1))
names(pb210) = c("depth.ind", "pb210", "pb210.pre")

# Depth/mass data, assume 5% uncertinaty in cdm
mass = data.frame(pb210$depth.ind, dd$cdm, 1 / (dd$cdm * 0.05) ^ 2)
names(mass) = c("depth.ind", "cdm", "cdm.pre")

data = list("ds" = ds, "nd" = nd, "pb210" = pb210, "mass" = mass)

parms = c("ar.pre", "dr", "age", "ar", "pb", "cdm", "dens")

post = jags.parallel(data, NULL, parms, "code/model210.R", n.burnin = 1e3,
                     n.iter = 5e3, n.chains = 3)
View(post$BUGSoutput$summary)

plot(post$BUGSoutput$mean$pb, depths, ylim = rev(range(depths)), type = "l",
     lwd = 2, xlim = range(pb210$pb210))
points(pb210$pb210, depths[pb210$depth.ind], pch = 21)

plot(post$BUGSoutput$mean$cdm, depths, ylim = rev(range(depths)), 
     type = "l", lwd = 2, xlim = range(mass$cdm))
points(mass$cdm, depths[mass$depth.ind], pch = 21)

plot(post$BUGSoutput$mean$age, post$BUGSoutput$mean$ar)
plot(depths, post$BUGSoutput$mean$dens)
