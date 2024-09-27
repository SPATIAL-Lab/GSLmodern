library(R2jags)
set.seed(14568)

# Read and parse data from Olivers et al., 2009
d = read.csv("data/pb210.csv")
d.3510 = d[grep("3510", d$ID),]
d.DDQ = d[grep("DDQ", d$ID),]
d.DDL = d[grep("DDL", d$ID),]

# 3510
dd = d.3510

## Step size and number, cumulative mass units
ds = 0.1
bd = 40
nd = bd / ds
depths = round(seq(ds, bd, by = ds), 1)

## Data for JAGS; convert to precision, generate cms index values
pb210 = dd[c("Depth", "pb210", "pb210.sd")]
pb210$pb210.sd = 1 / pb210$pb210.sd ^ 2
pb210$Depth = match(pb210$Depth, round(depths, 1))
names(pb210) = c("depth.ind", "pb210", "pb210.pre")

## Depth/mass data, assume 5% uncertinaty in cdm
mass = data.frame(pb210$depth.ind, dd$cdm, 1 / (dd$cdm * 0.05) ^ 2)
names(mass) = c("depth.ind", "cdm", "cdm.pre")

## Objects for analysis
data = list("ds" = ds, "nd" = nd, "pb210" = pb210, "mass" = mass)
parms = c("ar.pre", "dr", "age", "ar", "pb", "cdm", "rho")

## MCMC
post = jags.parallel(data, NULL, parms, "code/model210.R", n.burnin = 2e3,
                     n.iter = 6e3, n.chains = 3)

## Check results
View(post$BUGSoutput$summary)

plot(post$BUGSoutput$mean$pb, depths, ylim = rev(range(depths)), type = "l",
     lwd = 2, xlim = range(pb210$pb210))
points(pb210$pb210, depths[pb210$depth.ind], pch = 21)

plot(post$BUGSoutput$mean$cdm, depths, ylim = rev(range(depths)), 
     type = "l", lwd = 2, xlim = range(mass$cdm))
points(mass$cdm, depths[mass$depth.ind], pch = 21)

ar = apply(post$BUGSoutput$sims.list$ar, 2, quantile, c(0.025, 0.5, 0.975))
plot(ar[2,], depths, type = "l", lwd = 2, ylim = rev(range(depths)),
     xlim = range(ar))
lines(ar[1,], depths)
lines(ar[3,], depths)

rho = apply(post$BUGSoutput$sims.list$rho, 2, quantile, c(0.025, 0.5, 0.975))
plot(rho[2,], depths, type = "l", lwd = 2, ylim = rev(range(depths)),
     xlim = range(rho))
lines(rho[1,], depths)
lines(rho[3,], depths)

ages = apply(post$BUGSoutput$sims.list$age, 2, quantile, c(0.025, 0.5, 0.975))
plot(ages[2,], depths, type = "l", lwd = 2, ylim = rev(range(depths)),
     xlim = range(ages))
lines(ages[1,], depths)
lines(ages[3,], depths)


## Save
post.3510 = post
ad.3510 = data.frame("Depth" = depths, "Age.025" = ages[1,], 
                     "Age.med" = ages[2,], "Age.975" = ages[3,])
save(post.3510, file = "bigout/ageMod3510.rda")
save(ad.3510, file = "out/ad3510.rda")

# DDQ
dd = d.DDQ

## Step size and number, cumulative mass units
ds = 0.1
bd = 32
nd = bd / ds
depths = round(seq(ds, bd, by = ds), 1)

## Data for JAGS; convert to precision, generate cms index values
pb210 = dd[c("Depth", "pb210", "pb210.sd")]
pb210$pb210.sd = 1 / pb210$pb210.sd ^ 2
pb210$Depth = match(pb210$Depth, round(depths, 1))
names(pb210) = c("depth.ind", "pb210", "pb210.pre")

## Depth/mass data, assume 5% uncertinaty in cdm
mass = data.frame(pb210$depth.ind, dd$cdm, 1 / (dd$cdm * 0.05) ^ 2)
names(mass) = c("depth.ind", "cdm", "cdm.pre")

## Objects for analysis
data = list("ds" = ds, "nd" = nd, "pb210" = pb210, "mass" = mass)
parms = c("ar.pre", "dr", "age", "ar", "pb", "cdm", "rho")

## MCMC
post = jags.parallel(data, NULL, parms, "code/model210.R", n.burnin = 5e3,
                          n.iter = 1e4, n.chains = 3)

## Check results
View(post$BUGSoutput$summary)

plot(post$BUGSoutput$mean$pb, depths, ylim = rev(range(depths)), type = "l",
     lwd = 2, xlim = range(pb210$pb210))
points(pb210$pb210, depths[pb210$depth.ind], pch = 21)

plot(post$BUGSoutput$mean$cdm, depths, ylim = rev(range(depths)), 
     type = "l", lwd = 2, xlim = range(mass$cdm))
points(mass$cdm, depths[mass$depth.ind], pch = 21)

ar = apply(post$BUGSoutput$sims.list$ar, 2, quantile, c(0.025, 0.5, 0.975))
plot(ar[2,], depths, type = "l", lwd = 2, ylim = rev(range(depths)),
     xlim = range(ar))
lines(ar[1,], depths)
lines(ar[3,], depths)

rho = apply(post$BUGSoutput$sims.list$rho, 2, quantile, c(0.025, 0.5, 0.975))
plot(rho[2,], depths, type = "l", lwd = 2, ylim = rev(range(depths)),
     xlim = range(rho))
lines(rho[1,], depths)
lines(rho[3,], depths)

ages = apply(post$BUGSoutput$sims.list$age, 2, quantile, c(0.025, 0.5, 0.975))
plot(ages[2,], depths, type = "l", lwd = 2, ylim = rev(range(depths)),
     xlim = range(ages))
lines(ages[1,], depths)
lines(ages[3,], depths)

## Save
post.DDQ = post
ad.DDQ = data.frame("Depth" = depths, "Age.025" = ages[1,], 
                     "Age.med" = ages[2,], "Age.975" = ages[3,])
save(post.DDQ, file = "bigout/ageModDDQ.rda")
save(ad.DDQ, file = "out/adDDQ.rda")

# DDL
dd = d.DDL

## Step size and number, cumulative mass units
ds = 0.1
bd = 32
nd = bd / ds
depths = round(seq(ds, bd, by = ds), 1)

## Data for JAGS; convert to precision, generate cms index values
pb210 = dd[c("Depth", "pb210", "pb210.sd")]
pb210$pb210.sd = 1 / pb210$pb210.sd ^ 2
pb210$Depth = match(pb210$Depth, round(depths, 1))
names(pb210) = c("depth.ind", "pb210", "pb210.pre")

## Depth/mass data, assume 5% uncertinaty in cdm
mass = data.frame(pb210$depth.ind, dd$cdm, 1 / (dd$cdm * 0.05) ^ 2)
names(mass) = c("depth.ind", "cdm", "cdm.pre")

## Objects for analysis
data = list("ds" = ds, "nd" = nd, "pb210" = pb210, "mass" = mass)
parms = c("ar.pre", "dr", "age", "ar", "pb", "cdm", "rho")

## MCMC
post = jags.parallel(data, NULL, parms, "code/model210.R", n.burnin = 2e3,
                     n.iter = 6e3, n.chains = 3)

## Check results
View(post$BUGSoutput$summary)

plot(post$BUGSoutput$mean$pb, depths, ylim = rev(range(depths)), type = "l",
     lwd = 2, xlim = range(pb210$pb210))
points(pb210$pb210, depths[pb210$depth.ind], pch = 21)

plot(post$BUGSoutput$mean$cdm, depths, ylim = rev(range(depths)), 
     type = "l", lwd = 2, xlim = range(mass$cdm))
points(mass$cdm, depths[mass$depth.ind], pch = 21)

ar = apply(post$BUGSoutput$sims.list$ar, 2, quantile, c(0.025, 0.5, 0.975))
plot(ar[2,], depths, type = "l", lwd = 2, ylim = rev(range(depths)),
     xlim = range(ar))
lines(ar[1,], depths)
lines(ar[3,], depths)

rho = apply(post$BUGSoutput$sims.list$rho, 2, quantile, c(0.025, 0.5, 0.975))
plot(rho[2,], depths, type = "l", lwd = 2, ylim = rev(range(depths)),
     xlim = range(rho))
lines(rho[1,], depths)
lines(rho[3,], depths)

ages = apply(post$BUGSoutput$sims.list$age, 2, quantile, c(0.025, 0.5, 0.975))
plot(ages[2,], depths, type = "l", lwd = 2, ylim = rev(range(depths)),
     xlim = range(ages))
lines(ages[1,], depths)
lines(ages[3,], depths)

## Save
post.DDL = post
ad.DDL = data.frame("Depth" = depths, "Age.025" = ages[1,], 
                    "Age.med" = ages[2,], "Age.975" = ages[3,])
save(post.DDL, file = "bigout/ageModDDL.rda")
save(ad.DDL, file = "out/adDDL.rda")
