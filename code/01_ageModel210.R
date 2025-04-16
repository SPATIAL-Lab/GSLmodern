library(R2jags)
source("code/X1_prep210.R")

# Read and parse data from Oliver et al., 2009
d = read.csv("data/pb210.csv")
d.DDQ = d[grep("DDQ", d$ID),]
d.DDL = d[grep("DDL", d$ID),]

# Free parameters to report
vars = c("phi.0", "rho.0", "z.c", "ar[1]", "ar.pre", "dr")

# DDL ----
## Objects for analysis
depths = dvec(32, 0.1)
data210.DDL = prep.210(d.DDL, depths)
parms = c("ar.pre", "dr", "age", "ar", "pb", "cdm", "rho", "rho.0", "phi.0", "z.c")

## MCMC
post.DDL = jags.parallel(data210.DDL, NULL, parms, "code/models/model210.R", n.burnin = 0.5e4,
                     n.iter = 1.5e4, n.chains = 3, n.thin = 10)

## Check results
post.DDL$BUGSoutput$summary[row.names(post.DDL$BUGSoutput$summary) %in% vars,]
min(post.DDL$BUGSoutput$summary[, "n.eff"])

## Save
save(data210.DDL, file = "out/data210_DDL.rda")

ages = apply(post.DDL$BUGSoutput$sims.list$age, 2, quantile, c(0.025, 0.5, 0.975))
ad.DDL = data.frame("Depth" = depths, "Age.025" = ages[1,], 
                    "Age.med" = ages[2,], "Age.975" = ages[3,])
save(ad.DDL, file = "out/adDDL.rda")

if(!dir.exists("bigout")) dir.create("bigout")
save(post.DDL, file = "bigout/ageModDDL.rda")

# DDQ ----
## Objects for analysis
data210.DDQ = prep.210(d.DDQ, depths)

## MCMC
post.DDQ = jags.parallel(data210.DDQ, NULL, parms, "code/models/model210.R", n.burnin = 1e4,
                          n.iter = 2e4, n.chains = 3, n.thin = 10)

## Check results
post.DDQ$BUGSoutput$summary[row.names(post.DDQ$BUGSoutput$summary) %in% vars,]
min(post.DDQ$BUGSoutput$summary[, "n.eff"])

## Save
save(data210.DDQ, file = "out/data210_DDQ.rda")

ages = apply(post.DDQ$BUGSoutput$sims.list$age, 2, quantile, c(0.025, 0.5, 0.975))
ad.DDQ = data.frame("Depth" = depths, "Age.025" = ages[1,], 
                    "Age.med" = ages[2,], "Age.975" = ages[3,])
save(ad.DDQ, file = "out/adDDQ.rda")

if(!dir.exists("bigout")) dir.create("bigout")
save(post.DDQ, file = "bigout/ageModDDQ.rda")
