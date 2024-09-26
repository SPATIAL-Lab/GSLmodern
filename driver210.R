library(R2jags)

ts = 0.5
nt = 200 / ts

pb210 = data.frame(c(0.152, 0.509, 1.063, 1.818, 2.536, 3.173, 3.832, 4.551, 5.314),
                  c(10.91, 8.35, 2.15, 0.08, 0.65, 0.28, 0.09, 0.20, 0.12),
                  c(1.31, 1.09, 0.47, 0.29, 0.34, 0.32, 0.28, 0.28, 0.3))
pb210[, 3] = 1 / pb210[, 3] ^ 2

data = list("ts" = ts, "nt" = nt, "pb210" = pb210)

parms = c("ar.pre", "ar.phi", "dr", "cm", "pb")

post = jags(data, NULL, parms, "model210.R")
