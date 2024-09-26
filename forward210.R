
# Timestep
ts = 0.1

# Accumulation rate (g / cm2 / year)
ar = rnorm(1e3, 0.05, 0.005)

# Accelerate in recent
ar[1:200] = ar[1:200] + seq(0.04, 0, length = 200)

# Unsupported 210Pb flux (drm / year)
dr = 0.4

# Space for US 210 concentration and cumulative mass
c.210 = cum.mass = rep(0, length(ar))

# Decay constant (1 / year)
lambda = 0.03114

for(i in seq_along(ar)){
  # Cumulative mass at time i
  cum.mass[i] = sum(ar[1:i]) * ts
  c.210[i] = dr / ar[i] * exp(-lambda * i * ts)
}

c.210 = c.210[order(cum.mass)]
cum.mass = sort(cum.mass)
plot(c.210, cum.mass, ylim = rev(range(cum.mass)))
plot(cum.mass)
