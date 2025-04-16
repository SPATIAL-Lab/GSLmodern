# Prepare 210Pb data for MCMC
prep.210 = function(d, depths){
  ## Depth vector properties
  nd = length(depths)
  ds = diff(depths)[1]
  
  ## Data for JAGS; convert to precision, generate cms index values
  pb210 = d[c("Depth", "pb210", "pb210.sd")]
  pb210$pb210.sd = 1 / pb210$pb210.sd ^ 2
  pb210$Depth = match(pb210$Depth, round(depths, 1))
  names(pb210) = c("depth.ind", "pb210", "pb210.pre")
  
  ## Depth/mass data, assume 5% uncertinaty in cdm
  mass = data.frame(pb210$depth.ind, d$cdm, 1 / (d$cdm * 0.05) ^ 2)
  names(mass) = c("depth.ind", "cdm", "cdm.pre")
  
  return(list(ds = ds, nd = nd, pb210 = pb210, mass = mass))
}

# Depth vector from basal depth and step size
dvec = function(bd, ds){
  depths = round(seq(ds, bd, by = ds), 1)
  return(depths)
}

