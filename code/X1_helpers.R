add.d13C = function(rd, sd){
  rd$d13Cc = rep(0)
  for(i in seq_along(rd$Depth.mean)){
    doff = abs(sd$Depth.mean - rd$Depth.mean[i])
    rd$d13Cc[i] = sd$d13C.carb[match(min(doff), doff)]
  }
  return(rd)
}

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

dvec = function(bd, ds){
  # Depth vector from basal depth and step size
  depths = round(seq(ds, bd, by = ds), 1)
  return(depths)
}

reage = function(a){
  # Age function for Fig 1 x-axis
  return(sqrt(2007 - a))
}

# Plot colors
alpha = 100
qbg = "cadetblue"
qbg.t = col2rgb(qbg)
qbg.t = rgb(qbg.t[1], qbg.t[2], qbg.t[3], alpha, maxColorValue = 255)
lbg = "coral2"
lbg.t = col2rgb(lbg)
lbg.t = rgb(lbg.t[1], lbg.t[2], lbg.t[3], alpha, maxColorValue = 255)
obg = "palegreen3"
mbg = "darkgoldenrod2"
ybg = "red3"
