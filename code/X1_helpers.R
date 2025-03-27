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

clines = function(s){
  for(i in 1:2){
    for(j in (i+1):3){
      lines(s$d13Cc[c(i, j)], s$FmA[c(i, j)], lwd = 2)
    }
  }
  
  # Index and dimensions
  inds = c(1, 2, 3, 1)
  fracs = seq(0, 1, by = 0.1)
  xs = ys = matrix(nrow = length(fracs), ncol = 3)

  # Ternary line vertices
  for(i in 1:3){
    j = inds[i + 1]
    xs[, i] = s$d13Cc[i] + fracs * (s$d13Cc[j] - s$d13Cc[i])
    ys[, i] = s$FmA[i] + fracs * (s$FmA[j] - s$FmA[i])
  }  

  labs = c("Rock", "Atm", "Org")
  
  for(i in 1:3){
    for(j in seq_along(fracs)){
      lines(c(xs[j, inds[i]], xs[length(fracs) + 1 - j, inds[i + 1]]), 
              c(ys[j, inds[i]], ys[length(fracs) + 1 - j, inds[i + 1]]), 
              col = "grey30", lty = 3)
    }
    dy = diff(c(ys[1, inds[i]], ys[length(fracs), inds[i + 1]])) / 
      diff(par("usr")[3:4])
    dx = diff(c(xs[1, inds[i]], xs[length(fracs), inds[i + 1]])) / 
      diff(par("usr")[1:2])
    if(dy == 0){
      inbds = seq_along(fracs)[ys[, i] > par("usr")[3] & fracs != 0]
      text(rep(par("usr")[1]), ys[inbds, i], 
           paste0(fracs[inbds] * 100, "% ", labs[i]), 
           cex = 0.6, adj = c(-0.1, -0.2))
    } else if(dy / dx > 0){
      inbds = seq_along(fracs)[xs[, i] > par("usr")[1] & fracs != 0]
      text(xs[inbds, i], ys[inbds, i], 
           paste0(fracs[inbds] * 100, "% ", labs[i]), 
           cex = 0.6, adj = c(-0.1, 1), srt = 180 * atan2(dy, dx) / pi + 180)
    } else{
      inbds = seq_along(fracs)[ys[, i] > par("usr")[3] & fracs != 0]
      text(xs[inbds, i], ys[inbds, i], 
           paste0(fracs[inbds] * 100, "% ", labs[i]), 
           cex = 0.6, adj = c(-0.2, 0.5), srt = 180 * atan2(-dy, -dx) / pi)
    }
  }
}
