add.d13C = function(rd, sd){
  rd$d13Cc = rep(0)
  for(i in seq_along(rd$Depth.mean)){
    doff = abs(sd$Depth.mean - rd$Depth.mean[i])
    rd$d13Cc[i] = sd$d13C.carb[match(min(doff), doff)]
  }
  return(rd)
}

