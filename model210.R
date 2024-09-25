model {
  
  # Data model
  ## pb210 c1: cdm index, c2: 210Pb obs, c3: 210Pb precision
  for(i in length(pb210[, 1])){
    pb210[i, 2] ~ dnorm(pb210_m[i], pb210[i, 3])
    
    pb210_m[i] = pb[pbo[i]]
    pbo[i] = pb.order[i, 1]
    pb.order[i, 1:nt] = order(cm.off[i,])
    for(j in 1:nt){
      cm.off[i, j] = abs(pb210[i, 1] - cm[j])
    }
  }
 
  for(i in 1:nt){
    pb[i] = dr / ar[i] * exp(-lam * i * ts)
  }
   
  for(i in 2:nt){
    cm[i] = cm[i - 1] + ar[i] * ts
    
    ar[i] = ar[i - 1] * (ar.eps[i] + 1)
    ar.eps[i] ~ dnorm(ar.eps[i - 1] * ar.phi, ar.pre)
  }
  
  cm[1] = ar[1] * ts
  
  ar.eps[1] = 0
  
  dr ~ dgamma(4, 1e1)
  
  ar[1] ~ dgamma(5, 1e2)
  ar.phi ~ dbeta(50, 2)
  ar.pre ~ dgamma(10, 1)
  
  lam = 0.03114
  
  # Input:
  # ts = timestep in years
  # nt = number of timesteps
}