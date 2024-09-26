model {
  
  # Data model
  ## pb210 c1: cdm index, c2: 210Pb obs, c3: 210Pb precision
  for(i in 1:length(pb210[, 1])){
    pb210[i, 2] ~ dnorm(pb[pb210[i, 1]], pb210[i, 3])
    mass[i, 2] ~ dnorm(cdm[mass[i, 1]], mass[i, 3])
  }
 
  for(i in 1:nd){
    pb[i] = dr / ar[i] * exp(-lam * age[i])
  }
   
  for(i in 2:nd){
    
    # Age years before 2007
    age[i] = age[i - 1] + ds * dens[i] / ar[i]

    # Accumulation rate g / cm2 / year
    ar[i] = ar[i - 1] * (ar.eps[i] + 1)
    ar.eps[i] ~ dnorm(0, ar.pre)
    
    # Cumulative mass g / cm2
    cdm[i] = cdm[i - 1] + dens[i] * ds
    
    # Density g / cm3
    dens[i] = dens[i - 1] * (dens.eps[i] + 1)
    dens.eps[i] ~ dnorm(0, dens.pre)
  }
  
  age[1] = ds * dens[1] / ar[1]
  cdm[1] = dens[1] * ds
  
  ar.eps[1] = 0
  dens.eps[1] = 0
  
  dr ~ dunif(0.1, 0.7)
  
  ar[1] ~ dunif(0.01, 0.2)
  ar.pre ~ dgamma(10, 0.01)

  dens[1] ~ dunif(0.1, 0.6)
  dens.pre ~ dgamma(20, 0.01)
  
  lam = 0.03114
  
  # Input:
  # ds = depth-step in cm
  # nd = number of depth steps
}