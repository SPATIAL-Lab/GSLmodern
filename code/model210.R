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
    age[i] = age[i - 1] + ds * rho[i] / ar[i]
    
    # Accumulation rate g / cm2 / year
    ar[i] = ar[i - 1] * (ar.eps[i] + 1)
    ar.eps[i] ~ dnorm(0, ar.pre)
    
    # Cumulative mass g / cm2
    cdm[i] = cdm[i - 1] + rho[i] * ds
    
  }
  
  age[1] = ds * rho[1] / ar[1]
  cdm[1] = rho[1] * ds
  
  dr ~ dunif(0.1, 0.7)
  
  ar[1] ~ dunif(0.01, 0.2)
  ar.eps[1] = 0
  ar.pre ~ dgamma(10, 0.01)

  for(i in 1:nd){
    
    # Density g / cm3
    rho[i] = rho.0 * (1 - phi[i]) / (1 - phi.0)
    phi[i] = phi.0 * exp(-i * ds / z.c)
    
  }
  
  rho.0 ~ dgamma(30, 100)
  z.c ~ dgamma(8, 1)
  phi.0 ~ dbeta(10, 6)
  
  lam = 0.03114
  
  # Input:
  # ds = depth-step in cm
  # nd = number of depth steps
}