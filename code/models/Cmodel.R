Cmod = function(Fatm, tc){
  # Carbon source isotope endmembers
  ## Terrestrial organic carbon CO2, lowest charcoal and seed from https://doi.org/10.1017/RDC.2019.62
  d13C.org = -24.25
  
  ## Preindustrial atmosphere
  d13C.atm = -6.5
  
  ## Assumed calcite precipitation temperature
  tmp = tc + 273.15
  
  ## CO2 - calcite equilibrium 
  alpha = exp((-2.9880e6 / tmp ^ 2 + 7.6663e3 / tmp - 2.4612) / 1e3)
  epsilon = (alpha - 1) * 1000
  
  ## OC-derived calcite
  d13Cc.org = (d13C.org - epsilon) / alpha
  
  ## Atmospheric-derived calcite
  d13Cc.atm = (d13C.atm - epsilon) / alpha
  
  ## Carbonate d13C
  d13Cc = d13Cc.org * (1 - Fatm) + d13Cc.atm * Fatm
  
  return(data.frame(Fatm, d13Cc))
}
