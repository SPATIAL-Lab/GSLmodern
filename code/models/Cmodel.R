Cmod = function(Fatm, Forg, tc){
  # Rock fraction as difference
  Frox = 1 - Fatm - Forg
  
  # CO2 source isotope endmembers
  ## Terrestrial organic carbon CO2, lowest charcoal and seed from https://doi.org/10.1017/RDC.2019.62
  d13C.org = -24.25
  
  ## Preindustrial atmosphere
  d13C.atm = -6.5
  
  ## Assumed calcite precipitation temperature
  tmp = tc + 273.15
  
  ## CO2 - aragonite equilibrium, https://doi.org/10.1016/0016-7037(92)90142-6
  epsilon = -13.88 + 0.13 * tc
  alpha = epsilon / 1000 + 1
  
  ## OC-derived aragonite
  d13Cc.org = (d13C.org - epsilon) / alpha
  
  ## Atmospheric-derived aragonite
  d13Cc.atm = (d13C.atm - epsilon) / alpha
  
  ## Carbonate d13C
  d13Cc = d13Cc.org * Forg + d13Cc.atm * Fatm + 1 * Frox 
  
  return(data.frame(Fatm, Forg, d13Cc))
}
