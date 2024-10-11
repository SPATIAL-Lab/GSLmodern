Omod = function(EI, tc){
  # Evaporation conditions taken from https://doi.org/10.1002/2016WR019104 
  tmp = 13 + 273.15
  h = 0.51
  dP = -16
  
  # Steady state model from https://doi.org/10.1016/j.jhydrol.2015.02.010 
  ## Fractionation factors
  alphaplus = exp((-7.685 + 6.7123e3 / tmp - 1.6664e6 / tmp ^ 2 + 0.35041e9 / tmp ^ 3) / 1e3)
  eplus = (alphaplus - 1) * 1e3
  epsilonK = (1 - h) * 14.2
  epsilon = eplus / alphaplus + epsilonK
  
  ## Slope parameter
  m = (h - epsilon / 1000) / (1 - h + epsilonK / 1000)
  
  ## Atmospheric d18O, equilibrium assumption
  dA = (dP - eplus) / alphaplus
  
  ## Limiting d18O
  dS = (h * dA + epsilon) / (h - epsilon / 1e3)
  
  ## Lake water d18O
  d18Ow = (EI * m * dS + dP) / (1 + EI * m)

  # Carbonate d18O
  ## Calcite precipitation temperature
  temp.cal = tc + 273.15
  
  ## Kim and O'Neil 1997 fractionation factor https://doi.org/10.1016/S0016-7037(97)00169-5
  alpha.cal = exp(18.03 / temp.cal - 0.03242)
  epsilon.cal = (alpha.cal - 1) * 1000
  
  ## d18Oc as VSMOW
  d18Oc.VSMOW = (d18Ow + epsilon.cal) * alpha.cal
  
  ## d18Oc as VPDB
  d18Oc = 0.97001 * d18Oc.VSMOW - 29.99
 
  return(data.frame(EI, d18Ow, d18Oc))
}
