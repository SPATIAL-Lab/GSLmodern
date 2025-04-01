Omod = function(EI, tc){
  # Evaporation conditions taken from https://doi.org/10.1002/2016WR019104 
  tmp = tc + 273.15
  h = 0.5
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
  ## Aragonite precipitation temperature
  temp.ag = tc + 273.15
  
  ## Kim et al 2007 fractionation factor https://doi.org/10.1016/j.gca.2007.04.019
  alpha.ag = exp(17.88 / temp.ag - 0.03114)
  epsilon.ag = (alpha.ag - 1) * 1000

  ## d18Oc as VSMOW
  d18Oc.VSMOW = (d18Ow + epsilon.ag) * alpha.ag
  
  ## d18Oc as VPDB
  d18Oc = 0.97001 * d18Oc.VSMOW - 29.99
 
  return(data.frame(EI, d18Ow, d18Oc))
}
