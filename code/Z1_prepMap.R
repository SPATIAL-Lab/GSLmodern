# Prep map layers - not portable

library(terra)
library(assignR)

# Downloaded from 
# https://sgid-utah.opendata.arcgis.com/datasets/utah::utah-great-salt-lake-shoreline/about 
# on 6/11/25
gsl = vect("data/GSLShoreline.shp")
gsl = aggregate(gsl[gsl$ELEVATION == 4200])

# Downloaded from 
# https://opendata.gis.utah.gov/datasets/utahDNR::utah-quaternary-faults/about 
# on 6/11/25
faults = vect("bigdata/Utah_Quaternary_Faults.shp")

faults = project(faults, gsl)
faults = intersect(faults, gsl)
faults = faults[faults$FaultAge == "<15,000"]

sites = read.csv("data/Sites.csv")
sites = vect(sites, geom = c("Longitude", "Latitude"), crs = "WGS84")
sites = project(sites, gsl)

ut = states[states$STATE_NAME == "Utah"]
ut = project(ut, gsl)

# Get bathymetric DEM elevations, downloaded from
# http://www.hydroshare.org/resource/582060f00f6b443bb26e896426d9f62a
# on 6/11/25, then masked and projected
bath = rast("bigdata/GSLDEM.tif")
sites$Elev = extract(bath, sites, ID = FALSE)
sites$Depth = 4200 * 0.3048 - sites$Elev
