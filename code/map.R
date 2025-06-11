library(terra)

# From https://opendata.gis.utah.gov/datasets/utahDNR::utah-quaternary-faults/about on 6/11/25
faults = vect("bigdata/Utah_Quaternary_Faults.shp")
plot(faults)

# From https://sgid-utah.opendata.arcgis.com/datasets/utah::utah-great-salt-lake-shoreline/about on 6/11/25
gsl = vect("data/GSLShoreline.shp")
names(gsl)
unique(gsl$ELEVATION)
plot(gsl)
plot(gsl[gsl$ELEVATION == 4200])

gsla = aggregate(gsl[gsl$ELEVATION == 4200])
plot(gsla)
