# GSLmodern
Data and code for manuscript on sedimentary isotope record of human impacts to Great Salt Lake, UT

## code/
Scripts used for data analysis and plotting

- **X1_prep210.R** Helper functions for 210Pb modeling
- **X2_prep14C.R** Helper functions for 14C reservoir age modeling
- **X3_prepFigs.R** Helper functions for plotting
- **Z1_prepMap.R** Helper code for preparing map objects used in Figure 1; not portable due to use of large data sets not included in the repository (see code for data sources)
- **01_ageModel210.R** Fit 210Pb age models
- **02_reservoirAges.R** Calculate reservoir ages
- **11_Figure1.R** Plot Fig 1
- **12_Figure2.R** Plot Fig 2
- **13_Figure3.R** Plot Fig 3
- **14_FigureS1.R** Plot Fig S1
- **15_FigureS2.R** Plot Fig S2
- **models/Cmodel.R** Carbon isotope mixing model for GSL DIC
- **models/Omodel.R** Oxygen isotope mass balance model for GSL water
- **models/model210.R** Unsupported 210Pb age model 

## data/
Data assets used in analysis

- **bombCurve.csv** Calibration curve data from [Hua, et al. (2022)](https://doi.org/10.1017/RDC.2021.95 )
- **GSL_brines.csv** GSL brine chemistry data from [Utah Geological Survey](https://geology.utah.gov/map-pub/data-databases/)
- **intcal20.14C** [IntCal20](https://intcal.org/curves/intcal20.14c) calibration curve data
- **longCarb.csv** New carbonate isotope data from the long (Holocene) GLAD1B core
- **longReservoir.csv** 14C data and reservoir age estimates for the GLAD1B core from [Bowen, et al. (2019)](https://doi.org/10.1017/RDC.2019.62)
- **pb210.csv** 210Pb and physical property data for Common Era cores from [Oliver, et al. (2009)](https://doi.org/10.1016/j.apgeochem.2009.02.023)
- **shortAll.csv** New isotope data from short (Common Era) cores

## out/
Objects output by code, including data products and figures
