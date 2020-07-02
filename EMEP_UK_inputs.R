setwd("//nercbuctdb.ad.nerc.ac.uk/projects1/NEC03642_Mapping_Ag_Emissions_AC0112/NAEI_data_and_SNAPS")



source("N:/dump/EMEP_inputs/workspace.R")
source("N:/dump/EMEP_inputs/emissions_functions.R")

###########################################################
####                                                   ####
####    THIS MASTER SCRIPT WILL TAKE NAEI EMISSIONS    ####
#### FILES ALREADY CONVERTED TO LATLONG FOR THE UK     ####
####  0.01 DEGREE AND OUTPUT EMEP4UK NETCDF FILES:     ####
####  THESE ARE REQUIRED IN GNFR AND ALL POLLUTANTS    ####
####       FOR ONE GIEVN YEAR IN A SINGLE NETCDF       ####
####                                                   ####
###########################################################

### CHOOSE WHICH YEARS AND WHICH POLLUTANTS/GHGS TO PUT IN THE NETCDF ###
years <- 2016:2017
pollutants <- c("nox","sox","","","","")
mapping.yr <- 2017 # i.e. what year is the NAEI spatial distribution for the data
region <- "ukeire" # 'uk' = UK only, 'eire' = Eire only, 'ukeire' = UK and Eire combined
class <- "GNFR" # Sector classification system: 'SNAP' or 'GNFR'

#### PROCESSING ####

# 1. For years and pollutants - check if the netcdf exists and if it does, whether that pollutant is in it
yr.poll.subset <- check.netcdf.status(years = years, pollutants = pollutants, mapping.yr = mapping.yr, region = region)

# 2. For the remaining years & pollutants, take the regional emissions in Lat Long and;
       #   i) convert point emissions (.csv) into a raster
       #  ii) combine the points with the diffuse (.tif) data as required for the model 

pt.diff.data <- combine.all.region.data(yr.poll.subset = yr.poll.subset, mapping.yr = mapping.yr, class = class, region = region) # c. 2.5 mins per year & pollutant

# 3. Using the newly classified GNFR data, place into a netcdf (either existing or new)
input.to.netcdf(pt.diff.data = pt.diff.data, years = unique(yr.poll.subset[,years]), pollutants = unique(yr.poll.subset[,pollutants]), yr.poll.subset = yr.poll.subset, mapping.yr = mapping.yr, class = class, region = region)


########################################

