setwd("//nercbuctdb.ad.nerc.ac.uk/projects1/NEC03642_Mapping_Ag_Emissions_AC0112/NAEI_data_and_SNAPS")



source("C:/FastProcessingSam/Git_repos/EMEP_inputs/workspace.R")
source("C:/FastProcessingSam/Git_repos/EMEP_inputs/emissions_functions.R")

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
pollutants <- c("nox", "sox")
mapping.yr <- 2017 # i.e. what year is the NAEI spatial distribution for the data
region <- "ukeire" # 'uk' = UK only, 'eire' = Eire only, 'ukeire' = UK and Eire combined
class <- "GNFR" # Sector classification system: 'SNAP' or 'GNFR'

#### PROCESSING ####

# 1. Take the regions emissions in Lat Long and;
       #   i) convert point emissions (.csv) into a raster
       #  ii) combine the points with the diffuse (.tif) data as required for the model 

pt.diff.data <- combine.all.region.data(years = years, pollutants = pollutants, mapping.yr = mapping.yr, class = class, region = region)

# 2. Check if the netcdf exists and if it does, whether to insert new data or leave
check.netcdf.status(pt.diff.data = pt.diff.data, years = years, pollutants = pollutants, mapping.yr = mapping.yr, region = region)

# 3. Using the newly classified GNFR data, place into a netcdf (either existing or new)
input.to.netcdf(reclassified.data = reclassified.data, years = years, pollutants = pollutants, mapping.yr = mapping.yr)


########################################


st <- stack()

for(sn in paste0("S",1:11)){
  
  r <- raster(paste0("C:/FastProcessingSam/EMEP_new_grid/nox_2017_",sn,"_t_cell_0.01_LL_2017NAEImap.tif"))
  rm <- mask(r, mask)
  
  names(rm) <- sn
  
  st <- stack(st, rm)
  
}

names(st)







#####


ncfile <- paste0("C:/FastProcessingSam/EMEP_new_grid/EMEP4UK_UKems_",year,"_0.01.nc")
nc <- nc_open(ncfile)

names(nc$var)

st2 <- brick(ncfile, names(nc$var)[1])
plot(st2)

attributes(nc)$names

attributes(nc$dim)$names
attributes(nc$dim$sectors)

ncatt_get()

nc$dim$sectors

ncatt_get(nc, "sectors", "GNFR names")$value

nc_close(nc)
