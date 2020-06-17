setwd("//nercbuctdb.ad.nerc.ac.uk/projects1/NEC03642_Mapping_Ag_Emissions_AC0112/NAEI_data_and_SNAPS")



setwd("C:/FastProcessingSam/Git_repos/EMEP_inputs")

source("./workspace.R")
source("./emissions_functions.R")


###########################################################
####                                                   ####
####    THIS MASTER SCRIPT WILL TAKE NAEI EMISSIONS    ####
#### FILES ALREADY CONVERTED TO LATLONG FOR THE UK     ####
####  0.01 DEGREE AND OUTPUT EMEP4UK NETCDF FILES:     ####
####  THESE ARE REQUIRED IN GNFR AND ALL POLLUTANTS    ####
####       FOR ONE GIEVN YEAR IN A SINGLE NETCDF       ####
####                                                   ####
###########################################################


# coordinate reference systems required #
LL <- CRS("+init=epsg:4326") # For EMEP European data

# This is the lat long equivalent raster of the UK domain at 1km in BNG
uk.latlon.grid <- raster(xmn = -13.6, xmx = 3.6, ymn = 49.5, ymx = 61.1, res = 0.01, crs = LL, vals = NA)

# mask the UK emissions data to terrestrial (plus some coastal cells) - Massimo wants EMEP emissions data on the sea
# the mask is in 0.1 degree, disaggregate to 0.01 so masking can be done
mask <- crop(extend(disaggregate(raster("Emissions_mask.tif"), fact=10), uk.latlon.grid), uk.latlon.grid)

### CHOOSE WHICH YEARS AND WHICH POLLUTANTS/GHGS TO PUT IN THE NETCDF ###
years <- 2016
pollutants <- c("nox","sox")
mapping.yr <- 2017 # i.e. what year is the NAEI spatial distribution for the data

#### PROCESSING ####

# 1. Take the UK NAEI emissions in Lat Long and;
        #  i) combine the point and diffuse data as required for the model 
        # ii) convert from SNAP sector to GNFR classification

reclassified.data <- NAEI.LL.to.GNFR(years = years, pollutants = pollutants, uk.latlon.grid = uk.latlon.grid, mapping.yr = mapping.yr)

# 2. Check if the netcdf exists and if it does, whether to insert new data or leave
reclassified.data.adj <- check.netcdf.status(reclassified.data = reclassified.data, years = years, pollutants = pollutants, uk.latlon.grid = uk.latlon.grid, mapping.yr = mapping.yr)

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
