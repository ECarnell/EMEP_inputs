setwd("//nercbuctdb.ad.nerc.ac.uk/projects1/NEC03642_Mapping_Ag_Emissions_AC0112/NAEI_data_and_SNAPS")

source("./LL_Emissions_model/src/workspace.R")

setwd("C:/FastProcessingSam/Git_repos/EMEP_inputs")

source("./workspace.R")


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
years <- 2017
pollutants <- c("nox")

#### PROCESSING ####

# 1. Take the UK NAEI emissions, converted to LL, and convert from SNAP sector to GNFR classification




########################################


st <- stack()

for(sn in paste0("S",1:11)){
  
  r <- raster(paste0("C:/FastProcessingSam/EMEP_new_grid/nox_2017_",sn,"_t_cell_0.01_LL_2017NAEImap.tif"))
  rm <- mask(r, mask)
  
  names(rm) <- sn
  
  st <- stack(st, rm)
  
}

names(st)






# my netcdf
filename = paste0("test_uk_ncdf_2017.nc")

# generate lons, lats and set time
lonvals <- as.array(seq(-13.595,3.595,0.01))
nlon <- length(lonvals)
latvals <- as.array(seq(49.505,61.095,0.01))
nlat <- length(latvals)

#secvals <- as.array(1:11)
secvals <- 1:11
nsecs <- length(secvals)

timevals <- 1
ntime <- 1


# create dims
dimlon <- ncdim_def(name = "lon", longname= "longitude", units = "degrees", vals = lonvals)
dimlat <- ncdim_def(name = "lat", longname= "latitude", units = "degrees", vals = latvals)
dimtime <- ncdim_def(name = "time", longname = "years since 2017", units = "years", vals = timevals)
dimsecs <- ncdim_def(name = "sectors", longname = "emission SNAP sectors", units = "SNAP number", vals = secvals)

# data array
a <- array(flip(st,2), dim = c(nlon, nlat, ntime, nsecs ))



# create variables
fillvalue <- -9999

nox_def <- ncvar_def(name = "nox", 
                     units = "Mg cell-1 yr-1", 
                     dim = list(dimlon,dimlat,dimtime,dimsecs), 
                     missval = fillvalue,
                     longname = "nox emissions for UK terrestrial domain",
                     prec = "float",
                     compression=4)


vars <- list(nox_def)

ncnew <- nc_create(filename, vars)

ncvar_put(ncnew, nox_def, a) 



ncatt_put(ncnew,"sectors","SNAP names","1: energy; 2: domestic; 3: ind; 4: proc; 5: solv; 6: fugi; 7: road; 8: trans; 9: waste; 10: agri; 11: other")
ncatt_put(ncnew,"lon","axis","X") #,verbose=FALSE) #,definemode=FALSE)
ncatt_put(ncnew,"lat","axis","Y")
ncatt_put(ncnew,"time","axis","T")
#ncatt_put(ncnew,"sectors","snap_names",letters[1:11], prec="char")



ncatt_put(ncnew, 0, "Conventions","CF-1.0", prec="char")
ncatt_put(ncnew, 0, "projection","WGS84", prec="char")
ncatt_put(ncnew, 0, "Grid_resolution", "0.01", prec="char")
ncatt_put(ncnew, 0, "Created_with", R.Version()$version.string, prec="char")
ncatt_put(ncnew, 0, "ncdf4_version", packageDescription("ncdf4")$Version, prec="char")
ncatt_put(ncnew, 0,"Created_by","Sam Tomlinson samtom@ceh.ac.uk", prec="char")
ncatt_put(ncnew, 0, "Created_date", as.character(Sys.time()), prec="char")
ncatt_put(ncnew, 0, "Sector_names", "GNFR", prec="char")

#zr <- lapply(X = v_lkup$Sect,function(x){
#  ncatt_put(ncnew,0,x,v_lkup[Sect == x, Desc],prec="char")})



nc_close(ncnew)




#####


ncfile <- "test_uk_ncdf_nox_2017.nc"
nc <- nc_open(ncfile)

names(nc$var)

st2 <- brick(ncfile, names(nc$var)[1])
plot(st2)

attributes(nc)$names

attributes(nc$dim)$names
attributes(nc$dim$sectors)

ncatt_get()

nc$dim$sectors

ncatt_get(nc, "sectors", "SNAP names")$value

nc_close(nc)
